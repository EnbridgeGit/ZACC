REPORT zacc_gpfxml_load MESSAGE-ID zacc LINE-SIZE 80.

***********************************************************************
*  Loads an Accuity GPF bank data file.                               *
***********************************************************************
*  06/12/2007  Thomas Kaciuba, Steria Mummert Consulting AG / TDI     *
*  30/01/2008  Thomas Kaciuba: Added progress messages in job log.    *
*  28/04/2009  Anton Wieser: Added Region and new country association *
*                            table (zacc_bic)                         *
*  09/11/2009  Anton Wieser: Added key handling mode C (Bank number)  *
*  18/12/2009  Thomas Kaciuba: Added user-exit concept and sample     *
*  15/03/2010  Anton Wieser: v 2.3: add parameter 'Only delete unused *
*                                   banks'                            *
*  10/03/2014  Thomas Kaciuba: fix to handle add/delete bank entries  *
*  18/07/2014  Thomas Kaciuba: v 2.4 add local language load option   *
*  21/08/2014  Thomas Kaciuba: test and fix standard address versions *
*  09/02/2016  Thomas Kaciuba, v2.5: added S_TCODE authority check    *
*              Changed LFBK/KNBK access to dynamic read to allow for  *
*              program execution on systems without LFBK/KNBK.        *
*  23/02/2016  Thomas Kaciuba: v2.6: Extended user exit to support    *
*              NAME2 in CAM address                                   *
*  21/10/2016  Thomas Kaciuba: v2.7: Added option to skip bank types  *
*  11/10/2017  Thomas Kaciuba: v2.8: Added option to remove XXX pad-  *
*                                    ding from Swift BIC and to split *
*                                    long bank names                  *
***********************************************************************

TABLES: bnka, zacc_map,
        zacc_regi, zacc_bic, t005s,      " 27/04/2009 AW
        tsadvc, zacc_local_lang,                            " TK180714
        zacc_bank_code,                                     " TK211016
        zacc_default.                                       " TK231017

***********************************************************************
*  Selection Screen                                                   *
***********************************************************************

* File name and type
SELECTION-SCREEN BEGIN OF BLOCK fil WITH FRAME TITLE TEXT-001.
PARAMETERS: p_fname LIKE rlgrap-filename OBLIGATORY,
            p_xupdt RADIOBUTTON GROUP ftyp DEFAULT 'X',
            p_xfull RADIOBUTTON GROUP ftyp,
            p_xpart RADIOBUTTON GROUP ftyp,
            p_xlang RADIOBUTTON GROUP ftyp.
SELECTION-SCREEN END OF BLOCK fil.

* Data selection
SELECTION-SCREEN BEGIN OF BLOCK sel WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS: p_banks FOR bnka-banks,
                p_bankl FOR bnka-bankl,
                p_swift FOR bnka-swift.
PARAMETERS: p_xpref AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK sel.

* Processing options
SELECTION-SCREEN BEGIN OF BLOCK opt WITH FRAME TITLE TEXT-003.
PARAMETERS: p_keyhd TYPE zacc_keyhd OBLIGATORY DEFAULT 'A',
            p_cnter TYPE i DEFAULT '0',
            p_xaddr AS CHECKBOX DEFAULT space,
            p_xname AS CHECKBOX DEFAULT space,
            p_xldel AS CHECKBOX DEFAULT 'X',
            p_xudel AS CHECKBOX DEFAULT space,  " 15/03/2010 AW
            p_xtrun AS CHECKBOX DEFAULT 'X',
            p_xrxxx AS CHECKBOX DEFAULT space,
            p_xtest AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK opt.

* Batch Input Session handling
SELECTION-SCREEN BEGIN OF BLOCK bis WITH FRAME TITLE TEXT-004.
PARAMETERS: p_mappe LIKE apqi-groupid DEFAULT 'ZACC_GPFLOAD',
            p_xkeep AS CHECKBOX DEFAULT 'X',
            p_xexec AS CHECKBOX DEFAULT 'X',
            p_ximme AS CHECKBOX DEFAULT space,
            p_rtime TYPE t.
SELECTION-SCREEN END OF BLOCK bis.

***********************************************************************
*  Data Declaration                                                   *
***********************************************************************

TYPE-POOLS: ixml.
CLASS cl_ixml DEFINITION LOAD.

* iXML Library - XML file parsing
DATA: o_ixml          TYPE REF TO if_ixml,
      o_streamfactory TYPE REF TO if_ixml_stream_factory,
      o_stream        TYPE REF TO if_ixml_istream,
      o_parser        TYPE REF TO if_ixml_parser,
      o_parseerror    TYPE REF TO if_ixml_parse_error,
      o_document      TYPE REF TO if_ixml_document,
      o_event         TYPE REF TO if_ixml_event,
      v_event_sub     TYPE i.

* Application log management
DATA: v_loghd TYPE bal_s_log,
      v_logid TYPE balloghndl,
      v_logms TYPE bal_s_msg,
      v_logdp TYPE bal_s_prof,
      t_logid TYPE bal_t_logh.

* Current bank data from file
DATA: BEGIN OF v_bank,
        action       TYPE string,
        country      TYPE string,
        keyval       TYPE string,
        name         TYPE string,
        countrycsp   TYPE string, " 28/04/2009 AW
        city         TYPE string,
        bic          TYPE string,
        bicpref      TYPE string,
        natid        TYPE string,
        natidpref    TYPE string,
        natidtype    TYPE string,
        branch       TYPE string,
        address      TYPE string,
        postcode     TYPE string,
        pobox        TYPE string,
        phone        TYPE string,
        fax          TYPE string,
        email        TYPE string,
        valid_action,
        sap_country  LIKE t005-land1,
        sap_bnkey    LIKE t005-bnkey,
        sap_keyval   LIKE bnka-bankl,
        sap_swift    LIKE bnka-swift,
        sap_region   LIKE t005s-bland, " 28/04/2009 AW
        sap_adver    TYPE ad_nation,                        "TK180714
        sap_xnoplz   TYPE xfeld,                            "TK290714
        sap_name2    TYPE ad_name2,                         "TK230216
        sap_name3    TYPE ad_name2,                         "TK231017
        sap_name4    TYPE ad_name2,                         "TK231017
      END OF v_bank,
      cur_element TYPE string.

* Batch Input Session Handling.
DATA: v_biqid LIKE apqi-qid.

* Bank tracking for full initial load
DATA: BEGIN OF t_banks OCCURS 0,
        banks TYPE bnka-banks,
        bankl TYPE bnka-bankl,
      END OF t_banks.
* TK100314: Deleted banks from current file
DATA: BEGIN OF t_delbnk OCCURS 0,
        banks TYPE bnka-banks,
        bankl TYPE bnka-bankl,
      END OF t_delbnk.

* Statistics
DATA: v_cntbk TYPE i,     "Banks analysed
      v_cntpr TYPE i,     "Banks processed
      v_cntcr TYPE i,     "Banks created
      v_cntup TYPE i,     "Banks updated
      v_cntnu TYPE i,     "Banks unchanged
      v_cntdr TYPE i,     "Banks reactivated
      v_cntds TYPE i,     "Banks already deleted
      v_cntdl TYPE i,     "Banks deleted
      v_cntdu TYPE i,     "Deleted banks still in use
      v_cntsk TYPE i,     "Banks skipped due to selection
      v_cntse TYPE i,     "Banks skipped due to errors
      v_cntbi TYPE i.     "Batch Input transactions in total
DATA: v_cnter TYPE i,
      v_cntls TYPE i.

* Other data
DATA: v_fname    TYPE string,
      v_tag      TYPE string,
      v_val      TYPE string,
      v_count    TYPE i,
      v_index    TYPE i,
      v_mod      TYPE i,
      v_len      TYPE i,
      v_buf(250), v_msg(100),
      v_tcode    TYPE tstc-tcode.
DATA: v_xcanc.
DATA: v_inuse.                             " 15/03/2010 AW

***********************************************************************
*  Macro Definitions                                                  *
***********************************************************************

* Log message to Application Log (internal source)
DEFINE _logmsg.
  _logmsgx &1 &2 &3 &4 &5 &6 'ZACC'.
END-OF-DEFINITION.

* Log message to Application Log (external source)
DEFINE _logmsgx.
* Write message to application log
  PERFORM log_message USING '&1' &2 &3 &4 &5 &6 &7.
* Actively monitor number of error messages and abort after maximum
  IF p_cnter > 0 AND '&1' = 'E'.
    ADD 1 TO v_cnter.
    IF v_cnter >= p_cnter.
      PERFORM log_message USING 'A' 217 space space space space 'ZACC'.
      v_xcanc = 'X'.
    ENDIF.
  ENDIF.
END-OF-DEFINITION.

* TK181209: Collect message in Application Log (display only once)
* Note: This is not meant to be used for error messages, so no count
*       is maintained on the number of messages passed through here.
DEFINE _logmsgc.
* Write message to application log
  PERFORM collect_message USING '&1' &2 &3 &4 &5 &6 'ZACC'.
END-OF-DEFINITION.

DEFINE _logflag.
  IF &2 = 'X'.
    v_msg = 'Yes'.
  ELSE.
    v_msg = 'No'.
  ENDIF.
  _logmsg i 201 &1 v_msg space space.
END-OF-DEFINITION.

DEFINE _logcnt1.
  WRITE &2 TO v_buf(20) LEFT-JUSTIFIED.
  _logmsg s &1 v_buf(20) space space space.
END-OF-DEFINITION.

DEFINE _logcnt2.
  WRITE &2 TO v_buf(20) LEFT-JUSTIFIED.
  WRITE &3 TO v_buf+20(20) LEFT-JUSTIFIED.
  _logmsg s &1 v_buf(20) v_buf+20(20) space space.
END-OF-DEFINITION.

DEFINE _logcnt3.
  WRITE &2 TO v_buf(20) LEFT-JUSTIFIED.
  WRITE &3 TO v_buf+20(20) LEFT-JUSTIFIED.
  WRITE &4 TO v_buf+40(20) LEFT-JUSTIFIED.
  _logmsg s &1 v_buf(20) v_buf+20(20) v_buf+40(20) space.
END-OF-DEFINITION.

DEFINE _validate_length.
  v_len = strlen( &1 ).
  IF v_len > &2.
    IF p_xtrun = 'X'.
      _logmsg w 230 &3 bank-sap_country bank-sap_keyval bank-keyval.
    ENDIF.
    CLEAR v_buf.
    v_buf(&2) = &1.
    &1 = v_buf.
  ENDIF.
END-OF-DEFINITION.

DEFINE _info.
  IF sy-batch IS INITIAL.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        text = &1.
  ENDIF.
END-OF-DEFINITION.

DEFINE _pmsg.
  IF sy-batch = 'X'.
    MESSAGE i101 WITH 'Current step:' &1.
  ENDIF.
END-OF-DEFINITION.

***********************************************************************
*  Main Program                                                       *
***********************************************************************

INITIALIZATION.
* Check transaction code authorisation
  SELECT tcode FROM tstc UP TO 1 ROWS INTO v_tcode WHERE pgmna = sy-repid.
  ENDSELECT.
  IF sy-subrc = 0.
    AUTHORITY-CHECK OBJECT 'S_TCODE' ID 'TCD' FIELD v_tcode.
    IF sy-subrc <> 0.
      MESSAGE e383(ra) WITH v_tcode.
    ENDIF.
  ENDIF.
* Set Swift BIC handling default
  SELECT SINGLE * FROM zacc_default.
  IF sy-subrc = 0.
    p_xrxxx = zacc_default-xrxxx.
  ELSE.
    CLEAR zacc_default.
  ENDIF.

AT SELECTION-SCREEN ON p_keyhd.
* Verify External Key Handling Mode
*  IF p_keyhd CN 'AB'.          " 09/11/2009 AW
  IF p_keyhd CN 'ABC'.          " 09/11/2009 AW
    MESSAGE e210 WITH p_keyhd.
  ENDIF.

AT SELECTION-SCREEN.
* If we are not doing a test run, batch input session name is a must
  IF p_xtest IS INITIAL AND p_mappe IS INITIAL.
    MESSAGE e228.
  ENDIF.
* TK180714: If we load a local language data file, we need to check a
*           few things
  IF p_xlang = 'X'.
*   Check if CAM address creation is active
    IF p_xaddr IS INITIAL.
      MESSAGE e242.
    ENDIF.
*   Check if an international version has been maintained
    SELECT SINGLE * FROM tsadvc WHERE active = 'X'.
    IF sy-subrc = 4.
      MESSAGE e243.
    ENDIF.
*   Check if an address version assignment has been maintained
    SELECT SINGLE l~land1 FROM zacc_local_lang AS l INNER JOIN tsadvc AS t
                            ON l~nation = t~nation
                          INTO zacc_local_lang-land1
                          WHERE t~active = 'X'.
    IF sy-subrc = 4.
      MESSAGE e244.
    ENDIF.
*   Notify superfluous flags
    IF p_xldel = 'X' OR p_xudel = 'X'.
      MESSAGE w245.
    ENDIF.
  ENDIF.
* Store default selection if it has changed
  IF p_xrxxx <> zacc_default-xrxxx.
    zacc_default-xrxxx = p_xrxxx.
    UPDATE zacc_default.
    IF sy-subrc <> 0.
      INSERT zacc_default.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON BLOCK bis.
* If automatic processing is at midmight, time may not have been set
  IF p_xtest IS INITIAL AND p_xexec = 'X' AND
     p_ximme IS INITIAL AND p_rtime IS INITIAL.
    MESSAGE w219.
  ENDIF.

AT SELECTION-SCREEN ON BLOCK fil.
* Warn that full initial load will delete banks not present in GPF file
  IF p_xfull = 'X'.
    MESSAGE w235.
  ENDIF.

AT SELECTION-SCREEN ON BLOCK opt.
* Warn if name split is selected, but CAM address is not
  IF p_xaddr IS INITIAL AND p_xname = 'X'.
    MESSAGE w252.
  ENDIF.

START-OF-SELECTION.
* Create an Application Log to handle all messages
  CLEAR: v_loghd, v_logid.
  v_loghd-extnumber = 'Accuity GPF XML File Load Log'.
  v_loghd-object    = 'ZACC'.
  v_loghd-subobject = 'IMPORT'.
  v_loghd-aldate    = sy-datum.
  v_loghd-altime    = sy-uzeit.
  v_loghd-aluser    = sy-uname.
  v_loghd-altcode   = sy-tcode.
  v_loghd-alprog    = sy-repid.
  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      i_s_log                 = v_loghd
    IMPORTING
      e_log_handle            = v_logid
    EXCEPTIONS
      log_header_inconsistent = 1
      OTHERS                  = 2.
  IF sy-subrc <> 0.
    MESSAGE e000 WITH 'Unable to create application log.'.
  ENDIF.
* Save selected parameters to log
  IF p_xtest = 'X'.
    _logmsg i 200 'Load GPF XML file (test run only)' space space space.
  ELSE.
    _logmsg i 200 'Load GPF XML file' space space space.
  ENDIF.
  v_len = strlen( p_fname ).
  IF v_len <= 50.
    _logmsg i 201 '<Import File>' p_fname space space.
  ELSE.
    v_len = v_len - 49.
    v_msg = '~'.
    v_msg+1 = p_fname+v_len.
    _logmsg i 201 '<Import File (shortened)>' v_msg space space.
  ENDIF.
  IF p_xupdt = 'X'.
    v_msg = 'Update file'.
  ELSEIF p_xfull = 'X'.
    v_msg = 'Full initial file'.
  ELSEIF p_xpart = 'X'.
    v_msg = 'Partial initial file'.
  ELSE.
    v_msg = 'Local language file'.
  ENDIF.
  _logmsg i 201 '<File Type>' v_msg space space.
  CLEAR v_msg.
  IF NOT p_banks[] IS INITIAL OR NOT p_bankl[] IS INITIAL OR
     NOT p_swift[] IS INITIAL.
    v_msg = 'X'.
  ENDIF.
  _logflag '<Bank Selection Active>' v_msg.
  _logflag '<Preferred Banks Only>' p_xpref.
  _logmsg i 201 '<Key Handling Mode>' p_keyhd space space.
  _logflag '<Maintain CAM Address>' p_xaddr.
  _logflag '<Split Long Bank Names>' p_xname.
  _logflag '<Log Deleted Banks in Use>' p_xldel.
  _logflag '<Only Delete Unused Banks>' p_xudel.  " 15/03/2010 AW
  _logflag '<Remove XXX from Swift BIC>' p_xrxxx.
  IF p_cnter > 0.
    WRITE p_cnter TO v_buf LEFT-JUSTIFIED.
    _logmsg i 201 '<Maximum error count>' v_buf space space.
  ENDIF.
* Read existing banks if full initial upload is requested
  REFRESH: t_banks, t_delbnk.
  IF p_xfull = 'X'.
    _info 'Reading SAP bank directory...'.
    SELECT banks bankl FROM bnka INTO TABLE t_banks
           WHERE banks IN p_banks
             AND bankl IN p_bankl
             AND swift IN p_swift
             AND loevm = space.
    IF sy-subrc <> 0.
      _logmsg w 236 space space space space.
    ELSE.
      SORT t_banks.          "so we can do a binary search later
    ENDIF.
  ENDIF.
* Create XML manager
  _info 'Initiating file processing...'.
  _pmsg 'Initiating file processing'.
  CLEAR v_xcanc.
  o_ixml = cl_ixml=>create( ).
  IF o_ixml IS INITIAL.
    _logmsg a 211 'Error creating XML manager' space space space.
    v_xcanc = 'X'.
  ENDIF.
* Create input stream
  IF v_xcanc IS INITIAL.
    o_streamfactory = o_ixml->create_stream_factory( ).
    IF o_streamfactory IS INITIAL.
      _logmsg a 211 'Error creating stream factory' space space space.
      v_xcanc = 'X'.
    ENDIF.
  ENDIF.
  IF v_xcanc IS INITIAL.
    CONCATENATE 'file://' p_fname INTO v_fname.
    o_stream = o_streamfactory->create_istream_uri( v_fname ).
    IF o_stream IS INITIAL.
      _logmsg a 211 'Error creating stream' space space space.
      v_xcanc = 'X'.
    ENDIF.
  ENDIF.
* Create document for DOM representation (not actively used later)
  IF v_xcanc IS INITIAL.
    o_document = o_ixml->create_document( ).
    IF o_document IS INITIAL.
      _logmsg a 211 'Error creating DOM document' space space space.
      v_xcanc = 'X'.
    ENDIF.
  ENDIF.
* Create XML parser
  IF v_xcanc IS INITIAL.
    o_parser = o_ixml->create_parser( stream_factory = o_streamfactory
                                      istream        = o_stream
                                      document       = o_document ).
    IF o_parser IS INITIAL.
      _logmsg a 211 'Error creating XML parser' space space space.
      v_xcanc = 'X'.
    ENDIF.
  ENDIF.
* Process the XML file
  IF v_xcanc IS INITIAL.
*   Let the parser know which events we want to handle
    v_event_sub = if_ixml_event=>co_event_element_pre +
                  if_ixml_event=>co_event_attribute_post +
                  if_ixml_event=>co_event_element_post.
    o_parser->set_event_subscription( events = v_event_sub ).
*   Explicitly disable DOM generation as the XML is too large for this
    o_parser->set_dom_generating( is_generating = '-' ).
*   Initialise XML and Batch Input administration data
    CLEAR: v_count, v_cntls, v_cnter, v_biqid.
    CLEAR: v_cntbk,     "Banks analysed
           v_cntpr,     "Banks processed
           v_cntcr,     "Banks created
           v_cntup,     "Banks updated
           v_cntnu,     "Banks unchanged
           v_cntdr,     "Banks reactivated
           v_cntds,     "Banks already deleted
           v_cntdl,     "Banks deleted
           v_cntdu,     "Deleted banks still in use
           v_cntsk,     "Banks skipped due to selection
           v_cntse,     "Banks skipped due to errors
           v_cntbi.     "Batch Input transactions in total
*   Read and parse the file
    _pmsg 'Processing file'.
    WHILE v_xcanc IS INITIAL.       "as long as not aborted, parse file
      o_event = o_parser->parse_event( ).
      IF o_event IS INITIAL.
        EXIT. "either end reached or error (check below)
      ENDIF.
      CASE o_event->get_type( ).
*       Event: New element starts
        WHEN if_ixml_event=>co_event_element_pre.
*         Set context (current element) for attribute evaluation
          cur_element = o_event->get_name( ).
*         Initialise bank data if new bank starts, update progress bar
          IF cur_element = 'BANK'.
            CLEAR v_bank.
            IF sy-batch IS INITIAL.   "in dialogue mode only
              ADD 1 TO v_count.
              v_mod = v_count MOD 100.
              IF v_mod = 0.
                WRITE v_count TO v_buf LEFT-JUSTIFIED.
                CONCATENATE 'Reading XML file:' v_buf 'bank entries...'
                            INTO v_buf SEPARATED BY space.
                CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
                  EXPORTING
                    text = v_buf.
                v_mod = v_count MOD 100000.
                IF v_mod = 0.
                  WRITE v_count TO v_buf LEFT-JUSTIFIED.
                  CONCATENATE v_buf 'bank entries read so far'
                              INTO v_buf SEPARATED BY space.
                  _pmsg v_buf.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
*       Event: An attribute has been read
        WHEN if_ixml_event=>co_event_attribute_post.
          v_tag = o_event->get_name( ).
          v_val = o_event->get_value( ).
*         Store data for later processing if relevant
          IF cur_element = 'BANK' AND v_tag = 'ACTION'.
            v_bank-action = v_val.
          ELSEIF cur_element = 'BIC' AND v_tag = 'PREFERRED'.
            v_bank-bicpref = v_val.
          ELSEIF cur_element = 'NATID' AND v_tag = 'PREFERRED'.
            v_bank-natidpref = v_val.
          ENDIF.
*       Event: An element has been read
        WHEN if_ixml_event=>co_event_element_post.
          v_tag = o_event->get_name( ).
          v_val = o_event->get_value( ).
          CASE v_tag.
            WHEN 'COUNTRY'.    v_bank-country = v_val.
            WHEN 'KEYVALUE'.   v_bank-keyval = v_val.
            WHEN 'NAME'.       v_bank-name = v_val.
            WHEN 'COUNTRYCSP'. v_bank-countrycsp = v_val. " 28/04/2009AW
            WHEN 'CITY'.       v_bank-city = v_val.
            WHEN 'BIC'.        v_bank-bic = v_val.
            WHEN 'NATID'.      v_bank-natid = v_val.
            WHEN 'NATIDTYPE'.  v_bank-natidtype = v_val.
            WHEN 'BRANCH'.     v_bank-branch = v_val.
            WHEN 'ADDRESS'.    v_bank-address = v_val.
            WHEN 'POSTCODE'.   v_bank-postcode = v_val.
            WHEN 'POBOX'.      v_bank-pobox = v_val.
            WHEN 'PHONE'.      v_bank-phone = v_val.
            WHEN 'FAX'.        v_bank-fax = v_val.
            WHEN 'EMAIL'.      v_bank-email = v_val.
            WHEN 'BANK'.
              PERFORM handle_bank CHANGING v_bank v_xcanc.
          ENDCASE.
      ENDCASE.
    ENDWHILE.
    _pmsg 'File processing complete'.
*   Delete banks that are not mentioned in the file in full upload
*   The file contains ALL valid banks, so any others should not be used.
    IF p_xfull = 'X' AND v_xcanc IS INITIAL.
      _pmsg 'Deleting unconfirmed bank entries'.
      DESCRIBE TABLE t_banks LINES v_len.
      _logcnt1 237 v_len.
*     Emulate deletion records to delete unconfirmed bank entries
      IF p_xtest IS INITIAL.
        _info 'Deleting unconfirmed banks...'.
        LOOP AT t_banks.
          CLEAR v_bank.
          v_bank-valid_action = 'D'.
          v_bank-sap_country  = t_banks-banks.
          v_bank-sap_keyval   = t_banks-bankl.
          PERFORM create_bi_transactions USING v_bank v_xcanc.
          CHECK v_xcanc IS INITIAL.
        ENDLOOP.
      ELSE.
        WRITE: / 'The following banks would be deleted.'.
        LOOP AT t_banks.
          WRITE: / sy-tabix, t_banks-banks, t_banks-bankl.
          IF p_xldel = 'X'.                         " 15/03/2010 AW
            PERFORM check_deleted_bank_in_use       " 15/03/2010 AW
              USING t_banks-banks t_banks-bankl     " 15/03/2010 AW
              CHANGING v_inuse.                     " 15/03/2010 AW
          ENDIF.                                    " 15/03/2010 AW
        ENDLOOP.
      ENDIF.
    ENDIF.
*   Close batch input session if any is open, regardless of read success
    _info 'Saving Batch Input session and log...'.
    _pmsg 'Saving Batch Input session'.
    PERFORM check_close_bisession USING p_mappe v_biqid
                                  CHANGING v_xcanc.
*   Check for errors while parsing and print final infos
    IF o_parser->num_errors( ) <> 0.
      v_count = o_parser->num_errors( ).
      WRITE v_count TO v_buf LEFT-JUSTIFIED.
      _logmsg a 212 v_buf space space space.
      v_index = 0.
      WHILE v_index < v_count.
        o_parseerror = o_parser->get_error( index = v_index ).
        v_len = o_parseerror->get_line( ).
        WRITE v_len TO v_buf(20) LEFT-JUSTIFIED.
        v_len = o_parseerror->get_column( ).
        WRITE v_len TO v_buf+20(20) LEFT-JUSTIFIED.
        v_msg = o_parseerror->get_reason( ).
        _logmsg e 213 v_buf(20) v_buf+20(20) v_msg(50) v_msg+50(50).
        ADD 1 TO v_index.
      ENDWHILE.
    ELSEIF v_xcanc = 'X'.
      _logmsg a 216 space space space space.
    ELSE.
*     If no errors occurred, print statistics
      _logmsg i 218 space space space space.
      _logcnt2 208 v_cntbk v_cntpr.
      _logcnt2 209 v_cntse v_cntsk.
      IF p_xtest <> 'X'.
        _logcnt1 231 v_cntcr.
        _logcnt3 232 v_cntup v_cntnu v_cntdr.
        _logcnt2 233 v_cntdl v_cntds.
        IF p_xldel = 'X'.
          _logcnt1 215 v_cntdu.
        ENDIF.
        _logcnt1 234 v_cntbi.
      ENDIF.
    ENDIF.
  ENDIF.
* Release unused memory
  _pmsg 'Releasing memory'.
  FREE: o_ixml, o_document, o_event, o_parser, o_stream,
        o_streamfactory, t_banks.
* Save Application Log to database
  _pmsg 'Saving Application Log'.
  REFRESH t_logid.
  APPEND v_logid TO t_logid.
  CALL FUNCTION 'BAL_DB_SAVE'
    EXPORTING
      i_client         = sy-mandt
      i_in_update_task = ' '
      i_save_all       = ' '
      i_t_log_handle   = t_logid
    EXCEPTIONS
      log_not_found    = 1
      save_not_allowed = 2
      numbering_error  = 3
      OTHERS           = 4.
  IF sy-subrc <> 0.
    MESSAGE s000 WITH 'Error saving log to DB:'(105) sy-subrc.
    _logmsg w 000 'Error saving log to DB:'(105) sy-subrc space space.
  ENDIF.
* Display Application Log
  _pmsg 'Printing Application Log'.
  CLEAR v_logdp.
  CALL FUNCTION 'BAL_DSP_PROFILE_NO_TREE_GET'
    IMPORTING
      e_s_display_profile = v_logdp.
  v_logdp-disvariant-report = sy-repid.
  v_logdp-cwidth_opt        = 'X'.
  CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
    EXPORTING
      i_s_display_profile  = v_logdp
      i_t_log_handle       = t_logid
      i_amodal             = ' '
    EXCEPTIONS
      profile_inconsistent = 1
      internal_error       = 2
      no_data_available    = 3
      no_authority         = 4
      OTHERS               = 5.
  IF sy-subrc <> 0.
    MESSAGE s000 WITH 'Error displaying log:'(106) sy-subrc.
  ENDIF.
  _pmsg 'Processing complete'.

***********************************************************************
*  Sub-Routines                                                       *
***********************************************************************

* Handle bank after all bank data has been read from file
FORM handle_bank CHANGING bank STRUCTURE v_bank
                          xcanc.
  DATA: v_xskip,
        v_landk     LIKE t005-landk,
        v_intca     LIKE t005-intca,
        v_natidtype TYPE zacc_natidtype.
* Register bank entry in statistics
  ADD 1 TO v_cntbk.
* Verify minimum data requirements
  IF bank-country IS INITIAL OR
     bank-keyval IS INITIAL OR
     bank-name IS INITIAL OR
     ( bank-bic IS INITIAL AND bank-natid IS INITIAL ).
    _logmsg e 220 bank-country bank-keyval bank-name bank-natid.
    ADD 1 TO v_cntse.
    EXIT.
  ENDIF.
* Verify bank action code
  bank-valid_action = bank-action.
  IF bank-valid_action <> bank-action OR
     bank-valid_action CN 'AMD'.
    _logmsg e 221 bank-action bank-keyval space space.
    ADD 1 TO v_cntse.
    EXIT.
  ENDIF.
  IF p_xlang = 'X' AND bank-valid_action = 'D'.
    _logmsg w 246 bank-keyval space space space.
    ADD 1 TO v_cntsk.
    EXIT.
  ENDIF.
* Verify ISO country code and derive SAP country code and bank key type
* If country is not found in T005, try remap via ZACC_MAP (territories)
  SELECT SINGLE land1 bnkey FROM t005
                            INTO (bank-sap_country, bank-sap_bnkey)
                            WHERE intca = bank-country.
  IF sy-subrc = 8.
    _logmsg e 222 bank-country bank-keyval space space.
    ADD 1 TO v_cntse.
    EXIT.
  ELSEIF sy-subrc <> 0.
    SELECT SINGLE mapto FROM zacc_map INTO zacc_map-mapto
                        WHERE intca = bank-country.
    IF sy-subrc = 0 AND NOT zacc_map-mapto IS INITIAL.
      SELECT SINGLE land1 bnkey FROM t005
                                INTO (bank-sap_country, bank-sap_bnkey)
                                WHERE intca = zacc_map-mapto.
      IF sy-subrc = 8.
        _logmsg e 222 bank-country bank-keyval zacc_map-mapto space.
        ADD 1 TO v_cntse.
        EXIT.
      ELSEIF sy-subrc <> 0.
        _logmsg e 223 bank-country bank-keyval zacc_map-mapto space.
        ADD 1 TO v_cntse.
        EXIT.
      ENDIF.
    ELSE.
      _logmsg e 223 bank-country bank-keyval space space.
      ADD 1 TO v_cntse.
      EXIT.
    ENDIF.
  ENDIF.
* Preselect by country to minimise error messages in log
  IF NOT bank-sap_country IN p_banks.
    ADD 1 TO v_cntsk.
    EXIT.
  ENDIF.
* TK211016: Select by bank code type if available
  v_natidtype = bank-natidtype.
  IF v_natidtype IS NOT INITIAL.
    SELECT SINGLE * FROM zacc_bank_code WHERE country = bank-sap_country
                                          AND natidtype = v_natidtype.
    IF sy-subrc = 0.
      ADD 1 TO v_cntsk.
      EXIT.
    ENDIF.
  ENDIF.
* Verify if a preferred entry is present
  IF bank-bicpref = '1' AND bank-bic IS INITIAL.
    _logmsg e 227 'Swift BIC' bank-keyval space space.
    ADD 1 TO v_cntse.
    EXIT.
  ENDIF.
  IF bank-natidpref = '1' AND bank-natid IS INITIAL.
    _logmsg e 227 'Bank Code' bank-keyval space space.
    ADD 1 TO v_cntse.
    EXIT.
  ENDIF.
* Verify Swift BIC (formal requirements only)
* The current country or any of its territories are accepted
  v_len = strlen( bank-bic ).
  bank-sap_swift = bank-bic.
  IF NOT bank-sap_swift IS INITIAL.
    IF NOT ( v_len = 8 OR v_len = 11 ).
      _logmsg e 226 bank-bic bank-keyval bank-country space.
      ADD 1 TO v_cntse.
      EXIT.
    ELSEIF bank-sap_swift+4(2) <> bank-country AND
           bank-sap_swift+4(2) <> bank-sap_country.

* The following select is left for existing customers    28/04/2009 AW
* who have customised zacc_map only but not zacc_bic.    28/04/2009 AW
      SELECT SINGLE mapto FROM zacc_map INTO zacc_map-mapto
                          WHERE intca = bank-sap_swift+4(2).
      IF sy-subrc <> 0 OR zacc_map-mapto <> bank-sap_country.

* Country code in BIC is validated against                28/04/2009 AW
* new table zacc_bic                                      28/04/2009 AW
        SELECT SINGLE intca biccc FROM zacc_bic          "28/04/2009 AW
               INTO (zacc_bic-intca, zacc_bic-biccc)     "28/04/2009 AW
               WHERE intca = bank-sap_country            "28/04/2009 AW
                 AND biccc = bank-sap_swift+4(2).        "28/04/2009 AW
        IF sy-subrc <> 0.                                "28/04/2009 AW
          IF bank-valid_action = 'D'.
            _logmsg w 226 bank-bic bank-keyval bank-country space.
          ELSE.
            _logmsg e 226 bank-bic bank-keyval bank-country space.
          ENDIF.
          ADD 1 TO v_cntse.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.
*   If requested, remove XXX padding from Swift BIC
    IF p_xrxxx = 'X' AND bank-sap_swift+8(3) = 'XXX'.
      CLEAR bank-sap_swift+8(3).
    ENDIF.
  ENDIF.
* If the above is true, we can be fairly confident that a GPF XML file
* is being loaded. Try to process the bank now.
* Map bank key
  CLEAR v_xskip.
  PERFORM map_bankkey USING bank-sap_bnkey bank-sap_country bank-keyval
                            bank-sap_swift bank-natid bank-bicpref
                            bank-natidpref
                      CHANGING bank-sap_keyval v_xskip.
  CHECK v_xskip IS INITIAL.
* Check if bank should be processed (country and bank code selection)
  IF NOT ( bank-sap_country IN p_banks AND
           bank-sap_keyval IN p_bankl AND
           bank-sap_swift IN p_swift ).
    ADD 1 TO v_cntsk.
    EXIT.
  ENDIF.
* Check if bank should be processed (preferred banks option)
  IF p_xpref = 'X' AND bank-bicpref <> '1' AND bank-natidpref <> '1'.
    ADD 1 TO v_cntsk.
    EXIT.
  ENDIF.
* Extract country code from post code if this has been added (D-52772)
  IF bank-postcode CA '-'.
    SPLIT bank-postcode AT '-' INTO v_msg v_buf.
    SELECT SINGLE landk intca FROM t005 INTO (v_landk, v_intca)
                              WHERE land1 = bank-sap_country.
    IF sy-subrc = 0.
      IF v_msg = v_landk OR v_msg = v_intca.
        bank-postcode = v_buf.
      ENDIF.
    ENDIF.
  ENDIF.

* Block Begin: Add Region 28/04/2009 AW
* Check if Region should be mapped from ZACC_REGI
  IF bank-countrycsp <> ''.
    SELECT SINGLE region FROM zacc_regi INTO zacc_regi-region
                          WHERE country = bank-sap_country
                            AND countrycsp = bank-countrycsp.
    IF sy-subrc = 0.
      bank-sap_region = zacc_regi-region.
    ELSE.
* No entry is found in ZACC_REGI -> check if
* countrycsp is a valid region in T005S
      SELECT SINGLE bland FROM t005s INTO t005s-bland
                          WHERE land1 = bank-sap_country
                            AND bland = bank-countrycsp.
      IF sy-subrc = 0.
        bank-sap_region = bank-countrycsp.
      ELSE.
        _logmsg w 238 bank-bic bank-keyval bank-country bank-countrycsp.
      ENDIF.
    ENDIF.
  ENDIF.
* Block End: Add Region 28/04/2009 AW

* TK231017: If we were asked to split the bank name, we do so for the CAM address
  IF p_xname = 'X'.
    v_buf = bank-name.
    bank-sap_name2 = v_buf+40(40).
    bank-sap_name3 = v_buf+80(40).
    bank-sap_name4 = v_buf+120(40).
  ENDIF.
* TK250714: If we are importing local language data, get the target address version
  IF p_xlang = 'X'.
*   Read address version from customising table
    SELECT SINGLE nation xnoplz FROM zacc_local_lang
                                INTO (bank-sap_adver, bank-sap_xnoplz)
                                WHERE land1 = bank-sap_country.
    IF sy-subrc <> 0.
      SELECT SINGLE nation FROM zacc_local_lang INTO bank-sap_adver
                           WHERE land1 = space.
      IF sy-subrc <> 0.
        _logmsg e 247 bank-sap_country bank-sap_keyval bank-keyval space.
        ADD 1 TO v_cntse.
        EXIT.
      ENDIF.
    ENDIF.
*   Check if address version exists and is active
    IF bank-sap_adver IS INITIAL.
      _logmsg e 248 bank-sap_country bank-sap_keyval bank-keyval bank-sap_adver.
      ADD 1 TO v_cntse.
      EXIT.
    ENDIF.
    SELECT SINGLE * FROM tsadvc WHERE nation = bank-sap_adver
                                  AND active = 'X'.
    IF sy-subrc <> 0.
      _logmsg e 249 bank-sap_country bank-sap_keyval bank-keyval bank-sap_adver.
      ADD 1 TO v_cntse.
      EXIT.
    ENDIF.
  ENDIF.
* TK250714: End
* TK181209: Call user exit function if requested
  CLEAR v_xskip.
  PERFORM call_user_exit CHANGING bank v_xskip.
  IF v_xskip = 'X'.
    ADD 1 TO v_cntsk.
    EXIT.
  ENDIF.
* TK181209: End
* TK250714: If e are importing local language data, we only add upon existing
*           bank records. Check if the current one has been created.
  IF p_xlang = 'X'.
    SELECT SINGLE * FROM bnka WHERE banks = bank-sap_country
                                AND bankl = bank-sap_keyval.
    IF sy-subrc <> 0.
      _logmsg e 250 bank-sap_country bank-sap_keyval space space.
      ADD 1 TO v_cntse.
      EXIT.
    ENDIF.
*   If the bank exists, but has been marked for deletion, that may suggest that the
*   associated GPF update file has not been imported. But we will assume the bank
*   was deleted on purpose and only issue a warning.
    IF bnka-loevm = 'X'.
      _logmsg w 251 bank-sap_country bank-sap_keyval space space.
      ADD 1 TO v_cntsk.
      EXIT.
    ENDIF.
  ENDIF.
* TK250714: End

* Validate maximum field length of selected input fields
  IF p_xaddr = 'X' AND p_xname = 'X'.
    _validate_length bank-name 160 'Bank name'.
  ELSE.
    _validate_length bank-name 60 'Bank name'.
  ENDIF.
  IF p_xaddr = 'X'.
    _validate_length bank-address 60 'Street'.
  ELSE.
    _validate_length bank-address 35 'Street'.
  ENDIF.
  _validate_length bank-city 35 'City'.
  _validate_length bank-branch 40 'Bank branch'.
  _validate_length bank-natid 15 'Bank routing code'.
  IF p_xaddr = 'X'.
    _validate_length bank-postcode 10 'Post code'.
    _validate_length bank-pobox 10 'PO Box'.
    _validate_length bank-phone 30 'Phone'.
    _validate_length bank-fax 30 'Fax'.
    _validate_length bank-email 241 'Email'.
  ENDIF.
* If we got here, consider the bank "processed"
  ADD 1 TO v_cntpr.
* In full load mode, match the bank entry against existing banks
  IF p_xfull = 'X'.
    READ TABLE t_banks WITH KEY banks = bank-sap_country
                                bankl = bank-sap_keyval
                       BINARY SEARCH.
    IF sy-subrc = 0.
      DELETE t_banks INDEX sy-tabix.
    ENDIF.
  ENDIF.
* If this is a proper run, create the relevant BI session entries
  IF p_xtest IS INITIAL.
*   Create the batch input session, if it is not open yet
    PERFORM check_open_bisession USING p_mappe CHANGING v_biqid xcanc.
    CHECK xcanc IS INITIAL.
*   Create transaction(s) to create, update, or delete bank
    PERFORM create_bi_transactions USING bank CHANGING xcanc.
  ENDIF.
* If test run, print bank data of the first 100 banks that would process
  IF p_xtest = 'X' AND v_cntls < 100.
    ADD 1 TO v_cntls.
    WRITE: / 'Bank', v_count, 50 'Action', bank-valid_action.
    ULINE.
    WRITE: / 'Country  ', bank-country, 50 'SAP Key', bank-sap_country,
                         bank-sap_keyval,
           / 'Key      ', bank-keyval, 50 'SAP Key', bank-sap_keyval,
           / 'Name     ', bank-name,
           / 'Name 2   ', bank-sap_name2,
           / 'Name 3   ', bank-sap_name3,
           / 'Name 4   ', bank-sap_name4,
           / 'Region   ', bank-countrycsp,
                         50 'SAP Key', bank-sap_region, " 28/04/2009 AW
           / 'City     ', bank-city,
           / 'Address  ', bank-address,
           / 'Postcode ', bank-postcode,
           / 'BIC      ', bank-bic, 50 'Preferred', bank-bicpref,
           / 'NATID    ', bank-natid, 50 'Preferred', bank-natidpref,
           / 'NATIDTYPE', bank-natidtype,
           / 'Branch   ', bank-branch,
           / 'POBox    ', bank-pobox,
           / 'Phone    ', bank-phone,
           / 'Fax      ', bank-fax,
           / 'E-Mail   ', bank-email.
    IF p_xlang = 'X'.
      WRITE: / 'Target address version for local language data would have been:', bank-sap_adver.
    ENDIF.
    ULINE.
  ENDIF.
ENDFORM.                    "handle_bank

* Map SAP bank key from bank data depending on customising setting
FORM map_bankkey USING VALUE(bnkey)
                       VALUE(country)
                       VALUE(keyval)
                       VALUE(swift)
                       VALUE(natid)
                       VALUE(bicpref)
                       VALUE(natidpref)
                 CHANGING sap_keyval
                          xskip.
  CASE bnkey.
*   Bank key is domestic bank code
    WHEN '1'.
      IF natidpref = '1'.
        sap_keyval = natid.
      ELSE.
        ADD 1 TO v_cntsk.
        xskip = 'X'.
      ENDIF.
*   Bank key is assigned internally
    WHEN '3'.
*     Search bank via bank code first; if multiple use undeleted one
      IF natidpref = '1'.
        SELECT SINGLE bankl FROM bnka INTO sap_keyval
                            WHERE banks = country
                              AND bnklz = natid.
        IF sy-subrc = 4.
          CLEAR sap_keyval.
        ELSEIF sy-subrc <> 0.
          SELECT SINGLE bankl FROM bnka INTO sap_keyval
                              WHERE banks = country
                                AND bnklz = natid
                                AND loevm = space.
          IF sy-subrc <> 0.
            _logmsg e 225 country natid space space.
            ADD 1 TO v_cntse.
            xskip = 'X'.
          ENDIF.
        ENDIF.
*     Search bank via Swift BIC next; if multiple use undeleted one
      ELSEIF bicpref = '1'.
        SELECT SINGLE bankl FROM bnka INTO sap_keyval
                            WHERE banks = country
                              AND swift = swift.
        IF sy-subrc = 4.
          CLEAR sap_keyval.
        ELSEIF sy-subrc <> 0.
          SELECT SINGLE bankl FROM bnka INTO sap_keyval
                              WHERE banks = country
                                AND swift = swift
                                AND loevm = space.
          IF sy-subrc <> 0.
            _logmsg e 225 country swift space space.
            ADD 1 TO v_cntse.
            xskip = 'X'.
          ENDIF.
        ENDIF.
*     Else skip entry (only preferred banks accepted with int. assgnm.)
      ELSE.
        ADD 1 TO v_cntsk.
        xskip = 'X'.
      ENDIF.
*   Bank key is assigned externally
    WHEN '4'.
      CASE p_keyhd.
        WHEN 'A'.
          sap_keyval = keyval.
        WHEN 'B'.
          IF bicpref = '1'.
            sap_keyval = swift.
          ELSE.
            ADD 1 TO v_cntsk.
            xskip = 'X'.
          ENDIF.
* Block insert 09/11/2009 AW
        WHEN 'C'.
          IF natidpref = '1'.
            sap_keyval = natid.
          ELSE.
            ADD 1 TO v_cntsk.
            xskip = 'X'.
          ENDIF.
* Block end 09/11/2009 AW
        WHEN OTHERS.    "If this happens, something is seriously wrong
          _logmsg a 000 'Invalid bank key handling code' p_keyhd
                  space space.
          ADD 1 TO v_cntse.
          xskip = 'X'.
          v_xcanc = 'X'.
      ENDCASE.
    WHEN OTHERS.
      _logmsg e 224 bnkey country keyval space.
      ADD 1 TO v_cntse.
      xskip = 'X'.
  ENDCASE.
ENDFORM.                    "map_bankkey

* Add message to application log; called via macro _logmsg(x)
FORM log_message USING a b c d e f g.
  CLEAR v_logms.
  v_logms-msgty = a.
  v_logms-msgid = g.
  v_logms-msgno = b.
  v_logms-msgv1 = c.
  v_logms-msgv2 = d.
  v_logms-msgv3 = e.
  v_logms-msgv4 = f.
  CASE a.
    WHEN 'A'.    v_logms-probclass = '1'.
    WHEN 'E'.    v_logms-probclass = '2'.
    WHEN 'W'.    v_logms-probclass = '3'.
    WHEN OTHERS. v_logms-probclass = '4'.
  ENDCASE.
  CALL FUNCTION 'BAL_LOG_MSG_ADD'
    EXPORTING
      i_log_handle     = v_logid
      i_s_msg          = v_logms
    EXCEPTIONS
      log_not_found    = 1
      msg_inconsistent = 2
      log_is_full      = 3
      OTHERS           = 4.
  IF sy-subrc <> 0.
    MESSAGE e000 WITH 'Error writing log, return code'(102) sy-subrc.
  ENDIF.
ENDFORM.                    "log_message

* TK181209: Add message to application log only once;
*           called via macro _logmsgc
FORM collect_message USING a b c d e f g.
  CLEAR v_logms.
  v_logms-msgty = a.
  v_logms-msgid = g.
  v_logms-msgno = b.
  v_logms-msgv1 = c.
  v_logms-msgv2 = d.
  v_logms-msgv3 = e.
  v_logms-msgv4 = f.
  CASE a.
    WHEN 'A'.    v_logms-probclass = '1'.
    WHEN 'E'.    v_logms-probclass = '2'.
    WHEN 'W'.    v_logms-probclass = '3'.
    WHEN OTHERS. v_logms-probclass = '4'.
  ENDCASE.
  CALL FUNCTION 'BAL_LOG_MSG_CUMULATE'
    EXPORTING
      i_log_handle         = v_logid
      i_s_msg              = v_logms
      i_compare_attributes = 'X'
      i_compare_context    = ' '
      i_compare_parameters = 'X'
    EXCEPTIONS
      log_not_found        = 1
      msg_inconsistent     = 2
      log_is_full          = 3
      OTHERS               = 4.
  IF sy-subrc <> 0.
    MESSAGE e000 WITH 'Error writing log, return code'(102) sy-subrc.
  ENDIF.
ENDFORM.                    "log_message

* Check if BI session has already been opened, and open if not
FORM check_open_bisession USING VALUE(bname)
                          CHANGING biqid
                                   xcanc.
  IF biqid IS INITIAL.
    PERFORM create_bi_session USING bname
                              CHANGING biqid xcanc.
  ENDIF.
ENDFORM.                    "check_open_bisession

* Check if a BI session has been opened, and close if so
FORM check_close_bisession USING VALUE(bname)
                                 VALUE(biqid)
                           CHANGING xcanc.
  DATA: v_sdate LIKE sy-datum.
  IF NOT biqid IS INITIAL.
    PERFORM close_bi_session USING bname CHANGING xcanc.
    IF xcanc IS INITIAL AND p_xexec = 'X'.
      GET TIME.
      v_sdate = sy-datum.
      IF sy-uzeit > p_rtime.
        ADD 1 TO v_sdate.
      ENDIF.
      PERFORM run_bi_session USING bname biqid v_sdate p_rtime
                                   p_ximme
                             CHANGING xcanc.
    ELSEIF p_xexec = 'X'.
      _logmsg i 166 bname biqid space space.
    ENDIF.
  ENDIF.
ENDFORM.                    "check_close_bisession

* Create transaction(s) to create, update, or delete bank
FORM create_bi_transactions USING VALUE(bank) STRUCTURE v_bank
                            CHANGING xcanc.
  DATA: v_bankl LIKE bnka-bankl,
        v_loevm LIKE bnka-loevm.
* TK250714: If local language import is requested, we do just that. We've already
*           checked earlier, that the bank exists.
  IF p_xlang = 'X'.
    ADD 1 TO v_cntup.
    PERFORM add_local_address USING bank CHANGING xcanc.
* TK250714: Otherwise carry on with the standard processing as before
  ELSE.
*   Find out if the bank exists first and if it is active
    IF bank-sap_keyval IS INITIAL.
      CLEAR: v_bankl, v_loevm.
    ELSE.
      SELECT SINGLE bankl loevm FROM bnka INTO (v_bankl, v_loevm)
                                WHERE banks = bank-sap_country
                                  AND bankl = bank-sap_keyval.
      IF sy-subrc <> 0.
        CLEAR: v_bankl, v_loevm.
      ENDIF.
    ENDIF.
*   Handle bank
    IF bank-valid_action CA 'AM'.
      IF NOT v_bankl IS INITIAL.
        IF NOT v_loevm IS INITIAL.     "undelete first if deleted
          ADD 1 TO v_cntdr.
          PERFORM delete_bank USING bank space CHANGING xcanc.
        ENDIF.
*       TK100314: Also undelete if we deleted this bank earlier
        READ TABLE t_delbnk WITH KEY banks = bank-sap_country bankl = bank-sap_keyval.
        IF sy-subrc = 0.
          ADD 1 TO v_cntdr.
          PERFORM delete_bank USING bank space CHANGING xcanc.
        ENDIF.
*       TK100314: End
        ADD 1 TO v_cntup.
        PERFORM change_bank USING bank CHANGING xcanc.
      ELSE.
        ADD 1 TO v_cntcr.
        PERFORM create_bank USING bank CHANGING xcanc.
      ENDIF.
    ELSEIF bank-valid_action = 'D'.
      IF v_loevm = space AND NOT v_bankl IS INITIAL.
        ADD 1 TO v_cntdl.
* Block insert 15/03/2010 AW
        CLEAR v_inuse.
        PERFORM check_deleted_bank_in_use USING bank-sap_country
                                                 bank-sap_keyval
                                          CHANGING v_inuse.
        IF v_inuse IS INITIAL OR p_xudel IS INITIAL.
          PERFORM delete_bank USING bank 'X' CHANGING xcanc.
        ENDIF.

* Block insert end  15/03/2010 AW

* Block delete 15/03/2010 AW
*      PERFORM delete_bank USING bank 'X' CHANGING xcanc.
*      IF p_xldel = 'X'.
*        PERFORM check_deleted_bank_in_use USING bank-sap_country
*                                                bank-sap_keyval.
*      ENDIF.
* Block delete end 15/03/2010 AW

      ELSE.
        ADD 1 TO v_cntds.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    "create_bi_transactions

* Create a bank
FORM create_bank USING VALUE(bank) STRUCTURE v_bank
                 CHANGING xcanc.
  DATA: t_bdc       TYPE bdcdata OCCURS 50 WITH HEADER LINE,
        v_name(40),
        v_banka     TYPE bnka-banka,
        v_stras(35).
  REFRESH t_bdc.
* Fill initial screen
  PERFORM add_dynpro TABLES t_bdc USING 'SAPMF02B' '0100'.
  PERFORM add_field TABLES t_bdc USING:
    'BNKA-BANKS' bank-sap_country,
    'BNKA-BANKL' bank-sap_keyval,
    'BDC_OKCODE' '/00'.
* Fill bank data screen
  v_stras = bank-address.
  v_banka = bank-name.
  PERFORM add_dynpro TABLES t_bdc USING 'SAPMF02B' '0110'.
  PERFORM add_field TABLES t_bdc USING:
    'BNKA-BANKA' v_banka,
    'BNKA-PROVZ' bank-sap_region, " 28/04/2009 AW
    'BNKA-STRAS' v_stras,
    'BNKA-ORT01' bank-city,
    'BNKA-BRNCH' bank-branch,
    'BNKA-SWIFT' bank-sap_swift.
  IF bank-sap_bnkey <> '1'.
    PERFORM add_field TABLES t_bdc USING 'BNKA-BNKLZ' bank-natid.
  ENDIF.
* If CAM address should be maintained, jump to address mgmt
  IF p_xaddr = 'X'.
    v_name = bank-name.          "Name is shorter on address
    PERFORM add_field TABLES t_bdc USING 'BDC_OKCODE' '=ADDR'.
    PERFORM add_dynpro TABLES t_bdc USING 'SAPLSZA1' '0201'.
    PERFORM add_field TABLES t_bdc USING:
      'ADDR1_DATA-NAME1' v_name,
      'ADDR1_DATA-NAME2' bank-sap_name2,
      'ADDR1_DATA-NAME3' bank-sap_name3,
      'ADDR1_DATA-NAME4' bank-sap_name4,
      'ADDR1_DATA-STREET' bank-address,
      'ADDR1_DATA-POST_CODE1' bank-postcode,
      'ADDR1_DATA-CITY1' bank-city,
      'ADDR1_DATA-COUNTRY' bank-sap_country,
      'ADDR1_DATA-REGION' bank-sap_region, " 28/04/2009 AW
      'SZA1_D0100-TEL_NUMBER' bank-phone,
      'SZA1_D0100-FAX_NUMBER' bank-fax,
      'SZA1_D0100-SMTP_ADDR' bank-email.
    IF NOT bank-pobox IS INITIAL.
      PERFORM add_field TABLES t_bdc USING:
        'ADDR1_DATA-PO_BOX' bank-pobox,
        'ADDR1_DATA-POST_CODE2' bank-postcode.
    ENDIF.
    PERFORM add_field TABLES t_bdc USING 'BDC_OKCODE' '=CONT'.
    PERFORM add_dynpro TABLES t_bdc USING 'SAPMF02B' '0110'.
  ENDIF.
* Save the bank changes
  PERFORM add_field TABLES t_bdc USING 'BDC_OKCODE' '/11'.
* Add the BI transaction to the session
  CALL FUNCTION 'BDC_INSERT'
    EXPORTING
      tcode     = 'FI01'
    TABLES
      dynprotab = t_bdc
    EXCEPTIONS
      OTHERS    = 1.
  IF sy-subrc <> 0.
    _logmsgx e sy-msgno sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 sy-msgid.
    _logmsg e 165 'FI01' 'Create bank' space space.
    xcanc = 'X'.
  ELSE.
    ADD 1 TO v_cntbi.
  ENDIF.
  REFRESH t_bdc.
ENDFORM.                    "create_bank

* Change a bank after checking if changes are relevant
FORM change_bank USING VALUE(bank) STRUCTURE v_bank
                 CHANGING xcanc.
  DATA: t_bdc       TYPE bdcdata OCCURS 50 WITH HEADER LINE,
        v_name(40),
        v_banka     TYPE bnka-banka,
        v_stras(35),
        v_bnka      TYPE bnka,
        v_adrc      TYPE adrc,
        v_smtp      TYPE adr6-smtp_addr,
        v_str35(35),
        v_nam40(40).
  REFRESH t_bdc.
* Validate that we have a bank to change
  IF bank-sap_country IS INITIAL OR bank-sap_keyval IS INITIAL.
    _logmsg e 000 'Internal error: invalid bank change' bank-sap_country
            bank-sap_keyval bank-name.
    ADD 1 TO v_cntse.
    SUBTRACT 1 FROM v_cntup.
    EXIT.
  ENDIF.
* Check if anything is to be changed to minimise generated transactions
* For reactivated banks we always generate a change transaction
  SELECT SINGLE * FROM bnka INTO v_bnka WHERE banks = bank-sap_country
                                          AND bankl = bank-sap_keyval.
  IF sy-subrc = 0.
    v_str35 = bank-address.
    v_banka = bank-name.
    IF v_bnka-banka = v_banka AND
       v_bnka-provz = bank-sap_region AND " 28/04/2009 AW
       v_bnka-stras = v_str35 AND
       v_bnka-ort01 = bank-city AND
       v_bnka-brnch = bank-branch AND
       v_bnka-swift = bank-sap_swift AND
       v_bnka-bnklz = bank-natid AND
       v_bnka-loevm = space.
      IF p_xaddr IS INITIAL.
        ADD 1 TO v_cntnu.
        SUBTRACT 1 FROM v_cntup.
        EXIT.
      ELSE.
        SELECT SINGLE * FROM adrc INTO v_adrc
                        WHERE addrnumber = v_bnka-adrnr
                          AND date_from <= sy-datum
                          AND nation = space.
        IF sy-subrc = 0.
          v_nam40 = bank-name.
          IF v_adrc-name1 = v_nam40 AND
             v_adrc-name2 = bank-sap_name2 AND
             v_adrc-name3 = bank-sap_name3 AND
             v_adrc-name4 = bank-sap_name4 AND
             v_adrc-street = bank-address AND
             v_adrc-post_code1 = bank-postcode AND
             v_adrc-city1 = bank-city AND
             v_adrc-country = bank-sap_country AND
             v_adrc-region = bank-sap_region AND " 28/04/2009 AW
             v_adrc-po_box = bank-pobox AND
             v_adrc-tel_number = bank-phone AND
             v_adrc-fax_number = bank-fax.
            SELECT SINGLE smtp_addr FROM adr6 INTO v_smtp
                                    WHERE addrnumber = v_bnka-adrnr
                                      AND persnumber = space
                                      AND date_from <= sy-datum
                                      AND flgdefault = 'X'.
            IF sy-subrc <> 0.
              CLEAR v_smtp.
            ENDIF.
            IF v_smtp = bank-email.
              ADD 1 TO v_cntnu.
              SUBTRACT 1 FROM v_cntup.
              EXIT.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
* Fill initial screen
  PERFORM add_dynpro TABLES t_bdc USING 'SAPMF02B' '0100'.
  PERFORM add_field TABLES t_bdc USING:
    'BNKA-BANKS' bank-sap_country,
    'BNKA-BANKL' bank-sap_keyval,
    'BDC_OKCODE' '/00'.
* Fill bank data screen
  v_stras = bank-address.
  v_banka = bank-name.
  PERFORM add_dynpro TABLES t_bdc USING 'SAPMF02B' '0110'.
  PERFORM add_field TABLES t_bdc USING:
    'BNKA-BANKA' v_banka,
    'BNKA-PROVZ' bank-sap_region, " 28/04/2009 AW
    'BNKA-STRAS' v_stras,
    'BNKA-ORT01' bank-city,
    'BNKA-BRNCH' bank-branch,
    'BNKA-SWIFT' bank-sap_swift.
  IF bank-sap_bnkey <> '1'.
    PERFORM add_field TABLES t_bdc USING 'BNKA-BNKLZ' bank-natid.
  ENDIF.
* If CAM address should be maintained, jump to address mgmt
  IF p_xaddr = 'X'.
    v_name = bank-name.          "Name is shorter on address
    PERFORM add_field TABLES t_bdc USING 'BDC_OKCODE' '=ADDR'.
    PERFORM add_dynpro TABLES t_bdc USING 'SAPLSZA1' '0201'.
    PERFORM add_field TABLES t_bdc USING:
      'ADDR1_DATA-NAME1' v_name,
      'ADDR1_DATA-NAME2' bank-sap_name2,
      'ADDR1_DATA-NAME3' bank-sap_name3,
      'ADDR1_DATA-NAME4' bank-sap_name4,
      'ADDR1_DATA-STREET' bank-address,
      'ADDR1_DATA-POST_CODE1' bank-postcode,
      'ADDR1_DATA-CITY1' bank-city,
      'ADDR1_DATA-COUNTRY' bank-sap_country,
      'ADDR1_DATA-REGION' bank-sap_region, " 28/04/2009 AW
      'SZA1_D0100-TEL_NUMBER' bank-phone,
      'SZA1_D0100-FAX_NUMBER' bank-fax,
      'SZA1_D0100-SMTP_ADDR' bank-email.
    IF NOT bank-pobox IS INITIAL.
      PERFORM add_field TABLES t_bdc USING:
        'ADDR1_DATA-PO_BOX' bank-pobox,
        'ADDR1_DATA-POST_CODE2' bank-postcode.
    ELSE.
      PERFORM add_field TABLES t_bdc USING:
        'ADDR1_DATA-PO_BOX' space,
        'ADDR1_DATA-POST_CODE2' space.
    ENDIF.
    PERFORM add_field TABLES t_bdc USING 'BDC_OKCODE' '=CONT'.
    PERFORM add_dynpro TABLES t_bdc USING 'SAPMF02B' '0110'.
  ENDIF.
* Save the bank changes
  PERFORM add_field TABLES t_bdc USING 'BDC_OKCODE' '/11'.
* Add the BI transaction to the session
  CALL FUNCTION 'BDC_INSERT'
    EXPORTING
      tcode     = 'FI02'
    TABLES
      dynprotab = t_bdc
    EXCEPTIONS
      OTHERS    = 1.
  IF sy-subrc <> 0.
    _logmsgx e sy-msgno sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 sy-msgid.
    _logmsg e 165 'FI02' 'Change bank' space space.
    xcanc = 'X'.
  ELSE.
    ADD 1 TO v_cntbi.
  ENDIF.
  REFRESH t_bdc.
ENDFORM.                    "change_bank

* Add or change the address data for a given address version (other than
* the default address version)
FORM add_local_address USING VALUE(bank) STRUCTURE v_bank
                       CHANGING xcanc.
  DATA: t_bdc       TYPE bdcdata OCCURS 50 WITH HEADER LINE,
        v_name(40),
        v_stras(35),
        v_bnka      TYPE bnka,
        v_adrc      TYPE adrc,
        v_smtp      TYPE adr6-smtp_addr,
        v_str35(35),
        v_nam40(40).
  REFRESH t_bdc.
* Validate that we have a bank to change
  IF bank-sap_country IS INITIAL OR bank-sap_keyval IS INITIAL.
    _logmsg e 000 'Internal error: invalid bank address change' bank-sap_country
            bank-sap_keyval bank-name.
    ADD 1 TO v_cntse.
    SUBTRACT 1 FROM v_cntup.
    EXIT.
  ENDIF.
  IF bank-sap_adver IS INITIAL.
    _logmsg e 000 'Internal error: invalid bank address change' bank-sap_country
            bank-sap_keyval bank-sap_adver.
    ADD 1 TO v_cntse.
    SUBTRACT 1 FROM v_cntup.
    EXIT.
  ENDIF.
* Check if anything is to be changed to minimise generated transactions
  SELECT SINGLE * FROM adrc INTO v_adrc
                  WHERE addrnumber = v_bnka-adrnr
                    AND date_from <= sy-datum
                    AND nation = bank-sap_adver.
  IF sy-subrc = 0.
    v_nam40 = bank-name.
    IF v_adrc-name1 = v_nam40 AND
       v_adrc-name2 = bank-sap_name2 AND
       v_adrc-street = bank-address AND
       v_adrc-post_code1 = bank-postcode AND
       v_adrc-city1 = bank-city AND
       v_adrc-country = bank-sap_country AND
       v_adrc-region = bank-sap_region AND
       v_adrc-po_box = bank-pobox AND
       v_adrc-tel_number = bank-phone AND
       v_adrc-fax_number = bank-fax.
      ADD 1 TO v_cntnu.
      SUBTRACT 1 FROM v_cntup.
      EXIT.
    ENDIF.
  ENDIF.
* Fill initial screen
  PERFORM add_dynpro TABLES t_bdc USING 'SAPMF02B' '0100'.
  PERFORM add_field TABLES t_bdc USING:
    'BNKA-BANKS' bank-sap_country,
    'BNKA-BANKL' bank-sap_keyval,
    'BDC_OKCODE' '/00'.
* Go straight to CAM from bank data screen
  v_stras = bank-address.
  v_name = bank-name.          "Name is shorter on address
  PERFORM add_dynpro TABLES t_bdc USING 'SAPMF02B' '0110'.
  PERFORM add_field TABLES t_bdc USING 'BDC_OKCODE' '=ADDR'.
* Call the local language address version popup
  PERFORM add_dynpro TABLES t_bdc USING 'SAPLSZA1' '0201'.
  PERFORM add_field TABLES t_bdc USING:
    'ADDR1_DATA-COUNTRY' bank-sap_country,   "if no CAM address was maintained before, we need at least the country code
    'BDC_OKCODE' '=VERS'.
* Select the local language address version
  PERFORM add_dynpro TABLES t_bdc USING 'SAPLSPO4' '0300'.
  PERFORM add_field TABLES t_bdc USING:
    'SVALD-VALUE(01)' bank-sap_adver,
    'BDC_OKCODE' '=FURT'.
* Enter the local language data
* TK290714: A different popup screen is used for Japan. Whether there are any more, testing will need to show.
* TK210814: There are indeed several more screen associations, especially for the Asian scripts
  IF bank-sap_adver CA 'KNHMC'.
    PERFORM add_dynpro TABLES t_bdc USING 'SAPLSZA1' '0213'.
  ELSEIF bank-sap_adver CA 'TI'.
    PERFORM add_dynpro TABLES t_bdc USING 'SAPLSZA1' '0204'.
  ELSE.
    PERFORM add_dynpro TABLES t_bdc USING 'SAPLSZA1' '0201'.
  ENDIF.
  PERFORM add_field TABLES t_bdc USING:
    'ADDR1_DATA-NAME1' v_name,
    'ADDR1_DATA-NAME2' bank-sap_name2,
    'ADDR1_DATA-NAME3' bank-sap_name3,
    'ADDR1_DATA-NAME4' bank-sap_name4,
    'ADDR1_DATA-STREET' bank-address,
    'ADDR1_DATA-CITY1' bank-city,
    'ADDR1_DATA-COUNTRY' bank-sap_country,
    'ADDR1_DATA-REGION' bank-sap_region,
    'SZA1_D0100-TEL_NUMBER' bank-phone,
    'SZA1_D0100-FAX_NUMBER' bank-fax,
    'SZA1_D0100-SMTP_ADDR' bank-email.
  IF bank-sap_xnoplz IS INITIAL.
    PERFORM add_field TABLES t_bdc USING:
      'ADDR1_DATA-POST_CODE1' bank-postcode.
  ENDIF.
  IF NOT bank-pobox IS INITIAL.
    PERFORM add_field TABLES t_bdc USING:
      'ADDR1_DATA-PO_BOX' bank-pobox,
      'ADDR1_DATA-POST_CODE2' bank-postcode.
  ELSE.
    PERFORM add_field TABLES t_bdc USING:
      'ADDR1_DATA-PO_BOX' space,
      'ADDR1_DATA-POST_CODE2' space.
  ENDIF.
  PERFORM add_field TABLES t_bdc USING 'BDC_OKCODE' '=CONT'.
* We need a second okay to close the dialogue for the default address
  PERFORM add_dynpro TABLES t_bdc USING 'SAPLSZA1' '0201'.
  PERFORM add_field TABLES t_bdc USING 'BDC_OKCODE' '=CONT'.
* Save the bank changes
  PERFORM add_dynpro TABLES t_bdc USING 'SAPMF02B' '0110'.
  PERFORM add_field TABLES t_bdc USING 'BDC_OKCODE' '/11'.
* Add the BI transaction to the session
  CALL FUNCTION 'BDC_INSERT'
    EXPORTING
      tcode     = 'FI02'
    TABLES
      dynprotab = t_bdc
    EXCEPTIONS
      OTHERS    = 1.
  IF sy-subrc <> 0.
    _logmsgx e sy-msgno sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 sy-msgid.
    _logmsg e 165 'FI02' 'Change bank' space space.
    xcanc = 'X'.
  ELSE.
    ADD 1 TO v_cntbi.
  ENDIF.
  REFRESH t_bdc.
ENDFORM.                    "add_local_address

* Delete or undelete a bank
FORM delete_bank USING VALUE(bank) STRUCTURE v_bank
                       VALUE(loevm)
                 CHANGING xcanc.
  DATA: t_bdc TYPE bdcdata OCCURS 50 WITH HEADER LINE.
  REFRESH t_bdc.
* Validate that we have a bank to change
  IF bank-sap_country IS INITIAL OR bank-sap_keyval IS INITIAL.
    _logmsg e 000 'Internal error: invalid bank deletion'
            bank-sap_country bank-sap_keyval bank-name.
    ADD 1 TO v_cntse.
    SUBTRACT 1 FROM v_cntdl.
    EXIT.
  ENDIF.
* Fill initial screen
  PERFORM add_dynpro TABLES t_bdc USING 'SAPMF02B' '0100'.
  PERFORM add_field TABLES t_bdc USING:
    'BNKA-BANKS' bank-sap_country,
    'BNKA-BANKL' bank-sap_keyval,
    'BDC_OKCODE' '/00'.
* Fill bank data screen
  PERFORM add_dynpro TABLES t_bdc USING 'SAPMF02B' '0120'.
  PERFORM add_field TABLES t_bdc USING:
    'BNKA-LOEVM' loevm,
    'BDC_OKCODE' '/11'.
* Add the BI transaction to the session
  CALL FUNCTION 'BDC_INSERT'
    EXPORTING
      tcode     = 'FI06'
    TABLES
      dynprotab = t_bdc
    EXCEPTIONS
      OTHERS    = 1.
  IF sy-subrc <> 0.
    _logmsgx e sy-msgno sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 sy-msgid.
    _logmsg e 165 'FI06' '(Un)delete bank' space space.
    xcanc = 'X'.
  ELSE.
    ADD 1 TO v_cntbi.
*   TK100314: Remember deleted banks for later adds
    t_delbnk-banks = bank-sap_country.
    t_delbnk-bankl = bank-sap_keyval.
    APPEND t_delbnk.
  ENDIF.
  REFRESH t_bdc.
ENDFORM.                    "delete_bank

* Verify if a bank to be deleted is still used by vendors or customers
* TK090216: Replace entire sub-routine with dynamic access to customer and vendor bank data
*           in order to allow overall program execution on systems without FI tables (i.e.
*           ABAP syntax check must not find any unknown tables).
FORM check_deleted_bank_in_use USING VALUE(country)
                                     VALUE(keyval)
                               CHANGING inuse.
*  DATA: v_lifnr LIKE lfa1-lifnr,
*        v_kunnr LIKE kna1-kunnr.
** Check bank use in vendor first
*  SELECT SINGLE lifnr FROM lfbk INTO v_lifnr WHERE banks = country
*                                               AND bankl = keyval.
*  IF sy-subrc <> 4.
*    ADD 1 TO v_cntdu.
*    IF p_xldel = 'X'.    " 15/03/2010 AW
*      _logmsg w 229 country keyval 'vendor' v_lifnr.
*    ENDIF.               " 15/03/2010 AW
** Block insert 15/03/2010 AW
*    inuse = 'X'.
** In test run list all vendors
*    IF p_xtest = 'X' AND p_xldel = 'X'.
*      SELECT lifnr FROM lfbk INTO v_lifnr WHERE banks = country
*                                             AND bankl = keyval.
*        WRITE: / 'Bank in use by vendor: ', v_lifnr.
*      ENDSELECT.
*    ENDIF.
** Block insert end 15/03/2010 AW
*    EXIT.
*  ENDIF.
** Check bank use in customers if not used with vendors
*  SELECT SINGLE kunnr FROM knbk INTO v_kunnr WHERE banks = country
*                                               AND bankl = keyval.
*  IF sy-subrc <> 4.
*    ADD 1 TO v_cntdu.
*    IF p_xldel = 'X'.    " 15/03/2010 AW
*      _logmsg w 229 country keyval 'customer' v_kunnr.
*    ENDIF.               " 15/03/2010 AW
** Block insert 15/03/2010 AW
*    inuse = 'X'.
** In test run list all customers
*    IF p_xtest = 'X' AND p_xldel = 'X'.
*      SELECT kunnr FROM knbk INTO v_kunnr WHERE banks = country
*                                               AND bankl = keyval.
*        WRITE: / 'Bank in use by customer: ', v_kunnr.
*      ENDSELECT.
*    ENDIF.
** Block insert end 15/03/2010 AW
*  ENDIF.
  DATA: v_number(10).
* Search for bank use in vendor accounts
  PERFORM check_dynamic_table USING 'LFBK' 'LIFNR' 'vendor'(t01) country keyval
                              CHANGING v_number inuse.
* Search for bank use in customer accounts (only if not found in vendors or if test mode is active, as
* the counting of the "to be deleted banks in use" is wrong if found in customers as well and it does
* take up unnecessary database time if entries found are not printed to screen).
  IF inuse IS INITIAL OR p_xtest = 'X'.
    PERFORM check_dynamic_table USING 'KNBK' 'KUNNR' 'customer'(t02) country keyval
                                CHANGING v_number inuse.
  ENDIF.
ENDFORM.                    "check_deleted_bank_in_use

* Check LFBK or KNBK for bank key use if present
FORM check_dynamic_table USING VALUE(tabname) TYPE dd02l-tabname
                               VALUE(keyname)
                               VALUE(keydesc)
                               VALUE(country)
                               VALUE(keyval)
                         CHANGING buffer
                                  inuse.
  DATA: v_subrc TYPE sy-subrc.
* Check if table exists
  CALL FUNCTION 'DD_EXIST_TABLE'
    EXPORTING
      tabname      = tabname
      status       = 'A'
    IMPORTING
      subrc        = v_subrc
    EXCEPTIONS
      wrong_status = 1
      OTHERS       = 2.
  IF sy-subrc <> 0 OR v_subrc <> 0.
    RETURN.
  ENDIF.
* Read table to check if bank key is in use
* Should the table exist but not know the fields BANKS/BANKL/keyname, something is seriously wrong -
* in this case, we let the run-time error happen as it will make error analysis easier.
  SELECT SINGLE (keyname) FROM (tabname) INTO buffer WHERE banks = country
                                                       AND bankl = keyval.
  IF sy-subrc <> 4.
    ADD 1 TO v_cntdu.
    IF p_xldel = 'X'.
      _logmsg w 229 country keyval keydesc buffer.
    ENDIF.
    inuse = 'X'.
*   In test mode, enumerate all vendors/customers using the bank key
    IF p_xtest = 'X' AND p_xldel = 'X'.
      SELECT (keyname) FROM (tabname) INTO buffer WHERE banks = country
                                                    AND bankl = keyval.
        WRITE: / 'Bank in use by', keydesc, buffer.
      ENDSELECT.
    ENDIF.
  ENDIF.
ENDFORM.                    "check_dynamic_table

* Create a new Batch Input Session
FORM create_bi_session USING VALUE(name)  TYPE apqi-groupid
                       CHANGING qid TYPE apqi-qid
                                xcanc.
  CALL FUNCTION 'BDC_OPEN_GROUP'
    EXPORTING
      client              = sy-mandt
      group               = name
      keep                = p_xkeep
      user                = sy-uname
    IMPORTING
      qid                 = qid
    EXCEPTIONS
      client_invalid      = 1
      destination_invalid = 2
      group_invalid       = 3
      group_is_locked     = 4
      holddate_invalid    = 5
      internal_error      = 6
      queue_error         = 7
      running             = 8
      system_lock_error   = 9
      user_invalid        = 10
      OTHERS              = 11.
  IF sy-subrc <> 0.
    _logmsgx e sy-msgno sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 sy-msgid.
    _logmsg a 161 name space space space.
    xcanc = 'X'.
  ENDIF.
ENDFORM.                    "create_bi_session

* Close Batch Input Session
FORM close_bi_session USING VALUE(name)
                      CHANGING xcanc.
  CALL FUNCTION 'BDC_CLOSE_GROUP'
    EXCEPTIONS
      not_open    = 1
      queue_error = 2
      OTHERS      = 3.
  IF sy-subrc <> 0.
    _logmsgx e sy-msgno sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 sy-msgid.
    _logmsg a 161 name space space space.
    xcanc = 'X'.
  ELSE.
    _logmsg s 162 name space space space.
  ENDIF.
ENDFORM.                    "close_bi_session

* Auto-run Batch Input Session in background (via job)
FORM run_bi_session USING VALUE(name)
                          VALUE(qid)   TYPE apqi-qid
                          VALUE(sdate) TYPE  tbtcjob-sdlstrtdt
                          VALUE(stime) TYPE  tbtcjob-sdlstrttm
                          VALUE(immed)
                    CHANGING xcanc.
  DATA: u_jobcount     LIKE tbtco-jobcount,
        u_jobname      LIKE tbtco-jobname,
        u_print_params LIKE pri_params,
        u_drucker      LIKE pri_params-pdest.
  u_drucker = sy-pdest.
  u_jobname = p_mappe.
* Get Jobcount
  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      jobname          = u_jobname
    IMPORTING
      jobcount         = u_jobcount
    EXCEPTIONS
      cant_create_job  = 1
      invalid_job_data = 2
      jobname_missing  = 3
      OTHERS           = 4.
  IF sy-subrc <> 0.
    _logmsgx e sy-msgno sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 sy-msgid.
    xcanc = 'X'.
  ENDIF.
* Change the print parameters
  CALL FUNCTION 'GET_PRINT_PARAMETERS'
    EXPORTING
      no_dialog              = 'X'
      destination            = u_drucker
      in_parameters          = u_print_params
      immediately            = ' '
    IMPORTING
      out_parameters         = u_print_params
    EXCEPTIONS
      archive_info_not_found = 1
      invalid_print_params   = 2
      invalid_archive_params = 3
      OTHERS                 = 4.
  IF sy-subrc <> 0.
    _logmsgx e sy-msgno sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 sy-msgid.
    xcanc = 'X'.
  ENDIF.
  CHECK xcanc IS INITIAL.
  SUBMIT rsbdcbtc
          WITH queue_id EQ qid
          USER sy-uname
          VIA JOB u_jobname NUMBER u_jobcount
          TO SAP-SPOOL
          SPOOL PARAMETERS u_print_params WITHOUT SPOOL DYNPRO
        AND RETURN.
  IF sy-subrc <> 0.
    _logmsgx e sy-msgno sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 sy-msgid.
    xcanc = 'X'.
  ENDIF.
  CHECK xcanc IS INITIAL.
  IF immed = 'X'.
    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount             = u_jobcount  "Jobnummer
        jobname              = u_jobname  "Jobname
        strtimmed            = 'X'  "Sofortstart
      EXCEPTIONS
        cant_start_immediate = 1
        invalid_startdate    = 2
        jobname_missing      = 3
        job_close_failed     = 4
        job_nosteps          = 5
        job_notex            = 6
        lock_failed          = 7
        OTHERS               = 8.
  ELSE.
    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount             = u_jobcount  "Jobnummer
        jobname              = u_jobname  "Jobname
        sdlstrtdt            = sdate
        sdlstrttm            = stime
      EXCEPTIONS
        cant_start_immediate = 1
        invalid_startdate    = 2
        jobname_missing      = 3
        job_close_failed     = 4
        job_nosteps          = 5
        job_notex            = 6
        lock_failed          = 7
        OTHERS               = 8.
  ENDIF.
  IF sy-subrc <> 0.
    _logmsgx e sy-msgno sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 sy-msgid.
    xcanc = 'X'.
  ELSE.
    _logmsg s 163 name qid u_jobname space.
  ENDIF.
ENDFORM.                    "run_bi_session

* Add dynpro to batch input data
FORM add_dynpro TABLES bdcdata STRUCTURE bdcdata
                USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.                    "add_dynpro

* Add field to batch input data
FORM add_field TABLES bdcdata STRUCTURE bdcdata
               USING fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.
ENDFORM.                    "add_field

* Handle user exit logic for single bank entry
* TK280714: Added address version for local language data import to user exit interface.
*           Local language data import can be identified in the user exit by checking
*           that SAP_ADVER is not initial.
* TK230216: Added possibility to populate NAME2 in CAM address
* TK211016: Added NATIDTYPE to user exit information
FORM call_user_exit CHANGING p_bank STRUCTURE v_bank
                             p_xskip.
  TABLES: zacc_exit.
  DATA: v_datin TYPE zacc_exit_data_in,
        v_datou TYPE zacc_exit_data_out,
        v_ufunc TYPE tfdir-funcname.
* Determine exit function to call
  SELECT SINGLE * FROM zacc_exit WHERE land1 = p_bank-sap_country.
  IF sy-subrc <> 0.
    SELECT SINGLE * FROM zacc_exit WHERE land1 = space.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
  ENDIF.
* Verify that the function we should call exists
  SELECT SINGLE funcname FROM tfdir INTO v_ufunc
                         WHERE funcname = zacc_exit-ufunc.
  IF sy-subrc <> 0.
    _logmsgc w 239 zacc_exit-ufunc p_bank-sap_country space space.
    EXIT.
  ENDIF.
* Log function use to help trouble-shooting
  _logmsgc i 240 zacc_exit-ufunc p_bank-sap_country space space.
* If we found any, prepare data for call
  MOVE-CORRESPONDING p_bank TO v_datin.
  MOVE-CORRESPONDING p_bank TO v_datou.
* Call exit function and evaluate exceptions
  CALL FUNCTION zacc_exit-ufunc
    EXPORTING
      data_in         = v_datin
      log_handle      = v_logid
    CHANGING
      data_out        = v_datou
    EXCEPTIONS
      skip_bank_entry = 1
      OTHERS          = 2.
  IF sy-subrc = 1.
    p_xskip = 'X'.
    EXIT.
  ELSEIF sy-subrc <> 0.
    _logmsg e 241 zacc_exit-ufunc p_bank-sap_country p_bank-sap_keyval
            p_bank-keyval.
    p_xskip = 'X'.
    EXIT.
  ENDIF.
* Pass back return information
  MOVE-CORRESPONDING v_datou TO p_bank.
ENDFORM.                    "CALL_USER_EXIT

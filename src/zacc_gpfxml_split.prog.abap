REPORT zacc_gpfxml_split MESSAGE-ID zacc LINE-SIZE 132.

***********************************************************************
*  Splits an Accuity GPF bank data file into more manageable bits up  *
*  to 999 files. The number of banks in one file can be selected.     *
*  The files are read from and written to the application server.     *
***********************************************************************
*  27/11/2007  Thomas Kaciuba, Steria Mummert Consulting AG / TDI     *
***********************************************************************
*  08/03/2010  Anton Wieser TDI, v 2.3: add select option country     *
*  09/02/2016  Thomas Kaciuba, v2.5: added S_TCODE authority check    *
***********************************************************************


TABLES: bnka.                                          " 08/03/2010 AW


SELECTION-SCREEN BEGIN OF BLOCK fil WITH FRAME TITLE text-001.
PARAMETERS: p_fname LIKE rlgrap-filename OBLIGATORY,
            p_fdest LIKE rlgrap-filename OBLIGATORY,
            p_count TYPE i OBLIGATORY DEFAULT '100000'.
SELECT-OPTIONS: p_banks FOR bnka-banks.                " 08/03/2010 AW
SELECTION-SCREEN END OF BLOCK fil.

***********************************************************************
*  Data Declaration                                                   *
***********************************************************************

DATA: v_count TYPE i,
      v_total TYPE i,
      v_proc  TYPE i,         "08/03/2010 AW
      v_flcnt(3) TYPE n,
      v_lnxml TYPE string,
      v_lndrt TYPE string,
      v_lnbank TYPE string,   "08/03/2010 AW
      v_pos TYPE i,           "08/03/2010 AW
      v_ctrok TYPE c,         "08/03/2010 AW
      v_banks TYPE bnka-banks,"08/03/2010 AW
      v_buf TYPE string,
      v_msg(100),
      v_msg2(100),            "08/03/2010 AW
      v_len TYPE i,
      v_tcode TYPE tstc-tcode.
DATA: v_loading,
      v_isbank,
      v_fname TYPE string,
      v_isopen,
      v_iserror.

* Application log management
DATA: v_loghd TYPE bal_s_log,
      v_logid TYPE balloghndl,
      v_logms TYPE bal_s_msg,
      v_logdp TYPE bal_s_prof,
      t_logid TYPE bal_t_logh.

***********************************************************************
*  Macro Definitions                                                  *
***********************************************************************

* Log message to Application Log (internal source)
DEFINE _logmsg.
  clear v_logms.
  v_logms-msgty = '&1'.
  v_logms-msgid = 'ZACC'.
  v_logms-msgno = '&2'.
  v_logms-msgv1 = &3.
  v_logms-msgv2 = &4.
  v_logms-msgv3 = &5.
  v_logms-msgv4 = &6.
  case '&1'.
    when 'A'.    v_logms-probclass = '1'.
    when 'E'.    v_logms-probclass = '2'.
    when 'W'.    v_logms-probclass = '3'.
    when others. v_logms-probclass = '4'.
  endcase.
  call function 'BAL_LOG_MSG_ADD'
    exporting
      i_log_handle     = v_logid
      i_s_msg          = v_logms
    exceptions
      log_not_found    = 1
      msg_inconsistent = 2
      log_is_full      = 3
      others           = 4.
  if sy-subrc <> 0.
    message e000 with 'Error writing log, return code'(102) sy-subrc.
  endif.
  if '&1' = 'A'.
    v_iserror = 'X'.
  endif.
END-OF-DEFINITION.

* Log message to Application Log (external source)
DEFINE _logmsgx.
  clear v_logms.
  v_logms-msgty = '&1'.
  v_logms-msgid = &7.
  v_logms-msgno = &2.
  v_logms-msgv1 = &3.
  v_logms-msgv2 = &4.
  v_logms-msgv3 = &5.
  v_logms-msgv4 = &6.
  case '&1'.
    when 'A'.    v_logms-probclass = '1'.
    when 'E'.    v_logms-probclass = '2'.
    when 'W'.    v_logms-probclass = '3'.
    when others. v_logms-probclass = '4'.
  endcase.
  call function 'BAL_LOG_MSG_ADD'
    exporting
      i_log_handle     = v_logid
      i_s_msg          = v_logms
    exceptions
      log_not_found    = 1
      msg_inconsistent = 2
      log_is_full      = 3
      others           = 4.
  if sy-subrc <> 0.
    message e000 with 'Error writing log, return code'(102) sy-subrc.
  endif.
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

START-OF-SELECTION.
* Initialise data
  CLEAR: v_count, v_total, v_flcnt, v_lnxml, v_lndrt, v_iserror.
  CLEAR: v_proc, v_lnbank, v_ctrok. "08/03/2010 AW
* Create an Application Log to handle all messages
  CLEAR: v_loghd, v_logid.
  v_loghd-extnumber = 'Accuity GPF XML File Split Log'.
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
  _logmsg i 200 'Split GPF XML file' space space space.
  v_len = strlen( p_fname ).
  IF v_len <= 50.
    _logmsg i 201 '<Import File>' p_fname space space.
  ELSE.
    v_len = v_len - 49.
    v_msg = '~'.
    v_msg+1 = p_fname+v_len.
    _logmsg i 201 '<Import File (shortened)>' v_msg space space.
  ENDIF.
  WRITE p_count TO v_msg LEFT-JUSTIFIED.
  _logmsg i 201 '<Block Size>' v_msg space space.
* Open file
  OPEN DATASET p_fname FOR INPUT IN TEXT MODE ENCODING DEFAULT
                       MESSAGE v_msg.
  IF sy-subrc <> 0.
    _logmsg e 203 v_msg(50) v_msg+50 space space.
    _logmsg a 202 'application server' space space space.
  ELSE.
    v_loading = 'X'.
    CLEAR: v_isbank, v_isopen, v_fname.
*   Read original file line by line
    WHILE v_loading = 'X'.
      READ DATASET p_fname INTO v_buf.
      IF sy-subrc <> 0.
        CLEAR v_loading.
      ELSE.
        PERFORM handle_line USING v_buf CHANGING v_loading.
      ENDIF.
    ENDWHILE.
*   Close last file and input file
    IF v_isopen = 'X'.
      PERFORM close_file USING v_fname.
    ENDIF.
    CLOSE DATASET p_fname.
  ENDIF.
* Statistics
  IF v_iserror IS INITIAL.
    WRITE v_total TO v_msg LEFT-JUSTIFIED.
    WRITE v_proc  TO v_msg2 LEFT-JUSTIFIED.  " 08/03/2010 AW
    _logmsg s 207 v_msg
                  v_msg2                     " 08/03/2010 AW
                  v_flcnt space.
  ENDIF.
* Save Application Log to database
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

***********************************************************************
*  Sub-Routines                                                       *
***********************************************************************

* Handle a line from the incoming file according to program status
FORM handle_line USING value(line) CHANGING loading.
  DATA: v_mod TYPE i.
* Is the line from the file header?
  IF line CS '<?xml'.
    v_lnxml = line.
  ELSEIF line CS '<DATAROOT'.
    v_lndrt = line.
  ELSEIF line CS '<BANK'.
    v_isbank = 'X'.
  ENDIF.


* Block delete 08/03/2010 AW
** If we are on the bank part, write the line into the output file
*  IF v_isbank = 'X'.
**   Do we need to close an old file first?
*    IF v_isopen = 'X' AND v_count = p_count.
*      PERFORM close_file USING v_fname.
*    ENDIF.
**   Do we need to open a new file
*    IF v_isopen IS INITIAL.
*      PERFORM open_file CHANGING v_fname loading.
*      CHECK loading = 'X'.
*    ENDIF.
**   Write the line
*    TRANSFER line TO v_fname.
* Block delete end 08/03/2010 AW


* Block insert 08/03/2010 AW
* If we are on the bank part, write the line into the output file
* if the country matches the selection parameter
  IF v_isbank = 'X'.

* First line of bank record: <BANK...
* Store line in v_lnbank
    IF v_banks IS INITIAL AND v_lnbank IS INITIAL. " 08/03/2010 AW
      v_lnbank = line.

* Second line of bank record: <COUNTRY>cc</COUNTRY>
* Read and check country. If OK, write bank line (v_lnbank) and
* current country line to file.
    ELSEIF v_banks IS INITIAL AND NOT v_lnbank IS INITIAL.
      FIND '<COUNTRY>' IN line MATCH OFFSET v_pos.
      IF sy-subrc = 0.
        v_pos = v_pos + 9.
        v_banks = line+v_pos(2).
        IF v_banks IN p_banks.
          v_ctrok = 'X'.
*   Do we need to close an old file first?
          IF v_isopen = 'X' AND v_count = p_count.
            PERFORM close_file USING v_fname.
          ENDIF.
*   Do we need to open a new file
          IF v_isopen IS INITIAL.
            PERFORM open_file CHANGING v_fname loading.
            CHECK loading = 'X'.
          ENDIF.
*   Write the line, first bank then country.
          TRANSFER v_lnbank TO v_fname.
          TRANSFER line TO v_fname.
        ELSE.
          CLEAR v_ctrok.
        ENDIF.
      ENDIF.

* Transfer all other lines
    ELSEIF v_ctrok = 'X'.
      TRANSFER line TO v_fname.

    ENDIF.
* Block end 08/03/2010 AW



*   Is this the end of the bank entry?
    IF line CS '</BANK>'.
      CLEAR v_isbank.
      CLEAR: v_lnbank, v_banks.
      ADD 1 TO v_total.
      IF v_ctrok = 'X'.                 "08/03/2010 AW
        ADD 1 TO: v_count, v_proc.     "08/03/2010 AW
        CLEAR v_ctrok.                 "08/03/2010 AW
      ENDIF.
      v_mod = v_total MOD 1000.
      IF v_mod = 0.
        WRITE v_total TO v_msg LEFT-JUSTIFIED.
        CONCATENATE v_msg 'bank entries processed' INTO v_msg
                    SEPARATED BY space.
        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
          EXPORTING
            text = v_msg.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    "handle_line

* Open a new output file
FORM open_file CHANGING fname loading.
* Can we open another file?
  IF v_flcnt = 999.
    _logmsg a 205 space space space space.
    CLEAR loading.
    EXIT.
  ENDIF.
* Create file name
  ADD 1 TO v_flcnt.
  CONCATENATE p_fdest v_flcnt '.xml' INTO fname.
* Open file
  OPEN DATASET fname FOR OUTPUT IN TEXT MODE ENCODING DEFAULT
                     MESSAGE v_msg.
  IF sy-subrc <> 0.
    _logmsg e 203 v_msg(50) v_msg+50 space space.
    _logmsg a 204 'application server' space space space.
    CLEAR loading.
  ELSE.
*   Write initial lines and set administrative data
    TRANSFER v_lnxml TO fname.
    TRANSFER v_lndrt TO fname.
    v_isopen = 'X'.
    CLEAR v_count.
  ENDIF.
ENDFORM.                    "open_file

* Close the current output file
FORM close_file USING value(fname).
* Update log
  WRITE v_count TO v_msg LEFT-JUSTIFIED.
  v_len = strlen( fname ).
  IF v_len <= 50.
    v_msg+50 = fname.
  ELSE.
    v_msg+50 = '~'.
    v_len = v_len - 49.
    v_msg+51 = fname+v_len.
  ENDIF.
  _logmsg i 206 v_msg v_msg+50 space space.
* write closing tag and close file
  v_buf = '</DATAROOT>'.
  TRANSFER v_buf TO fname.
  CLOSE DATASET fname.
  CLEAR v_isopen.
ENDFORM.                    "close_file

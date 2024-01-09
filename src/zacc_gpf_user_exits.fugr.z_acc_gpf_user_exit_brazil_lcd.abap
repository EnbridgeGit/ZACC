FUNCTION z_acc_gpf_user_exit_brazil_lcd .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(DATA_IN) TYPE  ZACC_EXIT_DATA_IN
*"     VALUE(LOG_HANDLE) TYPE  BALLOGHNDL OPTIONAL
*"  CHANGING
*"     REFERENCE(DATA_OUT) TYPE  ZACC_EXIT_DATA_OUT
*"  EXCEPTIONS
*"      SKIP_BANK_ENTRY
*"----------------------------------------------------------------------

* Sample User Exit for Brazilian banks
* Create the check digit for the Brazilian bank codes

  DATA: v_len TYPE i,
        v_code(15) TYPE c,
        v_cnew(15) TYPE c,
        v_chkno(2) TYPE n.

* Check if we got a 7 digit bank code to work with
  v_code = data_in-natid.
  v_len = strlen( v_code ).
  IF v_len = 7 AND v_code(7) CO '0123456789'.
*   Calculate check digit and create the new bank code
    v_chkno = ( v_code(1) * 7 + v_code+1(1) * 8 + v_code+2(1) * 9 ) MOD 11.
    CONCATENATE v_code(3) v_chkno+1(1) v_code+3(4) INTO v_cnew.
*   Update the local clearing code
    data_out-natid = v_cnew.
*   If the local clearing code is also the bank code, update the SAP bank key
    IF data_in-sap_bnkey = '1'.
      data_out-sap_keyval = v_cnew.
    ENDIF.
  ENDIF.

ENDFUNCTION.

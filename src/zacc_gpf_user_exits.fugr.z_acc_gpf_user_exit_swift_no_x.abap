FUNCTION z_acc_gpf_user_exit_swift_no_x .
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


* Remove XXX from Swift BIC if there is any
  IF data_in-sap_swift IS NOT INITIAL.
    IF data_in-sap_swift+8(3) = 'XXX'.
      data_out-sap_swift = data_in-sap_swift(8).
    ENDIF.
  ENDIF.

ENDFUNCTION.

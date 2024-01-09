FUNCTION z_acc_gpf_user_exit_no_swift .
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(DATA_IN) TYPE  ZACC_EXIT_DATA_IN
*"     VALUE(LOG_HANDLE) TYPE  BALLOGHNDL OPTIONAL
*"  CHANGING
*"     REFERENCE(DATA_OUT) TYPE  ZACC_EXIT_DATA_OUT
*"  EXCEPTIONS
*"      SKIP_BANK_ENTRY
*"--------------------------------------------------------------------


* If no Swift BIC is given, raise SKIP_BANK_ENTRY exception
  IF data_in-sap_swift IS INITIAL.
    RAISE skip_bank_entry.
  ENDIF.

ENDFUNCTION.

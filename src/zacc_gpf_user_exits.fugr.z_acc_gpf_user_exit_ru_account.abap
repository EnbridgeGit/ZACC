FUNCTION z_acc_gpf_user_exit_ru_account .
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

* Set bank branch (RU central bank account) during local language import
  IF data_in-sap_adver IS NOT INITIAL.
    data_out-sap_name2 = data_in-branch.
  ENDIF.

ENDFUNCTION.

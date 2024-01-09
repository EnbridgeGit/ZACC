FUNCTION z_acc_gpf_user_exit_us_territo .
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

* Sample User Exit for US banks

* Creates banks from US territories American Samoa (AS), Guam (GU),
* Northern Mariana Islands (MP), Puerto Rico (PR) and U.S. Virgin Islands (VI)
* under their ISO territory codes
  IF data_in-country = 'US' AND
     'AS/GU/MP/PR/VI' CS data_in-countrycsp.
*   If this is one of the above territories, set the country code and remove
*   the SAP region code (as this is now superfluous).
    data_out-sap_country = data_in-countrycsp.
    CLEAR data_out-sap_region.
  ENDIF.

ENDFUNCTION.

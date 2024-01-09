*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZACC_BANK_CODE
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZACC_BANK_CODE     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.

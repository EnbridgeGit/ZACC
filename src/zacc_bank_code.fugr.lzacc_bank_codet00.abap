*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZACC_BANK_CODE..................................*
DATA:  BEGIN OF STATUS_ZACC_BANK_CODE                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZACC_BANK_CODE                .
CONTROLS: TCTRL_ZACC_BANK_CODE
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZACC_BANK_CODE                .
TABLES: ZACC_BANK_CODE                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .

*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZACC_IBAN.......................................*
DATA:  BEGIN OF STATUS_ZACC_IBAN                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZACC_IBAN                     .
CONTROLS: TCTRL_ZACC_IBAN
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZACC_LOCAL_LANG.................................*
DATA:  BEGIN OF STATUS_ZACC_LOCAL_LANG               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZACC_LOCAL_LANG               .
CONTROLS: TCTRL_ZACC_LOCAL_LANG
            TYPE TABLEVIEW USING SCREEN '0003'.
*.........table declarations:.................................*
TABLES: *ZACC_IBAN                     .
TABLES: *ZACC_LOCAL_LANG               .
TABLES: ZACC_IBAN                      .
TABLES: ZACC_LOCAL_LANG                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .

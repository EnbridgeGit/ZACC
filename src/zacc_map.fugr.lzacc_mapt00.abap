*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZACC_BIC........................................*
DATA:  BEGIN OF STATUS_ZACC_BIC                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZACC_BIC                      .
CONTROLS: TCTRL_ZACC_BIC
            TYPE TABLEVIEW USING SCREEN '0200'.
*...processing: ZACC_EXIT.......................................*
DATA:  BEGIN OF STATUS_ZACC_EXIT                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZACC_EXIT                     .
CONTROLS: TCTRL_ZACC_EXIT
            TYPE TABLEVIEW USING SCREEN '0300'.
*...processing: ZACC_MAP........................................*
DATA:  BEGIN OF STATUS_ZACC_MAP                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZACC_MAP                      .
CONTROLS: TCTRL_ZACC_MAP
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZACC_BIC                      .
TABLES: *ZACC_EXIT                     .
TABLES: *ZACC_MAP                      .
TABLES: ZACC_BIC                       .
TABLES: ZACC_EXIT                      .
TABLES: ZACC_MAP                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .

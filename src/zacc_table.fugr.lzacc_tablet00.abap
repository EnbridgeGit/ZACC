*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZACC_CUKY.......................................*
DATA:  BEGIN OF STATUS_ZACC_CUKY                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZACC_CUKY                     .
CONTROLS: TCTRL_ZACC_CUKY
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZACC_CUKY                     .
TABLES: ZACC_CUKY                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .

*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZACC_REGI.......................................*
DATA:  BEGIN OF STATUS_ZACC_REGI                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZACC_REGI                     .
CONTROLS: TCTRL_ZACC_REGI
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZACC_REGI                     .
TABLES: ZACC_REGI                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .

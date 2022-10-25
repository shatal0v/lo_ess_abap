*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 14.09.2017 at 18:13:05
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZTHR_ESS_EKSTYPE................................*
DATA:  BEGIN OF STATUS_ZTHR_ESS_EKSTYPE              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTHR_ESS_EKSTYPE              .
CONTROLS: TCTRL_ZTHR_ESS_EKSTYPE
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTHR_ESS_EKSTYPE              .
TABLES: ZTHR_ESS_EKSTYPE               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .

*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 21.08.2017 at 16:28:25
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZTHR_AWART_MASSN................................*
DATA:  BEGIN OF STATUS_ZTHR_AWART_MASSN              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTHR_AWART_MASSN              .
CONTROLS: TCTRL_ZTHR_AWART_MASSN
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZTHR_AWART_MASSN              .
TABLES: ZTHR_AWART_MASSN               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .

*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 13.09.2017 at 15:06:22
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZHRUS_D2........................................*
TABLES: ZHRUS_D2, *ZHRUS_D2. "view work areas
CONTROLS: TCTRL_ZHRUS_D2
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZHRUS_D2. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZHRUS_D2.
* Table for entries selected to show on screen
DATA: BEGIN OF ZHRUS_D2_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZHRUS_D2.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZHRUS_D2_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZHRUS_D2_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZHRUS_D2.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZHRUS_D2_TOTAL.

*.........table declarations:.................................*
TABLES: HRUS_D2                        .

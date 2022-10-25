*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 31.01.2018 at 11:19:03
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZHRV_AWART_COLOR................................*
TABLES: ZHRV_AWART_COLOR, *ZHRV_AWART_COLOR. "view work areas
CONTROLS: TCTRL_ZHRV_AWART_COLOR
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZHRV_AWART_COLOR. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZHRV_AWART_COLOR.
* Table for entries selected to show on screen
DATA: BEGIN OF ZHRV_AWART_COLOR_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZHRV_AWART_COLOR.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZHRV_AWART_COLOR_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZHRV_AWART_COLOR_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZHRV_AWART_COLOR.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZHRV_AWART_COLOR_TOTAL.

*.........table declarations:.................................*
TABLES: T554S                          .
TABLES: T554T                          .
TABLES: ZHRT_AWART_COLOR               .

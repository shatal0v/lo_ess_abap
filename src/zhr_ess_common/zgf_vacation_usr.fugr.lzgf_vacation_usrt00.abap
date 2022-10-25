*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 31.01.2018 at 11:32:25
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZVVACATION_USERS................................*
TABLES: ZVVACATION_USERS, *ZVVACATION_USERS. "view work areas
CONTROLS: TCTRL_ZVVACATION_USERS
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZVVACATION_USERS. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZVVACATION_USERS.
* Table for entries selected to show on screen
DATA: BEGIN OF ZVVACATION_USERS_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZVVACATION_USERS.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZVVACATION_USERS_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZVVACATION_USERS_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZVVACATION_USERS.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZVVACATION_USERS_TOTAL.

*.........table declarations:.................................*
TABLES: ZTVACATION_USERS               .

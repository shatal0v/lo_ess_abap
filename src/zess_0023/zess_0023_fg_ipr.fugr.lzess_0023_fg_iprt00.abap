*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 14.04.2020 at 10:30:23
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZESS_0023_T_AEDU................................*
DATA:  BEGIN OF STATUS_ZESS_0023_T_AEDU              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZESS_0023_T_AEDU              .
CONTROLS: TCTRL_ZESS_0023_T_AEDU
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZESS_0023_V_AEDU................................*
TABLES: ZESS_0023_V_AEDU, *ZESS_0023_V_AEDU. "view work areas
CONTROLS: TCTRL_ZESS_0023_V_AEDU
TYPE TABLEVIEW USING SCREEN '0002'.
DATA: BEGIN OF STATUS_ZESS_0023_V_AEDU. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZESS_0023_V_AEDU.
* Table for entries selected to show on screen
DATA: BEGIN OF ZESS_0023_V_AEDU_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZESS_0023_V_AEDU.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZESS_0023_V_AEDU_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZESS_0023_V_AEDU_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZESS_0023_V_AEDU.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZESS_0023_V_AEDU_TOTAL.

*...processing: ZESS_0023_V_FEDU................................*
TABLES: ZESS_0023_V_FEDU, *ZESS_0023_V_FEDU. "view work areas
CONTROLS: TCTRL_ZESS_0023_V_FEDU
TYPE TABLEVIEW USING SCREEN '0008'.
DATA: BEGIN OF STATUS_ZESS_0023_V_FEDU. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZESS_0023_V_FEDU.
* Table for entries selected to show on screen
DATA: BEGIN OF ZESS_0023_V_FEDU_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZESS_0023_V_FEDU.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZESS_0023_V_FEDU_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZESS_0023_V_FEDU_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZESS_0023_V_FEDU.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZESS_0023_V_FEDU_TOTAL.

*...processing: ZESS_0023_V_LEDU................................*
TABLES: ZESS_0023_V_LEDU, *ZESS_0023_V_LEDU. "view work areas
CONTROLS: TCTRL_ZESS_0023_V_LEDU
TYPE TABLEVIEW USING SCREEN '0006'.
DATA: BEGIN OF STATUS_ZESS_0023_V_LEDU. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZESS_0023_V_LEDU.
* Table for entries selected to show on screen
DATA: BEGIN OF ZESS_0023_V_LEDU_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZESS_0023_V_LEDU.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZESS_0023_V_LEDU_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZESS_0023_V_LEDU_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZESS_0023_V_LEDU.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZESS_0023_V_LEDU_TOTAL.

*...processing: ZESS_0023_V_PEDU................................*
TABLES: ZESS_0023_V_PEDU, *ZESS_0023_V_PEDU. "view work areas
CONTROLS: TCTRL_ZESS_0023_V_PEDU
TYPE TABLEVIEW USING SCREEN '0003'.
DATA: BEGIN OF STATUS_ZESS_0023_V_PEDU. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZESS_0023_V_PEDU.
* Table for entries selected to show on screen
DATA: BEGIN OF ZESS_0023_V_PEDU_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZESS_0023_V_PEDU.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZESS_0023_V_PEDU_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZESS_0023_V_PEDU_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZESS_0023_V_PEDU.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZESS_0023_V_PEDU_TOTAL.

*...processing: ZESS_0023_V_REDU................................*
TABLES: ZESS_0023_V_REDU, *ZESS_0023_V_REDU. "view work areas
CONTROLS: TCTRL_ZESS_0023_V_REDU
TYPE TABLEVIEW USING SCREEN '0007'.
DATA: BEGIN OF STATUS_ZESS_0023_V_REDU. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZESS_0023_V_REDU.
* Table for entries selected to show on screen
DATA: BEGIN OF ZESS_0023_V_REDU_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZESS_0023_V_REDU.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZESS_0023_V_REDU_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZESS_0023_V_REDU_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZESS_0023_V_REDU.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZESS_0023_V_REDU_TOTAL.

*...processing: ZESS_T_REAS_ID..................................*
DATA:  BEGIN OF STATUS_ZESS_T_REAS_ID                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZESS_T_REAS_ID                .
CONTROLS: TCTRL_ZESS_T_REAS_ID
            TYPE TABLEVIEW USING SCREEN '0005'.
*...processing: ZVHR_EDU_PROGRAM................................*
TABLES: ZVHR_EDU_PROGRAM, *ZVHR_EDU_PROGRAM. "view work areas
CONTROLS: TCTRL_ZVHR_EDU_PROGRAM
TYPE TABLEVIEW USING SCREEN '0004'.
DATA: BEGIN OF STATUS_ZVHR_EDU_PROGRAM. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZVHR_EDU_PROGRAM.
* Table for entries selected to show on screen
DATA: BEGIN OF ZVHR_EDU_PROGRAM_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZVHR_EDU_PROGRAM.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZVHR_EDU_PROGRAM_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZVHR_EDU_PROGRAM_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZVHR_EDU_PROGRAM.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZVHR_EDU_PROGRAM_TOTAL.

*.........table declarations:.................................*
TABLES: *ZESS_0023_T_AEDU              .
TABLES: *ZESS_T_REAS_ID                .
TABLES: *ZESS_T_REAS_TXT               .
TABLES: T7RUOKIN                       .
TABLES: ZESS_0023_T_AEDU               .
TABLES: ZESS_T_REAS_ID                 .
TABLES: ZESS_T_REAS_TXT                .
TABLES: ZTHR_DLINET                    .
TABLES: ZTHR_EDU_PROGRAM               .
TABLES: ZTHR_EDU_PROGRAT               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .

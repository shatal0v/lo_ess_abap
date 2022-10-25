*&---------------------------------------------------------------------*
*&  Include           ZHR_PA014_TOP
*&---------------------------------------------------------------------*

TABLES: pernr.

NODES: peras.

INFOTYPES: 0000, 0001, 0002, 0004, 0007, 0008, 0016, 0019, 0041, 2001, 2006.

CLASS: lcl DEFINITION DEFERRED.

DATA: l_lcl TYPE REF TO lcl.

SELECTION-SCREEN: BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
PARAMETERS: p_norm  AS CHECKBOX
          , p_anzhl TYPE anzhl DEFAULT '2.33'
          , zz_new  type flag NO-DISPLAY DEFAULT 'X'
          .
SELECT-OPTIONS: s_ktart FOR p2006-ktart no INTERVALS.
SELECTION-SCREEN: END OF BLOCK block1.


PARAMETERS: p_explim TYPE flag NO-DISPLAY.

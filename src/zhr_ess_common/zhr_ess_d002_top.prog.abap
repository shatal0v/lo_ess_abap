*&---------------------------------------------------------------------*
*&  Include           ZHR_ESS_D002_TOP
*&---------------------------------------------------------------------*
TABLES: pa2001
        .
SELECTION-SCREEN: BEGIN OF BLOCK block1 WITH FRAME.
SELECT-OPTIONS: s_pernr FOR pa2001-pernr NO INTERVALS MATCHCODE OBJECT prem
              , s_awart FOR pa2001-awart NO INTERVALS

              .
PARAMETERS: p_begda TYPE begda
          , p_days  TYPE int2
          .
SELECTION-SCREEN: END OF BLOCK block1.

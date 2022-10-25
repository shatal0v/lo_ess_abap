*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZESS_0023_FG_IPR
*   generation date: 31.01.2020 at 12:14:05
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZESS_0023_FG_IPR   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.

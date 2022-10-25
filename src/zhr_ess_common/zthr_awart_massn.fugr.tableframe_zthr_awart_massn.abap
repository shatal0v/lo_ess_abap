*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZTHR_AWART_MASSN
*   generation date: 21.08.2017 at 16:28:22
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZTHR_AWART_MASSN   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.

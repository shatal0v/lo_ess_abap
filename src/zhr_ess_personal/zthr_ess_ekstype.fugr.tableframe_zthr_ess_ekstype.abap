*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZTHR_ESS_EKSTYPE
*   generation date: 14.09.2017 at 18:12:59
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZTHR_ESS_EKSTYPE   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.

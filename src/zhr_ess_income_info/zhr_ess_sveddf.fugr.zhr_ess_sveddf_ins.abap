FUNCTION zhr_ess_sveddf_ins.
*"----------------------------------------------------------------------
*"*"Функциональный модуль обновления:
*"
*"*"Локальный интерфейс:
*"  IMPORTING
*"     VALUE(IS_ZTHR_ESS_SVEDDF) TYPE  ZTHR_ESS_SVEDDF
*"  EXCEPTIONS
*"      NO_BLOCK
*"----------------------------------------------------------------------
  DATA: ls_ess_sveddf TYPE zthr_ess_sveddf.

  MOVE-CORRESPONDING is_zthr_ess_sveddf TO ls_ess_sveddf.

  ls_ess_sveddf-mandt = sy-mandt.

  CALL FUNCTION 'ENQUEUE_EZLO_SVEDDF'
    EXPORTING
      mode_zthr_ess_sveddf = 'E'
      mandt                = sy-mandt
      pernr                = is_zthr_ess_sveddf-pernr
*     ZYEAR                =
*     SEQNR                =
    EXCEPTIONS
      foreign_lock         = 1
      system_failure       = 2
      OTHERS               = 3.
  IF sy-subrc <> 0.
    RAISE no_block.
  ENDIF.

  INSERT INTO zthr_ess_sveddf VALUES ls_ess_sveddf .

  IF sy-subrc <> 0 .
    MESSAGE text-m01 TYPE 'A'.
  ENDIF.

  CALL FUNCTION 'DEQUEUE_EZLO_SVEDDF'
    EXPORTING
      mandt = sy-mandt
      pernr = is_zthr_ess_sveddf-pernr.

ENDFUNCTION.

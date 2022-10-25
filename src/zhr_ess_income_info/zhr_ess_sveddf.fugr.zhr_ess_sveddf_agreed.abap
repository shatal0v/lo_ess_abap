FUNCTION zhr_ess_sveddf_agreed.
*"----------------------------------------------------------------------
*"*"Функциональный модуль обновления:
*"
*"*"Локальный интерфейс:
*"  IMPORTING
*"     VALUE(IS_ESS_DOCREQ) TYPE  ZTHR_ESS_DOCREQ
*"  EXCEPTIONS
*"      NO_BLOCK
*"      NO_FILE
*"----------------------------------------------------------------------

  DATA: ls_ess_sveddf TYPE zthr_ess_sveddf,
        lv_pernr      TYPE pernr_d,
        lv_zyear      TYPE gjahr,
        lv_seqnr      TYPE seqnr.

  SELECT pernr zyear MAX( seqnr ) INTO (lv_pernr, lv_zyear, lv_seqnr)
    FROM zthr_ess_sveddf
    WHERE pernr = is_ess_docreq-pernr
      AND zyear = is_ess_docreq-gjahr
    GROUP BY pernr zyear.
  ENDSELECT.
  IF sy-subrc <> 0.
    RAISE no_file.
  ENDIF.
  CALL FUNCTION 'ENQUEUE_EZLO_SVEDDF'
    EXPORTING
      mode_zthr_ess_sveddf = 'E'
      mandt                = sy-mandt
      pernr                = is_ess_docreq-pernr
*     ZYEAR                =
*     SEQNR                =
    EXCEPTIONS
      foreign_lock         = 1
      system_failure       = 2
      OTHERS               = 3.
  IF sy-subrc <> 0.
    RAISE no_block.
  ENDIF.

  UPDATE zthr_ess_sveddf SET agreed = abap_false
   WHERE pernr = lv_pernr
     AND zyear = lv_zyear .

  UPDATE zthr_ess_sveddf SET agreed = abap_true
   WHERE pernr = lv_pernr
     AND zyear = lv_zyear
     AND seqnr = lv_seqnr .

  IF sy-subrc <> 0 .
    MESSAGE text-m01 TYPE 'A'.
  ENDIF.

  CALL FUNCTION 'DEQUEUE_EZLO_SVEDDF'
    EXPORTING
      mandt = sy-mandt
      pernr = is_ess_docreq-pernr.




ENDFUNCTION.

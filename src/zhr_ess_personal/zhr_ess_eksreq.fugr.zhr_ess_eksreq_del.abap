FUNCTION ZHR_ESS_EKSREQ_DEL.
*"--------------------------------------------------------------------
*"*"Функц. модуль обновления:
*"
*"*"Локальный интерфейс:
*"  IMPORTING
*"     VALUE(IV_REQ_ID) TYPE  ZEHR_ESS_EKSREQ_REQID
*"--------------------------------------------------------------------
  CHECK iv_req_id IS NOT INITIAL.

  DELETE FROM zthr_ess_eksreq WHERE req_id = iv_req_id.
ENDFUNCTION.

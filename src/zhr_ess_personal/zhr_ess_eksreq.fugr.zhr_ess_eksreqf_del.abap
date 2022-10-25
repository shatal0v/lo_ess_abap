FUNCTION zhr_ess_eksreqf_del.
*"----------------------------------------------------------------------
*"*"Функциональный модуль обновления:
*"
*"*"Локальный интерфейс:
*"  IMPORTING
*"     VALUE(IV_REQ_ID) TYPE  ZEHR_ESS_EKSREQ_REQID
*"----------------------------------------------------------------------
  CHECK iv_req_id IS NOT INITIAL.

  DELETE FROM zthr_ess_eksreqf WHERE req_id = iv_req_id.
ENDFUNCTION.

FUNCTION ZHR_ESS_EKSREQ_INS.
*"----------------------------------------------------------------------
*"*"Функциональный модуль обновления:
*"
*"*"Локальный интерфейс:
*"  IMPORTING
*"     VALUE(IS_DATA) TYPE  ZTHR_ESS_EKSREQ
*"----------------------------------------------------------------------
  CHECK is_data IS NOT INITIAL.

  INSERT INTO zthr_ess_eksreq VALUES @is_data.
ENDFUNCTION.

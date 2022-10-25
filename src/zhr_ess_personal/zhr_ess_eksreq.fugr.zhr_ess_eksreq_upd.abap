FUNCTION zhr_ess_eksreq_upd.
*"----------------------------------------------------------------------
*"*"Функциональный модуль обновления:
*"
*"*"Локальный интерфейс:
*"  IMPORTING
*"     VALUE(IT_DATA) TYPE  ZTHR_ESS_EKSREQ_T
*"----------------------------------------------------------------------
  CHECK it_data IS NOT INITIAL.

  UPDATE zthr_ess_eksreq FROM TABLE it_data.
ENDFUNCTION.

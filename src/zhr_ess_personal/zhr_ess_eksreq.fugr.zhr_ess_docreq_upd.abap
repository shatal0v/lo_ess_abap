FUNCTION ZHR_ESS_DOCREQ_UPD.
*"----------------------------------------------------------------------
*"*"Функциональный модуль обновления:
*"
*"*"Локальный интерфейс:
*"  IMPORTING
*"     VALUE(IT_DATA) TYPE  ZTHR_ESS_DOCREQ_T
*"----------------------------------------------------------------------
  CHECK it_data IS NOT INITIAL.

  UPDATE zthr_ess_docreq FROM TABLE it_data.
ENDFUNCTION.

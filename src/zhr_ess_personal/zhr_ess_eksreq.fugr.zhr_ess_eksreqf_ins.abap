FUNCTION ZHR_ESS_EKSREQF_INS.
*"----------------------------------------------------------------------
*"*"Функциональный модуль обновления:
*"
*"*"Локальный интерфейс:
*"  IMPORTING
*"     VALUE(IS_DATA) TYPE  ZTHR_ESS_EKSREQF
*"----------------------------------------------------------------------
  CHECK is_data IS NOT INITIAL.

  INSERT INTO zthr_ess_eksreqf VALUES @is_data.
ENDFUNCTION.

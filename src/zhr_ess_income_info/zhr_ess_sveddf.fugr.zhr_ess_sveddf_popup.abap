FUNCTION zhr_ess_sveddf_popup.
*"----------------------------------------------------------------------
*"*"Локальный интерфейс:
*"  IMPORTING
*"     REFERENCE(IV_PERNR) TYPE  PERNR_D
*"     REFERENCE(IV_YEAR) TYPE  GJAHR OPTIONAL
*"----------------------------------------------------------------------

  PERFORM fill_data USING iv_pernr iv_year.

  CREATE OBJECT lo_assistent.

  CHECK sy-batch IS INITIAL.

  CALL SCREEN 0200 STARTING AT 5  5
                     ENDING AT 95 20.



ENDFUNCTION.

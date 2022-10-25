*&---------------------------------------------------------------------*
*&  Include  ZRPTARQPOST_SELSCREEN_01
*&---------------------------------------------------------------------*
INITIALIZATION.

  SELECTION-SCREEN BEGIN OF BLOCK zb1 WITH FRAME TITLE zztext01.
  SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS:
    p_zz01  AS CHECKBOX.
  SELECTION-SCREEN COMMENT 3(30) zztext02 FOR FIELD p_zz01.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS:
    p_nodel AS CHECKBOX.
  SELECTION-SCREEN COMMENT 3(30) zztext03 FOR FIELD p_nodel.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN END OF BLOCK zb1.

INITIALIZATION.
  zztext01 = 'Параметры запуска (Z-доработка)'.
  zztext02 = 'Провести мероприятия'.
  zztext03 = 'Блокирование вместо удаления'.

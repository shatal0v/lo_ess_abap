*&---------------------------------------------------------------------*
*&  Include           ZHR_ESS_D001_TOP
*&---------------------------------------------------------------------*
TABLES: objec
      , pernr
      , zthr_ess_d001
      .

DATA: gv_subty  TYPE subty
    , gv_massn  TYPE massn
    , gv_awdtp  TYPE awdtp
    .

SELECTION-SCREEN: BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_begda FOR objec-begda NO-EXTENSION NO DATABASE SELECTION
              , s_objid FOR objec-objid NO INTERVALS
              , s_pernr FOR pernr-pernr MATCHCODE OBJECT prem.
SELECTION-SCREEN: END OF BLOCK block1.

SELECTION-SCREEN: BEGIN OF BLOCK block4 WITH FRAME TITLE text-004.
PARAMETERS p_itype TYPE zhr_ess_d001_t AS LISTBOX VISIBLE LENGTH 12 DEFAULT '0'.
SELECTION-SCREEN: END OF BLOCK block4.

*SELECTION-SCREEN: BEGIN OF BLOCK block6 WITH FRAME TITLE text-009.
*parameters: p_verp TYPE flag.
*SELECTION-SCREEN: END OF BLOCK block6.

SELECTION-SCREEN: BEGIN OF BLOCK block2 WITH FRAME TITLE text-002.
PARAMETERS: p_orgeh AS CHECKBOX DEFAULT 'X' "Подразделения   ( otype = O)
          , p_stell AS CHECKBOX DEFAULT 'X' "Должности       ( otype = С)
          , p_plans AS CHECKBOX DEFAULT 'X' "Штатные единицы ( otype = S)
          , p_per00 TYPE flag AS CHECKBOX DEFAULT 'X' "Общие сведения
          , p_per12 TYPE flag AS CHECKBOX DEFAULT 'X' "Знание иностранного языка "KLOKOVNYU 27.05.2019
          , p_per01 TYPE flag AS CHECKBOX DEFAULT 'X' "Воинский учет
          , p_per02 TYPE flag AS CHECKBOX DEFAULT 'X' "Образование
          , p_per13 TYPE flag AS CHECKBOX DEFAULT 'X' "Послевузовское профессиональное образование "KLOKOVNYU 27.05.2019
          , p_per03 TYPE flag AS CHECKBOX DEFAULT 'X' "Трудовая деятельность в государственном органе
          , p_per04 TYPE flag AS CHECKBOX DEFAULT 'X' "Трудовая должность до замещения должности в государственном органе
          , p_per05 TYPE flag AS CHECKBOX DEFAULT 'X' "Сведения об окладе и надбавках
          , p_per06 TYPE flag AS CHECKBOX DEFAULT 'X' "Состав семьи
          , p_per07 TYPE flag AS CHECKBOX DEFAULT 'X' "Повышение квалификации, Профессиональная переподготовка
          , p_per14 TYPE flag AS CHECKBOX DEFAULT 'X' "Награды (поощрения), почетные звания "KLOKOVNYU 27.05.2019
          , p_per08 TYPE flag AS CHECKBOX DEFAULT 'X' "Аттестация
          , p_per09 TYPE flag AS CHECKBOX DEFAULT 'X' "Отпуск
          , p_per15 TYPE flag AS CHECKBOX DEFAULT 'X' "Социальные льготы  "KLOKOVNYU 27.05.2019
          , p_per10 TYPE flag AS CHECKBOX DEFAULT 'X' "Классный чин, воинское звание
          , p_per11 TYPE flag AS CHECKBOX DEFAULT 'X' "Сведения об увольнении
          .
SELECTION-SCREEN: END OF BLOCK block2.

SELECTION-SCREEN: BEGIN OF BLOCK block3 WITH FRAME TITLE text-003.
PARAMETERS: p_folder TYPE string OBLIGATORY LOWER CASE DEFAULT 'C:\'
          , p_uid    TYPE string DEFAULT '37d7dd32-1996-4b5d-913b-6c7daf3831fa'
          , p_admin  TYPE orgeh  DEFAULT '50000154'
          .
SELECT-OPTIONS: s_2001s FOR gv_subty NO INTERVALS
              , s_mfire FOR gv_massn NO INTERVALS
              , s_0183s FOR gv_awdtp NO INTERVALS
              .
SELECTION-SCREEN: END OF BLOCK block3.

"PARAMETER p_seting TYPE char1 AS CHECKBOX USER-COMMAND set.
PARAMETER p_seting TYPE char1 AS CHECKBOX DEFAULT 'X'.
PARAMETER p_missd TYPE char1 AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN: BEGIN OF BLOCK block5 WITH FRAME TITLE text-007.
"PARAMETERS: p_p_weg TYPE wegid MODIF ID set
"          , p_o_weg TYPE wegid MODIF ID set
PARAMETERS: p_p_weg TYPE wegid DEFAULT 'ZPSCOO'
          , p_o_weg TYPE wegid DEFAULT 'ZOOSPC'
          .

SELECTION-SCREEN: END OF BLOCK block5.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_objid-low.
  IF s_begda[] IS INITIAL.
    MESSAGE |Ввод не возможен: необходлимо заполнить поле дата| TYPE 'I' DISPLAY LIKE 'W'.
    RETURN.
  ENDIF.
  DATA: lt_marked_objects   TYPE TABLE OF hrsobid
      , lt_sel_hrobject_tab TYPE TABLE OF hrobject
      .
  LOOP AT s_objid ASSIGNING FIELD-SYMBOL(<s_objid>).
    APPEND VALUE #( plvar = zcl_hr_get_data=>a_plvar otype = 'O' sobid = <s_objid>-low ) TO lt_marked_objects.
  ENDLOOP.

  CALL FUNCTION 'RH_OBJID_REQUEST'
    EXPORTING
      plvar            = zcl_hr_get_data=>a_plvar
      otype            = 'O'
      seark_begda      = VALUE wplog-begda( s_begda[ 1 ]-low OPTIONAL )
      seark_endda      = VALUE wplog-endda( s_begda[ 1 ]-high OPTIONAL )
      set_mode         = abap_true
    TABLES
      marked_objects   = lt_marked_objects
      sel_hrobject_tab = lt_sel_hrobject_tab
    EXCEPTIONS
      cancelled        = 1
      wrong_condition  = 2
      nothing_found    = 3
      internal_error   = 4
      illegal_mode     = 5
      OTHERS           = 6.

  IF sy-subrc = 0.
    REFRESH s_objid[].
    CLEAR: s_objid.
    LOOP AT lt_sel_hrobject_tab ASSIGNING FIELD-SYMBOL(<lt_sel_hrobject_tab>).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = <lt_sel_hrobject_tab>-objid ) TO s_objid[].
    ENDLOOP.
    READ TABLE s_objid INDEX 1.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_folder.
  DATA(lv_folder) = p_folder.

  CALL METHOD cl_gui_frontend_services=>directory_browse
    CHANGING
      selected_folder      = lv_folder
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.
  IF sy-subrc = 0.
    p_folder  = lv_folder.
  ENDIF.

AT SELECTION-SCREEN OUTPUT.

  IF s_objid IS NOT INITIAL AND s_pernr IS NOT INITIAL.
    CLEAR s_pernr.
  ENDIF.

  LOOP AT SCREEN.
    IF screen-group1 = 'SET'.
      IF p_seting IS INITIAL.
        screen-input = '0'.
        screen-output = '0'.
        screen-invisible = '1'.
      ELSE.
        screen-input = '1'.
        screen-output = '1'.
        screen-invisible = '0'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
    IF screen-name = 'P_ADMIN'.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

INITIALIZATION.
  s_begda[] = VALUE #( ( sign = 'I' option = 'BT' low = sy-datum high = sy-datum ) ).
  s_mfire-sign = 'I'.
  s_mfire-option = 'EQ'.
  s_mfire-low = 'ZO'.
  APPEND s_mfire.
  s_mfire-low = 'ZP'.
  APPEND s_mfire.
  s_mfire-low = 'ZN'.
  APPEND s_mfire.
  s_mfire-low = 'Y1'.
  APPEND s_mfire.
  s_2001s-sign = 'I'.
  s_2001s-option = 'EQ'.
  s_2001s-low = '0100'.
  APPEND s_2001s.
  s_2001s-low = '0202'.
  APPEND s_2001s.
  s_2001s-low = '0502'.
  APPEND s_2001s.
  s_0183s-sign = 'I'.
  s_0183s-option = 'CP'.
  s_0183s-low = '1*'.
  APPEND s_0183s.
  s_0183s-low = '2*'.
  APPEND s_0183s.
  s_0183s-low = '3*'.
  APPEND s_0183s.
  s_0183s-low = '4*'.
  APPEND s_0183s.
  s_0183s-low = '5*'.
  APPEND s_0183s.
  s_0183s-low = '6*'.
  APPEND s_0183s.
  s_0183s-low = '7*'.
  APPEND s_0183s.
  s_0183s-low = '8*'.
  APPEND s_0183s.
  s_0183s-low = '9*'.
  APPEND s_0183s.

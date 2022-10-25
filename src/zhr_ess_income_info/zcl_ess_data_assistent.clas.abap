class ZCL_ESS_DATA_ASSISTENT definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF GTYS_INPUT_DATA,
           main_user     TYPE string,
           entrance_user TYPE string,
         END OF GTYS_INPUT_DATA .
  types:
    gtytd_output_income_review TYPE STANDARD TABLE OF zshr_ess_ws_incomeinfo_review .
  types:
    gtytd_personal_family TYPE STANDARD TABLE OF zshr_ess_ws_personal_family .
  types:
    gtytd_personal_address TYPE STANDARD TABLE OF zshr_ess_ws_personal_address .
  types:
    gtytd_EXPERIENCE TYPE STANDARD TABLE OF ZSHR_ESS_WS_EXPERIENCE .

  constants GC_PERSONAL_MEMORY_ID_T2 type CHAR20 value 'ZHR_ESS_PERS_T2' ##NO_TEXT.

  class-methods EXPERIENCE_GET_DATA
    importing
      !IS_INPUT_DATA type GTYS_INPUT_DATA
    exporting
      !ET_OUTPUT_DATA type GTYTD_EXPERIENCE .
  class-methods GET_INPUT_FILTER
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET
      !IT_FILTER_SELECT_OPTIONS type /IWBEP/T_MGW_SELECT_OPTION
    exporting
      !ES_INPUT_DATA type ANY .
  class-methods INCOME_INFO_REVIEW
    importing
      !IS_INPUT_DATA type GTYS_INPUT_DATA
    exporting
      value(ET_OUTPUT_DATA) type GTYTD_OUTPUT_INCOME_REVIEW .
  class-methods INCOME_INFO_SUBMIT
    importing
      !IS_INPUT_DATA type ZSHR_ESS_WS_INCOMEINFO_REVIEW
    exporting
      !ES_OUTPUT_DATA type ZSHR_ESS_WS_INCOMEINFO_REVIEW .
  class-methods PERSONAL_GET_ADDRESS_DATA
    importing
      !IV_PERNR type PERNR_D
    exporting
      !ET_DATA type GTYTD_PERSONAL_ADDRESS .
  class-methods PERSONAL_GET_EMPLOYER_DATA
    importing
      !IV_PERNR type PERNR_D
    exporting
      !ES_DATA type ZSHR_ESS_WS_PERSONAL_EMP .
  class-methods PERSONAL_GET_FAMILY_DATA
    importing
      !IV_PERNR type PERNR_D
    exporting
      !ET_DATA type GTYTD_PERSONAL_FAMILY .
  class-methods PERSONAL_GET_T2_DATA
    importing
      !IV_PERNR type PERNR_D
    exporting
      !EV_CONTENT type XSTRING
      !EV_FILENAME type STRING .
  class-methods GET_REF_UNTIL_DATE
    importing
      !IV_PLANS type HROBJID
      !IV_YEAR type JAHR
    returning
      value(RV_DATE) type DATUM .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ESS_DATA_ASSISTENT IMPLEMENTATION.


METHOD experience_get_data.

  DATA: lr_filter_proce TYPE RANGE OF t7rusen-proce,
        lo_filter       TYPE REF TO /iwbep/cl_mgw_req_filter.

  "Определение табельного номера
  DATA(lo_hr_mss) = NEW zcl_mss_data_assistent( ).

  IF is_input_data-entrance_user IS NOT INITIAL.
    DATA(lv_pernr) = lo_hr_mss->get_pernr( iv_usrid = CONV #( is_input_data-entrance_user ) ).
  ELSE.
    lv_pernr = lo_hr_mss->get_pernr( iv_usrid = CONV #( is_input_data-main_user ) ).
  ENDIF.

  "Получение фильтра по возвращаемым стажам
  zcl_hr_get_data=>read_stvarv_ran( EXPORTING iv_name = 'ZHR_ESS_EXPERIENCE_PROCE'
                                    IMPORTING et_range = lr_filter_proce ).

  SELECT t7rusen~numbr,
         t7rusen~proce,
         t7rusent~proct
    INTO TABLE @DATA(lt_proce)
    FROM t7rusen
    LEFT JOIN t7rusent ON t7rusent~molga = t7rusen~molga
                      AND t7rusent~numbr = t7rusen~numbr
                      AND t7rusent~endda = t7rusen~endda
                      AND t7rusent~spras = @sy-langu
    WHERE t7rusen~molga = @zcl_hr_data=>gc_molga
      AND t7rusen~proce IN @lr_filter_proce.

  DATA: lt_table TYPE TABLE OF zhr_explist_alv_01
      , lr_pernr TYPE RANGE OF pernr_d
      , lr_numbr TYPE RANGE OF t7rusen-numbr
      .

  APPEND VALUE #( sign = `I` option = `EQ` low = lv_pernr ) TO lr_pernr.

  cl_salv_bs_runtime_info=>set( display  = abap_false
                                metadata = abap_false
                                data     = abap_true ).

  SUBMIT zhr_explist_01 WITH pnppernr IN lr_pernr AND RETURN.

  TRY.
      cl_salv_bs_runtime_info=>get_data( IMPORTING t_data = lt_table ).
    CATCH cx_salv_bs_sc_runtime_info.
  ENDTRY.

  cl_salv_bs_runtime_info=>clear_all( ).
  READ TABLE lt_table ASSIGNING FIELD-SYMBOL(<lt_table>) INDEX 1.

  "Получение данных
  LOOP AT lt_proce ASSIGNING FIELD-SYMBOL(<ls_proce>).
    APPEND INITIAL LINE TO et_output_data ASSIGNING FIELD-SYMBOL(<ls_output>).
    <ls_output>-experience = <ls_proce>-proct.

*    IF <lt_table> IS ASSIGNED AND <ls_proce>-numbr IN lr_numbr.
*      DATA(lv_field) = `YY` && CONV numc2( CONV int4( <ls_proce>-numbr ) ).
*      ASSIGN COMPONENT lv_field OF STRUCTURE <lt_table> TO FIELD-SYMBOL(<fs_yy>).
*      lv_field = `MM` && CONV numc2( CONV int4( <ls_proce>-numbr ) ).
*      ASSIGN COMPONENT lv_field OF STRUCTURE <lt_table> TO FIELD-SYMBOL(<fs_mm>).
*      IF <fs_mm> IS ASSIGNED AND <fs_yy> IS ASSIGNED.
*        <ls_output>-calmm = <fs_mm>.
*        <ls_output>-calyy = <fs_yy>.
*
*        UNASSIGN: <fs_yy>
*                , <fs_mm>
*                .
*        CONTINUE.
*      ENDIF.
*    ENDIF.

    zcl_hr_data_utils=>get_exper( EXPORTING i_pernr = lv_pernr
                                            i_proce = <ls_proce>-proce
                                  IMPORTING e_mm    = <ls_output>-calmm
                                            e_yy    = <ls_output>-calyy ).
  ENDLOOP.
ENDMETHOD.


METHOD get_input_filter.

  DATA: lr_filter TYPE RANGE OF string,
        lo_filter TYPE REF TO /iwbep/cl_mgw_req_filter.

  "Получение фильтров
  TRY .
      lo_filter ?= io_tech_request_context->get_filter( ).

      DATA(lt_prop) = lo_filter->get_entity_type( )->get_properties( ).

    CATCH cx_root.

  ENDTRY.

  LOOP AT it_filter_select_options INTO DATA(ls_filter).

    READ TABLE lt_prop
      ASSIGNING FIELD-SYMBOL(<ls_prop>)
      WITH KEY name = ls_filter-property.
    CHECK sy-subrc = 0.

    ls_filter-property = <ls_prop>-technical_name.

    DATA(lv_property) = ls_filter-property.
    TRANSLATE lv_property TO UPPER CASE.
    ASSIGN COMPONENT ls_filter-property OF STRUCTURE es_input_data TO FIELD-SYMBOL(<lv_input>).
    CHECK sy-subrc = 0.
    REFRESH lr_filter.
    lo_filter->/iwbep/if_mgw_req_filter~convert_select_option( EXPORTING is_select_option = ls_filter
                                                               IMPORTING et_select_option = lr_filter ).
    READ TABLE lr_filter ASSIGNING FIELD-SYMBOL(<ls_filter>) INDEX 1.
    IF sy-subrc = 0.
      <lv_input> = <ls_filter>-low.
    ENDIF.
  ENDLOOP.

ENDMETHOD.


method GET_REF_UNTIL_DATE.
  CLEAR rv_date.
  DATA(lv_empty_year) = VALUE zthr_argus_dates-jyear( ).
  SELECT * FROM zthr_argus_dates INTO TABLE @DATA(lt_dates) WHERE jyear = @iv_year.
  IF sy-subrc <> 0.
    SELECT * FROM zthr_argus_dates INTO TABLE @lt_dates WHERE jyear = @lv_empty_year.
  ENDIF.
  DATA(lv_p1) = VALUE char1( ).
  zcl_hr_data=>get_plans_param(
    EXPORTING
      i_plans  = iv_plans
    IMPORTING
      e_param1 = lv_p1
  ).
  TRY.
      DATA(lv_until) = lt_dates[ category = lv_p1 ]-endda.
      rv_date = |{ iv_year + 1 }{ lv_until+3(2) }{ lv_until(2) }|.
    CATCH cx_sy_itab_line_not_found.
      LOOP AT lt_dates ASSIGNING FIELD-SYMBOL(<ls_dates>) WHERE category IS INITIAL.
        lv_until = <ls_dates>-endda.
        rv_date = |{ iv_year + 1 }{ lv_until+3(2) }{ lv_until(2) }|.
        RETURN.
      ENDLOOP.
  ENDTRY.
endmethod.


METHOD income_info_review.

  DATA: lv_lastdayy    TYPE sy-datum,
        lt_p9110       TYPE STANDARD TABLE OF p9110,
        lv_decl_need   TYPE char1,
        lr_filter_stat TYPE RANGE OF zthr_ess_docreq-status,
        lr_year        TYPE RANGE OF zthr_ess_docreq-gjahr,
        lt_docreq      TYPE STANDARD TABLE OF zthr_ess_docreq,
        lt_docreq_new  TYPE STANDARD TABLE OF zthr_ess_docreq,
        lv_guid        TYPE guid_32,
        lt_dd07v       TYPE STANDARD TABLE OF dd07v.

  FIELD-SYMBOLS: <ls_output> TYPE LINE OF gtytd_output_income_review,
                 <ls_docreq> TYPE zthr_ess_docreq.

  "Определение табельного номера
  DATA(lo_hr_mss) = NEW zcl_mss_data_assistent( ).
  IF  is_input_data-entrance_user IS NOT INITIAL.
    DATA(lv_pernr) = lo_hr_mss->get_pernr( iv_usrid = CONV #( is_input_data-entrance_user ) ).
  ELSE.
    lv_pernr = lo_hr_mss->get_pernr( iv_usrid = CONV #( is_input_data-main_user ) ).
  ENDIF.

  lv_lastdayy = |{ sy-datum(4) - 1 }1231|.

  zcl_hr_data=>get_plans( EXPORTING i_pernr = lv_pernr
                                    i_begda = lv_lastdayy
                                    i_endda = lv_lastdayy
                          IMPORTING e_plans = DATA(lv_plans) ).

  DATA(lo_get_data) = NEW zcl_hr_get_data( ).

  lo_get_data->read_om_infty( EXPORTING i_otype  = 'S'
                                       i_objid  = lv_plans
                                       i_infty  = '9110'
                                       i_subty  = '0002'
                                       i_begda  = lv_lastdayy
                                       i_endda  = lv_lastdayy
                             IMPORTING e_pnnnn  = lt_p9110 ).
  READ TABLE lt_p9110 ASSIGNING FIELD-SYMBOL(<ls_p9110>) INDEX 1.
  IF sy-subrc = 0.
    lv_decl_need  = <ls_p9110>-corrupt.
  ENDIF.

  "Сведения о доходах подавать не нужно
  IF lv_decl_need IS INITIAL.
*    APPEND INITIAL LINE TO et_output_data ASSIGNING <ls_output>.
*    MESSAGE s000(cl) WITH 'Сведения о доходах подавать не нужно' INTO <ls_output>-message.
*    <ls_output>-msg_id = sy-msgid.
    RETURN.
  ENDIF.

  "Получение фильтра по статусам
  zcl_hr_get_data=>read_stvarv_ran( EXPORTING iv_name  = 'ZHR_ESS_INCOMEINFO_VIEW'
                                    IMPORTING et_range = lr_filter_stat ).

  lr_year = VALUE #( sign = 'I'  option = 'EQ' ( low = lv_lastdayy(4) )
                                               ( low = lv_lastdayy(4) - 1 ) ).

  SELECT *
    INTO TABLE lt_docreq
    FROM zthr_ess_docreq
    WHERE pernr  = lv_pernr
      AND status IN lr_filter_stat
      AND gjahr  IN lr_year.

  "Если за последний отчетный год записи нет , то добавить в таблицу
  READ TABLE lt_docreq TRANSPORTING NO FIELDS WITH KEY gjahr = lv_lastdayy(4).
  IF sy-subrc <> 0.
    APPEND INITIAL LINE TO lt_docreq_new ASSIGNING <ls_docreq>.

    CALL FUNCTION 'GUID_CREATE'
      IMPORTING
        ev_guid_32 = lv_guid.
    <ls_docreq>-req_id = lv_guid.

    <ls_docreq>-gjahr = lv_lastdayy(4).
    "Получение статуса
    zcl_hr_get_data=>read_stvarv_par( EXPORTING i_name  = 'ZHR_ESS_INCOMEINFO_VIEW'
                                      IMPORTING e_value = <ls_docreq>-status ).
    <ls_docreq>-pernr       = lv_pernr.
    <ls_docreq>-date_create = <ls_docreq>-date_change = sy-datum.
    <ls_docreq>-time_create = <ls_docreq>-time_change = sy-uzeit.
    <ls_docreq>-uname       = is_input_data-main_user.

    APPEND LINES OF lt_docreq_new TO lt_docreq.
  ENDIF.

  CALL FUNCTION 'DD_DOMVALUES_GET'
    EXPORTING
      domname        = 'ZDHR_IPRREQ_STATUS'   "<-- Your Domain Here
      text           = 'X'
      langu          = sy-langu
    TABLES
      dd07v_tab      = lt_dd07v
    EXCEPTIONS
      wrong_textflag = 1
      OTHERS         = 2.

  "Заполнение выходной таблицы
  LOOP AT lt_docreq ASSIGNING <ls_docreq>.

    APPEND INITIAL LINE TO et_output_data ASSIGNING <ls_output>.
    MOVE-CORRESPONDING is_input_data TO <ls_output>.
    MOVE-CORRESPONDING <ls_docreq> TO <ls_output>.
    <ls_output>-decl_need = lv_decl_need.
    <ls_output>-year = <ls_docreq>-gjahr.
*    CLEAR <ls_output>-date_create.
*    WRITE <ls_docreq>-date_create TO <ls_output>-date_create.
    <ls_output>-date_create = <ls_docreq>-date_create.

    READ TABLE lt_dd07v ASSIGNING FIELD-SYMBOL(<ls_dd07v>) WITH KEY domvalue_l = <ls_docreq>-status.
    IF sy-subrc = 0.
      <ls_output>-status_txt = <ls_dd07v>-ddtext.
    ENDIF.
    "FILE
*    <ls_output>-date_end = |30.04.{ <ls_docreq>-gjahr + 1 }|.
    DATA(lv_until_date) = get_ref_until_date(
        iv_plans = lv_plans
        iv_year  = <ls_docreq>-gjahr
    ).
    IF lv_until_date IS NOT INITIAL.
      <ls_output>-date_end = |{ lv_until_date }|.
    ELSE.
      <ls_output>-date_end = |{ <ls_docreq>-gjahr + 1 }0430|.
    endif.
  ENDLOOP.

  IF lt_docreq_new IS NOT INITIAL.
    MODIFY zthr_ess_docreq FROM TABLE lt_docreq_new.
    COMMIT WORK.
  ENDIF.
ENDMETHOD.


METHOD income_info_submit.

  DATA: ls_docreq TYPE zthr_ess_docreq,
        lv_req_id TYPE zthr_ess_docreq-req_id.

  "Определение табельного номера
  DATA(lo_hr_mss) = NEW zcl_mss_data_assistent( ).
*20/02/2020  DATA(lv_pernr) = lo_hr_mss->get_pernr( iv_usrid = CONV #( is_input_data-entrance_user ) ).
  DATA(lv_pernr) = lo_hr_mss->get_pernr( iv_usrid = CONV #( is_input_data-main_user ) ).
  lv_req_id = is_input_data-req_id.

  SELECT SINGLE *
    INTO ls_docreq
    FROM zthr_ess_docreq
    WHERE req_id = lv_req_id.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  UPDATE zthr_ess_docreq
    SET status      = '0'
        date_create = sy-datum
        date_change = sy-datum
        time_create = sy-uzeit
        time_change = sy-uzeit
        uname       = is_input_data-main_user
        commentary  = is_input_data-comment
     WHERE  req_id = lv_req_id.

ENDMETHOD.


METHOD PERSONAL_GET_ADDRESS_DATA.

  CONSTANTS: lc_infty TYPE p0021-infty VALUE '0006'.

  DATA: lt_p0006  TYPE STANDARD TABLE OF p0006.

  DATA(lo_data) = NEW zcl_hr_get_data( ).

  "
  lo_data->read_pa_infty( EXPORTING i_pernr = iv_pernr
                                    i_begda = sy-datum
                                    i_endda = sy-datum
                                    i_infty = lc_infty
                          IMPORTING e_pnnnn = lt_p0006 ).

  LOOP AT lt_p0006 ASSIGNING FIELD-SYMBOL(<ls_p0006>).
*
    CHECK <ls_p0006>-subty = 1
       OR <ls_p0006>-subty = 2.

    APPEND INITIAL LINE TO et_data ASSIGNING FIELD-SYMBOL(<ls_data>).
    "Адрес (Фактический, Регистрации)
    "Таблица буферизована
    SELECT SINGLE stext
      INTO <ls_data>-address
      FROM t591s
      WHERE sprsl = sy-langu
        AND infty = lc_infty
        AND subty = <ls_p0006>-subty.

    "Регион (STATE)
    SELECT SINGLE bezei
      INTO <ls_data>-region
      FROM t005u
      WHERE spras = sy-langu
        AND land1 = <ls_p0006>-land1
        AND bland = <ls_p0006>-state.

    "Город (ORT01)
    <ls_data>-city = <ls_p0006>-ort01.

    "Улица, дом, кв.
    IF <ls_p0006>-stras IS NOT INITIAL.
      <ls_data>-street = <ls_p0006>-stras.
    ENDIF.

    IF <ls_p0006>-hsnmr IS NOT INITIAL.
      "Дом
      <ls_data>-street = |{ <ls_data>-street }, { <ls_p0006>-hsnmr }|.

      "Корпус
      IF <ls_p0006>-bldng IS NOT INITIAL.
        <ls_data>-street = |{ <ls_data>-street }/{ <ls_p0006>-bldng }|.
      ENDIF.
      "Квартира
      IF <ls_p0006>-posta IS NOT INITIAL.
        <ls_data>-street = |{ <ls_data>-street }, кв.{ <ls_p0006>-posta }|.
      ENDIF.

    ENDIF.

    CONDENSE <ls_data>-street.
    SHIFT <ls_data>-street LEFT DELETING LEADING ','.
    CONDENSE <ls_data>-street.

  ENDLOOP.

ENDMETHOD.


METHOD PERSONAL_GET_EMPLOYER_DATA.

  DATA: lt_p0002  TYPE STANDARD TABLE OF p0002,
        lv_ddtext TYPE dd07v-ddtext,
        lt_p0105  TYPE STANDARD TABLE OF p0105,
        lv_dats_c TYPE char10.

  DATA(lo_data) = NEW zcl_hr_get_data( ).

  "
  lo_data->read_pa_infty( EXPORTING i_pernr = iv_pernr
                                    i_begda = sy-datum
                                    i_endda = sy-datum
                                    i_infty = '0002'
                          IMPORTING e_pnnnn = lt_p0002 ).
  READ TABLE lt_p0002 ASSIGNING FIELD-SYMBOL(<ls_p0002>) INDEX 1.
  IF sy-subrc = 0.
    "ФИО
    es_data-emp_fio = |{ <ls_p0002>-nachn } { <ls_p0002>-vorna } { <ls_p0002>-midnm }|.
    "Год рождения сотрудника
*    WRITE <ls_p0002>-gbdat TO lv_dats_c.
    es_data-emp_bdat = <ls_p0002>-gbdat.
    "Национальность (NATIO)
    SELECT SINGLE landx50
      INTO es_data-emp_nation
      FROM t005t
      WHERE spras = sy-langu
        AND land1 = <ls_p0002>-natio.
    "Пол (GESCH)
    CALL FUNCTION 'DOMAIN_VALUE_GET'
      EXPORTING
        i_domname  = 'GESCH'
        i_domvalue = CONV dd07v-domvalue_l( <ls_p0002>-gesch )
      IMPORTING
        e_ddtext   = lv_ddtext
      EXCEPTIONS
        not_exist  = 1
        OTHERS     = 2.
    es_data-emp_gender = lv_ddtext.
  ENDIF.

  """ Почта и телефон """
  lo_data->read_pa_infty( EXPORTING i_pernr = iv_pernr
                                    i_begda = sy-datum
                                    i_endda = sy-datum
                                    i_infty = '0105'
                          IMPORTING e_pnnnn = lt_p0105 ).

  LOOP AT lt_p0105 ASSIGNING FIELD-SYMBOL(<ls_p0105>).
    CASE <ls_p0105>-subty.
      WHEN 'CELL'.
        es_data-telephone = <ls_p0105>-usrid_long.

      WHEN 'MAIL' OR '0010'.
        es_data-email = <ls_p0105>-usrid_long.
    ENDCASE.
   ENDLOOP.

ENDMETHOD.


METHOD PERSONAL_GET_FAMILY_DATA.

  CONSTANTS: lc_infty TYPE p0021-infty VALUE '0021'.

  DATA: lt_p0021  TYPE STANDARD TABLE OF p0021,
        lv_dats_c TYPE char10.

  DATA(lo_data) = NEW zcl_hr_get_data( ).

  "
  lo_data->read_pa_infty( EXPORTING i_pernr = iv_pernr
                                    i_begda = sy-datum
                                    i_endda = sy-datum
                                    i_infty = lc_infty
                          IMPORTING e_pnnnn = lt_p0021 ).
  LOOP AT lt_p0021 ASSIGNING FIELD-SYMBOL(<ls_p0021>).

    CHECK <ls_p0021>-subty = 1
       OR <ls_p0021>-subty = 2
       OR <ls_p0021>-subty = 5
       OR <ls_p0021>-subty = 6.

    APPEND INITIAL LINE TO et_data ASSIGNING FIELD-SYMBOL(<ls_data>).
    "Член семьи (Муж, Жена, Сын, Дочь, FAMSA)

    "Таблица буферизована
    SELECT SINGLE stext
      INTO <ls_data>-fam_member
      FROM t591s
      WHERE sprsl = sy-langu
        AND infty = lc_infty
        AND subty = <ls_p0021>-subty.

    "ФИО члена семьи
    <ls_data>-fio = |{ <ls_p0021>-fanam } { <ls_p0021>-favor } { <ls_p0021>-fnac2 }|.
    "Дата рождения члена семьи
*    WRITE <ls_p0021>-fgbdt TO lv_dats_c.
    <ls_data>-bdat = <ls_p0021>-fgbdt.


  ENDLOOP.


ENDMETHOD.


METHOD personal_get_t2_data.
  DATA: lr_pernr TYPE RANGE OF pernr_d.
  EXPORT from_ws = 'X' TO MEMORY ID gc_personal_memory_id_t2.
  APPEND INITIAL LINE TO lr_pernr ASSIGNING FIELD-SYMBOL(<ls_pernr>).
  <ls_pernr>(3) = 'IEQ'.
  <ls_pernr>-low = iv_pernr.
  SUBMIT hrult2_2004
    USING SELECTION-SET 'CUS&ESS'
    WITH pnppernr IN lr_pernr
    WITH pnpendda = sy-datum
    AND RETURN.
  IMPORT word_xstr = ev_content FROM MEMORY ID gc_personal_memory_id_t2.
  FREE MEMORY ID gc_personal_memory_id_t2.
  ev_filename = 'ZHR_T2_PRINT_' && sy-datum && sy-uzeit && '.docx'.
ENDMETHOD.
ENDCLASS.

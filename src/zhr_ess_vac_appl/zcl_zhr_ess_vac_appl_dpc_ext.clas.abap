class ZCL_ZHR_ESS_VAC_APPL_DPC_EXT definition
  public
  inheriting from ZCL_ZHR_ESS_VAC_APPL_DPC
  create public .

public section.

  constants C_MEMORY_ID type TEXT20 value 'ZXML_T6' ##NO_TEXT.

  methods GET_ORDER_FORM
    importing
      !IT_KEY type /IWBEP/T_MGW_NAME_VALUE_PAIR
    exporting
      !E_XSTRING type XSTRING
      !E_FILENAME type STRING .
  class-methods GET_ORDER
    importing
      !I_PERNR type PERSNO
      !I_SUBTY type SUBTY
      !I_BEGDA type BEGDA
      !I_ENDDA type ENDDA
    exporting
      !E_XSTRING type XSTRING
      !E_FILENAME type STRING
      !EV_SUBRC type SYSUBRC .

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_EXPANDED_ENTITY
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_EXPANDED_ENTITYSET
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_STREAM
    redefinition .
protected section.

  methods ENTITYAPPLICATIO_GET_ENTITYSET
    redefinition .
private section.

  data MO_MESS type ref to /IWBEP/IF_MESSAGE_CONTAINER .
  class-data MV_DUMMY type STRING .

  methods ADD_MSG .
  methods RAISE_EXCEPTION
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
ENDCLASS.



CLASS ZCL_ZHR_ESS_VAC_APPL_DPC_EXT IMPLEMENTATION.


  METHOD /iwbep/if_mgw_appl_srv_runtime~get_expanded_entity.

    DATA: lv_loginactdir TYPE zlogin_act_dir
        , lv_begda TYPE begda
        .

    DATA: ls_main TYPE zcl_zhr_ess_vac_appl_mpc_ext=>ts_deep_entity.
*    DATA: lt_main TYPE TABLE OF zcl_z_vacation_applica_mpc_ext=>ts_deep_entity.

    LOOP AT it_key_tab ASSIGNING FIELD-SYMBOL(<it_key_tab>).
      CASE <it_key_tab>-name.
*        WHEN 'LoginActDir'.
*          lv_loginactdir = <it_key_tab>-value.
        WHEN 'MainUser'.
          DATA(lv_mainuser) = CONV string( <it_key_tab>-value ).
        WHEN 'EntranceUser'.
          DATA(lv_entranceuser) = CONV string( <it_key_tab>-value ).
        WHEN 'Begda'.
          lv_begda = <it_key_tab>-value.
      ENDCASE.
    ENDLOOP.

    IF lv_entranceuser IS NOT INITIAL.
      ls_main-login_act_dir =  lv_entranceuser.
    ELSE.
      ls_main-login_act_dir = lv_mainuser.
    ENDIF.

    ls_main-begda = lv_begda.

    APPEND INITIAL LINE TO ls_main-navappl ASSIGNING FIELD-SYMBOL(<application>).
    <application>-request_id = '0050569437E01ED798CB019F0D7CB5B8'.
    <application>-abstype = 'Основной отпуск'.
    <application>-owner_fio = 'Иванов Иван Иванович'.
    <application>-owner_number = '00000588'.
    <application>-approver_fio = 'Иванов Иван Иванович'.
    <application>-approver_number = '00000588'.
    <application>-status = 'SENT'.
    <application>-begda = '20170713'.
    <application>-endda = '20170717'.
    <application>-create_date = '20170706'.
    <application>-approve_date = '20170706'.
    <application>-abs_days = 2.


**    APPEND INITIAL LINE TO ls_main-NavErr ASSIGNING FIELD-SYMBOL(<error>).
**    <error>-type = 'E'.
**    <error>-id = '1234568'.
**    <error>-number = '123'.
**    <error>-message = 'Тест Тест Тест'.
**    APPEND INITIAL LINE TO ls_main-NavErr ASSIGNING <error>.
**    <error>-type = 'E'.
**    <error>-id = 'PT_ABS'.
**    <error>-number = '123'.
**    <error>-message = 'Нет данных по заявкам для табельного номера'.

    copy_data_to_ref(
         EXPORTING
           is_data = ls_main
         CHANGING
           cr_data = er_entity ).
  ENDMETHOD.


  METHOD /iwbep/if_mgw_appl_srv_runtime~get_expanded_entityset.

    DATA: lv_loginactdir TYPE zlogin_act_dir
        , lv_begda TYPE begda
        , lv_property TYPE string
        .

    DATA: ls_main TYPE zcl_zhr_ess_vac_appl_mpc_ext=>ts_deep_entity.
    DATA: lt_main TYPE TABLE OF zcl_zhr_ess_vac_appl_mpc_ext=>ts_deep_entity.
    CASE iv_entity_set_name.
      WHEN 'EntityMainSet'.
        LOOP AT it_filter_select_options ASSIGNING FIELD-SYMBOL(<filter>).
          LOOP AT <filter>-select_options ASSIGNING FIELD-SYMBOL(<range>).
            lv_property = <filter>-property.
            TRANSLATE lv_property TO UPPER CASE.
            CASE lv_property.
              WHEN 'BEGDA'.
                lv_begda = <range>-low.
*              WHEN 'LOGINACTDIR'.
*                lv_loginactdir = <range>-low.
              WHEN 'MAINUSER'.
                DATA(lv_mainuser) = CONV string( <range>-low ).
              WHEN 'ENTRANCEUSER'.
                DATA(lv_entranceuser) = CONV string( <range>-low ).
            ENDCASE.
          ENDLOOP.
        ENDLOOP.


        IF lv_entranceuser IS NOT INITIAL.
          ls_main-login_act_dir =  lv_entranceuser.
        ELSE.
          ls_main-login_act_dir = lv_mainuser.
        ENDIF.

        ls_main-begda = lv_begda.

        APPEND INITIAL LINE TO ls_main-navappl ASSIGNING FIELD-SYMBOL(<application>).
        <application>-request_id = '0050569437E01ED798CB019F0D7CB5B8'.
        <application>-abstype = 'Основной отпуск'.
        <application>-owner_fio = 'Иванов Иван Иванович'.
        <application>-owner_number = '00000588'.
        <application>-approver_fio = 'Иванов Иван Иванович'.
        <application>-approver_number = '00000588'.
        <application>-status = 'SENT'.
        <application>-begda = '20170713'.
        <application>-endda = '20170717'.
        <application>-create_date = '20170706'.
        <application>-approve_date = '20170706'.
        <application>-abs_days = 2.


**        APPEND INITIAL LINE TO ls_main-NavErr ASSIGNING FIELD-SYMBOL(<error>).
**        <error>-type = 'E'.
**        <error>-id = '1234568'.
**        <error>-number = '123'.
**        <error>-message = 'Тест Тест Тест'.
**        APPEND INITIAL LINE TO ls_main-NavErr ASSIGNING <error>.
**        <error>-type = 'E'.
**        <error>-id = 'PT_ABS'.
**        <error>-number = '123'.
**        <error>-message = 'Нет данных по заявкам для табельного номера'.
        APPEND ls_main TO lt_main.
        copy_data_to_ref(
             EXPORTING
               is_data = lt_main
             CHANGING
               cr_data = er_entityset ).
      WHEN OTHERS.

    ENDCASE.
  ENDMETHOD.


  METHOD /iwbep/if_mgw_appl_srv_runtime~get_stream.
    DATA: ls_stream     TYPE ty_s_media_resource
            , ls_lheader    TYPE ihttpnvp
            .

    CASE iv_entity_name.
      WHEN 'OrderForm'.
        ls_stream-mime_type = 'application/msword'.
        get_order_form(
          EXPORTING
            it_key    = it_key_tab
          IMPORTING
            e_xstring = ls_stream-value
            e_filename = DATA(lv_file) ).

        ls_lheader-name = 'Content-Disposition'.
        lv_file = cl_http_utility=>if_http_utility~escape_url(
                    unescaped = lv_file ).

        ls_lheader-value = 'inline; filename="' && lv_file && '";'.
        set_header( is_header = ls_lheader ).
    ENDCASE.

    copy_data_to_ref( EXPORTING is_data = ls_stream
                      CHANGING  cr_data = er_stream ).
  ENDMETHOD.


  METHOD add_msg.
    DATA: lt_return TYPE bapiret2_t.

    IF mo_mess IS INITIAL.
      mo_mess = mo_context->get_message_container( ).
    ENDIF.

    APPEND INITIAL LINE TO lt_return ASSIGNING FIELD-SYMBOL(<return>).
    <return>-type = sy-msgty.
    <return>-id   = sy-msgid.
    <return>-number = sy-msgno.
    <return>-message = mv_dummy.
    mo_mess->add_messages_from_bapi( it_bapi_messages = lt_return ).
  ENDMETHOD.


  METHOD entityapplicatio_get_entityset.
    DATA: lv_loginactdir TYPE zlogin_act_dir
          , lv_begda TYPE begda
          , lv_endda TYPE endda
          , lv_property TYPE string
*          , lv_pernr TYPE persno
          , lt_p0298 TYPE TABLE OF p0298
          .

    DATA: lt_return TYPE TABLE OF bapiret2
        , lv_err_number TYPE i VALUE 1
        , lv_message TYPE string
        , lo_message_container TYPE REF TO /iwbep/if_message_container
        .


    DATA: lt_messages TYPE ptarq_uia_messages_tab
        , lt_command  TYPE ptarq_uia_command_tab
        , lt_request  TYPE ptarq_uia_reqlist_tab
        , lv_status   TYPE string
*        , lv_abstype  TYPE string

        , lv_begda_tmp  TYPE dats
        .

    DATA:
      lt_ptreq_header TYPE TABLE OF ptreq_header,
      ls_ptreq_header LIKE LINE OF lt_ptreq_header.


    lo_message_container = mo_context->get_message_container( ).

    DATA(lo_assistent) = zcl_mss_data_assistent=>get_instance( ).

    LOOP AT it_filter_select_options ASSIGNING FIELD-SYMBOL(<filter>).
      LOOP AT <filter>-select_options ASSIGNING FIELD-SYMBOL(<range>).
        lv_property = <filter>-property.
        TRANSLATE lv_property TO UPPER CASE.
        CASE lv_property.
          WHEN 'BEGDA'.
            lv_begda = <range>-low.
          WHEN 'ENDDA'.
            lv_endda = <range>-low.
          WHEN 'STATUS'.
            lv_status = <range>-low.
          WHEN 'ABSTYPE'.
            DATA(lv_abstype) = CONV numc4( <range>-low ).
*          WHEN 'LOGINACTDIR'.
*            lv_loginactdir = <range>-low.
*            TRANSLATE lv_loginactdir TO UPPER CASE.
*            lv_pernr = zcl_vacation_appl_renault=>get_pernr_by_ad( i_ad = lv_loginactdir ).
*            IF lv_pernr IS INITIAL.
*              APPEND INITIAL LINE TO lt_return ASSIGNING FIELD-SYMBOL(<return>).
*              MESSAGE e001(zhr_ess) INTO <return>-message.
*              <return>-type = sy-msgty.
*              <return>-id   = sy-msgid.
*              <return>-number = sy-msgno.
*              lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).
*
*              RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*                EXPORTING
*                  message_container = lo_message_container.
*            ENDIF.
          WHEN `ENTRANCEUSER`.
            DATA(lv_entrance_pernr) = lo_assistent->get_pernr( iv_usrid = CONV #( <range>-low ) ).
            IF lv_entrance_pernr IS INITIAL.
              APPEND INITIAL LINE TO lt_return ASSIGNING FIELD-SYMBOL(<return>).
              MESSAGE e001(zhr_ess) INTO <return>-message.
              <return>-type = sy-msgty.
              <return>-id   = sy-msgid.
              <return>-number = sy-msgno.
              lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

              RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
                EXPORTING
                  message_container = lo_message_container.
            ENDIF.
          WHEN `MAINUSER`.
            DATA(lv_main_pernr) = lo_assistent->get_pernr( iv_usrid = CONV #( <range>-low ) ).
            IF lv_main_pernr IS INITIAL.
              APPEND INITIAL LINE TO lt_return ASSIGNING <return>.
              MESSAGE e001(zhr_ess) INTO <return>-message.
              <return>-type = sy-msgty.
              <return>-id   = sy-msgid.
              <return>-number = sy-msgno.
              lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

              RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
                EXPORTING
                  message_container = lo_message_container.
            ENDIF.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.

    IF lv_begda IS INITIAL.
      APPEND INITIAL LINE TO lt_return ASSIGNING <return>.
      MESSAGE e003(zhr_ess) WITH 'Begda' INTO <return>-message.
      <return>-type = sy-msgty.
      <return>-id   = 'ZTEST'.
      <return>-number = sy-msgno.
      lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.
    ENDIF.

    IF lv_endda IS INITIAL.
      APPEND INITIAL LINE TO lt_return ASSIGNING <return>.
      MESSAGE e003(zhr_ess) WITH 'Endda' INTO <return>-message.
      <return>-type = sy-msgty.
      <return>-id   = 'ZTEST'.
      <return>-number = sy-msgno.
      lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.
    ENDIF.

    IF lv_entrance_pernr IS NOT INITIAL.
      zcl_vacation_appl=>get_reqlist(
        EXPORTING
          i_pernr     = lv_entrance_pernr " Табельный номер
          i_begda     = lv_begda          " Начало срока действия
          i_endda     = lv_endda          " Конец срока действия
        IMPORTING
          et_req      = lt_request        " UIA: список заявок
          et_messages = lt_messages ).
    ELSE.
      zcl_vacation_appl=>get_reqlist(
        EXPORTING
          i_pernr     = lv_main_pernr " 21/03/2020 " Табельный номер
          i_begda     = lv_begda          " Начало срока действия
          i_endda     = lv_endda          " Конец срока действия
        IMPORTING
          et_req      = lt_request        " UIA: список заявок
          et_messages = lt_messages ).

    ENDIF.

*    CALL FUNCTION 'PT_ARQ_REQLIST_GET'
*      EXPORTING
*        im_pernr            = lv_pernr
*        im_date             = lv_begda
*        im_command          = 'SHOW_REQLIST'
*        im_modus            = 'R'
*        im_deactivate_check = abap_true
*      IMPORTING
*        ex_request_list     = lt_request
*      TABLES
*        ex_messages         = lt_messages
*        ex_commands         = lt_command.

**<<<<обработка ошибок

**<< SAP возвращает ошибку, если пользователь не имеет ТН в системе
**<< поскольку данные тоже возвращаются, то игнорим эту ошибку
    DELETE lt_messages WHERE type = 'E' AND number = '075' AND id = 'HRTIM_ABS_REQ'.

    LOOP AT lt_messages ASSIGNING FIELD-SYMBOL(<messages>) WHERE type = 'E'.
      APPEND INITIAL LINE TO lt_return ASSIGNING <return>.
      <return>-type = <messages>-type.
      <return>-id   = <messages>-id.
      <return>-number = <messages>-number.
      <return>-message = <messages>-message.
      lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).
**        <fs_header>-value = cl_http_utility=>escape_url( lv_message ).
    ENDLOOP.
    IF  lt_return IS NOT INITIAL .
**      <<<< выводим ошибки в боди
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.
    ENDIF.

    IF lv_abstype IS NOT INITIAL.
      DELETE lt_request  WHERE subty NE lv_abstype.
    ENDIF.

    IF lv_status IS NOT INITIAL.
      DELETE lt_request WHERE status NE lv_status.
    ENDIF.

    DELETE lt_request WHERE sprps EQ abap_true.
    CHECK lt_request IS NOT INITIAL.
    IF lv_entrance_pernr IS NOT INITIAL.

      zcl_hr_data=>read_pa_infty( EXPORTING i_pernr = lv_entrance_pernr
                                            i_infty = zcl_hr_data=>c_infty_0298
                                            i_begda = zcl_hr_data=>c_begda_low
                                            i_endda = zcl_hr_data=>c_endda_high
                                  IMPORTING e_pnnnn = lt_p0298 ).
    ELSE.

      zcl_hr_data=>read_pa_infty( EXPORTING i_pernr = lv_main_pernr "21/03/2020 lv_entrance_pernr
                                          i_infty = zcl_hr_data=>c_infty_0298
                                          i_begda = zcl_hr_data=>c_begda_low
                                          i_endda = zcl_hr_data=>c_endda_high
                                IMPORTING e_pnnnn = lt_p0298 ).
    ENDIF.

    LOOP AT lt_request ASSIGNING FIELD-SYMBOL(<request>).
      APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<application>).

        <application>-request_id = <request>-request_id.

*     параметр changing_vis:
      "TRUE - заполняем если для заявки ОДНОВРЕМЕННО выполняется:
*     1. статус POSTED
*     2. нет другой заявки, в которой текущее отсутствие стоит с пометкой DEL
*       (т.е. не давать переносить отпуск, по которому пользоватль уже запросил перенос и этот перенос не удалил)
*       FALSE - во всех прочих случаях"
      IF <request>-status EQ 'POSTED' OR <request>-request_or_attabs NE 'R'.
        zcl_vacation_appl=>find_running_del_vacation(
          EXPORTING
             i_pernr = <request>-pernr
             i_subty = <request>-subty
             i_begda = <request>-begda
             i_endda = <request>-endda
          IMPORTING
            e_fnd = DATA(lv_fnd) ).
        IF lv_fnd EQ abap_false.
          <application>-changing_vis = abap_true.
        ENDIF.
      ENDIF.
*     параметр deletion_vis: заполняем значение " " по всем заявкам
*     TRUE - для заявки в статусе SENT;
*     FALSE - для заявок во всех прочих статусах, а также для "чистых" отсутствий (чистые отсутствия на портал должны поступать со статусом POSTED)
      IF <request>-request_or_attabs EQ 'R' AND
          <request>-status EQ 'SENT'.
        <application>-deletion_vis = abap_true.
      ENDIF.

*     параметр print_vis: если на дату отсутвия (2001 ИТ) есть приказ (298 ИТ) мероприятие - 'RR'/'R3'
*     только если это 2001 ИТ
      IF <request>-request_or_attabs NE 'R'.
        LOOP AT lt_p0298 TRANSPORTING NO FIELDS WHERE ( massn = 'RR' OR massn = 'R3' )
                                                AND begda = <request>-begda.
          <application>-print_vis = abap_true.
          EXIT.
        ENDLOOP.
        IF sy-subrc NE 0. " скорей всего это перенос, там дата старого отсутствия должна быть
          IF lv_entrance_pernr IS NOT INITIAL.
            lv_begda_tmp = zcl_vacation_appl=>get_old_vacation( i_pernr     = lv_entrance_pernr    " Табельный номер
                                                                i_begda_new = <request>-begda ).
          ELSE.
            lv_begda_tmp = zcl_vacation_appl=>get_old_vacation( i_pernr     = lv_main_pernr "21/03/2020 lv_entrance_pernr    " Табельный номер
                                                    i_begda_new = <request>-begda ).
          ENDIF.
          IF lv_begda_tmp IS NOT INITIAL.
            LOOP AT lt_p0298 TRANSPORTING NO FIELDS
                  WHERE massn = 'RR'
                    AND begda = lv_begda_tmp.
              <application>-print_vis = abap_true.
              EXIT.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.

      <application>-abstype = <request>-subty.
      <application>-owner_fio = <request>-owner-name.
      <application>-owner_number = <request>-owner-pernr.
      <application>-approver_fio = <request>-next_processor-name."?????
      <application>-approver_number = <request>-next_processor-pernr."??????
      <application>-status = <request>-status.
      <application>-begda = <request>-begda.
      <application>-endda = <request>-endda.
      <application>-create_date = <request>-first_subm_date.
      <application>-approve_date = <request>-first_subm_date."?????
      <application>-abs_days = <request>-kaltg.

      zcl_vacation_appl=>fix_uppercase_fio(
        CHANGING
          cv_fio = <application>-approver_fio ).

      zcl_vacation_appl=>fix_uppercase_fio(
        CHANGING
          cv_fio = <application>-owner_fio ).
    ENDLOOP.
  ENDMETHOD.


  METHOD get_order.
    DATA: lr_pernr    TYPE RANGE OF persno

        , lt_p2001    TYPE TABLE OF p2001
        , lt_p0298    TYPE TABLE OF p0298
        , lv_ad       TYPE persno
        , lv_old_date TYPE dats
        .
    CLEAR: ev_subrc.

    zcl_pa_utils=>read_infty(
      EXPORTING
        i_pernr         = i_pernr
        i_infty         = '2001'
        i_subty         = i_subty
        i_begda         = i_begda
        i_endda         = i_endda
      IMPORTING
        e_pnnnn         = lt_p2001 ).

    DELETE lt_p2001 WHERE begda NE i_begda OR endda NE i_endda.

    IF lt_p2001 IS INITIAL.
      MESSAGE e002(zhr_ess) WITH '2001' i_pernr i_begda i_endda INTO mv_dummy.
      ev_subrc = 4.
      RETURN.
    ENDIF.

    zcl_pa_utils=>read_infty(
      EXPORTING
        i_pernr         = i_pernr
        i_infty         = '0298'
        i_begda         = i_begda
        i_endda         = i_begda
      IMPORTING
        e_pnnnn         = lt_p0298 ).
    DELETE lt_p0298 WHERE massn NE 'RR' AND massn NE 'R3'.
    READ TABLE lt_p0298 INDEX 1 ASSIGNING FIELD-SYMBOL(<fs_p0298>).
    IF sy-subrc NE 0.
      lv_old_date = zcl_vacation_appl=>get_old_vacation(
          i_pernr     = i_pernr     " Табельный номер
          i_begda_new = i_begda ).
      zcl_pa_utils=>read_infty(
        EXPORTING
          i_pernr         = i_pernr
          i_infty         = '0298'
          i_begda         = lv_old_date
          i_endda         = lv_old_date
        IMPORTING
          e_pnnnn         = lt_p0298 ).
      DELETE lt_p0298 WHERE massn NE 'RR'.  " здесь же только перенос отпуска должен быть!
      READ TABLE lt_p0298 INDEX 1 ASSIGNING <fs_p0298>.
      IF sy-subrc NE 0.
        MESSAGE e002(zhr_ess) WITH '0298' i_pernr lv_old_date lv_old_date INTO mv_dummy.
        ev_subrc = 4.
        RETURN.
      ENDIF.
    ENDIF.

    APPEND INITIAL LINE TO lr_pernr ASSIGNING FIELD-SYMBOL(<fs_pernr>).
    <fs_pernr>-low    = i_pernr.
    <fs_pernr>-sign   = 'I'.
    <fs_pernr>-option = 'EQ'.

    sy-langu = 'RU'.
    SUBMIT hrua_order_t6
      WITH pnppernr IN lr_pernr
      WITH pnpbegda = <fs_p0298>-begda
      WITH pnpendda = <fs_p0298>-endda
      WITH pnptimr6 = 'X'
      WITH massn = <fs_p0298>-massn
      WITH massg = <fs_p0298>-massg
      WITH orddt = <fs_p0298>-orddt
      WITH ordnu = <fs_p0298>-ordnu
      WITH p_tabel = abap_false
      WITH p_xml   = abap_true
      AND RETURN.

    IMPORT lv_xstring TO e_xstring FROM MEMORY ID zcl_zhr_ess_vac_appl_dpc_ext=>c_memory_id.
    FREE MEMORY ID zcl_zhr_ess_vac_appl_dpc_ext=>c_memory_id.
    e_filename = 'Приказ_' && i_pernr && `_` && <fs_p0298>-orddt && `.doc`.
  ENDMETHOD.


  METHOD get_order_form.
    DATA: lr_pernr  TYPE RANGE OF persno
        , lv_begda  TYPE begda
        , lv_endda  TYPE endda
        , lv_pernr  TYPE persno
        , lv_subty  TYPE subty

        , lt_p2001  TYPE TABLE OF p2001
        , lt_p0298  TYPE TABLE OF p0298
        , lv_ad     TYPE persno
        , lv_old_date TYPE dats
*        , lv_xstring  TYPE xstring
        .

    LOOP AT it_key ASSIGNING FIELD-SYMBOL(<fs_key>).
      CASE <fs_key>-name.
*        WHEN 'loginAD'.
*          lv_ad = zcl_vacation_appl_renault=>get_pernr_by_ad( i_ad = <fs_key>-value ).
        WHEN 'ownerNumber'.
          lv_pernr = <fs_key>-value.
        WHEN 'absType'.
          lv_subty = <fs_key>-value.
        WHEN 'begda'.
          lv_begda = <fs_key>-value.
        WHEN 'endda'.
          lv_endda = <fs_key>-value.
      ENDCASE.
    ENDLOOP.

    IF lv_pernr IS INITIAL.
      MESSAGE e003(zhr_ess) WITH 'ownerNumber' INTO mv_dummy.
      add_msg( ).
      raise_exception( ).
    ENDIF.

    IF lv_begda IS INITIAL.
      MESSAGE e003(zhr_ess) WITH 'begda' INTO mv_dummy.
      add_msg( ).
      raise_exception( ).
    ENDIF.

    IF lv_endda IS INITIAL.
      MESSAGE e003(zhr_ess) WITH 'endda' INTO mv_dummy.
      add_msg( ).
      raise_exception( ).
    ENDIF.

    IF lv_subty IS INITIAL.
      MESSAGE e003(zhr_ess) WITH 'subty' INTO mv_dummy.
      add_msg( ).
      raise_exception( ).
    ENDIF.

    get_order(
      EXPORTING
        i_pernr    = lv_pernr    " Табельный номер
        i_subty    = lv_subty    " Подтип
        i_begda    = lv_begda    " Начало срока действия
        i_endda    = lv_endda    " Конец срока действия
      IMPORTING
        e_xstring  = e_xstring
        e_filename = e_filename
        ev_subrc   = DATA(lv_subrc) ).
    IF lv_subrc NE 0.
      add_msg( ).
      raise_exception( ).
    ENDIF.
  ENDMETHOD.


  METHOD raise_exception.
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        message_container = mo_mess.
  ENDMETHOD.
ENDCLASS.

class ZCL_ZHR_ESS_CREATE_VAC_DPC_EXT definition
  public
  inheriting from ZCL_ZHR_ESS_CREATE_VAC_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_EXPANDED_ENTITYSET
    redefinition .
protected section.

  methods ENTITYABSSET_GET_ENTITYSET
    redefinition .
  methods ENTITYCHECKSET_CREATE_ENTITY
    redefinition .
  methods ENTITYCHECKSET_GET_ENTITY
    redefinition .
  methods ENTITYMAINSET_GET_ENTITY
    redefinition .
  methods ENTITYSAVESET_CREATE_ENTITY
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZHR_ESS_CREATE_VAC_DPC_EXT IMPLEMENTATION.


  METHOD /iwbep/if_mgw_appl_srv_runtime~get_expanded_entityset.

    DATA: ls_deep_entity TYPE zcl_zhr_ess_create_vac_mpc_ext=>ts_deep_entity
        , lt_deep_entity TYPE TABLE OF zcl_zhr_ess_create_vac_mpc_ext=>ts_deep_entity
        .

    DATA: lv_loginactdir TYPE zlogin_act_dir
        , lv_pernr TYPE persno
        , lv_property TYPE string
        .
    DATA: lt_header TYPE /iwbep/t_mgw_name_value_pair          .
    FIELD-SYMBOLS: <fs_header>  TYPE LINE OF /iwbep/t_mgw_name_value_pair
                 .
    CASE iv_entity_set_name.
      WHEN 'EntityMainSet'.
        LOOP AT it_filter_select_options ASSIGNING FIELD-SYMBOL(<filter>).
          LOOP AT <filter>-select_options ASSIGNING FIELD-SYMBOL(<range>).
            lv_property = <filter>-property.
            TRANSLATE lv_property TO UPPER CASE.
            CASE lv_property.
              WHEN 'LOGINACTDIR'.
                lv_loginactdir = <range>-low.
              WHEN 'PERNR'.
                lv_pernr = <range>-low.
            ENDCASE.
          ENDLOOP.
        ENDLOOP.

        IF lv_pernr IS INITIAL.
          APPEND INITIAL LINE TO lt_header ASSIGNING <fs_header>.
          <fs_header>-name  = 'err1'.
          <fs_header>-value = cl_http_utility=>escape_url( 'Неверный ТН' ).
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
            EXPORTING
              http_header_parameters = lt_header.
        ENDIF.

        IF lv_loginactdir IS INITIAL.
          APPEND INITIAL LINE TO lt_header ASSIGNING <fs_header>.
          <fs_header>-name  = 'err1'.
          <fs_header>-value = cl_http_utility=>escape_url( 'Ошибка при передачи логина' ).
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
            EXPORTING
              http_header_parameters = lt_header.
        ENDIF.

**CALL FUNCTION 'PT_ARQ_REQUEST_PREPARE'
**  EXPORTING
***   IM_REQUEST_ID             =
***   IM_WI_ID                  =
**   IM_COMMAND                = 'CREATE'
**    im_pernr                  = lv_pernr
**    im_modus                  = 'R' "????????
***   IM_DEBUG                  =
***   IM_DEACTIVATE_CHECK       =
***   IM_POPUP                  =
*** IMPORTING
***   EX_REQUEST                =
***   EX_DATA_READ_ONLY         =
***   EX_NOTICE_READ_ONLY       =
***   EX_CHANGED                =
***   EX_HAS_ERRORS             =
***   EX_SHOW_CHANGE            =
**  TABLES
**    ex_messages               =
**    ex_commands               =
**          .
***<<<<ЗАГЛУШКА
        ls_deep_entity-request_id = '0050569437E01ED798CB019F0D7CB5B8'.
        ls_deep_entity-default_begda = sy-datum.
        ls_deep_entity-default_endda  = sy-datum.
        ls_deep_entity-abstype = '0101'.
        ls_deep_entity-approver_fio = 'Иванов Иван Иванович'.
        ls_deep_entity-approver_number = '00000588'.
        ls_deep_entity-approver_vis = 'X'.
        ls_deep_entity-owner_fio = 'Иванов Иван Иванович'.
        ls_deep_entity-owner_number = '00000588'.
        ls_deep_entity-owner_vis = 'X'.
        ls_deep_entity-initiator_fio = 'Иванов Иван Иванович'.
        ls_deep_entity-initiator_number = '00000588'.
        ls_deep_entity-abs_limit = '0028'.

        APPEND INITIAL LINE TO ls_deep_entity-navabs ASSIGNING FIELD-SYMBOL(<abs>).
        <abs>-abstype = '0101'.
        <abs>-begda = '20170717'.
        <abs>-endda ='20170717'.
        APPEND INITIAL LINE TO ls_deep_entity-navabs ASSIGNING <abs>.
        <abs>-abstype = '0101'.
        <abs>-begda = '20170720'.
        <abs>-endda ='20170725'.

        APPEND ls_deep_entity TO lt_deep_entity.
        APPEND ls_deep_entity TO lt_deep_entity.
****>>>>КОНЕЦ
        copy_data_to_ref(
                EXPORTING
                  is_data = lt_deep_entity
                CHANGING
                  cr_data = er_entityset ).

    ENDCASE.
  ENDMETHOD.


  METHOD entityabsset_get_entityset.
    DATA: lv_entranceuser TYPE zlogin_act_dir
        , lv_pernr        TYPE persno
        , lv_property     TYPE string
        , lt_return       TYPE bapiret2_t
        , lv_begda        TYPE begda
        , lv_endda        TYPE endda
        , lv_datum        TYPE datum
        .

    DATA: lt_team_tab         TYPE  ptreq_uia_team_tab
        , lt_weekday_tab      TYPE  ptarq_uia_weekday_tab
        , lt_legend_tab       TYPE  ptreq_uia_legend_tab
        , lt_commands         TYPE  ptarq_uia_command_tab
        , lt_custcolumns_tab  TYPE  ptarq_uia_custcolumns_tab
        , lt_messages         TYPE  ptarq_uia_messages_tab
        , wa_cache_validity   TYPE int4
        .

    DATA(lo_assistent) = zcl_mss_data_assistent=>get_instance( ).

    DATA lo_message_container TYPE REF TO /iwbep/if_message_container.
    lo_message_container = mo_context->get_message_container( ).

    LOOP AT it_filter_select_options ASSIGNING FIELD-SYMBOL(<filter>).
      LOOP AT <filter>-select_options ASSIGNING FIELD-SYMBOL(<range>).
        lv_property = <filter>-property.
        TRANSLATE lv_property TO UPPER CASE.
        CASE lv_property.
          WHEN 'ENTRANCEUSER'.
            lv_entranceuser = <range>-low.
          WHEN 'MAINUSER'.
            lv_entranceuser = <range>-low.
          WHEN 'PERNR'.
            lv_pernr = <range>-low.
          WHEN 'DATA'.
            lv_datum = <range>-low.
            lv_begda = lv_endda = lv_datum.
            lv_begda(4) = lv_begda(4) - 2.
            lv_endda(4) = lv_endda(4) + 2.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.

    IF lv_entranceuser IS INITIAL.
      APPEND INITIAL LINE TO lt_return ASSIGNING FIELD-SYMBOL(<return>).
      MESSAGE e019(zhr_ess) INTO <return>-message.
      <return>-type   = sy-msgty.
      <return>-id     = sy-msgid.
      <return>-number = sy-msgno.

      lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.
    ENDIF.

    IF lv_pernr IS INITIAL.
      lv_pernr = lo_assistent->get_pernr( iv_usrid = CONV #( lv_entranceuser ) ).

      IF lv_pernr IS INITIAL.
        APPEND INITIAL LINE TO lt_return ASSIGNING <return>.
        MESSAGE e017(zhr_ess) INTO <return>-message.
        <return>-type   = sy-msgty.
        <return>-id     = sy-msgid.
        <return>-number = sy-msgno.

        lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            message_container = lo_message_container.
      ENDIF.
    ENDIF.

    CALL METHOD cl_pt_arq_customizing=>get_tcale_attribs
      EXPORTING
        im_pernr            = lv_pernr
        im_date             = lv_datum
      IMPORTING
        ex_cache_validity   = wa_cache_validity
      EXCEPTIONS
        it0001_not_found    = 1
        it0008_not_found    = 2
        missing_customizing = 3.

    IF sy-subrc <> 0.
      APPEND INITIAL LINE TO lt_return ASSIGNING <return>.
      MESSAGE e018(zhr_ess) INTO <return>-message.
      <return>-type   = sy-msgty.
      <return>-id     = sy-msgid.
      <return>-number = sy-msgno.

      lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.
    ENDIF.
**    CALL FUNCTION 'PT_ARQ_TEAMCALE_GET'
**      EXPORTING
***       IM_COMMAND         =
**        im_pernr           = lv_pernr
**        im_user            = LV_MAIN_USER
***       IM_LANGUAGE        = SY-LANGU
**        im_team            = 'MSS_LTV_EE_ALL' "??????
**        im_begda           = lv_begda
**        im_endda           = lv_endda
**        im_cache_validity  = wa_cache_validity
***       IM_GOTO_PAGE_NUMBER        = 1
***       IM_TIME_FORMAT     = 2
**        im_modus           = 'R' "????
***       IM_DEBUG           =
***       IM_DEACTIVATE_CHECK        =
***       IM_DEACTIVATE_PAGING       =
***       IM_ABAP_FLAG       =
*** IMPORTING
***       EX_TEAM            =
***       EX_CACHE_DATE      =
***       EX_CACHE_TIME      =
***       EX_CALENDAR_TAB    =
***       EX_ROWLABEL        =
***       EX_POSITION        =
**      TABLES
**        ex_team_tab        = lt_team_tab
**        ex_weekday_tab     = lt_weekday_tab
**        ex_legend_tab      = lt_legend_tab
**        ex_commands        = lt_commands
**        ex_custcolumns_tab = lt_custcolumns_tab
**        ex_messages        = lt_messages.

    lt_return = lt_messages.
    DELETE lt_return WHERE type <> 'E'.
    IF lt_return IS NOT INITIAL.
      lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.
    ENDIF.


*****<<<ЗАГЛУШКА
    APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<abs>).
    <abs>-abstype = '0101'.
    <abs>-begda = '20170717'.
    <abs>-endda ='20170717'.
    APPEND INITIAL LINE TO et_entityset ASSIGNING <abs>.
    <abs>-abstype = '0101'.
    <abs>-begda = '20170720'.
    <abs>-endda ='20170725'.
****>>>КОНЕЦ
  ENDMETHOD.


  METHOD entitycheckset_create_entity.
    DATA: ls_request    TYPE ptarq_uia_request
            , pernr_tab     TYPE pernr_us_tab
            , lv_errors     TYPE ptreq_has_error_flag
            .
    DATA: lt_return   TYPE  bapiret2_t
        , lt_messages TYPE  ptarq_uia_messages_tab
        , lt_commands TYPE  ptarq_uia_command_tab
          .

    DATA lo_message_container TYPE REF TO /iwbep/if_message_container.
    lo_message_container = mo_context->get_message_container( ).

    io_data_provider->read_entry_data(
      IMPORTING
        es_data = er_entity ).

    DATA(lo_assistent) = zcl_mss_data_assistent=>get_instance( ).

*21.03.2020
    IF er_entity-entranceuser IS NOT INITIAL.
      DATA(lv_owner_pernr) = lo_assistent->get_pernr( iv_usrid = CONV #( er_entity-entranceuser ) ).
    ELSE.
      lv_owner_pernr = lo_assistent->get_pernr( iv_usrid = CONV #( er_entity-mainuser ) ).
    ENDIF.
    IF lv_owner_pernr IS INITIAL.
      APPEND INITIAL LINE TO lt_return ASSIGNING FIELD-SYMBOL(<return>).
      MESSAGE e019(zhr_ess) INTO <return>-message.
      <return>-type   = sy-msgty.
      <return>-id     = sy-msgid.
      <return>-number = sy-msgno.

      lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.
    ENDIF.

    DATA(lv_main_pernr)  = lo_assistent->get_pernr( iv_usrid = CONV #( er_entity-mainuser ) ).
    IF lv_main_pernr IS INITIAL.
      APPEND INITIAL LINE TO lt_return ASSIGNING <return>.
      MESSAGE e019(zhr_ess) INTO <return>-message.
      <return>-type   = sy-msgty.
      <return>-id     = sy-msgid.
      <return>-number = sy-msgno.

      lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.
    ENDIF.

*    IF er_entity-owner_number IS INITIAL.
*      APPEND INITIAL LINE TO lt_return ASSIGNING FIELD-SYMBOL(<return>).
*      MESSAGE e003(zhr_ess) WITH 'owner_number' INTO <return>-message.
*
*      <return>-type   = sy-msgty.
*      <return>-id     = sy-msgid.
*      <return>-number = sy-msgno.
*
*      lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).
*
*      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*        EXPORTING
*          message_container = lo_message_container.
*    ENDIF.

    IF er_entity-begda IS INITIAL OR er_entity-endda IS INITIAL OR er_entity-begda > er_entity-endda.
      APPEND INITIAL LINE TO lt_return ASSIGNING <return>.
      IF er_entity-begda IS INITIAL.
        MESSAGE e003(zhr_ess) WITH 'begda' INTO <return>-message.
      ELSEIF er_entity-begda IS INITIAL.
        MESSAGE e003(zhr_ess) WITH 'endda' INTO <return>-message.
      ELSE.
        MESSAGE e005(zhr_ess) INTO <return>-message.
      ENDIF.

      <return>-type   = sy-msgty.
      <return>-id     = sy-msgid.
      <return>-number = sy-msgno.

      lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.
    ENDIF.

*    IF er_entity-approver_number IS INITIAL .
*      APPEND INITIAL LINE TO lt_return ASSIGNING <return>.
*      MESSAGE e003(zhr_ess) WITH 'approver_number' INTO <return>-message.
*
*      <return>-type   = sy-msgty.
*      <return>-id     = sy-msgid.
*      <return>-number = sy-msgno.
*
*      lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).
*
*      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*        EXPORTING
*          message_container = lo_message_container.
*
*    ENDIF.
**    <<<< ПОЛУЧИМ дефолтные данные
    zcl_vacation_operation=>request_prepare( EXPORTING pernr      = lv_owner_pernr
                                             IMPORTING req        = ls_request
                                                       ex_comm    = lt_commands
                                                       messages   = lt_messages ).
    LOOP AT lt_messages ASSIGNING FIELD-SYMBOL(<lt_messages>) WHERE type = 'E'.
      APPEND INITIAL LINE TO lt_return ASSIGNING <return>.
      <return>-type    = <lt_messages>-type.
      <return>-id      = <lt_messages>-id.
      <return>-number  = <lt_messages>-number.
      <return>-message = <lt_messages>-message.
    ENDLOOP.

    IF lt_return IS NOT INITIAL.
      lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.
    ENDIF.

*    data(lv_approver_number) = lo_assistent->get_pernr_ruk( iv_pernr = lv_owner_pernr iv_begda = conv #( er_entity-begda ) ).
*
*    if lv_approver_number is INITIAL.
*            APPEND INITIAL LINE TO lt_return ASSIGNING <return>.
*      MESSAGE e019(zhr_ess) INTO <return>-message.
*      <return>-type   = sy-msgty.
*      <return>-id     = sy-msgid.
*      <return>-number = sy-msgno.
*
*      lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).
*
*      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*        EXPORTING
*          message_container = lo_message_container.
*    ENDIF.

    DATA(lv_subty) = CONV numc4( er_entity-abstype ).

    zcl_vacation_operation=>modify_antrag( EXPORTING endda    = CONV #( er_entity-endda )
                                                     begda    = CONV #( er_entity-begda )
*                                                     subty    = er_entity-abstype
                                                     subty    = CONV #( lv_subty )
                                                     notice   = CONV #( er_entity-comment )
                                                     next_pro = er_entity-approver_number
                                           CHANGING  req      = ls_request ).
    zcl_vacation_operation=>request_check(   EXPORTING request          = ls_request
                                                       pernr            = lv_owner_pernr
                                             IMPORTING checked_request  = ls_request
                                                       messages         = lt_messages ).
    LOOP AT lt_messages ASSIGNING <lt_messages> WHERE type = 'E'.
      APPEND INITIAL LINE TO lt_return ASSIGNING <return>.
      <return>-type = <lt_messages>-type.
      <return>-id   = <lt_messages>-id.
      <return>-number = <lt_messages>-number.
      <return>-message = <lt_messages>-message.
    ENDLOOP.

    IF lt_return IS NOT INITIAL.
      lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.
    ENDIF.
**    ЕСЛИ ВСЕ ОК, ТО ОТПРАВЛЯЕМ "ОК"
    er_entity-check = 'X'.
  ENDMETHOD.


  METHOD entitycheckset_get_entity.
    ASSERT 1 = 0.
  ENDMETHOD.


  METHOD entitymainset_get_entity.
*сервис заполняет данные по умолчанию, потом пользователь уже выбирает необходимый тип заявки и тд
    CONSTANTS: c_default_time_type TYPE ptarq_uia_timetype VALUE '01'.
    DATA: lv_loginactdir      TYPE zlogin_act_dir
        , lv_login_orig       TYPE string
        , lv_pernr            TYPE persno
        , lv_pernr_initiator  TYPE persno
        , lv_property         TYPE string
        , lt_return           TYPE bapiret2_t
        , lt_p0002            TYPE TABLE OF p0002
        , lv_fio_initiator    TYPE string
        , lo_rfc              TYPE rfcdest
        .

    DATA: lt_message      TYPE ptarq_uia_messages_tab
        , lt_command      TYPE ptarq_uia_command_tab
        , ls_request      TYPE ptarq_uia_request
        , lv_change_flag  TYPE ptreq_change_flag
        , lv_changed      TYPE boolean
        , lt_accounts     TYPE ptarq_uia_quota_status_all_tab
        , lv_p            TYPE p LENGTH 16 DECIMALS 2
        , lv_i            TYPE i

        , lv_abs_type     TYPE subty
        , lv_begda        TYPE begda
        , lv_endda        TYPE begda
        , lv_ktart        TYPE p2006-ktart
        .

    DATA lo_message_container TYPE REF TO /iwbep/if_message_container.
    lo_message_container = mo_context->get_message_container( ).


    LOOP AT it_key_tab ASSIGNING FIELD-SYMBOL(<it_key_tab>).
      CASE <it_key_tab>-name.
*        WHEN 'LoginActDir'.
*          lv_loginactdir = <it_key_tab>-value.
*          lv_login_orig = lv_loginactdir.
*          TRANSLATE lv_loginactdir TO UPPER CASE.
        WHEN `EntranceUser`.
          DATA(lv_entranceuser) = CONV string( <it_key_tab>-value ).
          TRANSLATE lv_entranceuser TO UPPER CASE.
        WHEN `MainUser`.
          DATA(lv_mainuser) = CONV string( <it_key_tab>-value ).
          TRANSLATE lv_mainuser TO UPPER CASE.
*        WHEN 'Pernr'.
*          lv_pernr = <it_key_tab>-value.
        WHEN 'Abstype'.
          lv_abs_type = <it_key_tab>-value.
        WHEN 'Begda'.
          lv_begda = <it_key_tab>-value.
        WHEN 'Endda'.
          lv_endda = <it_key_tab>-value.
      ENDCASE.
    ENDLOOP.

    DATA(lo_assistent) = zcl_mss_data_assistent=>get_instance( ).

    lv_pernr = lv_pernr = lo_assistent->get_pernr( iv_usrid = CONV #( lv_mainuser ) ).

    IF lv_pernr IS INITIAL.
      APPEND INITIAL LINE TO lt_return ASSIGNING FIELD-SYMBOL(<return>).
      MESSAGE e017(zhr_ess) INTO <return>-message.
      <return>-type    = sy-msgty.
      <return>-id      = sy-msgid.
      <return>-number  = sy-msgno.

      lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.
    ENDIF.

*    lv_pernr = lo_assistent->get_pernr( iv_usrid = CONV #( lv_entranceuser ) ).

*    IF lv_pernr IS INITIAL.
*      APPEND INITIAL LINE TO lt_return ASSIGNING <return>.
*      MESSAGE e017(zhr_ess) INTO <return>-message.
*      <return>-type    = sy-msgty.
*      <return>-id      = sy-msgid.
*      <return>-number  = sy-msgno.
*
*      lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).
*
*      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*        EXPORTING
*          message_container = lo_message_container.
*    ENDIF.

**    <<<< ПОЛУЧИМ дефолтные данные
    zcl_vacation_operation=>request_prepare( EXPORTING pernr      = lv_pernr
                                             IMPORTING req        = ls_request
                                                       ex_comm    = lt_command
                                                       messages   = lt_message ).

    LOOP AT lt_message ASSIGNING FIELD-SYMBOL(<lt_messages>) WHERE type = 'E'.
      APPEND INITIAL LINE TO lt_return ASSIGNING <return>.
      <return>-type    = <lt_messages>-type.
      <return>-id      = <lt_messages>-id.
      <return>-number  = <lt_messages>-number.
      <return>-message = <lt_messages>-message.
    ENDLOOP.
    IF lt_return IS NOT INITIAL.
      lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.
    ENDIF.

*   Поле Abs_limit формируется в соответствии с указанными:
*   ДАТАМИ ОТПУСКА
*   ТИПОМ ОТПУСКА"
    ls_request-ins_item-begda = lv_begda.
    ls_request-ins_item-endda = lv_endda.
    IF lv_abs_type IS NOT INITIAL.
      ls_request-ins_item-subty = lv_abs_type.
    ENDIF.


    er_entity-login_act_dir   = lv_mainuser.
    er_entity-pernr           = lv_pernr.
    er_entity-request_id      = ls_request-request_id.
    er_entity-begda           = ls_request-ins_item-begda.
    er_entity-endda           = ls_request-ins_item-endda.
    er_entity-abstype         = ls_request-ins_item-subty.
    er_entity-approver_fio    = ls_request-next_processor-name.
    er_entity-approver_number = ls_request-next_processor-pernr.

    er_entity-approver_number = lo_assistent->get_pernr_ruk( iv_pernr = lv_pernr iv_begda = lv_begda ).

*    >>>Шаталов Б.А. 08.09.2021 Согласующий делегировал полномочия
    DATA(lv_approver) = er_entity-approver_number.
    SELECT SINGLE usrid
             INTO @DATA(lv_user)
             FROM pa0105
        WHERE subty  = '9001'
          AND begda <= @sy-datum
          AND endda >= @sy-datum
          AND pernr  = @er_entity-approver_number.
    IF sy-subrc = 0.
      SELECT SINGLE main_user INTO @DATA(rv_euser)
        FROM zthr_entr_user
        WHERE entrance_user = @lv_user
        AND begda <= @sy-datum
        AND endda >= @sy-datum
        AND del = ''.
      IF sy-subrc = 0.
        DATA(lv_new_approver) = lo_assistent->get_pernr(
          iv_usrid  = CONV #( rv_euser )
        ).
        IF lv_new_approver IS NOT INITIAL.
          er_entity-approver_number = lv_new_approver.
        ENDIF.
      ENDIF.
    ENDIF.
*    <<<Шаталов Б.А. 08.09.2021 Согласующий делегировал полномочия

    er_entity-approver_fio    = lo_assistent->get_pernr_fio( iv_pernr = er_entity-approver_number iv_begda = lv_begda ).
*    ЗАГЛУШКА - НЕЛЬЗЯ РЕДАКТИРОВАТЬ
*    er_entity-approver_vis = ls_request-next_processor-controlrecord_state."??? поле согласующего доступно для редактирования
    er_entity-owner_fio       = ls_request-owner-name.
    er_entity-owner_number    = ls_request-owner-pernr.
*    ЗАГЛУШКА - НЕЛЬЗЯ РЕДАКТИРОВАТЬ
*    er_entity-owner_vis = ls_request-owner-controlrecord_state."??? поле отпускника доступно для редактирования
    er_entity-initiator_fio    = lo_assistent->get_pernr_fio( iv_pernr = lv_pernr )."ФИО user
    er_entity-initiator_number = lv_pernr."ТН user

*    ZCL_VACATION_APPL=>fix_uppercase_fio(
*        CHANGING
*          cv_fio = er_entity-approver_fio ).
*
*    ZCL_VACATION_APPL=>fix_uppercase_fio(
*        CHANGING
*          cv_fio = er_entity-owner_fio ).

**    er_entity-abs_limit = '0028'."откуда брать эти данные???
** БРАТЬ ДНИ ИЗ ФМ
*    CLEAR: lt_message, lt_command.
*    CALL FUNCTION 'PT_ARQ_ACCOUNTS_GET'
*      DESTINATION lo_rfc
*      EXPORTING
*        im_pernr     = ls_request-next_processor-pernr
*        im_sel_pernr = ls_request-owner-pernr
*        im_begda     = sy-datum "  ls_request-ins_item-begda  " выводить количество доступных дней отпуска на текущую дату.
*        im_endda     = sy-datum "ls_request-ins_item-endda
*        im_modus     = 'R'
*      IMPORTING
*        ex_accounts  = lt_accounts
*      TABLES
*        ex_messages  = lt_message
*        ex_commands  = lt_command.

    zcl_vacation_appl=>get_timetypes(
      EXPORTING
        i_subty  = ls_request-ins_item-subty
      IMPORTING
        et_types = DATA(lt_types) ).

    IF lt_types IS INITIAL.
      APPEND INITIAL LINE TO lt_return ASSIGNING <return>.
      MESSAGE e004(zhr_ess) WITH ls_request-ins_item-subty INTO <return>-message.
      <return>-type = sy-msgty.
      <return>-id   = sy-msgid.
      <return>-number = sy-msgno.
      lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.
    ENDIF.

    LOOP AT lt_types ASSIGNING FIELD-SYMBOL(<fs_types>).
      lv_ktart = <fs_types>-type.
      zcl_vacation_appl=>get_limit_days(
        EXPORTING
          i_pernr = ls_request-owner-pernr
          i_date  = sy-datum
          i_ktart = lv_ktart
        IMPORTING
          e_limit = lv_p ).
      lv_i = lv_p.
      ADD lv_i TO er_entity-abs_limit.
    ENDLOOP.
  ENDMETHOD.


  METHOD entitysaveset_create_entity.
    DATA: lt_header TYPE /iwbep/t_mgw_name_value_pair
          .
    FIELD-SYMBOLS: <fs_header>  TYPE LINE OF /iwbep/t_mgw_name_value_pair
                 .
    DATA: lt_return           TYPE bapiret2_t
        , lv_pernr            TYPE persno
        , ls_request          TYPE  ptarq_uia_request
        , lv_has_errors       TYPE  ptreq_has_error_flag
        , lt_messages         TYPE  ptarq_uia_messages_tab
        , lt_commands         TYPE  ptarq_uia_command_tab
        , lv_multiple_check   TYPE  ptreq_has_error_flag
        , ct_approver         TYPE  ptreq_approver_tab
        .
    io_data_provider->read_entry_data(
         IMPORTING
              es_data = er_entity ).

    DATA lo_message_container TYPE REF TO /iwbep/if_message_container.
    lo_message_container = mo_context->get_message_container( ).

    DATA(lo_assistent) = zcl_mss_data_assistent=>get_instance( ).

    lv_pernr = lo_assistent->get_pernr( iv_usrid = CONV #( er_entity-mainuser ) ).

    IF lv_pernr IS INITIAL.
      APPEND INITIAL LINE TO lt_return ASSIGNING FIELD-SYMBOL(<return>)..
      MESSAGE e020(zhr_ess) INTO <return>-message.
      <return>-type   = sy-msgty.
      <return>-id     = sy-msgid.
      <return>-number = sy-msgno.
      lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.
    ENDIF.

    lv_pernr = lo_assistent->get_pernr( iv_usrid = CONV #( er_entity-entranceuser ) ).
    IF lv_pernr IS INITIAL.
      lv_pernr = lo_assistent->get_pernr( iv_usrid = CONV #( er_entity-mainuser ) ).
    ENDIF.

    IF lv_pernr IS INITIAL.
      APPEND INITIAL LINE TO lt_return ASSIGNING <return>.
      MESSAGE e017(zhr_ess) INTO <return>-message.
      <return>-type   = sy-msgty.
      <return>-id     = sy-msgid.
      <return>-number = sy-msgno.
      lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.
    ENDIF.

*    <<<< ПОЛУЧИМ дефолтные данные
    zcl_vacation_operation=>request_prepare( EXPORTING pernr    = lv_pernr
                                             IMPORTING req      = ls_request
                                                       ex_comm  = lt_commands
                                                       messages = lt_messages ).

    LOOP AT lt_messages ASSIGNING FIELD-SYMBOL(<lt_messages>) WHERE type = 'E'.
      APPEND INITIAL LINE TO lt_return ASSIGNING <return>.
      <return>-type = <lt_messages>-type.
      <return>-id   = <lt_messages>-id.
      <return>-number = <lt_messages>-number.
      <return>-message = <lt_messages>-message.
    ENDLOOP.

    IF lt_return IS NOT INITIAL.
      lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.
    ENDIF.

    DATA(lv_subty) = CONV numc4( er_entity-abstype ).

    zcl_vacation_operation=>modify_antrag( EXPORTING endda    = CONV #( er_entity-endda )
                                                     begda    = CONV #( er_entity-begda )
*                                                     subty    = er_entity-abstype
                                                     subty    = CONV #( lv_subty )
                                                     notice   = CONV #( er_entity-comment )
                                                     next_pro = er_entity-approver_number
                                           CHANGING  req      = ls_request ).

    zcl_vacation_operation=>request_check(   EXPORTING request          = ls_request
                                                       pernr            = lv_pernr
                                             IMPORTING checked_request  = ls_request
                                                       messages         = lt_messages ).

    LOOP AT lt_messages ASSIGNING <lt_messages> WHERE type = 'E'.
      APPEND INITIAL LINE TO lt_return ASSIGNING <return>.
      <return>-type = <lt_messages>-type.
      <return>-id   = <lt_messages>-id.
      <return>-number = <lt_messages>-number.
      <return>-message = <lt_messages>-message.
    ENDLOOP.

    IF lt_return IS NOT INITIAL.
      lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.
    ENDIF.
***    <<<< ОТПРАВЛЯЕМ ЗАЯВКУ
    zcl_vacation_operation=>request_exec( EXPORTING request_id       = ls_request-request_id
                                                    pernr            = lv_pernr
*                                                    command          = lv_command
                                          IMPORTING messages         = lt_messages
                                          CHANGING  exec_req         = ls_request ).

    LOOP AT lt_messages ASSIGNING FIELD-SYMBOL(<messages>) WHERE type = 'E'.
      APPEND INITIAL LINE TO lt_return ASSIGNING <return>.
      <return>-type    = <messages>-type.
      <return>-id      = <messages>-id.
      <return>-number  = <messages>-number.
      <return>-message = <messages>-message.
      DATA(lv_has_error) = abap_true.
    ENDLOOP.

    lo_assistent->check_pernr_is_top_head(
      EXPORTING
        iv_pernr     = lv_pernr
        iv_begda     = sy-datum
        iv_endda     = sy-datum
      IMPORTING
*       ev_pernr_gov =
        ev_is_gov    = DATA(lv_is_gov) ).

    IF lv_is_gov IS NOT INITIAL. "Если заявку создает Губернатор - сразу в статус "Утверждено"

      DATA(lv_command) = cl_pt_req_const=>c_cmd_execute_approve.

      zcl_vacation_operation=>request_exec(
            EXPORTING
              request_id = ls_request-request_id
              pernr      = lv_pernr
              command    = lv_command
            IMPORTING
              messages   = lt_messages
            CHANGING
              exec_req   = ls_request ).

      LOOP AT lt_messages ASSIGNING <messages>
            WHERE type = 'E'.

        APPEND INITIAL LINE TO lt_return
            ASSIGNING <return>.

        lv_has_error     = abap_true.
        <return>-type    = <messages>-type.
        <return>-id      = <messages>-id.
        <return>-number  = <messages>-number.
        <return>-message = <messages>-message.
      ENDLOOP.

    ENDIF.

    DATA: lt_mapping  TYPE zttmail_mapping
        , lt_mail_to TYPE bcsy_smtpa
        .
    IF lv_has_error IS INITIAL.
      DATA(lv_approver) = er_entity-approver_number.
      SELECT SINGLE usrid
               INTO @DATA(lv_user)
               FROM pa0105
          WHERE subty  = '9001'
            AND begda <= @sy-datum
            AND endda >= @sy-datum
            AND pernr  = @er_entity-approver_number.
      IF sy-subrc = 0.
        SELECT SINGLE main_user INTO @DATA(rv_euser)
          FROM zthr_entr_user
          WHERE entrance_user = @lv_user
          AND begda <= @sy-datum
          AND endda >= @sy-datum
          AND del = ''.
        IF sy-subrc = 0.
          DATA(lv_new_approver) = lo_assistent->get_pernr(
            iv_usrid  = CONV #( rv_euser )
          ).
          IF lv_new_approver IS NOT INITIAL.
            lv_approver = lv_new_approver.
          ENDIF.
        ENDIF.
      ENDIF.
      DATA(lv_email) = lo_assistent->get_pernr_0105( iv_pernr = lv_approver ).
      IF lv_email IS NOT INITIAL.
        APPEND INITIAL LINE TO lt_mail_to ASSIGNING FIELD-SYMBOL(<lt_mail_to>).
        <lt_mail_to> = lv_email.

        IF lv_is_gov IS NOT INITIAL.
          DATA(lv_subject) = CONV string('Отпуск согласован').
          DATA(lv_form)    = CONV string('ZHR_REQUES_VACATION_APPROVED').
        ELSE.
          lv_subject = 'Новая заявка на отпуск'.
          lv_form    = `ZHR_REQUEST_SAVE_VACATION`.
        ENDIF.

        APPEND VALUE #( name = `<pernr>` value = CONV zsmail_mapping-value( lv_pernr && ` ` && lo_assistent->get_pernr_fio( iv_pernr = lv_pernr ) ) ) TO lt_mapping.

        zcl_send_email=>send_mail( EXPORTING itd_to          = lt_mail_to
                                             iv_textname     = CONV #( lv_form )
                                             iv_subject      = CONV #( lv_subject )
                                             itd_mapping     = lt_mapping
                                             iv_immediately  = abap_false ).
      ENDIF.
    ENDIF.

    IF lt_return IS NOT INITIAL.
      lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.
    ENDIF.

    er_entity-check = abap_true.
  ENDMETHOD.
ENDCLASS.

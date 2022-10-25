class ZCL_ZHR_ESS_VAC_ABS_DPC_EXT definition
  public
  inheriting from ZCL_ZHR_ESS_VAC_ABS_DPC
  create public .

public section.
protected section.

  methods ENTITYMAINSET_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZHR_ESS_VAC_ABS_DPC_EXT IMPLEMENTATION.


  METHOD entitymainset_get_entityset.
    DATA: lv_loginactdir TYPE zlogin_act_dir
            , lv_property TYPE string
            , lv_pernr TYPE persno
            , lv_pernr2 TYPE persno
            , lv_begda    TYPE begda
            , lv_endda    TYPE endda
            , lv_string TYPE string
            , lv_int    TYPE i
            , refresh	TYPE c
            , blockcal_tab  TYPE zzcalendar_data_tab
            .

    DATA: lt_return TYPE TABLE OF bapiret2
        , lv_err_number TYPE i VALUE 1
        , lv_message TYPE string
        , lo_message_container TYPE REF TO /iwbep/if_message_container
        .


    DATA: lt_messages TYPE ptarq_uia_messages_tab
        , lt_command  TYPE ptarq_uia_command_tab
        , lt_request  TYPE ptarq_uia_reqlist_tab
        , lt_acc      TYPE ptarq_uia_quota_status_all_tab
        .

    lo_message_container = mo_context->get_message_container( ).

    DATA(lo_hr_mss) = NEW zcl_mss_data_assistent( ).

    LOOP AT it_filter_select_options ASSIGNING FIELD-SYMBOL(<filter>).
      LOOP AT <filter>-select_options ASSIGNING FIELD-SYMBOL(<range>).
        lv_property = <filter>-property.
        TRANSLATE lv_property TO UPPER CASE.
        CASE lv_property.
          WHEN 'DATA'.
            lv_begda = <range>-low.
            lv_endda = lv_begda + 365.
            lv_begda = lv_begda - 365.
*          WHEN 'PERNR'.
*            lv_pernr = <range>-low.
          WHEN 'MAINUSER'.
            DATA(lv_main_pernr) = lo_hr_mss->get_pernr( iv_usrid = CONV #( <range>-low ) ).
            IF lv_main_pernr IS INITIAL.
              APPEND INITIAL LINE TO lt_return ASSIGNING FIELD-SYMBOL(<return>).
              MESSAGE e001(zhr_ess) INTO <return>-message.
              <return>-type   = sy-msgty.
              <return>-id     = sy-msgid.
              <return>-number = sy-msgno.
            ENDIF.
          WHEN 'ENTRANCEUSER'.
*            DATA(lv_entrance_pernr) = lo_hr_mss->get_pernr( iv_usrid = CONV #( <range>-low ) ).
*            IF lv_entrance_pernr IS INITIAL.
*              lv_entrance_pernr = lv_main_pernr.
*              APPEND INITIAL LINE TO lt_return ASSIGNING <return>.
*              MESSAGE e001(zhr_ess) INTO <return>-message.
*              <return>-type   = sy-msgty.
*              <return>-id     = sy-msgid.
*              <return>-number = sy-msgno.
*            ENDIF.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.

    IF lt_return IS NOT INITIAL.
      lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.
    ENDIF.

    blockcal_tab = zcl_vacation_operation=>eecale_get( EXPORTING pernr    = lv_main_pernr
                                                                 begda    = lv_begda
                                                                 endda    = lv_endda
                                                                 refresh  = refresh
                                                       IMPORTING messages = lt_messages ).

*    LOOP AT lt_messages ASSIGNING FIELD-SYMBOL(<messages>) WHERE type = 'E'.
*      APPEND INITIAL LINE TO lt_return ASSIGNING <return>.
*      <return>-type    = <messages>-type.
*      <return>-id      = <messages>-id.
*      <return>-number  = <messages>-number.
*      <return>-message = <messages>-message.
*      lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).
*    ENDLOOP.
    IF  lt_return IS NOT INITIAL .
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.
    ENDIF.

    LOOP AT blockcal_tab ASSIGNING FIELD-SYMBOL(<blockcal_tab>) WHERE subty IS NOT INITIAL.
      APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<et_entityset>).
      <et_entityset>-abs_begda = <blockcal_tab>-begda.
      <et_entityset>-abs_endda = <blockcal_tab>-endda.
      <et_entityset>-abstype = <blockcal_tab>-subty.
      IF <blockcal_tab>-request_or_attabs NE 'R'.
        <et_entityset>-status = 'POSTED'.
      ELSE.
        <et_entityset>-status = <blockcal_tab>-status.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

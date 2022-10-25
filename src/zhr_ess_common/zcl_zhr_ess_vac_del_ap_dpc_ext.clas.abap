class ZCL_ZHR_ESS_VAC_DEL_AP_DPC_EXT definition
  public
  inheriting from ZCL_ZHR_ESS_VAC_DEL_AP_DPC
  create public .

public section.
protected section.

  methods VACAPPLICATIONSE_GET_ENTITY
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZHR_ESS_VAC_DEL_AP_DPC_EXT IMPLEMENTATION.


  METHOD vacapplicationse_get_entity.

    DATA:
      lv_entrance_user     TYPE zlogin_act_dir,
      lv_request_id        TYPE tim_req_id,
      lv_pernr             TYPE pernr_d,
      lv_name              TYPE string,
      lv_abstype           TYPE char4,
      lv_owner_number      TYPE pernr_d,
      lv_begda             TYPE datum,
      lv_endda             TYPE datum,

      lt_return            TYPE TABLE OF bapiret2,
      ls_return            LIKE LINE OF lt_return,
      lt_mes               TYPE ptarq_uia_messages_tab,
      ls_mes               LIKE LINE OF lt_mes,
      lt_com               TYPE ptarq_uia_command_tab,
      ls_req               TYPE ptreq_header,
      lv_has_errors        TYPE ptreq_has_error_flag,
      ls_req_old           TYPE ptarq_uia_request,
      ls_req_new           TYPE ptarq_uia_request,
      lv_show_change       TYPE boolean,
      lv_need              TYPE flag,
      ls_pa2001            TYPE pa2001,

      lo_message_container TYPE REF TO /iwbep/if_message_container.

    FIELD-SYMBOLS:
                   <fs_key> LIKE LINE OF it_key_tab.


    lo_message_container = mo_context->get_message_container( ).

*   обработка входящих параметров
    LOOP AT it_key_tab ASSIGNING <fs_key>.

      lv_name = <fs_key>-name.
      TRANSLATE lv_name TO UPPER CASE.

      CASE lv_name.
        WHEN 'ENTRANCEUSER'.
          lv_entrance_user = <fs_key>-value.
          er_entity-entranceuser = lv_entrance_user.
        WHEN 'MAINUSER'.
          lv_entrance_user = <fs_key>-value.
          er_entity-entranceuser = lv_entrance_user.

        WHEN 'REQUESTID'.
          lv_request_id = <fs_key>-value.

        WHEN 'ABSTYPE'.
          lv_abstype = <fs_key>-value.
          er_entity-abstype = lv_abstype.

        WHEN 'OWNERNUMBER'.
          lv_owner_number = <fs_key>-value.
          er_entity-owner_number = lv_owner_number.

        WHEN 'BEGDA'.
          lv_begda = <fs_key>-value.
          er_entity-begda = lv_begda.

        WHEN 'ENDDA'.
          lv_endda = <fs_key>-value.
          er_entity-endda = lv_endda.

        WHEN OTHERS.
          CLEAR ls_return.
          ls_return-type = 'E'.
          ls_return-id   = 'zhr_ess_1'.
          ls_return-number = '014'.
          APPEND ls_return TO lt_return.

          lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
            EXPORTING
              message_container = lo_message_container.
      ENDCASE.
    ENDLOOP.

*   табельный того, кто решил изменить заявку берем из 'ENTRANCEUSER'
*   однако, это может быть и технический пользователь, тогда табельный берем из 'OWNERNUMBER'
    TRANSLATE lv_entrance_user TO UPPER CASE.

    CALL METHOD zcl_vacation_appl=>is_need_0105_check
      EXPORTING
        iv_login = lv_entrance_user
      RECEIVING
        rv_need  = lv_need.


    DATA(lo_assistent) = zcl_mss_data_assistent=>get_instance( ).
    lv_pernr = lo_assistent->get_pernr( iv_usrid = CONV #( lv_entrance_user ) ).

    IF sy-subrc NE 0.
      IF lv_need EQ 'X'.
        CLEAR ls_return.
        ls_return-type = 'E'.
        ls_return-id   = 'zhr_ess_1'.
        ls_return-number = '015'.
        ls_return-message_v1 = lv_entrance_user.
        APPEND ls_return TO lt_return.

        lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            message_container = lo_message_container.
      ELSE.
        lv_pernr = lv_owner_number.
      ENDIF.
    ENDIF.

*   читаем инфу по заявке
    IF lv_request_id IS NOT INITIAL.

      CALL METHOD zcl_vacation_appl=>get_req_header_data
        EXPORTING
          iv_request_id = lv_request_id
        IMPORTING
          es_request    = ls_req.

*     При удалении заявки нужно дополнительно проверить:
*     1. Можно удалять только заявки в статусе SENT
*     2. При удалении заявки нужно проверить, нет ли второй заявки от того же ТН И с той же пометкой DEL для записи ИТ2001

      CASE ls_req-status.
        WHEN 'SENT'.
        WHEN OTHERS.
          CLEAR ls_return.
          MESSAGE e016(zhr_ess_1) INTO ls_return-message.
          ls_return-type = sy-msgty.
          ls_return-id   = sy-msgid.
          ls_return-number = sy-msgno.
          APPEND ls_return TO lt_return.
      ENDCASE.


      zcl_vacation_appl=>get_attabs(
        EXPORTING
          i_req_id = lv_request_id    " Ид. документа
        IMPORTING
          es_data  = DATA(ls_attabs) ).

      zcl_vacation_appl=>find_running_del_vacation(
        EXPORTING
          i_pernr   = ls_attabs-pernr
          i_subty   = ls_attabs-subty
          i_begda   = ls_attabs-begda
          i_endda   = ls_attabs-endda
          i_req_id  = lv_request_id
        IMPORTING
          e_fnd     = DATA(lv_fnd)
          e_req_id  = DATA(lv_req_id) ).

      IF lv_fnd EQ abap_false.
        CLEAR: lv_req_id.
*        CLEAR: ls_return.
*        MESSAGE e021(zhr_ess_1) INTO ls_return-message.
*        ls_return-type = sy-msgty.
*        ls_return-id   = sy-msgid.
*        ls_return-number = sy-msgno.
*        APPEND ls_return TO lt_return.
      ENDIF.

      IF lt_return IS NOT INITIAL.
        lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            message_container = lo_message_container.
      ENDIF.
    ELSE.
      ls_req-status = 'POSTED'.
    ENDIF.

*   удаление заявки на отпуск
    CLEAR:
      lt_mes, lt_com.

    DO 2 TIMES.
      IF sy-index EQ 2.
*       обрабатываем вторую заявку при переносе отпуска, если она существует!!
        CHECK lv_req_id IS NOT INITIAL.
        lv_request_id = lv_req_id.
      ENDIF.

      CLEAR: er_entity-check.

      CASE ls_req-status.
        WHEN 'SENT' OR 'APPROVED'.

          er_entity-request_id = lv_request_id.

          CALL FUNCTION 'PT_ARQ_REQUEST_EXECUTE'
            EXPORTING
              im_request_id = lv_request_id
              im_command    = cl_pt_req_const=>c_cmd_execute_delete
              im_pernr      = lv_pernr
              im_modus      = cl_pt_req_const=>c_role_employee
            IMPORTING
              ex_has_errors = lv_has_errors
            TABLES
              ex_messages   = lt_mes
              ex_commands   = lt_com
            EXCEPTIONS
              OTHERS        = 8.
*{KLOKOVNYU 29.04.2019 Затираем ошибку т.к. пользователи против нормалного ведения данных
          DATA lt_pa0105 TYPE TABLE OF p0105.

          CALL FUNCTION 'HR_READ_INFOTYPE'
            EXPORTING
*             TCLAS           = 'A'
              pernr           = lv_pernr
              infty           = '0105'
              begda           = sy-datum
              endda           = sy-datum
            TABLES
              infty_tab       = lt_pa0105
            EXCEPTIONS
              infty_not_found = 1
              OTHERS          = 2.

          READ TABLE lt_pa0105 TRANSPORTING NO FIELDS WITH KEY subty = '9001'.
          IF sy-subrc = 0.

            READ TABLE lt_mes TRANSPORTING NO FIELDS WITH KEY id = 'HRTIM_ABS_REQ'
                                                              type = 'E'
                                                              number = '117'.
            IF sy-subrc = 0.
              CLEAR: lv_has_errors,
                     lt_mes.
            ENDIF.
          ENDIF.

*}KLOKOVNYU 29.04.2019 Затираем ошибку т.к. пользователи против нормалного ведения данных
          CLEAR lt_return.
          IF lv_has_errors IS INITIAL.
            er_entity-check = 'X'.
          ELSE.
            LOOP AT lt_mes INTO ls_mes WHERE type = 'E'.
              CLEAR ls_return.
              ls_return-type = ls_mes-type.
              ls_return-id   = ls_mes-id.
              ls_return-number = ls_mes-number.
              ls_return-message = ls_mes-message.
              APPEND ls_return TO lt_return.
            ENDLOOP.
          ENDIF.

        WHEN 'POSTED'.

          IF lv_request_id IS NOT INITIAL.
*       инфа по заявке для удаления
            CLEAR:
              lt_mes, lt_com.

            CALL FUNCTION 'PT_ARQ_REQUEST_PREPARE'
              EXPORTING
                im_request_id = lv_request_id
                im_command    = cl_pt_req_const=>c_cmd_prepare_delete
                im_pernr      = lv_pernr
                im_modus      = cl_pt_req_const=>c_role_employee
              IMPORTING
                ex_request    = ls_req_old
                ex_has_errors = lv_has_errors
              TABLES
                ex_messages   = lt_mes
                ex_commands   = lt_com.

            CLEAR lt_return.
            IF lv_has_errors IS INITIAL.

            ELSE.
              CLEAR er_entity-check.
              LOOP AT lt_mes INTO ls_mes WHERE type = 'E'.
                CLEAR ls_return.
                ls_return-type = ls_mes-type.
                ls_return-id   = ls_mes-id.
                ls_return-number = ls_mes-number.
                ls_return-message = ls_mes-message.
                APPEND ls_return TO lt_return.
              ENDLOOP.
            ENDIF.
            IF lt_return IS NOT INITIAL.
              lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).
              RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
                EXPORTING
                  message_container = lo_message_container.
            ENDIF.
          ENDIF.

*       создадим новую заявку на удаление
          CLEAR:
            lt_mes, lt_com.

          CALL FUNCTION 'PT_ARQ_REQUEST_PREPARE'
            EXPORTING
              im_request_id = '0'
              im_command    = 'CREATE'
              im_pernr      = lv_pernr
              im_modus      = cl_pt_req_const=>c_role_employee
            IMPORTING
              ex_request    = ls_req_new
              ex_has_errors = lv_has_errors
            TABLES
              ex_messages   = lt_mes
              ex_commands   = lt_com.

          CLEAR lt_return.
          IF lv_has_errors IS INITIAL.

          ELSE.
            CLEAR er_entity-check.
            LOOP AT lt_mes INTO ls_mes WHERE type = 'E'.
              CLEAR ls_return.
              ls_return-type = ls_mes-type.
              ls_return-id   = ls_mes-id.
              ls_return-number = ls_mes-number.
              ls_return-message = ls_mes-message.
              APPEND ls_return TO lt_return.
            ENDLOOP.
          ENDIF.
          IF lt_return IS NOT INITIAL.
            lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).
            RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
              EXPORTING
                message_container = lo_message_container.
          ENDIF.

          IF lv_request_id IS NOT INITIAL.
            ls_req_new-del_item = ls_req_old-del_item.
          ELSE.

            zcl_vacation_appl=>get_attabs(
           EXPORTING
             i_req_id = lv_request_id    " Ид. документа
           IMPORTING
             es_data  = DATA(ls_attabs1) ).


            SELECT SINGLE *
              INTO ls_pa2001
              FROM pa2001
              WHERE
                pernr EQ ls_attabs1-pernr   AND
                subty EQ ls_attabs1-subty AND
                sprps EQ ''         AND
                endda EQ ls_attabs1-endda   AND
                begda EQ ls_attabs1-begda.

            IF sy-subrc NE 0.
              CLEAR ls_return.
              ls_return-type = 'E'.
              ls_return-id   = 'zhr_ess_1'.
              ls_return-number = '020'.
              ls_return-message_v1 = lv_pernr.
              ls_return-message_v2 = lv_begda.
              ls_return-message_v3 = lv_endda.
              APPEND ls_return TO lt_return.

              IF lt_return IS NOT INITIAL.
                lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).
                RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
                  EXPORTING
                    message_container = lo_message_container.
              ENDIF.
            ENDIF.

            ls_req_new-del_item-operation = 'DEL'.
            ls_req_new-del_item-infotype = '2001'.
            ls_req_new-del_item-begda = lv_begda.
            ls_req_new-del_item-endda = lv_endda.
            ls_req_new-del_item-abwtg = ls_pa2001-abwtg.
            ls_req_new-del_item-attabs_hours = ls_pa2001-stdaz.
            ls_req_new-del_item-subty = lv_abstype.
          ENDIF.

          ls_req_new-del_item-item_id = ls_req_new-ins_item-item_id.
          CLEAR ls_req_new-ins_item.

          CLEAR:
            lt_mes, lt_com.

          er_entity-request_id = ls_req_new-request_id.

          CALL FUNCTION 'PT_ARQ_REQUEST_CHECK'
            EXPORTING
              im_request     = ls_req_new
              im_command     = cl_pt_req_const=>c_cmd_check_create
              im_pernr       = lv_pernr
              im_modus       = 'R'
            IMPORTING
              ex_has_errors  = lv_has_errors
              ex_request     = ls_req_new
              ex_show_change = lv_show_change
            TABLES
              ex_messages    = lt_mes
              ex_commands    = lt_com.

          CLEAR lt_return.
          IF lv_has_errors IS INITIAL.

          ELSE.
            CLEAR er_entity-check.
            LOOP AT lt_mes INTO ls_mes WHERE type = 'E'.
              CLEAR ls_return.
              ls_return-type = ls_mes-type.
              ls_return-id   = ls_mes-id.
              ls_return-number = ls_mes-number.
              ls_return-message = ls_mes-message.
              APPEND ls_return TO lt_return.
            ENDLOOP.
          ENDIF.
          IF lt_return IS NOT INITIAL.
            lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).
            RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
              EXPORTING
                message_container = lo_message_container.
          ENDIF.

          CLEAR:
            lt_mes, lt_com.

          CALL FUNCTION 'PT_ARQ_REQUEST_EXECUTE'
            EXPORTING
              im_request_id = ls_req_new-request_id
              im_command    = cl_pt_req_const=>c_cmd_execute_send
              im_pernr      = lv_pernr
              im_modus      = cl_pt_req_const=>c_role_employee
            IMPORTING
              ex_request    = ls_req_new
              ex_has_errors = lv_has_errors
            TABLES
              ex_messages   = lt_mes
              ex_commands   = lt_com.

          CLEAR lt_return.
          IF lv_has_errors IS INITIAL.
            er_entity-check = 'X'.
          ELSE.
            CLEAR er_entity-check.
            LOOP AT lt_mes INTO ls_mes WHERE type = 'E'.
              CLEAR ls_return.
              ls_return-type = ls_mes-type.
              ls_return-id   = ls_mes-id.
              ls_return-number = ls_mes-number.
              ls_return-message = ls_mes-message.
              APPEND ls_return TO lt_return.
            ENDLOOP.
          ENDIF.
          IF lt_return IS NOT INITIAL.
            lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).
            RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
              EXPORTING
                message_container = lo_message_container.
          ENDIF.

      ENDCASE.
    ENDDO.

    IF lt_return IS NOT INITIAL.
      lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.
    ENDIF.

  ENDMETHOD.
ENDCLASS.

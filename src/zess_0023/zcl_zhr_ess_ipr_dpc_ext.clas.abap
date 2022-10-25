class ZCL_ZHR_ESS_IPR_DPC_EXT definition
  public
  inheriting from ZCL_ZHR_ESS_IPR_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_STREAM
    redefinition .
protected section.

  methods IPRALLSET_GET_ENTITYSET
    redefinition .
  methods IPRREQS_CREATE_ENTITY
    redefinition .
  methods IPRREQS_DELETE_ENTITY
    redefinition .
  methods IPRREQS_GET_ENTITY
    redefinition .
  methods IPRREQS_GET_ENTITYSET
    redefinition .
  methods IPRREQS_UPDATE_ENTITY
    redefinition .
  methods IPRSTATUSSET_CREATE_ENTITY
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZHR_ESS_IPR_DPC_EXT IMPLEMENTATION.


  METHOD /iwbep/if_mgw_appl_srv_runtime~get_stream.


    CASE iv_entity_name.
      WHEN 'IprreqDoc'.

        DATA: ls_stream  TYPE ty_s_media_resource
              , lt_dd07v          TYPE STANDARD TABLE OF dd07v
                 , ls_lheader    TYPE ihttpnvp.
        DATA: lv_request_id        TYPE tim_req_id.
        DATA:  t_data_xml TYPE xstring,
               ls_iprreq  TYPE zhr_xss_iprreq.
        TYPES:



          BEGIN OF ty_main
                , fio TYPE text250
                , pernr TYPE text250
                , status TYPE text250
                , eduspec TYPE text250
                , edufrm TYPE text250
                , edugoal TYPE text250
                , edulength TYPE text250
                , eduresult TYPE text250
                , eduyear TYPE text250
             , END OF  ty_main .
        DATA: ls_main TYPE ty_main.
        DATA: BEGIN OF ls_input
                  , requestid     TYPE string
                  , entranceuser     TYPE string
                  , END OF ls_input.


        LOOP AT it_key_tab ASSIGNING FIELD-SYMBOL(<fs_key>).
          CASE <fs_key>-name.
*21/03/2020
            WHEN 'EntranceUser'.
              ls_input-entranceuser = <fs_key>-value.
            WHEN 'MainUser'.
              ls_input-entranceuser = <fs_key>-value.
            WHEN 'ReqId'.
              ls_input-requestid = <fs_key>-value.
              lv_request_id = ls_input-requestid.
          ENDCASE.
        ENDLOOP.

        IF lv_request_id IS NOT INITIAL.

          SELECT SINGLE * INTO  ls_iprreq FROM zhr_xss_iprreq WHERE req_id = lv_request_id .

          IF sy-subrc = 0.


            SELECT SINGLE cname INTO ls_main-edufrm
                  FROM t7ruokin
                  WHERE molga EQ 33 AND facet = 33 AND ccode = ls_iprreq-edu_frm.

            SELECT SINGLE text INTO ls_main-edugoal
               FROM zess_t_reas_txt
              WHERE reason_id = ls_iprreq-edu_reas
                AND lang = sy-langu.

            ls_main-edulength      = ls_iprreq-edu_length.
            ls_main-pernr         = ls_iprreq-pernr_creator.
            DATA(lo_assistent) = zcl_mss_data_assistent=>get_instance( ).

            ls_main-fio = lo_assistent->get_pernr_fio( iv_pernr = ls_iprreq-pernr_creator iv_begda = sy-datum ).



            DATA(lv_unit_type) = '0' && ls_iprreq-edu_length_type.
            SELECT SINGLE etext INTO ls_main-edulength
                  FROM t538t
                  WHERE sprsl EQ 'RU' AND zeinh = lv_unit_type.
            CONCATENATE ls_iprreq-edu_length ':' ls_main-edulength INTO ls_main-edulength.


            ls_main-eduyear = ls_iprreq-edu_year.
            ls_main-eduresult = ls_iprreq-edu_prog.

*            SELECT SINGLE name INTO ls_main-eduresult
*                  FROM zthr_edu_prograt
*                  WHERE id = ls_iprreq-edu_prog.
            SELECT single zt~name INTO ls_main-eduresult
              FROM zthr_edu_prograt AS zt
              INNER JOIN zthr_edu_program AS zm ON zm~id = zt~id
             WHERE zt~lang = sy-langu
               and zt~id = ls_iprreq-edu_prog
               AND zm~is_old = ' '.

            IF ls_iprreq-edu_result_another IS NOT INITIAL.
              ls_main-eduresult = ls_iprreq-edu_result_another.
            ENDIF.
*        er_entity-edu_spec = ls_iprreq-edu_spec.

            SELECT SINGLE stext INTO ls_main-eduspec
                  FROM zthr_dlinet
                  WHERE spras EQ 'RU' AND dline = ls_iprreq-edu_spec.

            IF   ls_iprreq-edu_spec_another  IS NOT INITIAL.
              ls_main-eduspec = ls_iprreq-edu_spec_another.
            ENDIF.

            CALL FUNCTION 'DD_DOMVALUES_GET'
              EXPORTING
                domname        = 'ZHR_XSS_IPRREQ_STATUS'   "<-- Your Domain Here
                text           = 'X'
                langu          = sy-langu
              TABLES
                dd07v_tab      = lt_dd07v
              EXCEPTIONS
                wrong_textflag = 1
                OTHERS         = 2.
            READ TABLE lt_dd07v ASSIGNING FIELD-SYMBOL(<ls_dd07v>) WITH KEY domvalue_l = ls_iprreq-status.
            IF sy-subrc = 0.
              ls_main-status = <ls_dd07v>-ddtext.
            ENDIF.
            CALL TRANSFORMATION zhr_ess_ipr_get
                        SOURCE main = ls_main
                        RESULT XML t_data_xml.
            ls_stream-mime_type = 'application/msword'.
            ls_stream-value = t_data_xml.
*              t_data_xml = cl_http_utility=>if_http_utility~escape_url( t_data_xml ).
            ls_lheader-name = 'Content-Disposition'.
            ls_lheader-value = 'inline; filename="' && 'zayavka.doc' && '";'.
            set_header( is_header = ls_lheader ).


            copy_data_to_ref( EXPORTING is_data = ls_stream
                             CHANGING  cr_data = er_stream ).

          ENDIF.

        ENDIF.
    ENDCASE.
  ENDMETHOD.


  METHOD iprallset_get_entityset.
**TRY.
*CALL METHOD SUPER->IPRALLSET_GET_ENTITYSET
*  EXPORTING
*    IV_ENTITY_NAME           =
*    IV_ENTITY_SET_NAME       =
*    IV_SOURCE_NAME           =
*    IT_FILTER_SELECT_OPTIONS =
*    IS_PAGING                =
*    IT_KEY_TAB               =
*    IT_NAVIGATION_PATH       =
*    IT_ORDER                 =
*    IV_FILTER_STRING         =
*    IV_SEARCH_STRING         =
**    io_tech_request_context  =
**  IMPORTING
**    et_entityset             =
**    es_response_context      =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.

    DATA:
      lt_iprreq         TYPE TABLE OF zhr_xss_iprreq,
      ls_select_options TYPE /iwbep/s_cod_select_option,
      lv_pernr          TYPE pernr_d,
      lv_entrance_user  TYPE string,
      lt_dd07v          TYPE STANDARD TABLE OF dd07v.

    FIELD-SYMBOLS:
      <ls_iprreq>    LIKE LINE OF lt_iprreq,
      <ls_entityset> LIKE LINE OF et_entityset.

    ls_select_options = it_filter_select_options[ 1 ]-select_options[ 1 ].
    lv_entrance_user = ls_select_options-low.

    " ТН создавшего заявку perner_creator
*    SELECT SINGLE pernr INTO lv_pernr FROM pa0105
*      WHERE
*            usrid = lv_entrance_user
*        AND subty EQ '9001'
*        AND begda <= sy-datum
*        AND endda >= sy-datum.

    DATA(lo_assistent) = zcl_mss_data_assistent=>get_instance( ).
    lv_pernr = lo_assistent->get_pernr( iv_usrid = CONV #( lv_entrance_user ) ).

    IF lv_pernr IS INITIAL.
      APPEND INITIAL LINE TO et_entityset ASSIGNING <ls_entityset>.
      <ls_entityset>-msg_id = 'E'.
      <ls_entityset>-message = 'Табельный номер не найден'.
      EXIT.
    ENDIF.
    " /perner_creator

    SELECT * INTO TABLE lt_iprreq FROM zhr_xss_iprreq WHERE pernr_creator = lv_pernr.

    IF sy-subrc = 0.
      LOOP AT  lt_iprreq ASSIGNING <ls_iprreq>.
        APPEND INITIAL LINE TO et_entityset ASSIGNING <ls_entityset>.

        <ls_entityset>-req_id   = <ls_iprreq>-req_id.
        <ls_entityset>-edu_year = <ls_iprreq>-edu_year.
        <ls_entityset>-reqdate  = <ls_iprreq>-date_create.

        SELECT SINGLE stext INTO <ls_entityset>-edu_spec_text
              FROM zthr_dlinet
              WHERE spras EQ 'RU' AND dline = <ls_iprreq>-edu_spec.

        SELECT SINGLE addedu_text INTO <ls_entityset>-edu_type_text
           FROM zess_0023_t_aedu
          WHERE addeduid = <ls_iprreq>-edu_type.

        " next string only for debuging
        <ls_entityset>-entrance_user = lv_entrance_user.

        CALL FUNCTION 'DD_DOMVALUES_GET'
          EXPORTING
            domname        = 'ZHR_XSS_IPRREQ_STATUS'   "<-- Your Domain Here
            text           = 'X'
            langu          = sy-langu
          TABLES
            dd07v_tab      = lt_dd07v
          EXCEPTIONS
            wrong_textflag = 1
            OTHERS         = 2.

        READ TABLE lt_dd07v ASSIGNING FIELD-SYMBOL(<ls_dd07v>) WITH KEY domvalue_l = <ls_iprreq>-status.
        IF sy-subrc = 0.
          <ls_entityset>-status_text = <ls_dd07v>-ddtext.
        ENDIF.

        CASE <ls_iprreq>-status.
          WHEN '3'.
            <ls_entityset>-status_text = 'Корректировка'.
            <ls_entityset>-correct = abap_true.
          WHEN OTHERS.
            <ls_entityset>-correct = abap_false.
        ENDCASE.

      ENDLOOP.

    ELSE.
      APPEND INITIAL LINE TO et_entityset ASSIGNING <ls_entityset>.
      <ls_entityset>-msg_id = 'W'.
      <ls_entityset>-message = 'ИПР не найдены'.
    ENDIF.

  ENDMETHOD.


  METHOD iprreqs_create_entity.
**TRY.
*CALL METHOD SUPER->IPRREQS_CREATE_ENTITY
*  EXPORTING
*    IV_ENTITY_NAME          =
*    IV_ENTITY_SET_NAME      =
*    IV_SOURCE_NAME          =
*    IT_KEY_TAB              =
**    io_tech_request_context =
*    IT_NAVIGATION_PATH      =
**    io_data_provider        =
**  IMPORTING
**    er_entity               =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.

    DATA:
      ls_request_input_data LIKE er_entity,
      lt_iprreq             TYPE TABLE OF zhr_xss_iprreq,
      ls_userinfo           TYPE zhr_xss_iprreq,
      lv_year               TYPE gjahr.
    FIELD-SYMBOLS: <ls_iprreq> TYPE zhr_xss_iprreq.
* Read Request Data

    io_data_provider->read_entry_data( IMPORTING es_data = ls_request_input_data ).
    DATA(lo_assistent) = zcl_mss_data_assistent=>get_instance( ).
    IF ls_request_input_data-edu_type IS NOT INITIAL.

      DATA(lv_edu_year) = ls_request_input_data-edu_year(4).
      DATA(lv_edu_quarter) = ls_request_input_data-edu_year+4(1).

      lv_year = sy-datum(4).
      IF lv_year > lv_edu_year.
        er_entity-msg_id  = 'W'.
        er_entity-message = 'Год обучения не может относиться к прошлому периоду'.
        EXIT.
      ENDIF.

      APPEND INITIAL LINE TO lt_iprreq ASSIGNING <ls_iprreq>.

      " ID заявки req_id
      DATA: lv_guid TYPE guid_32.
      CALL FUNCTION 'GUID_CREATE'
        IMPORTING
          ev_guid_32 = lv_guid.
      <ls_iprreq>-req_id = lv_guid.
      " /req_id

      " ТН создавшего заявку perner_creator
*      TRANSLATE ls_request_input_data-entrance_user TO UPPER CASE.
*      SELECT SINGLE pernr INTO <ls_iprreq>-pernr_creator FROM pa0105
*        WHERE
*              usrid = ls_request_input_data-entrance_user
*          AND subty EQ '9001'
*          AND begda <= sy-datum
*          AND endda >= sy-datum.
*
*

*21/03/2020
      IF ls_request_input_data-entrance_user IS NOT INITIAL.
        <ls_iprreq>-pernr_creator = lo_assistent->get_pernr( iv_usrid = CONV #( ls_request_input_data-entrance_user ) ).
      ELSE.
        <ls_iprreq>-pernr_creator = lo_assistent->get_pernr( iv_usrid = CONV #( ls_request_input_data-main_user ) ).
      ENDIF.
*      IF sy-subrc <> 0.
      IF <ls_iprreq>-pernr_creator IS INITIAL.
        er_entity-msg_id  = 'W'.
        er_entity-message = 'Табельный номер не найден'.
        EXIT.
      ENDIF.
      " /perner_creator

      IF ls_request_input_data-main_user IS INITIAL.
        <ls_iprreq>-uname =  ls_request_input_data-entrance_user.
      ELSE.
        <ls_iprreq>-uname =  ls_request_input_data-main_user.
      ENDIF.

      <ls_iprreq>-mandt = sy-mandt.
      <ls_iprreq>-status = '0'.
      <ls_iprreq>-date_create = sy-datum.
      <ls_iprreq>-time_create = sy-uzeit.
      <ls_iprreq>-data_change = <ls_iprreq>-date_create.
      <ls_iprreq>-time_change = <ls_iprreq>-time_create.
      <ls_iprreq>-edu_type = ls_request_input_data-edu_type.
      <ls_iprreq>-edu_spec = ls_request_input_data-edu_spec.
      <ls_iprreq>-edu_spec_another = ls_request_input_data-edu_spec_another.
      <ls_iprreq>-edu_frm = ls_request_input_data-edu_frm.
      <ls_iprreq>-edu_reas = ls_request_input_data-edu_goal.
      <ls_iprreq>-edu_length = CONV numc3( ls_request_input_data-edu_length ).
      <ls_iprreq>-edu_length_type = ls_request_input_data-edu_length_type.
      <ls_iprreq>-edu_year = lv_edu_year.
      <ls_iprreq>-edu_year_quarter = lv_edu_quarter.
      <ls_iprreq>-edu_prog = ls_request_input_data-edu_result.
      <ls_iprreq>-edu_result_another = ls_request_input_data-edu_result_another.
      <ls_iprreq>-edu_name           = ls_request_input_data-eduname.

      " Получаем оргединицу по ТН и дате
***    DATA: lv_orgeh TYPE orgeh.
***
***    CALL METHOD zess_0023_cl_hrutils=>get_orgeh
***      EXPORTING
****       im_o_objid              =
****       im_s_objid              =
***        _iv_pernr = <ls_iprreq>-pernr_creator
***        _iv_date  = sy-datum
****       im_plvar  = '01'
****       im_subty  = '0001'
****       im_filial_after         = ''
****       im_subty_filial         =
****       im_orgunit_up_struct    =
****       im_orgunit_up_str_subty = 'Z002'
***      IMPORTING
****       ex_filial_text          =
***        _ev_orgeh = lv_orgeh.
***
***
***    " Получить дерево оргструктуры по оргединице
***    DATA: lt_tmp_orgeh  TYPE TABLE OF objec,
***          lt_result_tab TYPE TABLE OF swhactor.
***    CALL FUNCTION 'RH_STRUC_GET'
***      EXPORTING
***        act_plvar      = '01'
***        act_otype      = 'O'
***        act_objid      = lv_orgeh
***        "act_wegid      = 'A002'
***        act_wegid      = 'O-O'
***      TABLES
***        "result_tab     = lt_tmp_orgeh
***        result_objec   = lt_tmp_orgeh
***      EXCEPTIONS
***        no_plvar_found = 1
***        no_entry_found = 2
***        OTHERS         = 3.
***    "

*      DATA:
*        lt_tmp_orgeh    TYPE TABLE OF objec,
*        lt_result_tab   TYPE TABLE OF swhactor,
*        lt_result_struc TYPE TABLE OF struc.
*
*      CALL FUNCTION 'RH_STRUC_GET'
*        EXPORTING
*          act_plvar      = '01'
*          act_otype      = 'P'
*          act_objid      = <ls_iprreq>-pernr_creator
*          act_wegid      = 'P-S-O-O'
*          "act_wegid      = 'O-O'
*        TABLES
*          "result_tab     = lt_result_tab
*          "result_objec   = lt_tmp_orgeh
*          result_struc   = lt_result_struc
*        EXCEPTIONS
*          no_plvar_found = 1
*          no_entry_found = 2
*          OTHERS         = 3.
*
*
*
*      "DATA(lv_tmp_objid) = lt_tmp_orgeh[ 2 ]-objid.
*      DATA(lv_tmp_objid) = lt_result_struc[ lines( lt_result_struc ) - 1 ]-objid.
*      CLEAR lt_tmp_orgeh.
*      CLEAR lt_result_tab.
*
*      CALL FUNCTION 'RH_STRUC_GET'
*        EXPORTING
*          act_plvar      = '01'
*          act_otype      = 'O'
*          act_objid      = lv_tmp_objid
*          act_wegid      = 'B012'
*          "act_wegid      = 'O-O'
*        TABLES
*          result_tab     = lt_result_tab
*          result_objec   = lt_tmp_orgeh
*        EXCEPTIONS
*          no_plvar_found = 1
*          no_entry_found = 2
*          OTHERS         = 3.
*
*
*      lv_tmp_objid = lt_result_tab[ 1 ]-objid.
*      CLEAR lt_tmp_orgeh.
*      CLEAR lt_result_tab.
*
*      CALL FUNCTION 'RH_STRUC_GET'
*        EXPORTING
*          act_plvar      = '01'
*          act_otype      = 'S'
*          act_objid      = lv_tmp_objid
*          "act_wegid      = 'A008'
*          act_wegid      = 'Z_OIVRUC'
*          act_begda      = sy-datum
*          act_endda      = sy-datum
*        TABLES
*          result_tab     = lt_result_tab
*          result_objec   = lt_tmp_orgeh
*        EXCEPTIONS
*          no_plvar_found = 1
*          no_entry_found = 2
*          OTHERS         = 3.
*
*      <ls_iprreq>-approver_number = lt_result_tab[ 1 ]-objid.

*<<<
*      DATA(lv_line_approver)  = lo_assistent->get_pernr_ruk0( iv_pernr = CONV #( <ls_iprreq>-pernr_creator ) ).
*      DATA(lv_learn_approver) = CONV pernr_d( `` ).
*      zcl_hr_get_data=>read_stvarv_par( EXPORTING i_name  = `ZZ_LEARN_APPROVER`
*                                        IMPORTING e_value = lv_learn_approver ).
*      IF lv_line_approver IS NOT INITIAL.
*        <ls_iprreq>-line_approver = 6. "Утверждение линейным руководителем
*      ELSE.
*        <ls_iprreq>-line_approver = lv_line_approver.
*      ENDIF.
*
*      IF lv_learn_approver IS NOT INITIAL.
*        <ls_iprreq>-line_approver = 8. "Утверждение линейным руководителем
*      ELSE.
*        <ls_iprreq>-line_approver = lv_line_approver.
*      ENDIF.

      lo_assistent->get_approver_number( EXPORTING iv_pernr    = CONV #( <ls_iprreq>-pernr_creator )
                                         IMPORTING ev_approver = <ls_iprreq>-approver_number
                                                   ev_status   = <ls_iprreq>-status ).
*>>>
      MODIFY zhr_xss_iprreq FROM TABLE lt_iprreq.

      lo_assistent->write_ipr_sta( iv_req_id = <ls_iprreq>-req_id
                                   iv_pernr  = CONV #( <ls_iprreq>-pernr_creator )
                                   iv_status = <ls_iprreq>-status ).

      er_entity = ls_request_input_data.
      er_entity-msg_id  = 'S'.
      er_entity-message = 'ИПР создан и отправлен на согласование'.
    ELSE.
      er_entity-msg_id  = 'W'.
      er_entity-message = 'Заполните все обязательные поля'.
    ENDIF.

  ENDMETHOD.


  METHOD iprreqs_delete_entity.
**TRY.
*CALL METHOD SUPER->IPRREQS_DELETE_ENTITY
*  EXPORTING
*    IV_ENTITY_NAME          =
*    IV_ENTITY_SET_NAME      =
*    IV_SOURCE_NAME          =
*    IT_KEY_TAB              =
**    io_tech_request_context =
*    IT_NAVIGATION_PATH      =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.

    DATA: lv_pernr TYPE pernr_d.

*21/03/2020
    READ TABLE it_key_tab ASSIGNING FIELD-SYMBOL(<ls_entrance_user>) WITH KEY name = 'EntranceUser'.
    IF sy-subrc <> 0.
      READ TABLE it_key_tab ASSIGNING <ls_entrance_user> WITH KEY name = 'MainUser'.
    ENDIF.

    READ TABLE it_key_tab ASSIGNING FIELD-SYMBOL(<ls_req_id>) WITH KEY name = 'ReqId'.

    " ТН создавшего заявку perner_creator
    DATA(lo_assistent) = zcl_mss_data_assistent=>get_instance( ).
    lv_pernr = lo_assistent->get_pernr( iv_usrid = CONV #( <ls_entrance_user>-value ) ).

*    SELECT SINGLE pernr INTO lv_pernr FROM pa0105
*      WHERE
*            usrid = <ls_entrance_user>-value
*        AND subty EQ '9001'
*        AND begda <= sy-datum
*        AND endda >= sy-datum.
*    CHECK sy-subrc = 0.
    CHECK lv_pernr IS NOT INITIAL.
    " /perner_creator

    DATA :
      lt_iprreq     TYPE TABLE OF zhr_xss_iprreq,
      ls_xss_iprreq TYPE  zhr_xss_iprreq.

    IF lv_pernr IS NOT INITIAL.
      SELECT * INTO ls_xss_iprreq FROM zhr_xss_iprreq
        WHERE
           pernr_creator = lv_pernr
        AND
           req_id = <ls_req_id>-value.
      ENDSELECT.

      CHECK sy-subrc = 0.
      ls_xss_iprreq-status = 1.
      ls_xss_iprreq-data_change = sy-datum.
      ls_xss_iprreq-time_change = sy-uzeit.
      ls_xss_iprreq-uname =  <ls_entrance_user>-value.

    ENDIF.
    APPEND ls_xss_iprreq TO  lt_iprreq.
    MODIFY zhr_xss_iprreq FROM TABLE  lt_iprreq.


  ENDMETHOD.


  METHOD iprreqs_get_entity.
**TRY.
*CALL METHOD SUPER->IPRREQS_GET_ENTITY
*  EXPORTING
*    IV_ENTITY_NAME          =
*    IV_ENTITY_SET_NAME      =
*    IV_SOURCE_NAME          =
*    IT_KEY_TAB              =
**    io_request_object       =
**    io_tech_request_context =
*    IT_NAVIGATION_PATH      =
**  IMPORTING
**    er_entity               =
**    es_response_context     =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.
    DATA:
      lt_iprreq         TYPE TABLE OF zhr_xss_iprreq,
      ls_iprreq         TYPE zhr_xss_iprreq,
      ls_select_options TYPE /iwbep/s_cod_select_option,
      lv_pernr          TYPE pernr_d,
      lv_entrance_user  TYPE string,
      ls_converted_keys LIKE er_entity,
      lt_dd07v          TYPE STANDARD TABLE OF dd07v.

    io_tech_request_context->get_converted_keys( IMPORTING es_key_values = ls_converted_keys ).
*21/03/2020
    lv_entrance_user = ls_converted_keys-entrance_user.
    IF lv_entrance_user IS INITIAL.
      lv_entrance_user = ls_converted_keys-main_user.
    ENDIF.

    SELECT * INTO TABLE lt_iprreq FROM zhr_xss_iprreq WHERE req_id = ls_converted_keys-req_id.

    IF sy-subrc = 0.
      LOOP AT  lt_iprreq INTO ls_iprreq.

        er_entity-req_id  = ls_iprreq-req_id.
        er_entity-edu_frm = ls_iprreq-edu_frm.
        er_entity-reqdate = ls_iprreq-date_create.

        SELECT SINGLE cname INTO er_entity-edu_frm_text
              FROM t7ruokin
              WHERE molga EQ 33 AND facet = 33 AND ccode = ls_iprreq-edu_frm.

        er_entity-edu_goal = ls_iprreq-edu_reas.

        SELECT SINGLE text INTO er_entity-edu_goal_text
           FROM zess_t_reas_txt
          WHERE reason_id = er_entity-edu_goal
            AND lang = sy-langu.

        er_entity-edu_length      = ls_iprreq-edu_length.
        er_entity-edu_length_type = ls_iprreq-edu_length_type.

        DATA(lv_unit_type) = '0' && er_entity-edu_length_type.
        SELECT SINGLE etext INTO er_entity-edu_length_type_text
              FROM t538t
              WHERE sprsl EQ 'RU' AND zeinh = lv_unit_type.


        er_entity-edu_year = ls_iprreq-edu_year.
        er_entity-edu_result = ls_iprreq-edu_prog.

*        SELECT SINGLE efct_text INTO er_entity-edu_result_text
*              FROM zess_0023_t_efct
*              WHERE efctid = er_entity-edu_result.

*        SELECT SINGLE name
*          FROM zthr_edu_prograt
*          INTO er_entity-edu_result_text
*         WHERE id = er_entity-edu_result
*           AND lang = sy-langu.

        SELECT SINGLE zt~name INTO er_entity-edu_result_text
          FROM zthr_edu_prograt AS zt
          INNER JOIN zthr_edu_program AS zm ON zm~id = zt~id
         WHERE zt~lang = sy-langu
           AND zt~id = er_entity-edu_result
           AND zm~is_old = ' '.

        er_entity-edu_result_another = ls_iprreq-edu_result_another.
        er_entity-edu_spec = ls_iprreq-edu_spec.

        SELECT SINGLE stext INTO er_entity-edu_spec_text
              FROM zthr_dlinet
              WHERE spras EQ 'RU' AND dline = er_entity-edu_spec.

        er_entity-edu_spec_another = ls_iprreq-edu_spec_another.
        er_entity-edu_type = ls_iprreq-edu_type.
        er_entity-eduname  = ls_iprreq-edu_name.

        SELECT SINGLE addedu_text INTO er_entity-edu_type_text
           FROM zess_0023_t_aedu
          WHERE addeduid = er_entity-edu_type.

        er_entity-msg_id = er_entity-msg_id = 'ALL_OK'.
        er_entity-message = 'All right message'.
        " next string only for debuging
        er_entity-entrance_user = lv_entrance_user.

        CALL FUNCTION 'DD_DOMVALUES_GET'
          EXPORTING
            domname        = 'ZHR_XSS_IPRREQ_STATUS'   "<-- Your Domain Here
            text           = 'X'
            langu          = sy-langu
          TABLES
            dd07v_tab      = lt_dd07v
          EXCEPTIONS
            wrong_textflag = 1
            OTHERS         = 2.

        er_entity-status = ls_iprreq-status.
        READ TABLE lt_dd07v ASSIGNING FIELD-SYMBOL(<ls_dd07v>) WITH KEY domvalue_l = ls_iprreq-status.
        IF sy-subrc = 0.
          er_entity-status_text = <ls_dd07v>-ddtext.
        ENDIF.

        er_entity-entrance_user = lv_entrance_user.

        CASE ls_iprreq-status.
          WHEN '3'.
            er_entity-status_text = 'Корректировка'.
            er_entity-correct = abap_true.
          WHEN OTHERS.
            er_entity-correct = abap_false.
        ENDCASE.

      ENDLOOP.
    ELSE.
      er_entity-msg_id = 'ALL_BAD'.
      er_entity-message = 'No entity found'.
    ENDIF.
  ENDMETHOD.


  METHOD iprreqs_get_entityset.
**TRY.
*CALL METHOD SUPER->IPRREQS_GET_ENTITYSET
*  EXPORTING
*    IV_ENTITY_NAME           =
*    IV_ENTITY_SET_NAME       =
*    IV_SOURCE_NAME           =
*    IT_FILTER_SELECT_OPTIONS =
*    IS_PAGING                =
*    IT_KEY_TAB               =
*    IT_NAVIGATION_PATH       =
*    IT_ORDER                 =
*    IV_FILTER_STRING         =
*    IV_SEARCH_STRING         =
**    io_tech_request_context  =
**  IMPORTING
**    et_entityset             =
**    es_response_context      =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.
    DATA:
      lt_iprreq         TYPE TABLE OF zhr_xss_iprreq,
      ls_select_options TYPE /iwbep/s_cod_select_option,
      lv_pernr          TYPE pernr_d,
      lv_main_user      TYPE string,
      lv_entrance_user  TYPE string.

    FIELD-SYMBOLS:
      <ls_iprreq>    LIKE LINE OF lt_iprreq,
      <ls_entityset> LIKE LINE OF et_entityset.

    LOOP AT it_filter_select_options ASSIGNING FIELD-SYMBOL(<fs_filter_select_options>).

      READ TABLE <fs_filter_select_options>-select_options
          ASSIGNING FIELD-SYMBOL(<fs_range>)
           INDEX 1.

      CHECK sy-subrc = 0.

      CASE <fs_filter_select_options>-property.
        WHEN 'EntranceUser'.                          "логин заместителя
          lv_entrance_user = <fs_range>-low.
        WHEN 'MainUser'.                              "логин текущего пользователя
          lv_main_user = <fs_range>-low.
      ENDCASE.
    ENDLOOP.

    IF lv_entrance_user IS INITIAL.
      DATA(lv_login) = lv_main_user.
    ELSE.
      lv_login = lv_entrance_user.
    ENDIF.

    " ТН создавшего заявку perner_creator
    DATA(lo_assistent) = zcl_mss_data_assistent=>get_instance( ).
    lv_pernr = lo_assistent->get_pernr( iv_usrid = CONV #( lv_login ) ).

    IF lv_pernr IS INITIAL.
      APPEND INITIAL LINE TO et_entityset ASSIGNING <ls_entityset>.
      <ls_entityset>-msg_id = 'ALL_BAD'.
      <ls_entityset>-message = 'No personele found'.
      EXIT.
    ENDIF.
    " /perner_creator

    DATA: lt_dd07v TYPE TABLE OF dd07v
        .
    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname        = 'ZHR_XSS_IPRREQ_STATUS'   "<-- Your Domain Here
        text           = 'X'
        langu          = sy-langu
      TABLES
        dd07v_tab      = lt_dd07v
      EXCEPTIONS
        wrong_textflag = 1
        OTHERS         = 2.

    SELECT * INTO TABLE lt_iprreq FROM zhr_xss_iprreq WHERE pernr_creator = lv_pernr. "#EC CI_NOFIELD

    IF sy-subrc = 0.
      LOOP AT  lt_iprreq ASSIGNING <ls_iprreq>.
        APPEND INITIAL LINE TO et_entityset ASSIGNING <ls_entityset>.
        <ls_entityset>-req_id  = <ls_iprreq>-req_id.
        <ls_entityset>-reqdate = <ls_iprreq>-date_create.

        <ls_entityset>-edu_frm = <ls_iprreq>-edu_frm.
        <ls_entityset>-status  = <ls_iprreq>-status.

        SELECT SINGLE cname INTO <ls_entityset>-edu_frm_text
              FROM t7ruokin
              WHERE molga EQ 33 AND facet = 33 AND ccode = <ls_iprreq>-edu_frm.


        <ls_entityset>-edu_goal = <ls_iprreq>-edu_reas.

*        SELECT SINGLE name INTO <ls_entityset>-edu_goal_text
*           FROM zthr_edu_prograt
*          WHERE id = <ls_entityset>-edu_goal
*            AND lang = sy-langu.

        SELECT SINGLE zt~name INTO <ls_entityset>-edu_goal_text
          FROM zthr_edu_prograt AS zt
          INNER JOIN zthr_edu_program AS zm ON zm~id = zt~id
         WHERE zt~lang = sy-langu
           AND zt~id = <ls_entityset>-edu_goal
           AND zm~is_old = ' '.

        SHIFT <ls_iprreq>-edu_length LEFT DELETING LEADING '0'.

        <ls_entityset>-edu_length = <ls_iprreq>-edu_length.
        <ls_entityset>-edu_length_type = <ls_iprreq>-edu_length_type.

        DATA(lv_unit_type) = '00' && <ls_entityset>-edu_length_type.
        SELECT SINGLE etext INTO <ls_entityset>-edu_length_type_text
              FROM t538t
              WHERE sprsl EQ 'RU' AND zeinh = lv_unit_type.

        <ls_entityset>-edu_length_type_text = '(' && <ls_entityset>-edu_length_type_text && ')'.

        <ls_entityset>-edu_year = <ls_iprreq>-edu_year.
        <ls_entityset>-edu_result = <ls_iprreq>-edu_prog.

        SELECT SINGLE text INTO <ls_entityset>-edu_result_text
              FROM zess_t_reas_txt
              WHERE reason_id = <ls_entityset>-edu_result
                AND lang = sy-langu.

        <ls_entityset>-edu_result_another = <ls_iprreq>-edu_result_another.
        <ls_entityset>-edu_spec = <ls_iprreq>-edu_spec.

        SELECT SINGLE stext INTO <ls_entityset>-edu_spec_text
              FROM zthr_dlinet
              WHERE spras EQ 'RU' AND dline = <ls_entityset>-edu_spec.

        <ls_entityset>-edu_spec_another = <ls_iprreq>-edu_spec_another.
        <ls_entityset>-edu_type = <ls_iprreq>-edu_type.

        SELECT SINGLE addedu_text INTO <ls_entityset>-edu_type_text
           FROM zess_0023_t_aedu
          WHERE addeduid = <ls_entityset>-edu_type.

        <ls_entityset>-msg_id = <ls_entityset>-msg_id = 'ALL_OK'.
        <ls_entityset>-message = 'All right message'.
        " next string only for debuging
        <ls_entityset>-entrance_user = lv_entrance_user.

        DATA(lv_status) = CONV dd07v-domvalue_l( <ls_iprreq>-status ). CONDENSE lv_status.
        READ TABLE lt_dd07v ASSIGNING FIELD-SYMBOL(<ls_dd07v>) WITH KEY domvalue_l = lv_status.
        IF sy-subrc = 0.
          <ls_entityset>-status_text = <ls_dd07v>-ddtext.
        ENDIF.

        CASE <ls_iprreq>-status.
*          WHEN '0'.
*            <ls_entityset>-status_text = 'Отправлено'.
*          WHEN '1'.
*            <ls_entityset>-status_text = 'Отменено'.
*          WHEN '2'.
*            <ls_entityset>-status_text = 'Отклонено'.
          WHEN '3'.
*            <ls_entityset>-status_text = 'Корректировка'.
            <ls_entityset>-correct = 'true'.
*          WHEN '4'.
*            <ls_entityset>-status_text = 'Согласовано'.
*          WHEN OTHERS.
*            <ls_entityset>-status_text = 'Статус неопределён'.
*            <ls_entityset>-msg_id = 'E'.
*            <ls_entityset>-message = 'Статус неопределён'.
        ENDCASE.

        <ls_entityset>-eduname = <ls_iprreq>-edu_name.

      ENDLOOP.
    ELSE.
*      APPEND INITIAL LINE TO et_entityset ASSIGNING <ls_entityset>.
*      <ls_entityset>-msg_id = 'ALL_BAD'.
*      <ls_entityset>-message = 'No entity found'.
    ENDIF.
  ENDMETHOD.


  METHOD iprreqs_update_entity.
**TRY.
*CALL METHOD SUPER->IPRREQS_UPDATE_ENTITY
*  EXPORTING
*    IV_ENTITY_NAME          =
*    IV_ENTITY_SET_NAME      =
*    IV_SOURCE_NAME          =
*    IT_KEY_TAB              =
**    io_tech_request_context =
*    IT_NAVIGATION_PATH      =
**    io_data_provider        =
**  IMPORTING
**    er_entity               =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.
    DATA:
*      ls_request_input_data TYPE zess_0023_t_all,
      ls_request_input_data LIKE er_entity,
      lv_pernr              TYPE pernr_d.

    io_data_provider->read_entry_data( IMPORTING es_data = ls_request_input_data ).


*    READ TABLE it_key_tab ASSIGNING FIELD-SYMBOL(<ls_entrance_user>) WITH KEY name = 'EntranceUser'.
*    READ TABLE it_key_tab ASSIGNING FIELD-SYMBOL(<ls_req_id>) WITH KEY name = 'ReqId'.

    " ТН создавшего заявку perner_creator
*    translate ls_request_input_data-entrance_user to upper case.
*    SELECT SINGLE pernr INTO lv_pernr FROM pa0105
*      WHERE
*            usrid = ls_request_input_data-entrance_user
*        AND subty EQ '9001'
*        AND begda <= sy-datum
*        AND endda >= sy-datum.
    DATA(lo_assistent) = zcl_mss_data_assistent=>get_instance( ).
*21/03/2020
    IF ls_request_input_data-entrance_user IS NOT INITIAL.
      lv_pernr = lo_assistent->get_pernr( iv_usrid = CONV #( ls_request_input_data-entrance_user ) ).
    ELSE.
      lv_pernr = lo_assistent->get_pernr( iv_usrid = CONV #( ls_request_input_data-main_user ) ).
    ENDIF.

    CHECK lv_pernr IS NOT INITIAL.
    " /perner_creator

    DATA :
      lt_iprreq     TYPE TABLE OF zhr_xss_iprreq,
      ls_xss_iprreq TYPE  zhr_xss_iprreq.

    CHECK lv_pernr IS NOT INITIAL.
    " IF lv_pernr IS NOT INITIAL.
    SELECT * INTO ls_xss_iprreq FROM zhr_xss_iprreq
      WHERE
*         pernr_creator = lv_pernr
*      AND
         req_id = ls_request_input_data-req_id.
    ENDSELECT.

    CHECK sy-subrc = 0.

    lo_assistent->get_approver_number( EXPORTING iv_pernr      = CONV #( ls_xss_iprreq-pernr_creator )
                                                 iv_status     = CONV #( ls_request_input_data-status )
                                                 iv_status_old = CONV #( ls_xss_iprreq-status )
                                       IMPORTING ev_approver   = ls_xss_iprreq-approver_number
                                                 ev_status     = ls_xss_iprreq-status
                                                 ev_status_log = DATA(lv_status_log) ).

    lo_assistent->write_ipr_sta( iv_req_id = ls_xss_iprreq-req_id
                                 iv_pernr  = CONV #( lv_pernr )
                                 iv_status = lv_status_log ).

    ls_xss_iprreq-data_change = sy-datum.
    ls_xss_iprreq-time_change = sy-uzeit.
    ls_xss_iprreq-uname       = ls_request_input_data-main_user.

    APPEND ls_xss_iprreq TO  lt_iprreq.
    MODIFY zhr_xss_iprreq FROM TABLE  lt_iprreq.
    "ENDIF.

    er_entity-msg_id = 'ALL_OK'.
    er_entity-message = 'Status changed'.

  ENDMETHOD.


  METHOD iprstatusset_create_entity.
**TRY.
*CALL METHOD SUPER->IPRSTATUSSET_CREATE_ENTITY
*  EXPORTING
*    IV_ENTITY_NAME          =
*    IV_ENTITY_SET_NAME      =
*    IV_SOURCE_NAME          =
*    IT_KEY_TAB              =
**    io_tech_request_context =
*    IT_NAVIGATION_PATH      =
**    io_data_provider        =
**  IMPORTING
**    er_entity               =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.

    DATA:
*      ls_request_input_data TYPE zess_0023_t_all,
      ls_request_input_data LIKE er_entity,
      lv_pernr              TYPE pernr_d.

    io_data_provider->read_entry_data( IMPORTING es_data = ls_request_input_data ).

    DATA(lo_assistent) = zcl_mss_data_assistent=>get_instance( ).


*    READ TABLE it_key_tab ASSIGNING FIELD-SYMBOL(<ls_entrance_user>) WITH KEY name = 'EntranceUser'.
*    READ TABLE it_key_tab ASSIGNING FIELD-SYMBOL(<ls_req_id>) WITH KEY name = 'ReqId'.

    " ТН создавшего заявку perner_creator
*    SELECT SINGLE pernr INTO lv_pernr FROM pa0105
*      WHERE
*            usrid = ls_request_input_data-entrance_user
*        AND subty EQ '9001'
*        AND begda <= sy-datum
*        AND endda >= sy-datum.
*    CHECK sy-subrc = 0.
*21/03/2020
    IF ls_request_input_data-entrance_user IS NOT   INITIAL.
      lv_pernr = lo_assistent->get_pernr( iv_usrid = CONV #( ls_request_input_data-entrance_user ) ).
    ELSE.
      lv_pernr = lo_assistent->get_pernr( iv_usrid = CONV #( ls_request_input_data-main_user ) ).
    ENDIF.

    CHECK lv_pernr IS NOT INITIAL.
    " /perner_creator

    DATA :
      lt_iprreq     TYPE TABLE OF zhr_xss_iprreq,
      ls_xss_iprreq TYPE  zhr_xss_iprreq.

    CHECK lv_pernr IS NOT INITIAL.
    " IF lv_pernr IS NOT INITIAL.
    SELECT * INTO ls_xss_iprreq FROM zhr_xss_iprreq
      WHERE
*         pernr_creator = lv_pernr
*      AND
         req_id = ls_request_input_data-req_id.
    ENDSELECT.

    IF sy-subrc <> 0.
      er_entity-msg_id = 'E'.
      er_entity-message = 'ИПР не найден'.
      EXIT.
    ENDIF.

    lo_assistent->get_approver_number( EXPORTING iv_pernr      = CONV #( ls_xss_iprreq-pernr_creator )
                                                 iv_status     = CONV #( ls_request_input_data-status )
                                                 iv_status_old = CONV #( ls_xss_iprreq-status )
                                       IMPORTING ev_approver   = ls_xss_iprreq-approver_number
                                                 ev_status     = ls_xss_iprreq-status
                                                 ev_status_log = DATA(lv_status_log) ).

    lo_assistent->write_ipr_sta( iv_req_id = ls_xss_iprreq-req_id
                                 iv_pernr  = CONV #( lv_pernr )
                                 iv_status = lv_status_log ).

*    ls_xss_iprreq-status = ls_request_input_data-status.
    ls_xss_iprreq-data_change = sy-datum.
    ls_xss_iprreq-time_change = sy-uzeit.
    ls_xss_iprreq-uname       = ls_request_input_data-main_user.


    APPEND ls_xss_iprreq TO  lt_iprreq.
    MODIFY zhr_xss_iprreq FROM TABLE  lt_iprreq.
    "ENDIF.

    er_entity-msg_id = 'S'.
    er_entity-message = 'Статус изменен'.
  ENDMETHOD.
ENDCLASS.

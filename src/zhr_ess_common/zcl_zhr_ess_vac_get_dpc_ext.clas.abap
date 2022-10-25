class ZCL_ZHR_ESS_VAC_GET_DPC_EXT definition
  public
  inheriting from ZCL_ZHR_ESS_VAC_GET_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_STREAM
    redefinition .
protected section.

  methods VACAPPLICATIONSE_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZHR_ESS_VAC_GET_DPC_EXT IMPLEMENTATION.


  METHOD /iwbep/if_mgw_appl_srv_runtime~get_stream.
    TYPES: BEGIN OF ty_guber,
             objid     TYPE hrobjid,
             pernr     TYPE pernr_d,
             fio       TYPE string,
             plans_txt TYPE string,
           END OF ty_guber.
    DATA: lt_objid_guber TYPE TABLE OF ty_guber.
    DATA lv_template_name TYPE w3objid.
*Skorobagatov A.N. IBS 10.12.2019 17:54:41
    DATA:
      lv_pernr   TYPE pernr_d,
      lv_begda   TYPE begda,
      lv_text1   TYPE text20,
      lv_komitet TYPE orgeh.
*Skorobagatov A.N. IBS 10.12.2019 17:55:19
    DATA: lv_text_kom   TYPE string,
          lv_orgeh_full TYPE string,
          lv_name_plans TYPE string.

    DATA: ls_stream  TYPE ty_s_media_resource
          , ls_lheader    TYPE ihttpnvp.
    DATA:
   lv_request_id        TYPE tim_req_id.
    DATA:  t_data_xml TYPE xstring.
    TYPES:
      BEGIN OF ty_main
            , fio_inits    TYPE text255
            , plans_init   TYPE text255
            , gub_plans    TYPE text250
            , gub_name     TYPE text250
            , dayfrom      TYPE text255
            , monthtxtfrom TYPE text255
            , yearfrom     TYPE text255
            , dayto        TYPE text255
            , monthtxtto   TYPE text255
            , yearto       TYPE text255
            , plans_head   TYPE text250
            , fio_head     TYPE text250
            , vac_days     TYPE text255
            , fio     TYPE text255
            , pernr   TYPE text255
            , abstext TYPE text255
            , begda   TYPE text255
            , endda   TYPE text255
            , comment TYPE text255
            , nexthandlerfio TYPE text255
            , pos      TYPE text255
         , END OF  ty_main.
    DATA: ls_main TYPE ty_main.


    DATA: BEGIN  OF ls_input
        , requestid     TYPE string
        , entranceuser  TYPE string
        , mainuser      TYPE string
        , END OF ls_input.


    LOOP AT it_key_tab ASSIGNING FIELD-SYMBOL(<fs_key>).
      CASE <fs_key>-name.
        WHEN 'ENTRANCEUSER'.
          ls_input-entranceuser = <fs_key>-value.
        WHEN 'MAINUSER'.
          ls_input-mainuser = <fs_key>-value.
        WHEN 'REQUESTID'.
          ls_input-requestid = <fs_key>-value.
          lv_request_id = ls_input-requestid.
      ENDCASE.
    ENDLOOP.

    IF  ls_input-entranceuser IS INITIAL.
      ls_input-entranceuser =  ls_input-mainuser.
    ENDIF.
*   читаем инфу по заявке
    IF lv_request_id IS NOT INITIAL.
      DATA(lo_assistent) = zcl_mss_data_assistent=>get_instance( ).
      zcl_vacation_appl=>get_attabs(
    EXPORTING
      i_req_id = lv_request_id    " Ид. документа
    IMPORTING
      es_data  = DATA(ls_attabs) ).

      CHECK ls_attabs IS NOT INITIAL.
      ls_main-abstext = zcl_hr_data=>get_awart_txt( i_awart = ls_attabs-subty ).
      lv_begda = ls_main-begda =  ls_attabs-begda.
      ls_main-endda = ls_attabs-endda.
      lv_pernr = ls_main-pernr = ls_attabs-pernr.
      ls_main-fio = ls_main-fio_inits = lo_assistent->get_pernr_fio( iv_pernr = ls_attabs-pernr iv_begda = sy-datum ).

      CALL METHOD zcl_calendar=>get_date_dd_month
        EXPORTING
          i_date = ls_attabs-begda
        IMPORTING
          e_date = lv_text1.

      SPLIT lv_text1 AT space INTO ls_main-dayfrom ls_main-monthtxtfrom.
      CONDENSE: ls_main-dayfrom, ls_main-monthtxtfrom .

      ls_main-yearfrom = ls_attabs-begda(4).

      CALL METHOD zcl_calendar=>get_date_dd_month
        EXPORTING
          i_date = ls_attabs-endda
        IMPORTING
          e_date = lv_text1.

      SPLIT lv_text1 AT space INTO ls_main-dayto ls_main-monthtxtto.
      ls_main-yearto = ls_attabs-endda(4).

      DATA: lt_pa0001 TYPE TABLE OF p0001.
      zcl_hr_data=>read_pa_infty( EXPORTING i_pernr = lv_pernr
                                            i_infty = '0001'
                                            i_begda = lv_begda
                                            i_endda = lv_begda
                                  IMPORTING e_pnnnn = lt_pa0001 ).
      IF lines( lt_pa0001 ) > 0.
        SORT lt_pa0001 BY begda DESCENDING.
        READ TABLE lt_pa0001 ASSIGNING FIELD-SYMBOL(<lt_pa0001>) INDEX 1.
        IF <lt_pa0001> IS ASSIGNED.
*          >>>Шаталов Б.А. 10.09.2021 Вывод полной структуры ШД
          CLEAR: lv_text_kom, lv_orgeh_full, lv_name_plans.
          CALL METHOD zcl_hr_data=>get_komitet
            EXPORTING
              i_orgeh = <lt_pa0001>-orgeh
              i_date  = <lt_pa0001>-begda
            IMPORTING
              e_lname = lv_text_kom.

          CALL METHOD zcl_hr_data=>get_name_orgeh_full
            EXPORTING
              i_pernr = <lt_pa0001>-pernr
              i_date  = <lt_pa0001>-begda
            IMPORTING
              e_lname = lv_orgeh_full.

          CALL METHOD zcl_hr_data=>get_name_plans
            EXPORTING
              i_plans = <lt_pa0001>-plans
              i_date  = <lt_pa0001>-begda
            IMPORTING
              e_lname = lv_name_plans.

          CONCATENATE lv_name_plans lv_orgeh_full lv_text_kom INTO DATA(lv_fulltxt) SEPARATED BY space.
          ls_main-plans_init = lv_fulltxt.
        ENDIF.
      ENDIF.

*          >>>Шаталов Б.А. Получаем Имя и должность Губернатора и Первого Вице
      SELECT objid FROM hrp1001
       WHERE plvar = '01'
        AND rsign = 'B'
        AND relat = 'ZSP'
        INTO CORRESPONDING FIELDS OF TABLE @lt_objid_guber.
      IF sy-subrc = 0.
        SORT lt_objid_guber BY objid.
        DELETE ADJACENT DUPLICATES FROM lt_objid_guber COMPARING objid.
        LOOP AT lt_objid_guber ASSIGNING FIELD-SYMBOL(<ls_objid_guber>).
          CALL METHOD zcl_hr_data=>get_name_plans
            EXPORTING
              i_plans = CONV plans( <ls_objid_guber>-objid )
              i_date  = lv_begda
            IMPORTING
              e_lname = <ls_objid_guber>-plans_txt.

          CALL METHOD zcl_hr_data=>get_pernr_for_plan
            EXPORTING
              iv_date  = lv_begda
              iv_plans = CONV plans( <ls_objid_guber>-objid )
            RECEIVING
              rv_pernr = <ls_objid_guber>-pernr.

          CALL METHOD zcl_hr_data=>get_fio
            EXPORTING
              i_pernr   = <ls_objid_guber>-pernr
              i_date    = lv_begda
            IMPORTING
              e_fio_end = <ls_objid_guber>-fio.
        ENDLOOP.
      ENDIF.
      DELETE lt_objid_guber WHERE objid IS INITIAL OR
                                  pernr IS INITIAL OR
                                  fio IS INITIAL OR
                                  plans_txt IS INITIAL.
      DELETE lt_objid_guber WHERE NOT plans_txt CS 'убер'.
      LOOP AT lt_objid_guber INTO DATA(ls_vice_data) WHERE ( plans_txt CS 'Первый' AND plans_txt CS 'вице' )
                                                        OR ( plans_txt CS 'Первый' AND plans_txt CS 'Вице' ).
        EXIT.
      ENDLOOP.
      LOOP AT lt_objid_guber INTO DATA(ls_guber_data) WHERE plans_txt CS 'Губер'.
        EXIT.
      ENDLOOP.
*          <<<Шаталов Б.А. 10.09.2021 Вывод полной структуры ШД

      " Руководитель
      CALL METHOD zcl_hr_data=>get_komitet
        EXPORTING
          i_pernr   = lv_pernr
          i_date    = lv_begda
        IMPORTING
          e_komitet = lv_komitet.

      IF lv_komitet IS NOT INITIAL.

        CALL METHOD zcl_hr_data=>get_komitet_lider
          EXPORTING
            i_komitet    = lv_komitet
            i_date       = lv_begda
          IMPORTING
            e_lider_name = ls_main-fio_head
            e_plans_name = ls_main-plans_head.
      ENDIF.

      " Количество дней

      ls_main-vac_days = ls_attabs-kaltg.


      "Следующий исполнитель

      DATA: lcl_request TYPE REF TO if_pt_req_request.



*-- Get the Persistent Leave Object

      CALL METHOD cl_pt_req_badi=>get_request
        EXPORTING
          im_req_id  = lv_request_id
        IMPORTING
          ex_request = lcl_request.
      CALL METHOD lcl_request->get_next_processor
        RECEIVING
          re_next_proc = DATA(ls_next_proc).

      ls_main-nexthandlerfio  = ls_next_proc-name.

      CALL METHOD lcl_request->get_notice
        IMPORTING
          ex_admin_curr_notice = DATA(adm_notice)
          ex_curr_notice       = DATA(curr_notice)
          ex_past_notice       = DATA(past_notice).
      ls_main-comment = curr_notice.

    ENDIF.
*{ KLOKOVNYU 15.05.2019 Губернатор, не губернатор
    DATA: lv_kodshd(3) TYPE c,
          lv_par1      TYPE c,
          lv_par2      TYPE c,
          lv_par3      TYPE c.

    IF <lt_pa0001> IS ASSIGNED.

      CALL METHOD zcl_hr_data=>get_plans_param
        EXPORTING
          i_plans  = <lt_pa0001>-plans
          i_date   = <lt_pa0001>-begda
        IMPORTING
          e_param1 = lv_par1
          e_param2 = lv_par2
          e_param3 = lv_par3.
    ENDIF.

    CONCATENATE lv_par1 lv_par2 lv_par3 INTO lv_kodshd.
    CLEAR lv_template_name.
    CASE ls_attabs-subty.
      WHEN '0109'.
        IF lv_kodshd = '2P1' OR lv_kodshd = '2P2'.
          ls_main-gub_plans = ls_guber_data-plans_txt.
          ls_main-gub_name =  ls_guber_data-fio.
        ELSE.
          ls_main-gub_plans = ls_vice_data-plans_txt.
          ls_main-gub_name =  ls_vice_data-fio.
        ENDIF.
        lv_template_name = 'ZHR_ESS_HOL_STUDY_109'.
      WHEN '0400'.
        IF lv_kodshd = '2P1' OR lv_kodshd = '2P2'.
          ls_main-gub_plans = ls_guber_data-plans_txt.
          ls_main-gub_name =  ls_guber_data-fio.
        ELSE.
          ls_main-gub_plans = ls_vice_data-plans_txt.
          ls_main-gub_name =  ls_vice_data-fio.
        ENDIF.
        lv_template_name = 'ZHR_ESS_HOL_NOPAY_400'.
      WHEN '0502'.
        IF lv_kodshd = '2P1' OR lv_kodshd = '2P2'.
          ls_main-gub_plans = ls_guber_data-plans_txt.
          ls_main-gub_name =  ls_guber_data-fio.
        ELSE.
          ls_main-gub_plans = ls_vice_data-plans_txt.
          ls_main-gub_name =  ls_vice_data-fio.
        ENDIF.
        lv_template_name = 'ZHR_ESS_HOL_CHILD_502'.
      WHEN '0301'.
        IF lv_kodshd = '2P1' OR lv_kodshd = '2P2'.
          ls_main-gub_plans = ls_guber_data-plans_txt.
          ls_main-gub_name =  ls_guber_data-fio.
        ELSE.
          ls_main-gub_plans = ls_vice_data-plans_txt.
          ls_main-gub_name =  ls_vice_data-fio.
        ENDIF.
        lv_template_name = 'ZHR_ESS_DIS_CHILD_301'.
      WHEN '0202'.
        IF lv_kodshd = '2P1' OR lv_kodshd = '2P2'.
          ls_main-gub_plans = ls_guber_data-plans_txt.
          ls_main-gub_name =  ls_guber_data-fio.
        ELSE.
          ls_main-gub_plans = ls_vice_data-plans_txt.
          ls_main-gub_name =  ls_vice_data-fio.
        ENDIF.
        lv_template_name = 'ZHR_ESS_PREGNANT_202'.
      WHEN '0304'.
        IF lv_kodshd = '2P1' OR lv_kodshd = '2P2'.
          ls_main-gub_plans = ls_guber_data-plans_txt.
          ls_main-gub_name =  ls_guber_data-fio.
        ELSE.
          ls_main-gub_plans = ls_vice_data-plans_txt.
          ls_main-gub_name =  ls_vice_data-fio.
        ENDIF.
        lv_template_name = 'ZHR_ESS_DONOR_DAY_OFF_G_304'.
      WHEN OTHERS.
        IF lv_kodshd = '2P1' OR lv_kodshd = '2P2'.
          ls_main-gub_plans = ls_guber_data-plans_txt.
          ls_main-gub_name =  ls_guber_data-fio.
*          CALL TRANSFORMATION zhr_ess_vac_get_np_g
*             SOURCE main = ls_main
*             RESULT XML t_data_xml.
        ELSE.
          ls_main-gub_plans = ls_vice_data-plans_txt.
          ls_main-gub_name =  ls_vice_data-fio.
*          CALL TRANSFORMATION zhr_ess_vac_get_np
*             SOURCE main = ls_main
*             RESULT XML t_data_xml.
        ENDIF.
        lv_template_name = 'ZHR_ESS_HOL_NOPAY_400'.
    ENDCASE.

    DATA: lt_mime TYPE TABLE OF w3mime.
    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING
        key    = VALUE wwwdatatab( relid = 'MI' objid = lv_template_name )
      TABLES
        mime   = lt_mime
      EXCEPTIONS
        OTHERS = 0.
    LOOP AT lt_mime ASSIGNING FIELD-SYMBOL(<ls_mime>).
      CONCATENATE t_data_xml <ls_mime>-line INTO t_data_xml IN BYTE MODE.
    ENDLOOP.

    TRY.
        DATA(lo_docx) = cl_docx_document=>load_document( iv_data = t_data_xml ).
        DATA(lo_documentpart) = lo_docx->get_maindocumentpart( ).
      CATCH cx_openxml_format cx_openxml_not_found.
    ENDTRY.
    DATA(lv_doctext) = cl_openxml_helper=>xstring_to_string( lo_documentpart->get_data( ) ).

    DATA(lt_comp) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( p_data = ls_main ) )->get_components( ).
    DATA: lo_str TYPE REF TO cl_abap_structdescr.
    LOOP AT lt_comp ASSIGNING FIELD-SYMBOL(<fs_comp>) WHERE as_include = abap_true.
      lo_str ?= <fs_comp>-type.
      DATA(lt_comps) = lo_str->components.
      LOOP AT lt_comps INTO DATA(ls_comps).
        APPEND INITIAL LINE TO lt_comp ASSIGNING FIELD-SYMBOL(<ls_comp>).
        <ls_comp>-name = ls_comps-name.
      ENDLOOP.
    ENDLOOP.
    DELETE lt_comp WHERE name IS INITIAL.

    LOOP AT lt_comp ASSIGNING <ls_comp>.
      ASSIGN COMPONENT <ls_comp>-name OF STRUCTURE ls_main TO FIELD-SYMBOL(<lv_fld>).
      CHECK sy-subrc = 0.
      lv_doctext = replace( val = lv_doctext sub = |{ <ls_comp>-name CASE = UPPER }| with = <lv_fld> occ = 0 ).
    ENDLOOP.
    lo_documentpart->feed_data( iv_data = cl_openxml_helper=>string_to_xstring( lv_doctext ) ).

    TRY.
        t_data_xml = lo_docx->get_package_data( ).
      CATCH cx_openxml_format.
        RETURN.
    ENDTRY.

    ls_stream-mime_type = 'application/msword'.
    ls_stream-value = t_data_xml.
*    t_data_xml = cl_http_utility=>if_http_utility~escape_url( t_data_xml ).
    ls_lheader-name = 'Content-Disposition'.
    ls_lheader-value = 'inline; filename="' && 'zayavka.docx' && '";'.
    set_header( is_header = ls_lheader ).


    copy_data_to_ref( EXPORTING is_data = ls_stream
                     CHANGING  cr_data = er_stream ).
  ENDMETHOD.


  METHOD vacapplicationse_get_entityset.
*
*    DATA:
*      lv_request_id        TYPE tim_req_id,
*      lv_name              TYPE string,
*      lt_return            TYPE TABLE OF bapiret2,
*      ls_return            LIKE LINE OF lt_return,
*      lo_message_container TYPE REF TO /iwbep/if_message_container.
*
*    FIELD-SYMBOLS:
*                   <fs_key> LIKE LINE OF it_key_tab.
*
*    DATA: BEGIN OF ls_input
*              , requestid     TYPE string
*              , entranceuser     TYPE string
*              , END OF ls_input
*
*              , lr_req TYPE RANGE OF string
*              , ls_request_id TYPE string.
*    lo_message_container = mo_context->get_message_container( ).
*
*
*
*    DATA(lo_filter) = io_tech_request_context->get_filter( ).
*
*    LOOP AT it_filter_select_options ASSIGNING FIELD-SYMBOL(<it_filter>).
*      REFRESH lr_req.
*      DATA(lv_property) = <it_filter>-property.
*      TRANSLATE lv_property TO UPPER CASE.
*      ASSIGN COMPONENT <it_filter>-property OF STRUCTURE ls_input TO FIELD-SYMBOL(<fs_input>).
*      CHECK <fs_input> IS ASSIGNED AND sy-subrc = 0.
*
*      lo_filter->convert_select_option( EXPORTING is_select_option = <it_filter>
*                                        IMPORTING et_select_option = lr_req ).
*      READ TABLE lr_req  INTO DATA(ls_user)   INDEX 1.
*      <fs_input> = ls_user-low.
*    ENDLOOP.
*
*    lv_request_id = ls_input-requestid.
*    DATA(lo_assistent) = zcl_mss_data_assistent=>get_instance( ).
*
*    lo_assistent->ws_get_compmaindata( EXPORTING  iv_euser         = ls_input-entranceuser
*                                       IMPORTING   et_compmaindata = DATA(lt_compmaindata)
*                                       EXCEPTIONS no_pernr         = 1
*                                                  no_sec_lev_ruk   = 2 ).
*
*    APPEND INITIAL LINE TO lt_return ASSIGNING FIELD-SYMBOL(<lt_return>).
*    APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<et_entityset0>).
*    CASE sy-subrc.
*      WHEN 1.
*        MESSAGE e002(zhr_mss) WITH ls_input-entranceuser INTO <lt_return>-message.
*        <lt_return>-type   = sy-msgty.
*        <lt_return>-id     = sy-msgid.
*        <lt_return>-number = sy-msgno.
**            <et_entityset0>-msgid = 'E'.
**
**            MESSAGE e015(zhr_mss) WITH ls_input-entranceuser INTO <et_entityset0>-message.
**            <et_entityset0>-message = 'Не удалось определить таб номер для пользователя'.
*      WHEN 2.
*        DATA(lv_name1) = lo_assistent->get_user_fio( iv_usrid = ls_input-entranceuser ).
*        MESSAGE e001(zhr_mss) WITH lv_name1 INTO <lt_return>-message.
*        <lt_return>-type   = sy-msgty.
*        <lt_return>-id     = sy-msgid.
*        <lt_return>-number = sy-msgno.
**            <et_entityset0>-msgid = 'E'.
**            MESSAGE e017(zhr_mss) WITH lv_name INTO <et_entityset0>-message.
**            <et_entityset0>-message = 'Для пользователя нет данных для просмотра'.
*      WHEN OTHERS.
*        REFRESH lt_return.
*        REFRESH et_entityset.
*    ENDCASE.
*    .
*    IF NOT lt_return IS INITIAL.
*      lo_message_container = mo_context->get_message_container( ).
*      lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).
*
*      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*        EXPORTING
*          message_container = lo_message_container.
*    ENDIF.
*
*
*
**   обработка входящих параметров
**    LOOP AT it_key_tab ASSIGNING <fs_key>.
**
**      lv_name = <fs_key>-name.
**      TRANSLATE lv_name TO UPPER CASE.
**
**      CASE lv_name.
**
**        WHEN 'REQUESTID'.
**          lv_request_id = <fs_key>-value.
**
**
**        WHEN OTHERS.
**          CLEAR ls_return.
**          ls_return-type = 'E'.
**          ls_return-id   = 'ZHR_PA'.
**          ls_return-number = '014'.
**          APPEND ls_return TO lt_return.
**
**          lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).
**
**          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
**            EXPORTING
**              message_container = lo_message_container.
**      ENDCASE.
**    ENDLOOP.
*
**   читаем инфу по заявке
*    IF lv_request_id IS NOT INITIAL.
*
*      zcl_vacation_appl=>get_attabs(
*    EXPORTING
*      i_req_id = lv_request_id    " Ид. документа
*    IMPORTING
*      es_data  = DATA(ls_attabs) ).
*
*      CHECK ls_attabs IS NOT INITIAL.
*      APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<fs_entity>).
*      <fs_entity>-abstext = zcl_hr_data=>get_awart_txt( i_awart = ls_attabs-subty ).
*      <fs_entity>-begda =  ls_attabs-begda.
*      <fs_entity>-endda = ls_attabs-endda.
*      <fs_entity>-pernr = ls_attabs-pernr.
*      <fs_entity>-requestid = lv_request_id.
**      DATA(lo_assistent) = zcl_mss_data_assistent=>get_instance( ).
*      <fs_entity>-fio = lo_assistent->get_pernr_fio( iv_pernr = ls_attabs-pernr iv_begda = sy-datum ).
*
*      "Следующий исполнитель
*
*      DATA: lcl_request TYPE REF TO if_pt_req_request.
*
*
*
**-- Get the Persistent Leave Object
*
*      CALL METHOD cl_pt_req_badi=>get_request
*        EXPORTING
*          im_req_id  = lv_request_id
*        IMPORTING
*          ex_request = lcl_request.
*      CALL METHOD lcl_request->get_next_processor
*        RECEIVING
*          re_next_proc = DATA(ls_next_proc).
*
*      <fs_entity>-nexthandlerfio  = ls_next_proc-name.
*
*CALL METHOD lcl_request->get_notice
*  IMPORTING
*    ex_admin_curr_notice = data(adm_notice)
*    ex_curr_notice       = data(curr_notice)
*    ex_past_notice       = data(past_notice)
*    .
*<fs_entity>-comment = curr_notice.
**      SELECT single *
**      INTO  @data(ls_ptreq_header)
**      FROM ptreq_header
**      WHERE
**        request_id  EQ @lv_request_id.
**      if ls_ptreq_header-next_processor_c is not initial.
**        Select single objid into @data(lv_pernr_proc) from PTREQ_ACTOR where actor_id eq @ls_ptreq_header-next_processor_c .
**          <fs_entity>-NextHandlerFIO = lo_assistent->get_pernr_fio( iv_pernr = lv_pernr_proc iv_begda = sy-datum ).
**      endif.
**      SELECT SINGLE notice_text INTO @<fs_entity>-comment FROM ptreq_notice WHERE request_id EQ @lv_request_id.
*
*    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS zcl_vacation_appl_wf DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES bi_object .
    INTERFACES bi_persistent .
    INTERFACES if_workflow .

    CLASS-METHODS get_request_data
      IMPORTING
        !iv_request_id TYPE tim_req_id
      EXPORTING
        !ev_name_init  TYPE string
        !ev_name_resp  TYPE string
        !ev_pernr_resp TYPE persno
        !ev_begda      TYPE begda
        !ev_endda      TYPE endda .
    CLASS-METHODS send_posted_req_email
      IMPORTING
        !io_req        TYPE REF TO cl_pt_req_wf_attribs OPTIONAL
        VALUE(i_owner) TYPE persno OPTIONAL
        !i_ruk         TYPE persno .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_VACATION_APPL_WF IMPLEMENTATION.


  METHOD get_request_data.

    DATA:
      lt_ptreq_header     TYPE TABLE OF ptreq_header,
      ls_ptreq_header     LIKE LINE OF lt_ptreq_header,
      lt_ptreq_items      TYPE TABLE OF ptreq_items,
      ls_ptreq_items      TYPE ptreq_items,
      lt_ptreq_attabsdata TYPE TABLE OF ptreq_attabsdata,
      ls_ptreq_attabsdata TYPE ptreq_attabsdata,

      ls_ptreq_actor1     TYPE ptreq_actor,
      ls_ptreq_actor2     TYPE ptreq_actor,
      lt_p0002            TYPE TABLE OF p0002,
      ls_p0002            LIKE LINE OF lt_p0002,

      lv_pernr            TYPE pernr_d.


    CLEAR:
      ev_name_init, ev_name_resp, ev_begda, ev_endda.

    SELECT *
      INTO TABLE lt_ptreq_header
      FROM ptreq_header
      WHERE
        request_id  EQ iv_request_id.

    CHECK sy-subrc EQ 0.

    SORT lt_ptreq_header BY version_no DESCENDING.
    READ TABLE lt_ptreq_header INTO ls_ptreq_header INDEX 1.

*   инициатор
    SELECT SINGLE *
      INTO ls_ptreq_actor1
      FROM ptreq_actor
      WHERE
        actor_id = ls_ptreq_header-initiator_ins.

    IF sy-subrc EQ 0.
      CLEAR lt_p0002.
      lv_pernr = ls_ptreq_actor1-objid.
      CALL FUNCTION 'PT_ARQ_READ_INFOTYPES'
        EXPORTING
          pernr     = lv_pernr
          begda     = sy-datum
          endda     = sy-datum
        TABLES
          itab_0002 = lt_p0002.

      READ TABLE lt_p0002 INTO ls_p0002 INDEX 1.
      IF sy-subrc EQ 0.
        CONCATENATE ls_p0002-nachn ls_p0002-vorna ls_p0002-midnm INTO ev_name_init SEPARATED BY space.
        CONDENSE ev_name_init.
      ENDIF.
    ENDIF.

*   ответственный
    SELECT SINGLE *
      INTO ls_ptreq_actor2
      FROM ptreq_actor
      WHERE
        actor_id = ls_ptreq_header-next_processor_i.

    IF sy-subrc EQ 0.
      CLEAR lt_p0002.
      ev_pernr_resp = ls_ptreq_actor2-objid.
      lv_pernr = ls_ptreq_actor2-objid.
      CALL FUNCTION 'PT_ARQ_READ_INFOTYPES'
        EXPORTING
          pernr     = lv_pernr
          begda     = sy-datum
          endda     = sy-datum
        TABLES
          itab_0002 = lt_p0002.

      READ TABLE lt_p0002 INTO ls_p0002 INDEX 1.
      IF sy-subrc EQ 0.
        CONCATENATE ls_p0002-nachn ls_p0002-vorna ls_p0002-midnm INTO ev_name_resp SEPARATED BY space.
        CONDENSE ev_name_resp.
      ENDIF.
    ENDIF.

*   даты
    SELECT *
      INTO TABLE lt_ptreq_items
      FROM ptreq_items
      WHERE
        item_list_id  EQ ls_ptreq_header-item_list_id.

    CHECK sy-subrc EQ 0.

    LOOP AT lt_ptreq_items INTO ls_ptreq_items.

      SELECT SINGLE *
        FROM ptreq_attabsdata
        INTO ls_ptreq_attabsdata
        WHERE
          item_id EQ ls_ptreq_items-item_ins.

      CHECK sy-subrc EQ 0.
      APPEND ls_ptreq_attabsdata TO lt_ptreq_attabsdata.
    ENDLOOP.

    LOOP AT lt_ptreq_attabsdata INTO ls_ptreq_attabsdata
      WHERE
        operation EQ 'INS'.
      ev_begda = ls_ptreq_attabsdata-begda.
      ev_endda = ls_ptreq_attabsdata-endda.
      EXIT.
    ENDLOOP.

    IF sy-subrc NE 0.
      LOOP AT lt_ptreq_attabsdata INTO ls_ptreq_attabsdata
        WHERE
          operation EQ 'DEL'.
        ev_begda = ls_ptreq_attabsdata-begda.
        ev_endda = ls_ptreq_attabsdata-endda.
        EXIT.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD send_posted_req_email.
    DATA: lo_bcs        TYPE REF TO cl_bcs
        , lv_subj       TYPE so_obj_des
        , lv_subj_long  TYPE string
        , lt_txt        TYPE TABLE OF tline
        , lv_mess       TYPE string
        , lt_mess_body  TYPE soli_tab

        , lv_owner      TYPE adr6-smtp_addr
        , lv_ruk        TYPE adr6-smtp_addr
        , lv_ruk_fio    TYPE string
        , lv_copy       TYPE flag
        , lv_fio        TYPE string

        , lv_abs_type   TYPE string
        , lv_abs_subty  TYPE subty
        , lv_abs_begda  TYPE text10
        , lv_abs_endda  TYPE text10

        , lv_begda      TYPE begda
        , lv_endda      TYPE endda

        , lv_date_tmp   TYPE dats
        , lv_gesch      TYPE gesch
        , lv_period     TYPE string

        , lv_xstr       TYPE xstring
        , lv_fname      TYPE string
        , lv_subrc      TYPE sysubrc
        .

    IF io_req IS BOUND.
      i_owner = io_req->owner-pernr.

      LOOP AT io_req->items_tab ASSIGNING FIELD-SYMBOL(<fs_items>).
        LOOP AT <fs_items>-atts ASSIGNING FIELD-SYMBOL(<fs_atts>)
            WHERE name EQ 'OPERATION' AND value EQ 'INS'.
          EXIT.
        ENDLOOP.
        IF sy-subrc NE 0.
          CONTINUE.
        ENDIF.

        LOOP AT <fs_items>-atts ASSIGNING <fs_atts>.
          CASE <fs_atts>-name.
            WHEN 'BEGDA'.
              lv_begda = <fs_atts>-value.
              WRITE lv_begda TO lv_abs_begda LEFT-JUSTIFIED.
            WHEN 'ENDDA'.
              lv_endda = <fs_atts>-value.
              WRITE lv_endda TO lv_abs_endda LEFT-JUSTIFIED.
            WHEN 'SUBTY'.
              lv_abs_subty = <fs_atts>-value.
            WHEN OTHERS.
          ENDCASE.
        ENDLOOP.
      ENDLOOP.
    ENDIF.

    lv_period = lv_abs_begda && ` - ` && lv_abs_endda.
    lv_abs_type = zcl_hr_data=>get_awart_txt( i_awart = lv_abs_subty ).
    lv_owner = zcl_vacation_appl=>get_email(
                  i_pernr = i_owner ).

    lv_ruk = zcl_vacation_appl=>get_email(
                  i_pernr = i_ruk ).

    lv_ruk_fio = zcl_vacation_appl=>get_pernr_name(
                    iv_pernr = i_ruk ).

    lv_fio = zcl_vacation_appl=>get_pernr_name(
                iv_pernr = i_owner ).

    SELECT SINGLE gesch FROM pa0002
      INTO lv_gesch
      WHERE pernr EQ i_owner.

    CASE lv_gesch.
      WHEN '1'.
        lv_fio = 'Уважаемый' && ` ` && lv_fio && `!`.
      WHEN '2'.
        lv_fio = 'Уважаемая' && ` ` && lv_fio && `!`.
      WHEN OTHERS.
        lv_fio = 'Уважаемый(-ая)' && ` ` && lv_fio && `!`.
    ENDCASE.

    lv_subj = lv_subj_long = 'Уведомление об отпуске'(001).
    TRY.
        lo_bcs = cl_bcs=>create_persistent( ).
        lv_mess = '<html><body class="sapBdyStd" scroll="AUTO" style="margin-left:2;margin-right:2;margin-top:2;margin-bottom:2">'.

        CALL FUNCTION 'SWU_GET_TASK_TEXTLINES'
          EXPORTING
            task              = 'TS92000016'
            usage             = 'W'
          TABLES
            itf_text_lines    = lt_txt
          EXCEPTIONS
            wrong_usage       = 1
            text_not_found    = 2
            text_system_error = 3
            OTHERS            = 4.

        LOOP AT lt_txt ASSIGNING FIELD-SYMBOL(<fs_txt>).
          IF <fs_txt>-tdformat EQ '*'.
            lv_mess = lv_mess && `<br> ` && <fs_txt>-tdline.
          ELSE.
            lv_mess = lv_mess && ` ` && <fs_txt>-tdline.
          ENDIF.
        ENDLOOP.

        REPLACE FIRST OCCURRENCE OF '<(>&<)>FIO<(>&<)>' IN lv_mess WITH lv_fio.
        REPLACE FIRST OCCURRENCE OF '<(>&<)>ABS_TYPE<(>&<)>' IN lv_mess WITH lv_abs_type.
        REPLACE FIRST OCCURRENCE OF '<(>&<)>PERIOD<(>&<)>' IN lv_mess WITH lv_period.
        REPLACE FIRST OCCURRENCE OF '<(>&<)>RUK<(>&<)>' IN lv_mess WITH lv_ruk_fio.

        lv_mess = lv_mess && '</body></html>'.
        CALL FUNCTION 'CONVERT_STRING_TO_TABLE'
          EXPORTING
            i_string         = lv_mess
            i_tabline_length = 255
          TABLES
            et_table         = lt_mess_body.

        DATA(lo_doc) = cl_document_bcs=>create_document(
                        i_type    = 'htm'
                        i_text    = lt_mess_body
                        i_subject = lv_subj ).

        zcl_zhr_ess_vac_appl_dpc_ext=>get_order(
          EXPORTING
            i_pernr = i_owner
            i_subty = lv_abs_subty
            i_begda = lv_begda
            i_endda = lv_endda
          IMPORTING
           e_xstring  = lv_xstr
           e_filename = lv_fname
           ev_subrc   = lv_subrc ).
        IF lv_subrc EQ 0.
          lo_doc->add_attachment(
               i_attachment_type     = 'BIN'
               i_attachment_subject  = CONV #( lv_fname )
                i_attachment_size     = CONV #( xstrlen( lv_xstr ) )
*               i_attachment_language = SPACE
*               i_att_content_text    =
               i_att_content_hex     = cl_document_bcs=>xstring_to_solix( ip_xstring = lv_xstr )
*               i_attachment_header   =
*               iv_vsi_profile        =
          ).
        ENDIF.

        lo_bcs->set_document( lo_doc ).

        DO 2 TIMES.
          CASE sy-index.
            WHEN 1.
              DATA(lo_receiver) = cl_cam_address_bcs=>create_internet_address( lv_owner ).
            WHEN 2.
              lv_copy = abap_true.
              CHECK lv_ruk IS NOT INITIAL.
              lo_receiver = cl_cam_address_bcs=>create_internet_address( lv_ruk ).
          ENDCASE.
          lo_bcs->add_recipient(
            i_recipient = lo_receiver
            i_express   = abap_true
            i_copy      = lv_copy ).
        ENDDO.



        lo_bcs->send( i_with_error_screen = abap_false ).
        COMMIT WORK AND WAIT.
      CATCH cx_send_req_bcs cx_root.
        RETURN.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

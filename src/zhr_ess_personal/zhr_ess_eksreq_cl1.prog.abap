*&---------------------------------------------------------------------*
*&  Include           ZHR_ESS_EKSREQ_CL1
*&---------------------------------------------------------------------*
CLASS lcl_handler DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_toolbar   FOR EVENT toolbar             OF cl_gui_alv_grid IMPORTING e_object,
      user_command FOR EVENT before_user_command OF cl_gui_alv_grid IMPORTING e_ucomm sender,
      double_click FOR EVENT double_click        OF cl_gui_alv_grid IMPORTING e_row,
      button_click FOR EVENT button_click        OF cl_gui_alv_grid IMPORTING es_col_id
                                                                              es_row_no .

ENDCLASS.
CLASS lcl_handler IMPLEMENTATION.
  METHOD on_toolbar.
    APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING FIELD-SYMBOL(<mt_toolbar>).
*    MOVE 3 TO <mt_toolbar>-butn_type.
    CLEAR: e_object->mt_toolbar.

    APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <mt_toolbar>.
    <mt_toolbar>-function  = 'ZJOB'.
    <mt_toolbar>-icon      = icon_set_state.
    <mt_toolbar>-text      = 'Взять в работу'.
    <mt_toolbar>-quickinfo = 'Взять в работу'(001).
    APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <mt_toolbar>.
    MOVE 3 TO <mt_toolbar>-butn_type.

    APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <mt_toolbar>.
    <mt_toolbar>-function  = 'ZREJ'.
    <mt_toolbar>-icon      = icon_reject.
    <mt_toolbar>-quickinfo = 'Отклонить'(002).
    <mt_toolbar>-text      = 'Отклонить'.
    APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <mt_toolbar>.
    MOVE 3 TO <mt_toolbar>-butn_type.

    APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <mt_toolbar>.
    <mt_toolbar>-function  = 'ZDON'.
    <mt_toolbar>-icon      = icon_allow.
    <mt_toolbar>-text      = 'Выполнено'.
    <mt_toolbar>-quickinfo = 'Выполнено'(003).
    APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <mt_toolbar>.
    MOVE 3 TO <mt_toolbar>-butn_type.

    APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING <mt_toolbar>.
    <mt_toolbar>-function  = 'ZREE'.
    <mt_toolbar>-icon      = icon_transfer.
    <mt_toolbar>-text      = 'Перенаправить'.
    <mt_toolbar>-quickinfo = 'Перенаправить'(004).

  ENDMETHOD.
  METHOD user_command.
    DATA: lt_update TYPE TABLE OF zthr_ess_eksreq,
          lv_error  TYPE boole_d VALUE abap_false.

    "Сбор выбранных строк
    go_alv->get_selected_rows( IMPORTING et_index_rows = DATA(lt_index_rows) ).
    LOOP AT lt_index_rows ASSIGNING FIELD-SYMBOL(<row>).
      READ TABLE gt_alv ASSIGNING FIELD-SYMBOL(<alv>) INDEX <row>-index.
      CHECK sy-subrc = 0 AND <alv> IS ASSIGNED.

      APPEND INITIAL LINE TO lt_update ASSIGNING FIELD-SYMBOL(<upd>).
      MOVE-CORRESPONDING <alv> TO <upd>.
    ENDLOOP.
    IF lt_update IS INITIAL.
      lv_error = abap_true.
      EXIT.
    ENDIF.

    CASE e_ucomm.
      WHEN 'ZJOB'.
        LOOP AT lt_update ASSIGNING <upd>.
          IF <upd>-status <> 0.
            lv_error = abap_true.
            EXIT.
          ENDIF.

          <upd>-status = 1.
          <upd>-date_change = sy-datum.
          <upd>-time_change = sy-timlo.
          <upd>-uname = sy-uname.
          <upd>-approver_pernr = gv_pernr.
        ENDLOOP.
      WHEN 'ZREJ'.
        LOOP AT lt_update ASSIGNING <upd>.
          IF <upd>-status <> 0 AND <upd>-status <> 1.
            lv_error = abap_true.
            EXIT.
          ENDIF.

          <upd>-status = 3.
          <upd>-date_change = sy-datum.
          <upd>-time_change = sy-timlo.
          <upd>-uname = sy-uname.
        ENDLOOP.
      WHEN 'ZDON'.
        LOOP AT lt_update ASSIGNING <upd>.
          IF <upd>-status <> 1.
            lv_error = abap_true.
            EXIT.
          ENDIF.

          <upd>-status = 4.
          <upd>-date_change = sy-datum.
          <upd>-time_change = sy-timlo.
          <upd>-uname = sy-uname.
        ENDLOOP.
      WHEN 'ZREE'.
        IF lines( lt_update ) <> 1.
          lv_error = abap_true.
          EXIT.
        ENDIF.
        READ TABLE lt_update ASSIGNING <upd> INDEX 1.
        IF <upd>-status <> 0 AND <upd>-status <> 1.
          lv_error = abap_true.
          EXIT.
        ENDIF.
        <upd>-date_change = sy-datum.
        <upd>-time_change = sy-timlo.
        <upd>-uname = sy-uname.
        DATA: lv_sel_objec TYPE objec,
              lv_mode      TYPE hrf4param-srk_mode.

        CALL FUNCTION 'RH_OBJID_REQUEST_46A'
          EXPORTING
            plvar           = '01'
            otype           = 'P'
            seark           = '*'
            dynpro_repid    = sy-repid
            dynpro_dynnr    = '1000'
            set_mode        = 'X'
          IMPORTING
            sel_object      = lv_sel_objec
          EXCEPTIONS
            cancelled       = 1
            wrong_condition = 2
            nothing_found   = 3
            illegal_mode    = 4
            internal_error  = 5
            OTHERS          = 6.

        IF sy-subrc <> 0.
          lv_error = abap_true.
          EXIT.
        ENDIF.

        DATA(lv_old_approver) = <upd>-approver_pernr.

        <upd>-approver_pernr = lv_sel_objec-objid.

    ENDCASE.

    DATA: lt_mapping TYPE zttmail_mapping
        , lt_mail_to TYPE bcsy_smtpa
        .

    DATA(lo_assist) = zcl_mss_data_assistent=>get_instance( ).

    IF lv_error = abap_true.
    ELSE.
      CALL FUNCTION 'ZHR_ESS_EKSREQ_UPD' IN UPDATE TASK
        EXPORTING
          it_data = lt_update.
      COMMIT WORK AND WAIT.


      LOOP AT lt_update ASSIGNING FIELD-SYMBOL(<lt_update>).
        REFRESH: lt_mail_to
               , lt_mapping
               .

        SELECT SINGLE text FROM zthr_ess_ekstype INTO @DATA(lv_text) WHERE code = @<lt_update>-req_type.
        CASE e_ucomm.
          WHEN 'ZJOB'."Взять в работу
            CONTINUE.
          WHEN 'ZREJ'."Отклонить
            DATA(lv_subject) = CONV so_obj_des( 'Заявка отклонена' ).
            DATA(lv_textname) = CONV ltext( 'ZHR_REQUEST_APPROVE' ).
            APPEND VALUE #( name = `<pernr>`   value = CONV zsmail_mapping-value( lo_assist->get_pernr_fio( iv_pernr = <lt_update>-perner_creator ) ) ) TO lt_mapping.
            APPEND VALUE #( name = `<status>`  value = `выполнена`  ) TO lt_mapping.
            APPEND VALUE #( name = `<reqtype>` value = lv_text  )   TO lt_mapping.
            DATA(lv_pernr) = <lt_update>-perner_creator.
          WHEN 'ZDON'."Выполнено
            lv_subject = CONV so_obj_des( 'Заявка выполнена' ).
            lv_textname = 'ZHR_REQUEST_APPROVE'.
            APPEND VALUE #( name = `<pernr>`   value = CONV zsmail_mapping-value( lo_assist->get_pernr_fio( iv_pernr = <lt_update>-perner_creator ) ) ) TO lt_mapping.
            APPEND VALUE #( name = `<status>`  value = `выполнена`  ) TO lt_mapping.
            APPEND VALUE #( name = `<reqtype>` value = lv_text  )   TO lt_mapping.
            lv_pernr = <lt_update>-perner_creator.
          WHEN 'ZREE'.
            lv_subject = CONV so_obj_des( 'Заявка перенаправлена' ).
            lv_textname = 'ZHR_REQUEST_RESEND'.
            APPEND VALUE #( name = `<pernr>`     value = CONV zsmail_mapping-value( lo_assist->get_pernr_fio( iv_pernr = <lt_update>-perner_creator ) ) ) TO lt_mapping.
            APPEND VALUE #( name = `<old_pernr>` value = CONV zsmail_mapping-value( lo_assist->get_pernr_fio( iv_pernr = lv_old_approver ) ) ) TO lt_mapping.
            APPEND VALUE #( name = `<new_pernr>` value = CONV zsmail_mapping-value( lo_assist->get_pernr_fio( iv_pernr = <upd>-approver_pernr ) ) ) TO lt_mapping.
            APPEND VALUE #( name = `<reqtype>`   value = lv_text  ) TO lt_mapping.
            lv_pernr = <upd>-approver_pernr.
          WHEN OTHERS.
            CONTINUE.
        ENDCASE.

        DATA(lv_email) = lo_assist->get_pernr_0105( iv_pernr = lv_pernr ).
        CHECK lv_email IS NOT INITIAL.
        APPEND INITIAL LINE TO lt_mail_to ASSIGNING FIELD-SYMBOL(<lt_mail_to>).
        <lt_mail_to> = lv_email.

        zcl_send_email=>send_mail( EXPORTING itd_to          = lt_mail_to
                                             iv_textname     = CONV #( lv_textname )
                                             iv_subject      = lv_subject
                                             itd_mapping     = lt_mapping
                                             iv_immediately  = abap_false ).
      ENDLOOP.
    ENDIF.

    PERFORM select_data.
    go_alv->refresh_table_display( is_stable = cs_stable ).
  ENDMETHOD.
  METHOD double_click.
  ENDMETHOD.
  METHOD button_click.
    DATA: lt_att      LIKE gt_att,
          lv_folder   TYPE string,
          lv_filename TYPE string,
          lv_ext      TYPE string,
          lv_path     TYPE string,
          lv_fullpath TYPE string,
          lt_data     TYPE solix_tab,
          lv_curpos   TYPE i VALUE 0,
          lv_line     TYPE i VALUE 255,
          ls_data     LIKE LINE OF lt_data,
          gs_selfield TYPE slis_selfield,
          g_exit(1)   TYPE c,
          lv_rc       TYPE i.

    READ TABLE gt_alv ASSIGNING FIELD-SYMBOL(<alv>) INDEX es_row_no-row_id.
    CHECK <alv> IS ASSIGNED AND sy-subrc = 0.

    CASE es_col_id.

      WHEN 'ATTACHEMENTS'.

        "По выбранной заявке - найдем файлы к ней

*    READ TABLE gt_alv ASSIGNING FIELD-SYMBOL(<alv>) INDEX es_row_no-row_id.
*    CHECK <alv> IS ASSIGNED AND sy-subrc = 0.

        LOOP AT gt_att ASSIGNING FIELD-SYMBOL(<att>) WHERE req_id = <alv>-req_id.
          APPEND <att> TO lt_att.
        ENDLOOP.
        CHECK lt_att IS NOT INITIAL.

        "Выбор файла для открытия
        CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
          EXPORTING
            i_zebra               = 'X'
            i_screen_start_column = 5
            i_screen_start_line   = 5
            i_screen_end_column   = 150
            i_screen_end_line     = 20
            i_tabname             = '1'
            i_structure_name      = 'ZTHR_ESS_EKSREQF'
*           it_fieldcat           = ct_fc
          IMPORTING
            es_selfield           = gs_selfield
            e_exit                = g_exit
          TABLES
            t_outtab              = lt_att
          EXCEPTIONS
            program_error         = 1
            OTHERS                = 2.
        IF sy-subrc <> 0 . ENDIF.
        CHECK gs_selfield IS NOT INITIAL.

        UNASSIGN <att>.
        READ TABLE lt_att ASSIGNING <att> INDEX gs_selfield-tabindex.
        CHECK sy-subrc = 0 AND <att>-filename IS NOT INITIAL.

        "Догрузим сам файл
        SELECT SINGLE * FROM zthr_ess_eksreqf INTO <att> WHERE req_id = <att>-req_id AND req_pos = <att>-req_pos.

        "Получение путей для сохранения файлы
        CALL METHOD cl_gui_frontend_services=>get_sapgui_workdir
          CHANGING
            sapworkdir            = lv_folder
          EXCEPTIONS
            get_sapworkdir_failed = 1
            cntl_error            = 2
            error_no_gui          = 3
            not_supported_by_gui  = 4
            OTHERS                = 5.
        CHECK sy-subrc = 0.
        cl_gui_cfw=>flush( ).

        "Диалог сохранения файла
        lv_filename = <att>-filename.
        CALL METHOD cl_gui_frontend_services=>file_save_dialog
          EXPORTING
            window_title         = 'Выберите путь для сохранения файла'
            default_file_name    = lv_filename
            initial_directory    = lv_folder
            prompt_on_overwrite  = 'X'
          CHANGING
            filename             = lv_filename
            path                 = lv_path
            fullpath             = lv_fullpath
          EXCEPTIONS
            cntl_error           = 1
            error_no_gui         = 2
            not_supported_by_gui = 3
            OTHERS               = 4.
        CHECK lv_fullpath IS NOT INITIAL.

        "Перекладывание данных из строки в таблицу
        DATA(lv_length) = xstrlen( <att>-filedata ).
*        WHILE 1 = 1.
*          ls_data-line = <att>-filedata+lv_curpos.
*          APPEND ls_data TO lt_data.
*          ADD 255 TO lv_curpos.
*          IF lv_curpos > lv_length.
*            EXIT.
*          ENDIF.
*        ENDWHILE.

        TRY.
            CALL METHOD cl_bcs_convert=>xstring_to_xtab
              EXPORTING
                iv_xstring = <att>-filedata
              IMPORTING
                et_xtab    = lt_data[].
          CATCH cx_bcs.
            exit.
        ENDTRY.

        "Сохранение файла
        CALL METHOD cl_gui_frontend_services=>gui_download
          EXPORTING
            filename                = lv_fullpath
            filetype                = 'BIN'
            bin_filesize            = lv_length
          CHANGING
            data_tab                = lt_data
          EXCEPTIONS
            file_write_error        = 1
            no_batch                = 2
            gui_refuse_filetransfer = 3
            invalid_type            = 4
            no_authority            = 5
            unknown_error           = 6
            header_not_allowed      = 7
            separator_not_allowed   = 8
            filesize_not_allowed    = 9
            header_too_long         = 10
            dp_error_create         = 11
            dp_error_send           = 12
            dp_error_write          = 13
            unknown_dp_error        = 14
            access_denied           = 15
            dp_out_of_memory        = 16
            disk_full               = 17
            dp_timeout              = 18
            file_not_found          = 19
            dataprovider_exception  = 20
            control_flush_error     = 21
            not_supported_by_gui    = 22
            error_no_gui            = 23
            OTHERS                  = 24.
        CHECK sy-subrc = 0.
        cl_gui_cfw=>flush( ).

        "Открытие файла
        CALL METHOD cl_gui_frontend_services=>execute
          EXPORTING
            document               = lv_fullpath
          EXCEPTIONS
            cntl_error             = 1
            error_no_gui           = 2
            bad_parameter          = 3
            file_not_found         = 4
            path_not_found         = 5
            file_extension_unknown = 6
            error_execute_failed   = 7
            synchronous_failed     = 8
            not_supported_by_gui   = 9
            OTHERS                 = 10.
        cl_gui_cfw=>flush( ).

      WHEN 'ZCOMBUT'.
*        IF <alv>-coment IS NOT INITIAL.
        gv_status = <alv>-status.
        gv_comment = <alv>-coment.        ##ENH_OK.
        gv_hrcomment = <alv>-hrcomment.        ##ENH_OK.
        gv_selrow = es_row_no-row_id.      ##ENH_OK
        CALL SCREEN 9001 STARTING AT 100 1.
*        ENDIF.

    ENDCASE.

  ENDMETHOD.
ENDCLASS.

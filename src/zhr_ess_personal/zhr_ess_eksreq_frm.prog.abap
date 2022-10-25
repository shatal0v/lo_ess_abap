*&---------------------------------------------------------------------*
*&  Include           ZHR_ESS_EKSREQ_FRM
*&---------------------------------------------------------------------*
FORM select_data.
  FREE: gt_alv, gt_att.

  "Получение данных из БД
  SELECT * FROM zthr_ess_eksreq
  INTO CORRESPONDING FIELDS OF TABLE gt_alv
  WHERE approver_pernr = gv_pernr.
  LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<alv>).
    DATA: ls_pa0002 TYPE pa0002.

    CLEAR: ls_pa0002.
    SELECT SINGLE nachn vorna midnm FROM pa0002 INTO CORRESPONDING FIELDS OF ls_pa0002 WHERE pernr = <alv>-perner_creator.
    CONCATENATE ls_pa0002-nachn ls_pa0002-vorna ls_pa0002-midnm INTO <alv>-pernercreatorfio SEPARATED BY space.

    CLEAR: ls_pa0002.
    SELECT SINGLE nachn vorna midnm FROM pa0002 INTO CORRESPONDING FIELDS OF ls_pa0002 WHERE pernr = <alv>-approver_pernr.
    CONCATENATE ls_pa0002-nachn ls_pa0002-vorna ls_pa0002-midnm INTO <alv>-approverfio SEPARATED BY space.

    SELECT SINGLE text FROM zthr_ess_ekstype INTO <alv>-reqtypename WHERE code = <alv>-req_type.

    READ TABLE gt_idd07v ASSIGNING FIELD-SYMBOL(<status>) WITH KEY domvalue_l = <alv>-status.
    IF sy-subrc = 0.
      <alv>-statustext = <status>-ddtext.
    ENDIF.

    SELECT req_id req_pos filename FROM zthr_ess_eksreqf APPENDING CORRESPONDING FIELDS OF TABLE gt_att WHERE req_id = <alv>-req_id.
    IF sy-subrc = 0.
      <alv>-attachements = icon_attachment.
    ENDIF.

    IF <alv>-coment IS NOT INITIAL.
      <alv>-zcombut = icon_display_text.
    ENDIF.

  ENDLOOP.
ENDFORM.
FORM pbo100.
  DATA: lt_fc TYPE lvc_t_fcat,
        ls_lo TYPE lvc_s_layo,
        ls_vr TYPE disvariant.

  IF go_cnt IS INITIAL.
    CREATE OBJECT go_cnt
      EXPORTING
        container_name              = 'CNT'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT go_alv
      EXPORTING
        i_parent          = go_cnt
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    PERFORM alv_fc CHANGING lt_fc.

    DATA(lo_handler) = NEW lcl_handler( ).
    SET HANDLER lo_handler->on_toolbar   FOR go_alv.
    SET HANDLER lo_handler->user_command FOR go_alv.
    "SET HANDLER lo_handler->double_click FOR go_alv.
    SET HANDLER lo_handler->button_click FOR go_alv.

    ls_lo-sel_mode   = 'A'.
    ls_lo-cwidth_opt = 'X'.

    ls_vr-report = sy-repid.

    go_alv->set_table_for_first_display(  EXPORTING is_layout                     = ls_lo
                                                    i_save                        = 'X'
                                                    is_variant                    = ls_vr
                                                    i_default                     = 'X'
                                          CHANGING  it_outtab                     = gt_alv
                                                    it_fieldcatalog               = lt_fc
                                        EXCEPTIONS  invalid_parameter_combination = 1
                                                    program_error                 = 2
                                                    too_many_lines                = 3
                                                    OTHERS                        = 4 ).
  ELSE.
    go_alv->refresh_table_display( is_stable = cs_stable ).
  ENDIF.
ENDFORM.
FORM pai100.
  CASE ok_code.
    WHEN  'BACK' OR 'EXIT' OR 'QUIT'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDFORM.
FORM alv_fc CHANGING ct_fc TYPE lvc_t_fcat.
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = cv_alv_structure
    CHANGING
      ct_fieldcat            = ct_fc
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc = 0.
    LOOP AT ct_fc ASSIGNING FIELD-SYMBOL(<fc>).
      CASE <fc>-fieldname.
        WHEN 'REQID'.
          <fc>-tech = abap_true.
        WHEN 'ATTACHEMENTS'.
          <fc>-icon = abap_true.
          <fc>-style = cl_gui_alv_grid=>mc_style_button.
        WHEN 'ZCOMBUT'.
          <fc>-icon = abap_true.
          <fc>-style = cl_gui_alv_grid=>mc_style_button.
      ENDCASE.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PAI9001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pai9001 USING zcom_old  TYPE zehr_ess_eksreq_coment
                   zcom_new  TYPE zehr_ess_eksreq_coment.


  DATA: lt_update TYPE TABLE OF zthr_ess_eksreq,
        lv_error  TYPE boole_d VALUE abap_false.
  CONSTANTS: line_length TYPE i VALUE 72.

  DATA: BEGIN OF stext OCCURS 200,
          line(line_length),
        END OF stext.

  CASE ok_code.
    WHEN  'OK'.
*

      CALL METHOD g_editor_new->get_text_as_r3table
        IMPORTING
          table = stext[].
      IF stext[] IS NOT INITIAL.
        CONCATENATE LINES OF stext[] INTO zcom_new SEPARATED BY space.
        IF zcom_new IS NOT INITIAL.
          READ TABLE gt_alv ASSIGNING FIELD-SYMBOL(<alv>) INDEX gv_selrow.
          CHECK sy-subrc = 0 AND <alv> IS ASSIGNED.
*
          APPEND INITIAL LINE TO lt_update ASSIGNING FIELD-SYMBOL(<upd>).
          MOVE-CORRESPONDING <alv> TO <upd>.
*
          IF lt_update IS INITIAL.
            lv_error = abap_true.
            CLEAR: zcom_new.
            LEAVE TO SCREEN 0.
          ENDIF.
*
          IF <upd>-status = 2 OR "<upd>-status = 3 OR
             <upd>-status = 4.
            lv_error = abap_true.
            CLEAR: zcom_new.
            LEAVE TO SCREEN 0.
          ENDIF.
*
*          IF zcom_old IS NOT INITIAL.
*            <upd>-coment = sy-uname && ` ` && '-->' && ` ` &&
*                           zcom_new && ` ` && '---' && ` ` && zcom_old.
*          ELSE.
          <upd>-hrcomment =  zcom_new.

*          ENDIF.
*
          IF lv_error = abap_true.
          ELSE.
            CALL FUNCTION 'ZHR_ESS_EKSREQ_UPD' IN UPDATE TASK
              EXPORTING
                it_data = lt_update.
            COMMIT WORK AND WAIT.
          ENDIF.
*
          PERFORM select_data.
          go_alv->refresh_table_display( is_stable = cs_stable ).
        ENDIF.
      ENDIF.
      CLEAR: zcom_new, zcom_old.
      LEAVE TO SCREEN 0.
*
    WHEN 'CANCEL'.
      CLEAR: zcom_new, zcom_old.
      LEAVE TO SCREEN 0.
*
  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*&  Include           ZHR_ESS_EKSREQ_P01
*&---------------------------------------------------------------------*
MODULE pbo100 OUTPUT.
  SET PF-STATUS 'GUIST100'.
  PERFORM pbo100.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PBO9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo9001 OUTPUT.
*  DATA: zcom_old TYPE zehr_ess_eksreq_coment,
*        zcom_new TYPE zehr_ess_eksreq_coment.
*
*  SET PF-STATUS 'GUIST9001'.
*  zcom_old = gv_coment.
*
*  IF  gv_status = 2 OR gv_status = 3 OR gv_status = 4.
*    LOOP AT SCREEN.
*      IF screen-name = 'ZCOM_NEW'.
*        screen-active = '0'.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.
  DATA: zcom_old TYPE zehr_ess_eksreq_coment,
        zcom_new TYPE zehr_ess_eksreq_coment.
  CONSTANTS: line_length TYPE i VALUE 72.
  DATA: BEGIN OF stext OCCURS 200,
          line(line_length),
        END OF stext.

  DATA: BEGIN OF stext_new OCCURS 200,
          line(line_length),
        END OF stext_new.

  SET PF-STATUS 'GUIST9001'.
  zcom_new = gv_comment.
  IF g_editor IS INITIAL.

*   create control container
    CREATE OBJECT g_editor_container
      EXPORTING
        container_name              = 'TEXTEDITOR1'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.
    IF sy-subrc NE 0.
*      add your handling
    ENDIF.

*   create calls constructor, which initializes, creats and links
*   TextEdit Control
    CREATE OBJECT g_editor
      EXPORTING
        parent                     = g_editor_container
        wordwrap_mode              =
*             cl_gui_textedit=>wordwrap_off
                                     cl_gui_textedit=>wordwrap_at_fixed_position
*             cl_gui_textedit=>WORDWRAP_AT_WINDOWBORDER
        wordwrap_position          = line_length
        wordwrap_to_linebreak_mode = cl_gui_textedit=>true.

  ENDIF.
  IF g_editor_new IS INITIAL.

*   create control container
    CREATE OBJECT g_editor_container_new
      EXPORTING
        container_name              = 'TEXTEDITOR_NEW'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.
    IF sy-subrc NE 0.
*      add your handling
    ENDIF.

*   create calls constructor, which initializes, creats and links
*   TextEdit Control
    CREATE OBJECT g_editor_new
      EXPORTING
        parent                     = g_editor_container_new
        wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
        wordwrap_position          = line_length
        wordwrap_to_linebreak_mode = cl_gui_textedit=>true.

  ENDIF.


  REFRESH stext.
  CALL FUNCTION 'RKD_WORD_WRAP'
    EXPORTING
      textline            = gv_comment
      outputlen           = line_length
    TABLES
      out_lines           = stext
    EXCEPTIONS
      outputlen_too_large = 1
      OTHERS              = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
  gtext[] =  stext[].

  REFRESH stext_new.
  CALL FUNCTION 'RKD_WORD_WRAP'
    EXPORTING
      textline            = gv_hrcomment
      outputlen           = line_length
    TABLES
      out_lines           = stext_new
    EXCEPTIONS
      outputlen_too_large = 1
      OTHERS              = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
  gtext_new[] =  stext_new[].

  CALL METHOD g_editor->set_text_as_r3table
    EXPORTING
      table = stext[].
  REFRESH stext[].

  CALL METHOD g_editor_new->set_text_as_r3table
    EXPORTING
      table = stext_new[].

  CALL METHOD g_editor->set_readonly_mode
    EXPORTING
      readonly_mode = 1.

  IF  gv_status = 1 or
      gv_status = 3.
    CALL METHOD g_editor_new->set_readonly_mode
      EXPORTING
        readonly_mode = 0.
  ELSE.

    CALL METHOD g_editor_new->set_readonly_mode
      EXPORTING
        readonly_mode = 1.

  ENDIF.
ENDMODULE.

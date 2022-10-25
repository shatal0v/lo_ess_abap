CLASS lcl_handler DEFINITION.
  PUBLIC SECTION.
    METHODS: on_toolbar   FOR EVENT toolbar             OF cl_gui_alv_grid IMPORTING e_object
           , user_command FOR EVENT before_user_command OF cl_gui_alv_grid IMPORTING e_ucomm sender
           , double_click FOR EVENT double_click        OF cl_gui_alv_grid IMPORTING e_row
           .
ENDCLASS.

CLASS lcl_handler IMPLEMENTATION.
  METHOD on_toolbar.
*    APPEND INITIAL LINE TO e_object->mt_toolbar ASSIGNING FIELD-SYMBOL(<mt_toolbar>).
*    <mt_toolbar>-function  = 'ZDELE'.
*    <mt_toolbar>-icon      = icon_delete.
*    <mt_toolbar>-quickinfo = 'Удалить'.
  ENDMETHOD.
  METHOD user_command.
*    DATA: ls_stable TYPE lvc_s_stbl VALUE '11'
*        .
*    CASE e_ucomm.
*      WHEN 'ZDELE'.
*        lo_grid->get_selected_rows( IMPORTING et_index_rows = DATA(lt_index_rows) ).
*        SORT  lt_index_rows BY index DESCENDING.
*        lo_grid->refresh_table_display( is_stable = ls_stable ).
*      WHEN OTHERS.
*        "
*    ENDCASE.
  ENDMETHOD.
  METHOD double_click.
    READ TABLE lt_data ASSIGNING FIELD-SYMBOL(<lt_data>) INDEX e_row-index.
    CHECK <lt_data> IS ASSIGNED AND sy-subrc = 0.

    PERFORM show_file USING <lt_data>.

  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
***INCLUDE LZHR_ESS_SVEDDFF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_0200_PBO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_0200_pbo .
  IF lo_container IS INITIAL AND lo_grid IS INITIAL.
    CREATE OBJECT lo_container
      EXPORTING
        container_name              = 'GRID1'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT lo_grid
      EXPORTING
        i_parent          = lo_container
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    DATA: lt_fieldcat TYPE lvc_t_fcat
        .

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = 'ZSHR_ESS_SVEDDF'
      CHANGING
        ct_fieldcat            = lt_fieldcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.

    FIELD-SYMBOLS: <lt_fieldcat> LIKE LINE OF lt_fieldcat
                 .
    LOOP AT lt_fieldcat ASSIGNING <lt_fieldcat>.
      CASE <lt_fieldcat>-fieldname.
        WHEN 'BITM_TYPE' OR 'BITM_DESCR'.
          <lt_fieldcat>-tech = abap_true.
          CONTINUE.
        WHEN OTHERS.
          CONTINUE.
      ENDCASE.

    ENDLOOP.

    DATA: ls_layout  TYPE lvc_s_layo
        , ls_variant TYPE disvariant
        .
    DATA(lo_handler) = NEW lcl_handler( ).

    SET HANDLER lo_handler->on_toolbar   FOR lo_grid.
    SET HANDLER lo_handler->user_command FOR lo_grid.
    SET HANDLER lo_handler->double_click FOR lo_grid.

    ls_layout-sel_mode   = 'A'.
    ls_layout-cwidth_opt = 'X'.
    ls_layout-grid_title = 'Имеющиеся документы'.
    lo_grid->set_table_for_first_display( EXPORTING is_layout                     = ls_layout
                                                    i_save                        = 'X'
                                                    is_variant                    = ls_variant
                                                    i_default                     = 'X'
                                          CHANGING  it_outtab                     = lt_data
                                                    it_fieldcatalog               = lt_fieldcat
                                         EXCEPTIONS invalid_parameter_combination = 1
                                                    program_error                 = 2
                                                    too_many_lines                = 3
                                                    OTHERS                        = 4 ).
  ELSE.
    DATA: ls_stable TYPE lvc_s_stbl VALUE '11'
        .
    lo_grid->refresh_table_display( is_stable = ls_stable ).
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IV_PERNR  text
*      -->P_IV_YEAR  text
*----------------------------------------------------------------------*
FORM fill_data  USING    iv_pernr TYPE pernr_d
                         iv_year  TYPE gjahr.

SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_data FROM zthr_ess_sveddf
  WHERE pernr  = iv_pernr
    and agreed <> ''.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SHOW_FILE
*&---------------------------------------------------------------------*
*       откытие файла со сведениями о доходе
*----------------------------------------------------------------------*
*      -->P_<ALV>  text
*----------------------------------------------------------------------*
FORM show_file  USING    is_data TYPE zshr_ess_sveddf.
  DATA:  lv_folder   TYPE string,
         lv_fullpath TYPE string,
         ls_sveddf   TYPE zthr_ess_sveddf,
         lt_data     TYPE solix_tab,
         lv_curpos   TYPE i VALUE 0,
         lv_line     TYPE i VALUE 255,
         ls_data     LIKE LINE OF lt_data,
         gs_selfield TYPE slis_selfield,
         g_exit(1)   TYPE c,
         lv_rc       TYPE i.

  SELECT      seqnr
              filename
              filedata
    INTO (ls_sveddf-seqnr,
          ls_sveddf-filename,
          ls_sveddf-filedata)
     FROM zthr_ess_sveddf
    WHERE pernr = is_data-pernr
      AND zyear = is_data-zyear
      AND agreed = 'X'.
  ENDSELECT.

  IF sy-subrc = 0.
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

    lv_fullpath = lv_folder && '\' &&  ls_sveddf-filename .

    "Перекладывание данных из строки в таблицу

    DATA(lv_length) = xstrlen( ls_sveddf-filedata ).

    CALL METHOD cl_bcs_convert=>xstring_to_xtab
      EXPORTING
        iv_xstring = ls_sveddf-filedata
      IMPORTING
        et_xtab    = lt_data[].


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
  ELSE.
    MESSAGE text-m01 TYPE 'S'.

  ENDIF.
ENDFORM.

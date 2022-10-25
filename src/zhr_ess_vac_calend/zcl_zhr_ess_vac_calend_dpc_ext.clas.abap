class ZCL_ZHR_ESS_VAC_CALEND_DPC_EXT definition
  public
  inheriting from ZCL_ZHR_ESS_VAC_CALEND_DPC
  create public .

public section.

  constants C_HOLIDAY type STRING value 'C' ##NO_TEXT.

  class-methods GET_HOLIDAYS
    importing
      !I_PERNR type PERSNO
      !I_BEGDA type BEGDA
      !I_ENDDA type ENDDA
    exporting
      !ET_ENTITYSET type ZCL_ZHR_ESS_VAC_CALEND_MPC=>TT_HOLIDAY .
  class-methods READ_WORK_SCHEDULE
    importing
      !IV_BEGDA type DATUM
      !IV_ENDDA type DATUM
      !IV_PERNR type PERNR_D
    exporting
      !ET_DAYGEN type PWSDAYGEN_TAB
      !ET_DAYINT type PWSDAYINT_TAB
      !ET_P0001 type PTT_P0001
      !ET_PSP type PTPSP_TAB .

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_EXPANDED_ENTITYSET
    redefinition .
protected section.

  methods HOLIDAYSET_GET_ENTITYSET
    redefinition .
private section.

  class-data MV_DUMMY type CHAR1 .
  data MO_MESS type ref to /IWBEP/IF_MESSAGE_CONTAINER .
  constants C_FREE type STRING value 'H' ##NO_TEXT.

  methods ADD_MSG .
  methods RAISE_EXCEPTION .
ENDCLASS.



CLASS ZCL_ZHR_ESS_VAC_CALEND_DPC_EXT IMPLEMENTATION.


  METHOD /iwbep/if_mgw_appl_srv_runtime~get_expanded_entityset.
    DATA: ls_deep_entity TYPE zcl_zhr_ess_vac_calend_mpc_ext=>ts_deep_entity
        , lt_deep_entity TYPE TABLE OF zcl_zhr_ess_vac_calend_mpc_ext=>ts_deep_entity
        .
    DATA: lv_loginactdir TYPE sy-uname "zlogin_act_dir
        , lv_begda TYPE begda
    , lv_endda TYPE endda
    , lv_property TYPE string
    , lt_return TYPE bapiret2_t
    .
    TYPES:
      BEGIN OF t_row_id,
        row_id TYPE ptarq_uia_weekcale_struc-row_id,
      END OF t_row_id.

    DATA:
      lv_pernr            TYPE pernr_d,
      lv_pernr2           TYPE pernr_d,
      lv_name             TYPE string,
      ls_return           LIKE LINE OF lt_return,
      lv_cache_validity   TYPE int4,

      lt_mes              TYPE ptarq_uia_messages_tab,
      ls_mes              LIKE LINE OF lt_mes,
      lt_col              TYPE ptarq_uia_custcolumns_tab,
      ls_col              LIKE LINE OF lt_col,
      lt_com              TYPE ptarq_uia_command_tab,
      ls_com              LIKE LINE OF lt_com,
      lt_legend           TYPE ptreq_uia_legend_tab,
      ls_legend           LIKE LINE OF lt_legend,
      lt_weekday          TYPE ptarq_uia_weekday_tab,
      ls_weekday          LIKE LINE OF lt_weekday,
      lt_team             TYPE ptreq_uia_team_tab,
      ls_team             LIKE LINE OF lt_team,

      ls_team_id          TYPE ptreq_uia_teamid_struc,
      ls_team_id2         TYPE ptreq_uia_teamid_struc,
      lv_cache_date       TYPE sy-datum,
      lv_cache_time       TYPE sy-uzeit,
      lt_calendar         TYPE ptarq_uia_weekcale_tab,
      ls_calendar         LIKE LINE OF lt_calendar,
      lt_rowlabel         TYPE ptarq_uia_rowlabel_tab,
      ls_rowlabel         LIKE LINE OF lt_rowlabel,
      ls_position         TYPE ptreq_uia_pagepos,

      ls_req              TYPE ptreq_uia_request_id_struc,
      lt_row_id           TYPE TABLE OF t_row_id,
      ls_row_id           LIKE LINE OF lt_row_id,

      lt_ptreq_items      TYPE TABLE OF ptreq_items,
      ls_ptreq_items      LIKE LINE OF lt_ptreq_items,
      lt_ptreq_header     TYPE TABLE OF ptreq_header,
      ls_ptreq_header     LIKE LINE OF lt_ptreq_header,
      lt_ptreq_attabsdata TYPE TABLE OF ptreq_attabsdata,
      ls_ptreq_attabsdata LIKE LINE OF lt_ptreq_attabsdata,

      ls_abs              LIKE LINE OF ls_deep_entity-navabs,

      lv_need             TYPE flag,
      lv_str              TYPE string
    , lv_notice_text      TYPE string
    .


    CASE iv_entity_set_name.
      WHEN 'EntityMainSet'.

        DATA lo_message_container TYPE REF TO /iwbep/if_message_container.
        lo_message_container = mo_context->get_message_container( ).
        LOOP AT it_filter_select_options ASSIGNING FIELD-SYMBOL(<filter>).
          LOOP AT <filter>-select_options ASSIGNING FIELD-SYMBOL(<range>).
            lv_property = <filter>-property.
            TRANSLATE lv_property TO UPPER CASE.
            CASE lv_property.
              WHEN 'ENDDA'.
                lv_endda = <range>-low.
              WHEN 'BEGDA'.
                lv_begda = <range>-low.
*20/02/2020              WHEN 'ENTRANCEUSER'.
              WHEN 'MAINUSER'.
                lv_loginactdir = conv #( <range>-low ).
            ENDCASE.
          ENDLOOP.
        ENDLOOP.

        IF lv_endda IS INITIAL OR
           lv_begda IS INITIAL OR
           lv_begda > lv_endda.
          APPEND INITIAL LINE TO lt_return ASSIGNING FIELD-SYMBOL(<return>).
          <return>-type   = 'E'.
          <return>-id     = 'ZTEST'.
          <return>-number = '009'.
          <return>-message = 'Неверный период'.

          lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
            EXPORTING
              message_container = lo_message_container.

        ENDIF.

        IF lv_loginactdir IS INITIAL.
          APPEND INITIAL LINE TO lt_return ASSIGNING <return>.
          <return>-type = 'E'.
          <return>-id   = 'ZTEST'.
          <return>-number = '007'.
          <return>-message = 'Ошибка при передачи логина'.

          lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
            EXPORTING
              message_container = lo_message_container.
        ENDIF.

        DATA: lv_sprps TYPE p2001-sprps
            .

        TRANSLATE lv_loginactdir TO UPPER CASE.

*        SELECT SINGLE pernr
*          FROM pa0105
*          INTO lv_pernr
*          WHERE
*            subty = '0001' AND
*            usrid = lv_loginactdir.

        DATA(lo_assistent) = zcl_mss_data_assistent=>get_instance( ).

        lv_pernr = lo_assistent->get_pernr( iv_usrid = CONV #( lv_loginactdir ) ).

        IF lv_pernr IS INITIAL.
          CLEAR ls_return.
          ls_return-type       = 'E'.
          ls_return-id         = 'ZHR_PA'.
          ls_return-number     = '015'.
          ls_return-message_v1 = lv_loginactdir.
          APPEND ls_return TO lt_return.

          lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
            EXPORTING
              message_container = lo_message_container.
        ENDIF.

        ls_deep_entity-owner_number = lv_pernr.

        CALL METHOD cl_pt_arq_customizing=>get_tcale_attribs
          EXPORTING
            im_pernr            = lv_pernr
            im_date             = lv_begda
          IMPORTING
            ex_cache_validity   = lv_cache_validity
          EXCEPTIONS
            it0001_not_found    = 1
            it0008_not_found    = 2
            missing_customizing = 3
            OTHERS              = 4.
        IF sy-subrc NE 0.
          lv_cache_validity = 1.
        ENDIF.

        ls_team_id-viewgroup = 'MSS_LTV_EE'.
        ls_team_id-viewid = 'MSS_LTV_EE_DIR'.

        CALL FUNCTION 'PT_ARQ_TEAMCALE_GET' DESTINATION 'NONE'
          EXPORTING
            im_command           = 'UPDATE'
            im_pernr             = lv_pernr
            im_user              = lv_loginactdir
*           IM_LANGUAGE          = SY-LANGU
            im_team              = ls_team_id
            im_begda             = lv_begda
            im_endda             = lv_endda
            im_cache_validity    = lv_cache_validity
*           IM_GOTO_PAGE_NUMBER  = 1
*           IM_TIME_FORMAT       = 2
            im_modus             = 'T'
*           IM_DEBUG             =
            im_deactivate_check  = 'X'
            im_deactivate_paging = 'X'
          IMPORTING
            ex_team              = ls_team_id2
            ex_cache_date        = lv_cache_date
            ex_cache_time        = lv_cache_time
            ex_calendar_tab      = lt_calendar
            ex_rowlabel          = lt_rowlabel
            ex_position          = ls_position
          TABLES
            ex_team_tab          = lt_team
            ex_weekday_tab       = lt_weekday
            ex_legend_tab        = lt_legend
            ex_commands          = lt_com
            ex_custcolumns_tab   = lt_col
            ex_messages          = lt_mes.

* соберем список табельных
        CLEAR lt_row_id.
        LOOP AT lt_rowlabel INTO ls_rowlabel.
          ls_row_id-row_id = ls_rowlabel-row_id.
          COLLECT ls_row_id INTO lt_row_id.
        ENDLOOP.

* заполним таблицу
        LOOP AT lt_row_id INTO ls_row_id.

          CLEAR:
            ls_deep_entity, lt_ptreq_header.

          lv_pernr2 = ls_row_id-row_id.

          IF zcl_vacation_appl=>check_valid_pernr(
              i_pernr = lv_pernr2 ) EQ abap_false.
            CONTINUE.
          ENDIF.

          lv_name = zcl_vacation_appl=>get_pernr_name( iv_pernr = lv_pernr2 ).

          ls_deep_entity-owner_fio = lv_name.
          ls_deep_entity-owner_number = lv_pernr2.

          LOOP AT lt_calendar INTO ls_calendar WHERE row_id EQ ls_row_id-row_id.
            LOOP AT ls_calendar-request INTO ls_req.

              SELECT *
                INTO TABLE lt_ptreq_header
                FROM ptreq_header
                WHERE
                  request_id  EQ ls_req-request_id.

              IF sy-subrc EQ 0.

                SORT lt_ptreq_header BY version_no DESCENDING.
                READ TABLE lt_ptreq_header INTO ls_ptreq_header INDEX 1.

                CLEAR: lv_notice_text.
                IF sy-subrc EQ 0.
                  SELECT SINGLE notice_text FROM ptreq_notice
                    INTO lv_notice_text
                    WHERE request_id EQ ls_ptreq_header-request_id AND
                          version_no EQ ls_ptreq_header-version_no.
                ENDIF.

                SELECT *
                  INTO TABLE lt_ptreq_items
                  FROM ptreq_items
                  WHERE
                    item_list_id  EQ ls_ptreq_header-item_list_id.

                CHECK sy-subrc EQ 0.

                LOOP AT lt_ptreq_items INTO ls_ptreq_items.

                  SELECT SINGLE *
                    INTO ls_ptreq_attabsdata
                    FROM ptreq_attabsdata
                    WHERE
                      item_id EQ ls_ptreq_items-item_ins.

                  CHECK sy-subrc EQ 0.
                  "CHECK ls_ptreq_attabsdata-operation EQ 'INS'.
                  CLEAR ls_abs.
                  ls_abs-request_id   = ls_ptreq_header-request_id.
                  ls_abs-status       = ls_ptreq_header-status.
                  ls_abs-comment      = lv_notice_text.
                  ls_abs-owner_number = lv_pernr2.
                  ls_abs-begda        = ls_calendar-begda.
                  ls_abs-endda        = ls_calendar-endda.
                  ls_abs-abstype      = ls_ptreq_attabsdata-subty.
                  COLLECT ls_abs INTO ls_deep_entity-navabs.

                ENDLOOP.

              ELSE.

                lv_str = ls_req-tooltip.
                CONDENSE lv_str NO-GAPS.
                IF strlen( lv_str ) GE 21.
                  CLEAR ls_abs.
                  ls_abs-status = 'POSTED'.
                  ls_abs-begda = lv_str+6(4) && lv_str+3(2) && lv_str(2).
                  IF lv_str+10(1) EQ '-'.
                    ls_abs-endda = lv_str+17(4) && lv_str+14(2) && lv_str+11(2).
                  ELSE.
                    ls_abs-endda = ls_abs-begda.
                  ENDIF.

                  SELECT SINGLE subty sprps
                    INTO (ls_abs-abstype, lv_sprps)
                    FROM pa2001
                    WHERE
                      pernr EQ lv_pernr2    AND
                      begda EQ ls_abs-begda AND
                      endda EQ ls_abs-endda.
                  CHECK sy-subrc EQ 0 AND lv_sprps EQ abap_false.

                  ls_abs-owner_number = lv_pernr2.

                  COLLECT ls_abs INTO ls_deep_entity-navabs.
                ENDIF.
              ENDIF.

            ENDLOOP.
          ENDLOOP.

          APPEND ls_deep_entity TO lt_deep_entity.

        ENDLOOP.

***CALL FUNCTION 'PT_ARQ_TEAMCALE_GET'
***  EXPORTING
****   IM_COMMAND                 =
***    im_pernr                   =
****   IM_USER                    = SY-UNAME
****   IM_LANGUAGE                = SY-LANGU
***    im_team                    =
***    im_begda                   = lv_begda
***    im_endda                   = lv_endda
***    im_cache_validity          =
****   IM_GOTO_PAGE_NUMBER        = 1
****   IM_TIME_FORMAT             = 2
***    im_modus                   =
****   IM_DEBUG                   =
****   IM_DEACTIVATE_CHECK        =
****   IM_DEACTIVATE_PAGING       =
****   IM_ABAP_FLAG               =
**** IMPORTING
****   EX_TEAM                    =
****   EX_CACHE_DATE              =
****   EX_CACHE_TIME              =
****   EX_CALENDAR_TAB            =
****   EX_ROWLABEL                =
****   EX_POSITION                =
***  TABLES
***    ex_team_tab                =
***    ex_weekday_tab             =
***    ex_legend_tab              =
***    ex_commands                =
***    ex_custcolumns_tab         =
***    ex_messages                =
***          .


*{   DELETE         HRDK957127                                        3
*\****<<< ВРЕМЕННАЯ ЗАГЛУШКА
*\        ls_deep_entity-owner_fio = 'Иванов Иван Иванович'.
*\        ls_deep_entity-owner_number = '00000588'.
*\**        ls_deep_entity-endda = lv_endda.
*\**        ls_deep_entity-begda = lv_begda.
*\        APPEND INITIAL LINE TO ls_deep_entity-navabs ASSIGNING FIELD-SYMBOL(<abs>).
*\        <abs>-owner_number = '00000588'.
*\        <abs>-abstype = '0101'.
*\        <abs>-begda = '20170713'.
*\        <abs>-endda ='20170717'.
*\        APPEND INITIAL LINE TO ls_deep_entity-navabs ASSIGNING <abs>.
*\        <abs>-owner_number = '00000588'.
*\        <abs>-abstype = '0101'.
*\        <abs>-begda = '20170720'.
*\        <abs>-endda ='20170725'.
*\
*\        APPEND ls_deep_entity TO lt_deep_entity.
*\
*\        ls_deep_entity-owner_fio = 'Иванов Иван Иванович2'.
*\        ls_deep_entity-owner_number = '00000589'.
*\**        ls_deep_entity-endda = lv_endda.
*\**        ls_deep_entity-begda = lv_begda.
*\        APPEND INITIAL LINE TO ls_deep_entity-navabs ASSIGNING <abs>.
*\        <abs>-owner_number = '00000589'.
*\        <abs>-abstype = '0101'.
*\        <abs>-begda = '20170713'.
*\        <abs>-endda ='20170717'.
*\
*\        APPEND ls_deep_entity TO lt_deep_entity.
*\****>>>КОНЕЦ ЗАГЛУШКИ
*}   DELETE

        copy_data_to_ref(
                EXPORTING
                  is_data = lt_deep_entity
                CHANGING
                  cr_data = er_entityset ).
    ENDCASE.
  ENDMETHOD.


  METHOD add_msg.
    DATA: lt_return TYPE bapiret2_t.

    IF mo_mess IS INITIAL.
      mo_mess = mo_context->get_message_container( ).
    ENDIF.

    APPEND INITIAL LINE TO lt_return ASSIGNING FIELD-SYMBOL(<return>).
    <return>-type    = sy-msgty.
    <return>-id      = sy-msgid.
    <return>-number  = sy-msgno.
    <return>-message = mv_dummy.
    mo_mess->add_messages_from_bapi( it_bapi_messages = lt_return ).
  ENDMETHOD.


  method GET_HOLIDAYS.
    DATA: lt_p0001  TYPE ptt_p0001
        , lt_psp    TYPE ptpsp_tab
        , lt_dayint TYPE TABLE OF  pwsdayint
        , lt_daygen TYPE TABLE OF  pwsdaygen

        , lo_badi_additional_info TYPE REF TO hress_b_team_calendar_enh
        , lt_ftkla                TYPE TABLE OF ftkla
        .

    read_work_schedule(
      EXPORTING
        iv_begda  = i_begda                      " datum
        iv_endda  = i_endda                      " datum
        iv_pernr  = i_pernr                               " pernr_d
      IMPORTING
        et_p0001  = lt_p0001                              " ptt_p0001
        et_psp    = lt_psp                                 " ptpsp_tab
        et_dayint = lt_dayint
        et_daygen = lt_daygen ).

    GET BADI lo_badi_additional_info.
    CALL BADI lo_badi_additional_info->set_disp_public_holidays
      CHANGING
        im_ftkla = lt_ftkla.
    LOOP AT lt_psp ASSIGNING FIELD-SYMBOL(<fs_psp>) WHERE ftkla EQ 0.
      READ TABLE lt_daygen ASSIGNING FIELD-SYMBOL(<fs_daygen>)
        WITH KEY datum = <fs_psp>-datum.
      IF sy-subrc EQ 0 AND <fs_daygen>-sollz IS INITIAL.    "LAK2057053
*       выходной
        APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<fs_out>).
        <fs_out>-type = c_free.
        <fs_out>-text = cl_wd_utilities=>get_otr_text_by_alias( alias = 'PAOC_ESS_WDA_OTR/NON_WORKDAY' ).
        <fs_out>-begda = <fs_psp>-datum.
        <fs_out>-endda = <fs_psp>-datum.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_psp ASSIGNING <fs_psp> WHERE ftkla NE 0.
      READ TABLE lt_ftkla WITH KEY table_line = <fs_psp>-ftkla TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 0.
        APPEND INITIAL LINE TO et_entityset ASSIGNING <fs_out>.
        <fs_out>-type = c_holiday.  " праздник
        <fs_out>-text = cl_hress_ptarq_tim_services=>get_public_holiday_description(
                                   iv_pernr = i_pernr
                                   iv_date = <fs_psp>-datum ).
        IF <fs_out>-text IS INITIAL.
          <fs_out>-type = c_free. " выходной
          <fs_out>-text = cl_wd_utilities=>get_otr_text_by_alias( alias = 'PAOC_ESS_WDA_OTR/NON_WORKDAY' ).
        ENDIF.
        <fs_out>-begda = <fs_psp>-datum.
        <fs_out>-endda = <fs_psp>-datum.
      ELSE.
        READ TABLE lt_daygen ASSIGNING <fs_daygen>
          WITH KEY datum = <fs_psp>-datum.
        IF sy-subrc EQ 0 AND <fs_daygen>-sollz IS INITIAL.
          APPEND INITIAL LINE TO et_entityset ASSIGNING <fs_out>.
          <fs_out>-type = c_free.
          <fs_out>-text = cl_wd_utilities=>get_otr_text_by_alias( alias = 'PAOC_ESS_WDA_OTR/NON_WORKDAY' ).
          <fs_out>-begda = <fs_psp>-datum.
          <fs_out>-endda = <fs_psp>-datum.
        ENDIF.
      ENDIF.
    ENDLOOP.

    SORT et_entityset BY begda.
  endmethod.


  METHOD holidayset_get_entityset.
*   логика взята из HRESS_C_LEA_TEAM_CALENDAR ракурс V_TEAM_CALENDAR метод BIND_CALENDAR_DATA_NEW

    DATA: lv_login  TYPE zlogin_act_dir
        , lv_begda  TYPE begda
        , lv_endda  TYPE endda
        , lv_pernr  TYPE persno
        .
    DATA(lo_hr_mss) = NEW zcl_mss_data_assistent( ).

    LOOP AT it_filter_select_options ASSIGNING FIELD-SYMBOL(<filter>).
      READ TABLE <filter>-select_options INDEX 1 ASSIGNING FIELD-SYMBOL(<range>).
      CHECK sy-subrc EQ 0.
      CASE <filter>-property.
*20/02/2020        WHEN 'EntranceUser'.
*          DATA(lv_entranceuser) = CONV string( <range>-low ).
        WHEN 'MainUser'.
          DATA(lv_mainuser) = CONV string( <range>-low ).
        WHEN 'begda'.
          lv_begda = <range>-low.
        WHEN 'endda'.
          lv_endda = <range>-low.
      ENDCASE.
    ENDLOOP.

    IF lv_begda IS INITIAL.
      MESSAGE e003(zhr_ess) WITH 'begda' INTO mv_dummy.
      add_msg( ).
      raise_exception( ).
    ENDIF.

    IF lv_endda IS INITIAL.
      MESSAGE e003(zhr_ess) WITH 'endda' INTO mv_dummy.
      add_msg( ).
      raise_exception( ).
    ENDIF.

*20/02/2020    TRANSLATE lv_entranceuser TO UPPER CASE.
*    lv_pernr = lo_hr_mss->get_pernr( iv_usrid = CONV #( lv_entranceuser ) ).
    TRANSLATE lv_mainuser TO UPPER CASE.
    lv_pernr = lo_hr_mss->get_pernr( iv_usrid = CONV #( lv_mainuser ) ).
    IF lv_pernr IS INITIAL.
      MESSAGE e001(zhr_ess) INTO mv_dummy.
      add_msg( ).
      raise_exception( ).
    ENDIF.

    get_holidays(
      EXPORTING
        i_pernr      = lv_pernr    " Табельный номер
        i_begda      = lv_begda    " Начало срока действия
        i_endda      = lv_endda    " Конец срока действия
      IMPORTING
        et_entityset = et_entityset ).
  ENDMETHOD.


  method RAISE_EXCEPTION.
  endmethod.


  METHOD read_work_schedule.
    DATA: lt_p0000  TYPE TABLE OF p0000,
          lt_p0001  TYPE TABLE OF p0001,
          lt_p0002  TYPE TABLE OF p0002,
          lt_p0007  TYPE TABLE OF p0007,
          lt_p2001  TYPE TABLE OF p2001,
          lt_p2002  TYPE TABLE OF p2002,
          lt_p2003  TYPE TABLE OF p2003,
          lt_psp    TYPE TABLE OF ptpsp,
          lt_daygen TYPE TABLE OF pwsdaygen,
          lt_dayint TYPE TABLE OF pwsdayint,
          lt_pernr  TYPE          tim_tmw_pernr_d_tab,
          lt_perws  TYPE          ptm_psp,
          ls_dayint TYPE          pwsdayint.

    DATA rdclust TYPE rdclst.
*  DATA ls_msgtext          TYPE string.
*  DATA constraints         TYPE ptarq_tconstr.                   "INS Note 1721171
*
*  CALL METHOD cl_pt_arq_customizing=>get_time_constraints        "INS Note 1721171
*    EXPORTING                                                    "INS Note 1721171
*      im_pernr       = wd_this->gv_pernr                         "INS Note 1721171
*      im_date        = sy-datum                                  "INS Note 1721171
*    IMPORTING                                                    "INS Note 1721171
*      ex_constraints = constraints                               "INS Note 1721171
*    EXCEPTIONS                                                   "INS Note 1721171
*      OTHERS         = 1.                                        "INS Note 1721171

    CALL FUNCTION 'HR_READ_INFOTYPE_AUTHC_DISABLE'.
    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
*       TCLAS           = 'A'
        pernr           = iv_pernr
        infty           = '0000'
*       BEGDA           = '18000101'
*       ENDDA           = '99991231'
*       BYPASS_BUFFER   = ' '
*       LEGACY_MODE     = ' '
* IMPORTING
*       SUBRC           =
      TABLES
        infty_tab       = lt_p0000
      EXCEPTIONS
        infty_not_found = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      MESSAGE e050(rp) WITH '0000' iv_pernr space space
         INTO mv_dummy.
      RETURN.
    ENDIF.

    CALL FUNCTION 'HR_READ_INFOTYPE_AUTHC_DISABLE'.
    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        pernr           = iv_pernr
        infty           = '0001'
      TABLES
        infty_tab       = lt_p0001
      EXCEPTIONS
        infty_not_found = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      MESSAGE e050(rp) WITH '0001' iv_pernr space space
         INTO mv_dummy.
      RETURN.
    ENDIF.

    CALL FUNCTION 'HR_READ_INFOTYPE_AUTHC_DISABLE'.
    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        pernr           = iv_pernr
        infty           = '0002'
      TABLES
        infty_tab       = lt_p0002
      EXCEPTIONS
        infty_not_found = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      MESSAGE e050(rp) WITH '0002' iv_pernr space space
         INTO mv_dummy.
      RETURN.
    ENDIF.

    CALL FUNCTION 'HR_READ_INFOTYPE_AUTHC_DISABLE'.
    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        pernr           = iv_pernr
        infty           = '0007'
      TABLES
        infty_tab       = lt_p0007
      EXCEPTIONS
        infty_not_found = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      MESSAGE e050(rp) WITH '0007' iv_pernr space space
         INTO mv_dummy.
      RETURN.
    ENDIF.

    CALL FUNCTION 'HR_READ_INFOTYPE_AUTHC_DISABLE'.
    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        pernr           = iv_pernr
        infty           = '2003'
      TABLES
        infty_tab       = lt_p2003
      EXCEPTIONS
        infty_not_found = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    rdclust = ' '.

    CALL FUNCTION 'HR_PERSONAL_WORK_SCHEDULE'
      EXPORTING
        pernr             = iv_pernr
        begda             = iv_begda
        endda             = iv_endda
*       KUG               =
*       REFRESH           = 'X'
*       WORKING_HOURS     = 'X'
        switch_activ      = 0
        i0001_i0007_error = '0'
        read_cluster      = rdclust
*    IMPORTING
*       WARNING_OCCURED   =
      TABLES
        i0000             = lt_p0000
        i0001             = lt_p0001
        i0002             = lt_p0002
        i0007             = lt_p0007
*       I0049             =
        i2001             = lt_p2001
        i2002             = lt_p2002
        i2003             = lt_p2003
        perws             = lt_psp
      EXCEPTIONS
        error_occured     = 1
        abort_occured     = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
*    call function 'HR_DISPLAY_ERROR_LIST'
*         EXPORTING
*              no_popup         = ' '
*              no_print         = 'X'
*              no_img           = ' '
*         EXCEPTIONS
*              invalid_linesize = 1
*              others           = 2.
**********************Begin of LAK1709799**********************
    ELSE.
*   Get the Work Schedule times.
      CALL FUNCTION 'HR_WORK_SCHEDULE_TIMES'
        EXPORTING
          pernr                   = iv_pernr
          begda                   = iv_begda
          endda                   = iv_endda
          kug                     = abap_false
          break_overtime          = abap_false
          refresh_infotype_buffer = abap_false
        TABLES
*         i0001                   = p0001
*         i0007                   = p0007
*         I0049                   = p0049
*         i2003                   = p2003
          i0001                   = lt_p0001
          i0007                   = lt_p0007
*         I0049                   = INFOTYPE_0000_TAB
          i2003                   = lt_p2003
          perws                   = lt_psp
          daygen                  = lt_daygen
          dayint                  = lt_dayint
        EXCEPTIONS
          error_occured           = 1
          perws_error             = 2
          OTHERS                  = 3.
**********************End of LAK1709799*******************
    ENDIF.

    et_psp = lt_psp.
    et_dayint = lt_dayint.                                  "LAK1790799
    et_p0001 = lt_p0001.                                    "LAK1709799
    et_daygen = lt_daygen.                                  "LAK2057073
  ENDMETHOD.
ENDCLASS.

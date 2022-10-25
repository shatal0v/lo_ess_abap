class ZCL_VACATION_APPL definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_massn_data,
      subty TYPE subty,
      edq_begda TYPE begda,
      edq_endda TYPE endda,
      begda     TYPE begda,  " первый период
      endda     TYPE endda,
      begda_2   TYPE begda,  " второй период
      endda_2   TYPE endda,
    END OF ty_massn_data .
  types:
    BEGIN OF ty_time_types,
        type TYPE ptarq_uia_timetype,
      END OF ty_time_types .
  types:
    tt_time_types TYPE HASHED TABLE OF ty_time_types WITH UNIQUE KEY type .

  class-data:
    MR_KTART TYPE RANGE OF p2006-ktart .
  constants C_MEMORY_RR_ID type TEXT20 value 'ZMASSN_DATA_RR' ##NO_TEXT.
  class-data MV_DUMMY type STRING .

  class-methods CLASS_CONSTRUCTOR .
  class-methods GET_REQ_HEADER_DATA
    importing
      !IV_REQUEST_ID type TIM_REQ_ID
    exporting
      !ES_REQUEST type PTREQ_HEADER .
  class-methods GET_PERNR_NAME
    importing
      !IV_PERNR type PERNR_D
      !IV_DATUM type SY-DATUM default SY-DATUM
    returning
      value(EV_NAME) type STRING .
  class-methods IS_NEED_0105_CHECK
    importing
      !IV_LOGIN type ZLOGIN_ACT_DIR
    returning
      value(RV_NEED) type FLAG .
  class-methods GET_PERNR_BY_AD
    importing
      !I_AD type ANY
    returning
      value(R_PERNR) type PERSNO .
  class-methods GET_REQLIST
    importing
      !I_PERNR type PERSNO
      !I_BEGDA type BEGDA
      !I_ENDDA type ENDDA
    exporting
      !ET_REQ type PTARQ_UIA_REQLIST_TAB
      !ET_MESSAGES type PTARQ_UIA_MESSAGES_TAB .
  class-methods GET_TIMETYPES
    importing
      !I_SUBTY type SUBTY
    exporting
      !ET_TYPES type TT_TIME_TYPES .
  class-methods CHANGE_INFOGROUP
    importing
      !I_MASSN type MASSN
      !I_MASSG type MASSG
    changing
      !CT_INFOGR type HRPADRU_INFOGROUP .
  class-methods FIX_UPPERCASE_FIO
    changing
      !CV_FIO type ANY .
  class-methods DETERMINE_PERNR
    importing
      !I_AD type ANY
      !I_OWNER_NUMBER type ANY
    returning
      value(R_PERNR) type PERSNO .
  class-methods GET_REQUESTS_WITH_DIVIDED
    importing
      !I_REQ_ID type TIM_REQ_ID
    exporting
      !ET_REQ type PTREQ_REQ_GUID_32_TAB
      !E_REQ type TIM_REQ_ID .
  class-methods GET_OLD_VACATION
    importing
      !I_PERNR type PERSNO
      !I_BEGDA_NEW type BEGDA
    returning
      value(R_BEGDA_OLD) type BEGDA .
  class-methods FIND_RUNNING_DEL_VACATION
    importing
      !I_PERNR type PERSNO
      !I_SUBTY type SUBTY
      !I_BEGDA type BEGDA
      !I_ENDDA type ENDDA
      !I_REQ_ID type TIM_REQ_ID optional
    exporting
      !E_FND type FLAG
      !E_REQ_ID type TIM_REQ_ID .
  class-methods GET_ATTABS
    importing
      !I_REQ_ID type TIM_REQ_ID
    exporting
      !ES_DATA type PTREQ_ATTABSDATA .
  class-methods CHECK_QUOTA_DEDUCE
    importing
      !I_PERNR type PERSNO
      !I_SUBTY type SUBTY
      !I_BEGDA type BEGDA
      !I_ENDDA type ENDDA
    exporting
      !E_OK type FLAG .
  class-methods FILL_KTART .
  class-methods GET_EMAIL
    importing
      !I_PERNR type PERSNO
      !I_BEGDA type BEGDA default SY-DATUM
      !I_ENDDA type ENDDA default SY-DATUM
    returning
      value(R_EMAIL) type STRING .
  class-methods GET_LIMIT_DAYS
    importing
      !I_PERNR type PERSNO
      !I_DATE type DATS
      !I_KTART type P2006-KTART
    exporting
      !E_LIMIT type ANY .
  class-methods CHECK_VALID_PERNR
    importing
      !I_PERNR type PERSNO
      !I_DATE type DATS default SY-DATUM
    returning
      value(R_OK) type FLAG .
protected section.
private section.
ENDCLASS.



CLASS ZCL_VACATION_APPL IMPLEMENTATION.


  METHOD CHANGE_INFOGROUP.
    DATA: ls_infgrp     TYPE pitgr
        , ls_massn_data TYPE ty_massn_data
        .

*   при проведении мероприятия 2001 ИТ уже создан
    IF i_massn EQ 'R3'.
      DELETE ct_infogr
        WHERE opera EQ 'DIS'.
      DELETE ct_infogr
        WHERE infty EQ '2001' AND
            ( opera EQ 'INSS' OR opera EQ 'INS' ).
    ENDIF.

    IF i_massn EQ 'RR'.
      IMPORT ls_massn_data TO ls_massn_data FROM MEMORY ID c_memory_rr_id.
*     мероприятие состоит из блокирования старого отпуска и вставки 2-х новых отпусков
      DELETE ct_infogr WHERE opera EQ 'DIS' OR opera EQ 'EDQ'.
*      READ TABLE ct_infogr ASSIGNING FIELD-SYMBOL(<fs_infgrp>)
*        WITH KEY opera = 'EDQ' infty = '2001'.
*      IF sy-subrc EQ 0.
*        <fs_infgrp>-begst = ls_massn_data-edq_begda.
*        <fs_infgrp>-endst = ls_massn_data-edq_endda.
*        <fs_infgrp>-subty = ls_massn_data-subty.
*      ENDIF.

      READ TABLE ct_infogr ASSIGNING FIELD-SYMBOL(<fs_infgrp>)
        WITH KEY opera = 'INSS' infty = '2001'.
      IF sy-subrc EQ 0.
        <fs_infgrp>-begst = ls_massn_data-begda.
        <fs_infgrp>-endst = ls_massn_data-endda.
        <fs_infgrp>-subty = ls_massn_data-subty.

        IF ls_massn_data-begda_2 IS NOT INITIAL.
          ls_infgrp = <fs_infgrp>.
          ls_infgrp-begst = ls_massn_data-begda_2.
          ls_infgrp-endst = ls_massn_data-endda_2.
          INSERT ls_infgrp INTO ct_infogr INDEX sy-tabix + 1.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD CHECK_QUOTA_DEDUCE.
*   взято из RPUQTA00
    DATA: lt_perws    TYPE TABLE OF ptpsp
        , lt_tpd      TYPE TABLE OF ptm_times_per_day
        , lt_p0001    TYPE TABLE OF p0001
        , lt_p2006    TYPE TABLE OF p2006

        , lt_b2006x   TYPE TABLE OF p2006x
        , lt_b2007x   TYPE TABLE OF p2007x

        , ls_p2001    TYPE p2001
        , lt_bquoded  TYPE TABLE OF ptquoded
        .

    e_ok = abap_true.
    CALL FUNCTION 'HR_PERSONAL_WORK_SCHEDULE'
      EXPORTING
        pernr          = i_pernr
        begda          = i_begda
        endda          = i_endda
        modify_entries = ' '
      TABLES
        perws          = lt_perws
      EXCEPTIONS
        error_occured  = 1
        abort_occured  = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CALL FUNCTION 'HR_READ_SUBTYPE'
      EXPORTING
        pernr           = i_pernr
        infty           = '0001'
        begda           = i_begda
        endda           = i_endda
        no_auth_check   = 'X'
      TABLES
        infty_tab       = lt_p0001
      EXCEPTIONS
        infty_not_found = 1
        invalid_input   = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    ls_p2001-awart = i_subty.
    CALL FUNCTION 'HR_ABS_ATT_TIMES'
      EXPORTING
        pernr             = i_pernr
        awart             = ls_p2001-awart
        begda             = i_begda
        endda             = i_endda
        beguz             = ls_p2001-beguz
        enduz             = ls_p2001-enduz
        vtken             = ls_p2001-vtken
*       NXDFL             =
        stdaz             = ls_p2001-stdaz
        alldf             = ls_p2001-alldf
*       CALC_FROM         = 00000000
*       CALC_TO           = 00000000
*       MANUAL_LEAVE      =
      TABLES
        m0001             = lt_p0001
        pws               = lt_perws
        times_per_day     = lt_tpd
      EXCEPTIONS
        it0001_missing    = 1
        customizing_error = 2
        error_occurred    = 3
        end_before_begin  = 4
        missing_pws       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        pernr           = i_pernr
        infty           = '2006'
        bypass_buffer   = abap_true
      TABLES
        infty_tab       = lt_p2006
      EXCEPTIONS
        infty_not_found = 1
        OTHERS          = 2.

    LOOP AT lt_p2006 ASSIGNING FIELD-SYMBOL(<t_p2006>).
      APPEND INITIAL LINE TO lt_b2006x ASSIGNING FIELD-SYMBOL(<t_b2006x>).
      MOVE-CORRESPONDING <t_p2006> TO <t_b2006x>.
    ENDLOOP.

    CALL FUNCTION 'HR_DEDUCE_QUOTA'
      EXPORTING
        pernr            = ls_p2001-pernr
        infty            = ls_p2001-infty
        awart            = ls_p2001-awart
        docnr            = ls_p2001-docnr
      TABLES
        times_per_day    = lt_tpd "<times_per_day>
        t2006x           = lt_b2006x
        t2007x           = lt_b2007x
        tquoded          = lt_bquoded
      EXCEPTIONS
        error_occurred   = 1
        zero_deduction   = 2
        wrong_infty      = 3
        not_enough_quota = 4.

    DELETE ADJACENT DUPLICATES FROM lt_bquoded COMPARING quonr.

    IF lines( lt_bquoded ) EQ 1.
      READ TABLE lt_bquoded ASSIGNING FIELD-SYMBOL(<t_bquoded>) INDEX 1.
      IF sy-subrc EQ 0.
        IF mr_ktart IS INITIAL.
          fill_ktart( ).
        ENDIF.

        READ TABLE lt_b2006x ASSIGNING <t_b2006x> WITH KEY quonr = <t_bquoded>-quonr.
        IF sy-subrc EQ 0 AND <t_b2006x>-ktart IN mr_ktart.
          IF <t_b2006x>-kverb < <t_b2006x>-anzhl.
            MESSAGE e001(zhr_pa) WITH 'Отсутствие должно полностью расходовать лимит'(001)
              INTO mv_dummy.
            CLEAR: e_ok.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD CHECK_VALID_PERNR.
*   Из списка нужно исключить работников:
*  1.       Работники с группой 6 «Несписочный состав»
*  2.       Работники со статусом занятости 1 «приостановлено»
*  3.       С нулевым процентом занятости на штатной должности на сегодня (отпуск по БИР)
    DATA: lt_p0001 TYPE TABLE OF p0001
        , lt_p0000 TYPE TABLE OF p0000
        , lv_prozt TYPE p1001-prozt
        .

    zcl_hr_data=>read_pa_infty(
      EXPORTING
        i_pernr         = i_pernr
        i_infty         = '0001'
        i_begda         = i_date
        i_endda         = i_date
      IMPORTING
        e_pnnnn         = lt_p0001 ).
    READ TABLE lt_p0001 INDEX 1 ASSIGNING FIELD-SYMBOL(<fs_p0001>).
    IF sy-subrc EQ 0 AND <fs_p0001>-persg EQ '6'.
      RETURN.
    ENDIF.

    zcl_hr_data=>read_pa_infty(
      EXPORTING
        i_pernr         = i_pernr
        i_infty         = '0000'
        i_begda         = i_date
        i_endda         = i_date
      IMPORTING
        e_pnnnn         = lt_p0000 ).
    READ TABLE lt_p0000 INDEX 1 ASSIGNING FIELD-SYMBOL(<fs_p0000>).
    IF sy-subrc EQ 0 AND <fs_p0000>-stat2 EQ '1'.
      RETURN.
    ENDIF.

    SELECT SINGLE prozt FROM hrp1001
      INTO lv_prozt
      WHERE otype EQ 'P'      AND
            objid EQ i_pernr  AND
            plvar EQ '01'     AND
            rsign EQ 'B'      AND
            relat EQ '008'    AND
            istat EQ '1'      AND
            begda <= i_date   AND
            endda >= i_date   AND
            sclas EQ 'S'.
    IF lv_prozt EQ 0.
      RETURN.
    ENDIF.

    r_ok = abap_true.
  ENDMETHOD.


  METHOD CLASS_CONSTRUCTOR.
    fill_ktart( ).
  ENDMETHOD.


  METHOD DETERMINE_PERNR.
    DATA: lv_login TYPE zlogin_act_dir
        .
    lv_login = i_ad.
    DATA(lv_need) = zcl_vacation_appl=>is_need_0105_check( iv_login = lv_login ).
    r_pernr = get_pernr_by_ad( i_ad = i_ad ).

    IF r_pernr IS INITIAL AND
        lv_need EQ abap_false.
      r_pernr = i_owner_number.
    ENDIF.
  ENDMETHOD.


  METHOD FILL_KTART.
    DATA: ls_t7rurptcst00 TYPE t7rurptcst00
        , lt_str          TYPE stringtab
        .

    SELECT SINGLE contn FROM t7rurptcst00
      INTO ls_t7rurptcst00-contn
      WHERE progr EQ 'MP200000' AND
            param EQ 'ZLIM'.

    CHECK sy-subrc EQ 0.

    SPLIT ls_t7rurptcst00-contn AT space INTO TABLE lt_str.

    LOOP AT lt_str ASSIGNING FIELD-SYMBOL(<fs_str>).
      APPEND INITIAL LINE TO mr_ktart ASSIGNING FIELD-SYMBOL(<r_ktart>).
      <r_ktart>-sign   = 'I'.
      <r_ktart>-option = 'EQ'.
      <r_ktart>-low    = <fs_str>.
    ENDLOOP.
  ENDMETHOD.


  METHOD FIND_RUNNING_DEL_VACATION.
    DATA: BEGIN OF ls_item
        ,   item_ins TYPE ptreq_items-item_ins
        , END OF ls_item
        , lt_items_ins LIKE TABLE OF ls_item
        .

    CLEAR: e_fnd, e_req_id.
    SELECT item_id FROM ptreq_attabsdata
      INTO TABLE @DATA(lt_items)
      WHERE operation EQ 'DEL' AND
            infotype  EQ '2001' AND
            pernr EQ @i_pernr AND
            subty EQ @i_subty AND
            endda EQ @i_endda AND
            begda EQ @i_begda.
    LOOP AT lt_items ASSIGNING FIELD-SYMBOL(<fs_items>).
      ls_item-item_ins = <fs_items>-item_id.
      APPEND ls_item TO lt_items_ins.
    ENDLOOP.

    CHECK lt_items_ins IS NOT INITIAL.
    SELECT request_id, version_no, status FROM ptreq_items AS a1
      INNER JOIN ptreq_header AS a2 ON a1~item_list_id = a2~item_list_id
      INTO TABLE @DATA(lt_req)
      FOR ALL ENTRIES IN @lt_items_ins
      WHERE item_ins EQ @lt_items_ins-item_ins.

    SORT lt_req BY request_id version_no DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_req COMPARING request_id.

    LOOP AT lt_req ASSIGNING FIELD-SYMBOL(<fs_req>)
        WHERE status EQ 'SENT' AND request_id NE i_req_id.
      e_fnd = abap_true.
      e_req_id = <fs_req>-request_id.
      RETURN.
    ENDLOOP.
  ENDMETHOD.


  METHOD FIX_UPPERCASE_FIO.
    DEFINE add_str.
      IF &1 is INITIAL.
        &1 = &2.
      else.
        &1 = &1 && ` ` && &2.
      ENDIF.
    END-OF-DEFINITION.

    DATA: lt_str TYPE TABLE OF text100.

    SPLIT cv_fio AT space INTO TABLE lt_str.
    CLEAR: cv_fio.
    LOOP AT lt_str ASSIGNING FIELD-SYMBOL(<fs_str>).
      TRANSLATE <fs_str> TO LOWER CASE.
      TRANSLATE <fs_str>(1) TO UPPER CASE.
      add_str cv_fio <fs_str>.
    ENDLOOP.
  ENDMETHOD.


  METHOD GET_ATTABS.
    DATA: lt_ptreq_header TYPE TABLE OF ptreq_header
        , ls_ptreq_header LIKE LINE OF lt_ptreq_header
        , lv_item_id      TYPE ptreq_attabsdata-item_id
        .

    CLEAR es_data.
    SELECT *
      INTO TABLE lt_ptreq_header
      FROM ptreq_header
      WHERE
        request_id  EQ i_req_id.

    CHECK sy-subrc EQ 0.

    SORT lt_ptreq_header BY version_no DESCENDING.
    READ TABLE lt_ptreq_header INTO ls_ptreq_header INDEX 1.

    SELECT SINGLE item_ins FROM ptreq_items
      INTO @DATA(lv_item_ins)
      WHERE item_list_id EQ @ls_ptreq_header-item_list_id.
    CHECK sy-subrc EQ 0.

    SELECT SINGLE * FROM ptreq_attabsdata
      INTO es_data
      WHERE item_id EQ lv_item_ins.
  ENDMETHOD.


  METHOD GET_EMAIL.
    DATA: lv_usrid_long TYPE pa0105-usrid_long
        .

    SELECT SINGLE usrid_long FROM pa0105
      INTO lv_usrid_long
      WHERE pernr EQ i_pernr
        AND subty EQ '0010'
        AND begda <= i_endda
        AND endda >= i_begda.
    IF sy-subrc EQ 0.
      r_email = lv_usrid_long.
    ENDIF.
  ENDMETHOD.


  METHOD GET_LIMIT_DAYS.
    TYPES: BEGIN OF ty_line
         , pernr     TYPE pernr_d
         , fio       TYPE text255
         , orgehtxt  TYPE text255
         , orgeh     TYPE p0001-orgeh
         , planstxt  TYPE text255
         , plans     TYPE p0001-plans
         , dtype     TYPE text20
         , pdate     TYPE text10
         , anzhlm    TYPE anzhl
         , anzhlo    TYPE anzhl
         , anzhlmtxt TYPE text10
         , anzhlotxt TYPE text10
         , anzhl_01  TYPE anzhl
         , anzhl_02  TYPE anzhl
         , anzhl_03  TYPE anzhl
         , anzhl_04  TYPE anzhl
         , anzhl_05  TYPE anzhl
         , anzhl_10  TYPE anzhl
         , anzhl_01txt TYPE text10
         , anzhl_02txt TYPE text10
         , anzhl_03txt TYPE text10
         , anzhl_04txt TYPE text10
         , anzhl_05txt TYPE text10
         , anzhl_10txt TYPE text10
         , END OF ty_line
         .
    DATA: lt_line TYPE TABLE OF ty_line
        .

    CLEAR: e_limit.
    SUBMIT zhr_pa014
      WITH pnptimed EQ 'K'
      WITH pnpbegda EQ i_date
      WITH pnppernr EQ i_pernr
      WITH p_norm   EQ abap_true
      WITH s_ktart  EQ i_ktart
      WITH p_explim EQ abap_true
      AND RETURN.
    IMPORT t_line TO lt_line FROM MEMORY ID 'ZLIM_PA_014'.
    READ TABLE lt_line INDEX 1 ASSIGNING FIELD-SYMBOL(<fs_line>).
    CHECK sy-subrc EQ 0.

    IF <fs_line>-anzhlmtxt IS NOT INITIAL.
      e_limit = <fs_line>-anzhlmtxt.
    ELSEIF <fs_line>-anzhlotxt IS NOT INITIAL.
      e_limit = <fs_line>-anzhlotxt.
    ENDIF.
  ENDMETHOD.


  METHOD GET_OLD_VACATION.
    SELECT SINGLE pa2001~begda FROM zthr_pa_n160 AS a1
      INNER JOIN pa2001 ON a1~docnr_old = pa2001~docnr
      INTO r_begda_old
      WHERE a1~pernr EQ i_pernr AND
            a1~begda_new EQ i_begda_new.
  ENDMETHOD.


  method GET_PERNR_BY_AD.
    SELECT SINGLE pernr
      FROM pa0105
      INTO r_pernr
      WHERE subty = '0001'
        AND usrid = i_ad.
  endmethod.


  METHOD GET_PERNR_NAME.

    DATA:
          ls_0002 TYPE pa0002.


    CLEAR ev_name.

    SELECT SINGLE *
      INTO ls_0002
      FROM pa0002
      WHERE
        pernr EQ iv_pernr AND
        begda LE iv_datum AND
        endda GE iv_datum.

    CHECK sy-subrc EQ 0.
    IF ls_0002-nachn IS NOT INITIAL.
      TRANSLATE ls_0002-nachn    TO LOWER CASE.
      TRANSLATE ls_0002-nachn(1) TO UPPER CASE.
    ENDIF.

    IF ls_0002-vorna IS NOT INITIAL.
      TRANSLATE ls_0002-vorna    TO LOWER CASE.
      TRANSLATE ls_0002-vorna(1) TO UPPER CASE.
    ENDIF.

    IF ls_0002-midnm IS NOT INITIAL.
      TRANSLATE ls_0002-midnm    TO LOWER CASE.
      TRANSLATE ls_0002-midnm(1) TO UPPER CASE.
    ENDIF.

    CONCATENATE ls_0002-nachn ls_0002-vorna ls_0002-midnm INTO ev_name SEPARATED BY space.

*    TRANSLATE ev_name TO UPPER CASE.
    CONDENSE ev_name.

  ENDMETHOD.


  METHOD get_reqlist.
*   inspired by PT_ARQ_REQLIST_GET

    INCLUDE: rptreq_const
           , rptarq_const
           .
    TYPES: BEGIN OF calendar_data_struc,
              pernr                 TYPE p_pernr,
              request_id            TYPE tim_req_id,
              request_or_attabs     TYPE pt_arq_request_or_att_abs,
              uia_operation         TYPE ioper,
              status                TYPE tim_req_status,
              status_text           TYPE val_text,
              infotype              TYPE infty,
              subty                 TYPE subty,
              begda                 TYPE begda,
              endda                 TYPE endda,
              begtm                 TYPE begtm,
              endtm                 TYPE endtm,
           END OF calendar_data_struc
         , calendar_data_tab     TYPE TABLE OF calendar_data_struc
         .
    DATA: status_range  TYPE rseloption
        , reqlist       TYPE REF TO cl_pt_arq_reqs_list
        , pernr_tab     TYPE ptim_pernr_tab
        , lt_req_flat   TYPE ptarq_reqlist_tab_flat
        , status        TYPE LINE OF rseloption
        , uia_work_tab  TYPE calendar_data_tab
        , lv_debug      TYPE flag VALUE abap_false
        .

*   get instance of the request list
    CALL METHOD cl_pt_arq_reqs_list=>instance_get
      RECEIVING
        result = reqlist.

*    IF refresh = c_true.
*      CALL METHOD reqlist->refresh_reqlist.
*    ENDIF.

*   required status
    status-sign = 'I'.
    status-option = 'EQ'.
    status-low = c_req_sent.
    status-high = ''.
    APPEND status TO status_range.

    status-sign = 'I'.
    status-option = 'EQ'.
    status-low = c_req_approved.
    status-high = ''.
    APPEND status TO status_range.

    status-sign = 'I'.
    status-option = 'EQ'.
    status-low = c_req_rejected.
    status-high = ''.
    APPEND status TO status_range.

    status-sign = 'I'.
    status-option = 'EQ'.
    status-low = c_req_error.
    status-high = ''.
    APPEND status TO status_range.

    status-sign = 'I'.
    status-option = 'EQ'.
    status-low = c_req_posted.
    status-high = ''.
    APPEND status TO status_range.

* just a single employee
    APPEND i_pernr TO pernr_tab.
    CALL METHOD reqlist->sel_reqs_attsabs_for_owner
      EXPORTING
        im_status_range    = status_range
        im_pernr_tab       = pernr_tab
        im_selection_begin = i_begda
        im_selection_end   = i_endda
        im_read_it_data    = 'X'
      IMPORTING
        ex_all_my_reqs     = lt_req_flat.

    PERFORM convert_reqlist IN PROGRAM saplpt_arq_request_uia
          USING
            lt_req_flat
          CHANGING
            uia_work_tab.
    PERFORM filter_leave_overview IN PROGRAM saplpt_arq_request_uia
          USING
            i_pernr
          CHANGING
            uia_work_tab
            lt_req_flat.


    DELETE lt_req_flat
    WHERE request_or_attabs EQ c_type_request
      AND ( version-status EQ c_req_withdrawn OR
            version-status EQ c_req_stopped OR
            version-status EQ c_req_posted ).

* keine Abwesenheiten anzeigen, falls dazu ein Antrag existiert,
* der gesendet ist
    DELETE lt_req_flat
      WHERE ( request_or_attabs EQ c_type_absence  OR
              request_or_attabs EQ c_type_attendance )
        AND ( version-status EQ c_req_new       OR
              version-status EQ c_req_draft     OR
              version-status EQ c_req_sent      OR
              version-status EQ c_req_approved  OR
*            version-status EQ c_req_rejected  OR          "DEL NOTE 863486
              version-status EQ c_req_error OR
              version-status EQ ' ' ).

* put all requests, attendances and absences into ui format
    PERFORM unpack_reqlist IN PROGRAM saplpt_arq_request_uia
      USING
        lt_req_flat
      CHANGING
        et_req.
    DELETE et_req WHERE operation EQ 'DEL'.

*    PERFORM get_messages IN PROGRAM saplpt_arq_request_uia
*    USING
*      lv_debug
*    CHANGING
*      et_messages[].
  ENDMETHOD.


  METHOD GET_REQUESTS_WITH_DIVIDED.
    TYPES: BEGIN OF ty_another_item,
             item_id TYPE ptreq_items-item_ins,
           END OF ty_another_item.
    DATA: lv_list_id TYPE ptreq_header-item_list_id
        , lv_status TYPE ptreq_header-status
        , BEGIN OF ls_item_id
        ,   item_id TYPE tim_req_item_id
        , END OF ls_item_id

        , lt_item_id LIKE TABLE OF ls_item_id

        , BEGIN OF ls_items
        ,   item_ins TYPE ptreq_items-item_ins
        , END OF ls_items

        , lt_items_tmp LIKE TABLE OF ls_items
        .

    CLEAR: et_req, e_req.
    APPEND i_req_id TO et_req.

    SELECT SINGLE status item_list_id FROM ptreq_header
      INTO ( lv_status, lv_list_id )
      WHERE request_id EQ i_req_id AND
            version_no EQ ( SELECT MAX( version_no ) FROM ptreq_header
                              WHERE request_id EQ i_req_id ).


    SELECT item_ins FROM ptreq_items AS a1
      INTO TABLE @DATA(lt_items)
      WHERE a1~item_list_id EQ @lv_list_id.
    LOOP AT lt_items ASSIGNING FIELD-SYMBOL(<fs_items>).
      ls_item_id-item_id = <fs_items>-item_ins.
      APPEND ls_item_id TO lt_item_id.
    ENDLOOP.

    IF lt_item_id IS NOT INITIAL.
*     перенесенный ли отпуск??
      SELECT item_id, pernr, subty, endda, begda FROM ptreq_attabsdata
        INTO TABLE @DATA(lt_attr)
        FOR ALL ENTRIES IN @lt_item_id
        WHERE item_id EQ @lt_item_id-item_id AND
              operation EQ 'DEL'.
      READ TABLE lt_attr INDEX 1 ASSIGNING FIELD-SYMBOL(<fs_attr>).
      IF sy-subrc EQ 0.
*       если нашли, значит перенесенный и нужно найти вторую заявку по тем же данным
        SELECT item_id FROM ptreq_attabsdata
          INTO TABLE @DATA(lt_another_item)
          WHERE item_id   NE @<fs_attr>-item_id AND
                pernr     EQ @<fs_attr>-pernr   AND
                subty     EQ @<fs_attr>-subty   AND
                endda     EQ @<fs_attr>-endda   AND
                begda     EQ @<fs_attr>-begda   AND
                operation EQ 'DEL'.
        IF sy-subrc EQ 0.
          CLEAR: lt_items_tmp.
          LOOP AT lt_another_item ASSIGNING FIELD-SYMBOL(<fs_another_item>).
            ls_items-item_ins = <fs_another_item>-item_id.
            APPEND ls_items TO lt_items_tmp.
          ENDLOOP.

          SELECT item_list_id FROM ptreq_items
            INTO TABLE @DATA(lt_list_id)
            FOR ALL ENTRIES IN @lt_items_tmp
            WHERE item_ins EQ @lt_items_tmp-item_ins.
          IF sy-subrc EQ 0.
            SELECT request_id, version_no, status FROM ptreq_header
              INTO TABLE @DATA(lt_req)
              FOR ALL ENTRIES IN @lt_list_id
              WHERE item_list_id EQ @lt_list_id-item_list_id.

            SORT lt_req BY request_id version_no DESCENDING.
            DELETE ADJACENT DUPLICATES FROM lt_req COMPARING request_id.
            LOOP AT lt_req ASSIGNING FIELD-SYMBOL(<fs_req>) WHERE status EQ lv_status.
              APPEND <fs_req>-request_id TO et_req.
              e_req = <fs_req>-request_id.
              EXIT.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.


*    SELECT SINGLE ITEM_LIST_ID FROM PTREQ_HEADER as a1
*      INNER JOIN PTREQ_ITEMS as a2 on a1~ITEM_LIST_ID = a2~ITEM_LIST_ID
*      INNER JOIN PTREQ_ATTABSDATA as a3 on a3~item_id = a2~ITEM_INS
*      INTO lv_list_id
*      WHERE a1~REQUEST_ID.
  ENDMETHOD.


  METHOD GET_REQ_HEADER_DATA.

    DATA:
      lt_ptreq_header TYPE TABLE OF ptreq_header,
      ls_ptreq_header LIKE LINE OF lt_ptreq_header.


    CLEAR es_request.

    SELECT *
      INTO TABLE lt_ptreq_header
      FROM ptreq_header
      WHERE
        request_id  EQ iv_request_id.

    CHECK sy-subrc EQ 0.

    SORT lt_ptreq_header BY version_no DESCENDING.
    READ TABLE lt_ptreq_header INTO ls_ptreq_header INDEX 1.

    es_request = ls_ptreq_header.

  ENDMETHOD.


  METHOD GET_TIMETYPES.
    DATA: lv_crule TYPE t554s-crule
        , lv_n2    TYPE numc2
        , ls_type   TYPE ty_time_types
        .
*   На вход получем тип отпуска. Считываем запись в поле V_554S_Q-CRULE по ключу V_554S_Q-MOABW = 33 и V_554S_Q = вид отсутсвия
*   Далее по V_554S_Q-CRULE = T556C-CRULE и T556C-MOZKO =33 считываем значение в таблице T556C в поле T556C-DEABP
*   Значение из поля T556C-DEABP возвращаем как тип лимита

    CLEAR: et_types.
    SELECT SINGLE crule FROM t554s
      INTO lv_crule
      WHERE moabw EQ '33'     AND
            subty EQ i_subty  AND
            endda >= sy-datum AND
            begda <= sy-datum.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    SELECT deabp FROM t556c
      INTO TABLE @DATA(lt_deabp)
      WHERE mozko EQ '33' AND
            crule EQ @lv_crule AND
            ctype EQ 'Q'.
    LOOP AT lt_deabp ASSIGNING FIELD-SYMBOL(<fs_deabp>).
      lv_n2 = <fs_deabp>.
      CHECK lv_n2 > 0.
      ls_type-type = lv_n2.
      INSERT ls_type INTO TABLE et_types.
    ENDLOOP.
  ENDMETHOD.


  METHOD IS_NEED_0105_CHECK.

    DATA:
          ls_vac_users  TYPE ztvacation_users.


    rv_need = 'X'.

    SELECT SINGLE *
      INTO ls_vac_users
      FROM ztvacation_users
      WHERE
        bname EQ iv_login.

    CHECK sy-subrc EQ 0.

    CLEAR rv_need.

  ENDMETHOD.
ENDCLASS.

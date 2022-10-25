class ZCL_ZHR_ESS_STAFF_DPC_EXT definition
  public
  inheriting from ZCL_ZHR_ESS_STAFF_DPC
  create public .

public section.
protected section.

  methods STAFFS_GET_ENTITYSET
    redefinition .
private section.

  class-data ENTRANCEUSER type STRING value 'EntranceUser' ##NO_TEXT.
  class-data MAINUSER type STRING value 'MainUser' ##NO_TEXT.
ENDCLASS.



CLASS ZCL_ZHR_ESS_STAFF_DPC_EXT IMPLEMENTATION.


  METHOD staffs_get_entityset.

    DATA: BEGIN OF ls_input
        , mainuser     TYPE string
        , entranceuser TYPE string
        , END OF ls_input

        , lr_user TYPE RANGE OF string
        .

    DATA:
      lt_iprreq              TYPE TABLE OF zhr_xss_iprreq,
      ls_select_options      TYPE /iwbep/s_cod_select_option,
      lv_pernr               TYPE pernr_d,
      lv_pernrfio            TYPE string,
      lv_pernr_cand          TYPE pernr_d,
      lv_objid_cand          TYPE vacid,
      lv_entrance_user       TYPE string,
      lv_yesterday           TYPE sy-datum,
      lv_tomorrow            TYPE sy-datum,
      lv_begda               TYPE numc04,
      lv_endda               TYPE numc04,
      lv_objid_begda         TYPE sy-datum,
      lv_sydatum_minus_three TYPE sy-datum,
      lv_sydatum_plus_three  TYPE sy-datum,
      lt_p9001               TYPE TABLE OF p9001,
      lt_p1002               TYPE TABLE OF p1002,
      lt_pb4002              TYPE TABLE OF pb4002,
      lt_pb4005              TYPE TABLE OF pb4005,
      ls_hrt1002             TYPE hrt1002,
      lt_p1000               TYPE TABLE OF p1000,
      ls_p1000               TYPE p1000
      .

    FIELD-SYMBOLS:
      <ls_iprreq>    LIKE LINE OF lt_iprreq,
      <ls_p9001>     LIKE LINE OF lt_p9001,
      <ls_entityset> LIKE LINE OF et_entityset.

    READ TABLE it_filter_select_options ASSIGNING FIELD-SYMBOL(<filter>) WITH KEY property = entranceuser.
    IF sy-subrc <> 0.
      READ TABLE it_filter_select_options ASSIGNING <filter> WITH KEY property = mainuser.
    ENDIF.
    IF sy-subrc = 0.
      READ TABLE <filter>-select_options ASSIGNING FIELD-SYMBOL(<option>) INDEX 1.
      IF sy-subrc = 0.
        lv_entrance_user = <option>-low.
      ENDIF.
    ENDIF.

    " ТН создавшего заявку
    DATA(lo_hr_mss) = NEW zcl_mss_data_assistent( ).
    lv_pernr = lo_hr_mss->get_pernr( iv_usrid = CONV #( lv_entrance_user ) ).

*    SELECT SINGLE pernr INTO lv_pernr FROM pa0105
*      WHERE
*            usrid = lv_entrance_user
*        AND subty EQ '9001'
*        AND begda <= sy-datum
*        AND endda >= sy-datum.
    IF lv_pernr IS INITIAL.
      APPEND INITIAL LINE TO et_entityset ASSIGNING <ls_entityset>.
      <ls_entityset>-msgid = 'W'.
      <ls_entityset>-message = 'Табельный номер пользователя' && ` ` &&  lv_entrance_user && ` ` && 'не найден'.
      EXIT.
    ENDIF.
    " /

*Кратко о пространственно-временном континууме:
*В високосный год вчера для первого марта будет 29 февраля
*но через три года его не будет
*Поэтому через три года вчера для 1 марта должно быть 28 февраля
    lv_yesterday = sy-datum - 1.
    IF lv_yesterday+4(4) = '0229'.
      lv_yesterday = sy-datum - 2.
    ENDIF.

    lv_endda = lv_yesterday+0(4) +  3.
    lv_sydatum_plus_three = lv_endda && lv_yesterday+4(4).

    lv_tomorrow = sy-datum + 1.
    IF lv_tomorrow+4(4) = '0229'.
      lv_tomorrow = sy-datum + 2.
    ENDIF.

    lv_begda = lv_tomorrow+0(4) - 3.
    lv_sydatum_minus_three = lv_begda && lv_tomorrow+4(4).

*-----------PA9001-PLANS

    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        pernr     = lv_pernr
        infty     = '9001'
        begda     = lv_sydatum_minus_three
        endda     = lv_sydatum_plus_three
      TABLES
        infty_tab = lt_p9001.

    DATA(lo_get_data) = NEW zcl_hr_get_data( ).



    LOOP AT  lt_p9001 ASSIGNING <ls_p9001> WHERE
      begda    >= lv_sydatum_minus_three AND "отсекаем закончившийся резерв
      begda    <= sy-datum AND "отсекаем неначавшийся резерв
      rbase IS INITIAL.

      DATA(lv_text) = lo_get_data->get_objid_verbal( i_otype = 'S' i_objid = <ls_p9001>-plans i_begda = sy-datum ).


*      CALL FUNCTION 'RH_READ_HRP1002'
*        EXPORTING
*          act_plvar     = '01'
*          act_otype     = 'S'
*          act_objid     = <ls_p9001>-plans
*          act_subty     = '0001'
*          act_langu     = 'R'
*          act_begda     = sy-datum
*          act_endda     = sy-datum
*        TABLES
*          act_p1002     = lt_p1002
*        EXCEPTIONS
*          no_data_found = 1
*          OTHERS        = 2.
*
*      IF sy-subrc <> 0.
*
*
*      ENDIF.
*
*      READ TABLE lt_p1002 ASSIGNING FIELD-SYMBOL(<ls_hrt1002>) WITH KEY subty = '0001'.
*
*      IF sy-subrc = 0."Если описание нашлось в HRT1002
*
*        SELECT * INTO  ls_hrt1002 FROM hrt1002   WHERE tabnr = <ls_hrt1002>-tabnr.
*          APPEND INITIAL LINE TO et_entityset ASSIGNING <ls_entityset>.
*          <ls_entityset>-begda = <ls_p9001>-begda.
*
*          lv_yesterday = <ls_p9001>-begda - 1.
*          IF lv_yesterday+4(4) = '0229'.
*            lv_yesterday = sy-datum - 2.
*          ENDIF.
*
*          lv_endda = lv_yesterday+0(4) +  3.
*
*          <ls_entityset>-endda = lv_endda && lv_yesterday+4(4).
*
*          IF ls_hrt1002-tline IS NOT INITIAL."Если tline оказалось заполненным
*            <ls_entityset>-plansdesc = ls_hrt1002-tline.
*          ELSE. "Если tline оказалось незаполненным
*
*            CALL FUNCTION 'RH_READ_INFTY'
*              EXPORTING
*                plvar                = '01'
*                otype                = 'S'
*                objid                = <ls_p9001>-plans
*               infty                = '1000'
**               subty                = 'B008'
*                begda                = sy-datum
*                endda                = sy-datum
*              TABLES
*                innnn                = lt_p1000
*              EXCEPTIONS
*                all_infty_with_subty = 1
*                nothing_found        = 2
*                no_objects           = 3
*                wrong_condition      = 4
*                wrong_parameters     = 5
*                OTHERS               = 6.
*
*            READ TABLE lt_p1000 ASSIGNING FIELD-SYMBOL(<ls_p1000>) WITH KEY langu = 'R'.
*            " APPEND INITIAL LINE TO et_entityset ASSIGNING <ls_entityset>.
*            <ls_entityset>-plansdesc = <ls_p1000>-stext.
*
*
*          ENDIF.
*
*
*          <ls_entityset>-msgid = 'ALL_OK'.
*          <ls_entityset>-message = 'Plans is found'.
*        ENDSELECT.
*
*      ELSE."Если описание не нашлось в HRT1002, ищем в HRT1000
*        CALL FUNCTION 'RH_READ_INFTY'
*          EXPORTING
*            plvar                = '01'
*            otype                = 'S'
*            objid                = <ls_p9001>-plans
*           infty                = '1000'
**           subty                = 'B008'
*            begda                = sy-datum
*            endda                = sy-datum
*          TABLES
*            innnn                = lt_p1000
*          EXCEPTIONS
*            all_infty_with_subty = 1
*            nothing_found        = 2
*            no_objects           = 3
*            wrong_condition      = 4
*            wrong_parameters     = 5
*            OTHERS               = 6.


      "READ TABLE lt_p1000 ASSIGNING FIELD-SYMBOL(<ls_p1000>) WITH KEY langu = 'R'.
*        READ TABLE lt_p1000 ASSIGNING <ls_p1000> WITH KEY langu = 'R'.

      APPEND INITIAL LINE TO et_entityset ASSIGNING <ls_entityset>.
      <ls_entityset>-begda = <ls_p9001>-begda.

      lv_yesterday = <ls_p9001>-begda - 1.
      IF lv_yesterday+4(4) = '0229'.
        lv_yesterday = sy-datum - 2.
      ENDIF.
      lv_endda = lv_yesterday+0(4) +  3.
      <ls_entityset>-endda = lv_endda && lv_yesterday+4(4).

*        <ls_entityset>-plansdesc = <ls_p1000>-stext.
      <ls_entityset>-plansdesc = lv_text.
      <ls_entityset>-msgid = 'S'.
      <ls_entityset>-message = 'Успешно'.
*      ENDIF.
    ENDLOOP.


    "-----------Находим кандидата для сотрудника pb4005-PERNR
    DATA:
          ls_pb4002_0 TYPE pb4002.
    FIELD-SYMBOLS:
          <ls_pb4002_0> TYPE pb4002.


    "========================
    SELECT SINGLE pernr INTO lv_pernr_cand FROM pb4005
      WHERE
           empnr     = lv_pernr

        AND begda <= sy-datum
        AND endda >= sy-datum.

    IF sy-subrc = 0.

      SELECT * INTO ls_pb4002_0 FROM pb4002
        WHERE
            pernr  = lv_pernr_cand
              AND begda >= lv_sydatum_minus_three
              AND begda <= sy-datum
              AND apstv = 3
              AND zzreason EQ ''
          "    AND endda <= lv_sydatum_plus_three "Это не работает при непавильном заполнении endda в таблице
          .



*      SELECT SINGLE objid INTO lv_objid_cand FROM pb4002
*          WHERE
*                pernr  = lv_pernr_cand
*            AND begda >= lv_sydatum_minus_three
*            AND begda <= sy-datum
*            AND apstv = 3
*            AND zzreason EQ ''
*        "    AND endda <= lv_sydatum_plus_three "Это не работает при непавильном заполнении endda в таблице
*        .


        SELECT SINGLE begda INTO lv_objid_begda FROM pb4002
                     WHERE
                           pernr  = lv_pernr_cand
                       AND objid  = ls_pb4002_0-objid
                       AND begda >= lv_sydatum_minus_three
                       AND apstv  = 3
                       AND zzreason EQ ''
                   "    AND endda <= lv_sydatum_plus_three "Это не работает при непавильном заполнении endda в таблице
                   .

*        CLEAR lt_p1002.
*
*        CALL FUNCTION 'RH_READ_HRP1002'
*          EXPORTING
*            act_plvar     = '01'
*            act_otype     = 'S'
*            act_objid     = ls_pb4002_0-objid
*            act_subty     = '0001'
*            act_langu     = 'R'
*            act_begda     = sy-datum
*            act_endda     = sy-datum
*          TABLES
*            act_p1002     = lt_p1002
*          EXCEPTIONS
*            no_data_found = 1
*            OTHERS        = 2.
*
*        IF sy-subrc = 0.
*          UNASSIGN <ls_hrt1002>.
*          READ TABLE lt_p1002 ASSIGNING <ls_hrt1002> WITH KEY subty = '0001'.
*
*          IF sy-subrc = 0 ."Если описание нашлось в HRT1002
*            SELECT * INTO  ls_hrt1002 FROM hrt1002   WHERE tabnr = <ls_hrt1002>-tabnr.
*              APPEND INITIAL LINE TO et_entityset ASSIGNING <ls_entityset>.
*              <ls_entityset>-begda = lv_objid_begda.
*
*              lv_yesterday = lv_objid_begda - 1.
*              IF lv_yesterday+4(4) = '0229'.
*                lv_yesterday = sy-datum - 2.
*              ENDIF.
*              lv_endda = lv_yesterday+0(4) +  3.
*              <ls_entityset>-endda = lv_endda && lv_yesterday+4(4).
*
*
*              IF ls_hrt1002-tline IS NOT INITIAL."Если tline оказалось заполненным
*                <ls_entityset>-plansdesc = ls_hrt1002-tline.
*              ELSE. "Если tline оказалось незаполненным
*
*                CALL FUNCTION 'RH_READ_INFTY'
*                  EXPORTING
*                    plvar                = '01'
*                    otype                = 'S'
*                    objid                = ls_pb4002_0-objid
*                   infty                = '1000'
**                   subty                = 'B008'
*                    begda                = sy-datum
*                    endda                = sy-datum
*                  TABLES
*                    innnn                = lt_p1000
*                  EXCEPTIONS
*                    all_infty_with_subty = 1
*                    nothing_found        = 2
*                    no_objects           = 3
*                    wrong_condition      = 4
*                    wrong_parameters     = 5
*                    OTHERS               = 6.
*                UNASSIGN <ls_p1000>.
*                READ TABLE lt_p1000 ASSIGNING <ls_p1000> WITH KEY langu = 'R'.
*                " APPEND INITIAL LINE TO et_entityset ASSIGNING <ls_entityset>.
*                <ls_entityset>-plansdesc = <ls_p1000>-stext.
*
*
*              ENDIF.
*              <ls_entityset>-msgid = 'ALL_OK'.
*              <ls_entityset>-message = 'Plans is found'.
*            ENDSELECT.
*
*          ELSE."Если описание не нашлось в HRT1002, ищем в HRT1000
*            CALL FUNCTION 'RH_READ_INFTY'
*              EXPORTING
*                plvar                = '01'
*                otype                = 'S'
*                objid                = ls_pb4002_0-objid
*               infty                = '1000'
**               subty                = 'B008'
*                begda                = sy-datum
*                endda                = sy-datum
*              TABLES
*                innnn                = lt_p1000
*              EXCEPTIONS
*                all_infty_with_subty = 1
*                nothing_found        = 2
*                no_objects           = 3
*                wrong_condition      = 4
*                wrong_parameters     = 5
*                OTHERS               = 6.
*            UNASSIGN <ls_p1000>.
*            READ TABLE lt_p1000 ASSIGNING <ls_p1000> WITH KEY langu = 'R'.
*
*            APPEND INITIAL LINE TO et_entityset ASSIGNING <ls_entityset>.
*            <ls_entityset>-begda = lv_objid_begda.
*
*            lv_yesterday = lv_objid_begda - 1.
*            IF lv_yesterday+4(4) = '0229'.
*              lv_yesterday = sy-datum - 2.
*            ENDIF.
*            lv_endda = lv_yesterday+0(4) +  3.
*            <ls_entityset>-endda = lv_endda && lv_yesterday+4(4).
*            <ls_entityset>-plansdesc = <ls_p1000>-stext.
*            <ls_entityset>-msgid = 'ALL_OK'.
*            <ls_entityset>-message = 'Plans is found'.
*          ENDIF.
*
*        ELSE."Если описание не нашлось в HRT1002, ищем в HRT1000
*          CALL FUNCTION 'RH_READ_INFTY'
*            EXPORTING
*              plvar                = '01'
*              otype                = 'S'
*              objid                = ls_pb4002_0-objid
*             infty                = '1000'
**             subty                = 'B008'
*              begda                = sy-datum
*              endda                = sy-datum
*            TABLES
*              innnn                = lt_p1000
*            EXCEPTIONS
*              all_infty_with_subty = 1
*              nothing_found        = 2
*              no_objects           = 3
*              wrong_condition      = 4
*              wrong_parameters     = 5
*              OTHERS               = 6.
*
*
*          "READ TABLE lt_p1000 ASSIGNING FIELD-SYMBOL(<ls_p1000>) WITH KEY langu = 'R'.
*          READ TABLE lt_p1000 ASSIGNING <ls_p1000> WITH KEY langu = 'R'.

        APPEND INITIAL LINE TO et_entityset ASSIGNING <ls_entityset>.
        <ls_entityset>-begda = ls_pb4002_0-begda.

        lv_yesterday = ls_pb4002_0-begda - 1.
        IF lv_yesterday+4(4) = '0229'.
          lv_yesterday = sy-datum - 2.
        ENDIF.
        lv_endda = lv_yesterday+0(4) +  3.
        <ls_entityset>-endda = lv_endda && lv_yesterday+4(4).

*          <ls_entityset>-plansdesc = <ls_p1000>-stext.
        <ls_entityset>-plansdesc = lo_get_data->get_objid_verbal( i_otype = 'S' i_objid = ls_pb4002_0-objid i_begda = sy-datum ).
        <ls_entityset>-msgid = 'S'.
        <ls_entityset>-message = 'Успешно'.

*        ENDIF.

      ENDSELECT.
    ENDIF.

    IF et_entityset[] IS INITIAL.
      APPEND INITIAL LINE TO et_entityset ASSIGNING <ls_entityset>.
      lv_pernrfio = lo_hr_mss->get_pernr_fio( iv_pernr = CONV #( lv_pernr ) ).
      <ls_entityset>-msgid = 'I'.
      <ls_entityset>-message = lv_pernrfio && ` ` && 'не состоит в кадровом резерве'.
    ENDIF.

  ENDMETHOD.
ENDCLASS.

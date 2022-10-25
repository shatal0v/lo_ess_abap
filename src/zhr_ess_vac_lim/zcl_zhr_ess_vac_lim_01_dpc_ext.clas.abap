class ZCL_ZHR_ESS_VAC_LIM_01_DPC_EXT definition
  public
  inheriting from ZCL_ZHR_ESS_VAC_LIM_01_DPC
  create public .

public section.
protected section.

  methods ENTITYMAINSET_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZHR_ESS_VAC_LIM_01_DPC_EXT IMPLEMENTATION.


  METHOD entitymainset_get_entityset.
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
             ,   END OF ty_line
             .
    DATA: lt_line TYPE TABLE OF ty_line
        , lv_ktart  TYPE p2006-ktart
        , lt_p2006  TYPE TABLE OF p2006
        , ls_556b   TYPE t556b
        .

    DATA: lv_loginactdir TYPE zlogin_act_dir
        , lv_property TYPE string
        , lv_pernr TYPE persno
        , lv_pernr2 TYPE persno
        , lv_begda    TYPE begda
        , lv_string TYPE string
        , lv_int    TYPE i
        .

    DATA: lt_return TYPE TABLE OF bapiret2
        , lv_err_number TYPE i VALUE 1
        , lv_message TYPE string
        , lo_message_container TYPE REF TO /iwbep/if_message_container
        .
    TYPES: BEGIN OF ty_limits,
             abstype        TYPE string,
*             limit_begda    TYPE begda,
*             limit_endda    TYPE endda,
             available_days TYPE p LENGTH 16 DECIMALS 2,
             used_days      TYPE p LENGTH 16 DECIMALS 2,
             limit_days     TYPE p LENGTH 16 DECIMALS 2,
           END OF ty_limits.

    DATA: lt_messages TYPE ptarq_uia_messages_tab
        , lt_command  TYPE ptarq_uia_command_tab
        , lt_request  TYPE ptarq_uia_reqlist_tab
        , lt_acc      TYPE ptarq_uia_quota_status_all_tab

        , lt_limits   TYPE SORTED TABLE OF ty_limits WITH UNIQUE KEY abstype
        , ls_limit    TYPE ty_limits
        , lv_login_orig TYPE string
        , lv_limit      TYPE string "p LENGTH 16 DECIMALS 2
        .

    lo_message_container = mo_context->get_message_container( ).

    DATA(lo_assistent) = zcl_mss_data_assistent=>get_instance( ).

    LOOP AT it_filter_select_options ASSIGNING FIELD-SYMBOL(<filter>).
      LOOP AT <filter>-select_options ASSIGNING FIELD-SYMBOL(<range>).
        lv_property = <filter>-property.
        TRANSLATE lv_property TO UPPER CASE.
        CASE lv_property.
          WHEN 'DATA'.
            lv_begda = <range>-low.
*          WHEN 'PERNR'.
*            lv_pernr = <range>-low.
          WHEN `MAINUSER`.
            lv_pernr2 = lo_assistent->get_pernr( iv_usrid = CONV #( <range>-low ) ).
            IF lv_pernr2 IS INITIAL.
              APPEND INITIAL LINE TO lt_return ASSIGNING FIELD-SYMBOL(<return>).
              MESSAGE e001(zhr_ess) INTO <return>-message.
              <return>-type   = sy-msgty.
              <return>-id     = sy-msgid.
              <return>-number = sy-msgno.
              lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

              RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
                EXPORTING
                  message_container = lo_message_container.
            ENDIF.
          WHEN `ENTRANCEUSER`.
            lv_pernr = lo_assistent->get_pernr( iv_usrid = CONV #( <range>-low ) ).
            IF lv_pernr IS INITIAL.
              APPEND INITIAL LINE TO lt_return ASSIGNING <return>.
              MESSAGE e001(zhr_ess) INTO <return>-message.
              <return>-type   = sy-msgty.
              <return>-id     = sy-msgid.
              <return>-number = sy-msgno.
              lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

              RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
                EXPORTING
                  message_container = lo_message_container.
            ENDIF.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.

    IF lv_pernr IS INITIAL.
      lv_pernr =  lv_pernr2.
    ENDIF.

    zcl_pa_utils=>read_infty(
      EXPORTING
        i_pernr         = lv_pernr
        i_infty         = '2006'
        i_begda         = hr_low_date
        i_endda         = lv_begda
      IMPORTING
        e_pnnnn         = lt_p2006 ).
    LOOP AT lt_p2006 ASSIGNING FIELD-SYMBOL(<fs_p2006>).
      CHECK <fs_p2006>-anzhl NE <fs_p2006>-kverb.
      ls_limit-abstype    = <fs_p2006>-ktart.
      ls_limit-used_days  = <fs_p2006>-kverb.
      ls_limit-limit_days = <fs_p2006>-anzhl.
      COLLECT ls_limit INTO lt_limits.
    ENDLOOP.

    DATA: lv_av_days   TYPE p0267-betrg
        , lv_us_days   TYPE p0267-betrg
        , lv_li_days   TYPE p0267-betrg
        , lr_ktart     TYPE RANGE OF ktart
        .

    APPEND VALUE #( sign = `I` option = `EQ` low = `01` ) TO lr_ktart.
    APPEND VALUE #( sign = `I` option = `EQ` low = `02` ) TO lr_ktart.
    APPEND VALUE #( sign = `I` option = `EQ` low = `03` ) TO lr_ktart.

    LOOP AT lt_limits ASSIGNING FIELD-SYMBOL(<fs_limits>).
      lv_ktart = <fs_limits>-abstype.
      zcl_vacation_appl=>get_limit_days( EXPORTING i_pernr = lv_pernr
                                                   i_date  = lv_begda
                                                   i_ktart = lv_ktart
                                         IMPORTING e_limit = lv_limit ).

      APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<fs_out>).
      MOVE-CORRESPONDING <fs_limits> TO <fs_out>.
      <fs_out>-abstype = lv_ktart.  "ls_556b-ktext.
      <fs_out>-pernr   = lv_pernr.
      <fs_out>-begda   = lv_begda.
*      IF <fs_line>-anzhlmtxt IS NOT INITIAL.
*        <fs_out>-available_days = <fs_line>-anzhlmtxt.
*      ELSEIF <fs_line>-anzhlotxt IS NOT INITIAL.
      <fs_out>-available_days = lv_limit. "<fs_line>-anzhlotxt.
*      ENDIF.

      CONDENSE: <fs_out>-used_days NO-GAPS, <fs_out>-limit_days NO-GAPS.
      <fs_out>-login_act_dir = lv_login_orig.

      IF NOT lv_ktart IN lr_ktart.
        CONTINUE.
      ENDIF.
      ADD lv_limit               TO lv_av_days.
      ADD <fs_limits>-limit_days TO lv_li_days.
      ADD <fs_limits>-used_days  TO lv_us_days.
    ENDLOOP.

*    REFRESH: et_entityset
*           .
    LOOP AT et_entityset ASSIGNING FIELD-SYMBOL(<et_entityset>) WHERE abstype IN lr_ktart.
      DATA(ls_entityset) = <et_entityset>.
      ls_entityset-abstype        = `99`.
      ls_entityset-available_days = CONV int4( lv_av_days ).
      ls_entityset-limit_days     = CONV int4( lv_li_days ).
      ls_entityset-used_days      = CONV int4( lv_us_days ).
      EXIT.
    ENDLOOP.

    IF sy-subrc = 0.
      DELETE et_entityset WHERE abstype IN lr_ktart.
      APPEND ls_entityset TO et_entityset.
    ENDIF.
*
    SORT lt_p2006 BY begda DESCENDING.
    IF lt_p2006 IS NOT INITIAL.
      DELETE lt_p2006 WHERE NOT ( begda = lt_p2006[ 1 ]-begda AND subty IN lr_ktart ).
      ASSIGN lt_p2006[ 1 ] TO FIELD-SYMBOL(<ls_2006>).
      INSERT INITIAL LINE INTO TABLE et_entityset ASSIGNING <et_entityset>.
      <et_entityset>-pernr   = lv_pernr.
      <et_entityset>-begda   = lv_begda.
      <et_entityset>-limit_begda = <ls_2006>-begda.
      <et_entityset>-limit_endda = <ls_2006>-endda.
      <et_entityset>-abstype = '99'.
      LOOP AT lt_p2006 ASSIGNING <ls_2006>.
        <et_entityset>-used_days = <et_entityset>-used_days + CONV i( <ls_2006>-kverb ).
        <et_entityset>-limit_days = <et_entityset>-limit_days + CONV i( <ls_2006>-anzhl ).
      ENDLOOP.
      <et_entityset>-available_days = <et_entityset>-limit_days - <et_entityset>-used_days.
    ENDIF.
    DELETE et_entityset INDEX 1.
    RETURN.

*    zcl_vacation_operation=>account_list_get( EXPORTING pernr     = lv_pernr
***                                                        SEL_PERNR = lv_pernr2
*                                                        acc_date  = lv_begda
*                                              IMPORTING acc_list  = lt_acc
*                                                        messages  = lt_messages ).
*
*********<<<<обработка ошибок
*********<< SAP возвращает ошибку, если пользователь не имеет ТН в системе
*********<< поскольку данные тоже возвращаются, то игнорим эту ошибку
*******    DELETE lt_messages WHERE type = 'E' AND number = '075' AND id = 'HRTIM_ABS_REQ'.
*
*    LOOP AT lt_messages ASSIGNING FIELD-SYMBOL(<messages>) WHERE type = 'E'.
*      APPEND INITIAL LINE TO lt_return ASSIGNING <return>.
*      <return>-type = <messages>-type.
*      <return>-id   = <messages>-id.
*      <return>-number = <messages>-number.
*      <return>-message = <messages>-message.
*      lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).
***        <fs_header>-value = cl_http_utility=>escape_url( lv_message ).
*    ENDLOOP.
*    IF  lt_return IS NOT INITIAL .
***      <<<< выводим ошибки в боди
*      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
*        EXPORTING
*          message_container = lo_message_container.
*    ENDIF.
*
*    LOOP AT lt_acc ASSIGNING FIELD-SYMBOL(<lt_acc>).
*      CHECK <lt_acc>-deducted_reduced <> <lt_acc>-entitle."выводим только те лимиты, у которых есть неизрасходованные дни
*      ls_limit-abstype = <lt_acc>-time_type.
**     Сервис возвращает два лимита по основному отпуску, а должен их суммировать и возвращать большую итоговую цифру.
**     поэтому даты тут не нужны!
**      ls_limit-limit_begda = <lt_acc>-deduct_begin.
**      ls_limit-limit_endda = <lt_acc>-deduct_end.
*      lv_string = <lt_acc>-rest_used.
*      REPLACE 'Дни' IN lv_string WITH space.
*      REPLACE 'д.' IN lv_string WITH space.
*      REPLACE ',' IN lv_string WITH '.'.
*      lv_int = lv_string.
*      ls_limit-available_days = lv_int.
*      lv_string = <lt_acc>-deducted_reduced.
*      REPLACE 'Дни' IN lv_string WITH space.
*      REPLACE 'д.' IN lv_string WITH space.
*      REPLACE ',' IN lv_string WITH '.'.
*      lv_int = lv_string.
*      ls_limit-used_days = lv_int.
*      lv_string = <lt_acc>-entitle.
*      REPLACE 'Дни' IN lv_string WITH space.
*      REPLACE 'д.' IN lv_string WITH space.
*      REPLACE ',' IN lv_string WITH '.'.
*      lv_int = lv_string.
*      ls_limit-limit_days = lv_int.
*      COLLECT ls_limit INTO lt_limits.
*    ENDLOOP.

*    LOOP AT lt_limits ASSIGNING FIELD-SYMBOL(<fs_limits>).
*      APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<fs_out>).
*      MOVE-CORRESPONDING <fs_limits> TO <fs_out>.
*      <fs_out>-begda         = lv_begda.
*      <fs_out>-pernr         = lv_pernr.
*      <fs_out>-login_act_dir = lv_login_orig.
*    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

class ZCL_ZHR_ESS_VAC_GET_LI_DPC_EXT definition
  public
  inheriting from ZCL_ZHR_ESS_VAC_GET_LI_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_EXPANDED_ENTITYSET
    redefinition .
protected section.

  methods ENTITYABSSET_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZHR_ESS_VAC_GET_LI_DPC_EXT IMPLEMENTATION.


  METHOD /iwbep/if_mgw_appl_srv_runtime~get_expanded_entityset.
    DATA:
      lo_message_container TYPE REF TO /iwbep/if_message_container,

      ls_deep_entity       TYPE zcl_zhr_ess_vac_get_li_mpc_ext=>ts_deep_entity,
      lt_deep_entity       TYPE TABLE OF zcl_zhr_ess_vac_get_li_mpc_ext=>ts_deep_entity,

      lv_property          TYPE string,
      lv_loginactdir       TYPE zlogin_act_dir,
      lv_pernr             TYPE pernr_d,
      lv_webmo             TYPE webmo,
      lv_back              TYPE string,
      ls_struc_content     TYPE pme95,
      lt_p0001             TYPE TABLE OF p0001,
      ls_p0001             TYPE p0001,
      lv_molga             TYPE molga,

      lt_return            TYPE bapiret2_t,
      ls_return            LIKE LINE OF lt_return,

      lt_t556a_web         TYPE TABLE OF t556a_web,
      ls_t556a_web         LIKE LINE OF lt_t556a_web,
      lt_t554s_web         TYPE TABLE OF t554s_web,
      ls_t554s_web         LIKE LINE OF lt_t554s_web,
      lt_t7ru_554t         TYPE TABLE OF t7ru_554t,
      ls_t7ru_554t         LIKE LINE OF lt_t7ru_554t,
      lt_t7ru_556b         TYPE TABLE OF t7ru_556b,
      ls_t7ru_556b         LIKE LINE OF lt_t7ru_556b,

      ls_limit             TYPE zvacation_limit_type,
      ls_abs               TYPE zvacation_abs_type.

    FIELD-SYMBOLS:
      <filter> LIKE LINE OF it_filter_select_options,
      <range>  LIKE LINE OF <filter>-select_options.

    DATA(lo_assistent) = zcl_mss_data_assistent=>get_instance( ).

    CASE iv_entity_set_name.
      WHEN 'EntityMainSet'.
        lo_message_container = mo_context->get_message_container( ).

* исходные данные
        LOOP AT it_filter_select_options ASSIGNING <filter>.
          LOOP AT <filter>-select_options ASSIGNING <range>.
            lv_property = <filter>-property.
            TRANSLATE lv_property TO UPPER CASE.
            CASE lv_property.
              WHEN 'ENTRANCEUSER'.
*                lv_loginactdir = <range>-low.
                DATA(lv_entranceuser) = CONV string( <range>-low ).
              WHEN 'MAINUSER'.
                lv_entranceuser = CONV string( <range>-low ).
            ENDCASE.
            IF lv_entranceuser IS NOT INITIAL.
              EXIT.
            ENDIF.
          ENDLOOP.
          IF lv_entranceuser IS NOT INITIAL.
            EXIT.
          ENDIF.
        ENDLOOP.
        TRANSLATE lv_entranceuser TO UPPER CASE.
        lv_pernr = lo_assistent->get_pernr( iv_usrid = CONV #( lv_entranceuser ) ).

*        SELECT SINGLE pernr
*          FROM pa0105
*          INTO lv_pernr
*          WHERE subty = '0001'
*            AND usrid = lv_loginactdir.
*
*        IF sy-subrc NE 0.
        IF lv_pernr IS INITIAL.
          CLEAR ls_return.
          ls_return-type       = 'E'.
          ls_return-id         = 'ZHR_PA'.
          ls_return-number     = '015'.
          ls_return-message_v1 = lv_entranceuser.
          APPEND ls_return TO lt_return.

          lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
            EXPORTING
              message_container = lo_message_container.
        ENDIF.

* значение признака
        CALL FUNCTION 'HR_READ_INFOTYPE'
          EXPORTING
*           TCLAS           = 'A'
            pernr           = lv_pernr
            infty           = '0001'
            begda           = sy-datum
            endda           = sy-datum
            bypass_buffer   = 'X'
          TABLES
            infty_tab       = lt_p0001
          EXCEPTIONS
            infty_not_found = 1
            OTHERS          = 2.

        IF sy-subrc NE 0.
          CLEAR ls_return.
          ls_return-type      = 'E'.
          ls_return-id         = 'ZHR_PA'.
          ls_return-number     = '018'.
          ls_return-message_v1 = '0001'.
          ls_return-message_v2 = sy-datum.
          ls_return-message_v3 = lv_pernr.
          APPEND ls_return TO lt_return.

          lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
            EXPORTING
              message_container = lo_message_container.
        ENDIF.

        READ TABLE lt_p0001 INTO ls_p0001 INDEX 1.
        CLEAR ls_struc_content.
        MOVE-CORRESPONDING ls_p0001 TO ls_struc_content.

        CALL FUNCTION 'HR_FEATURE_BACKFIELD'
          EXPORTING
            feature                     = 'WEBMO'
            struc_content               = ls_struc_content
          IMPORTING
            back                        = lv_back
          EXCEPTIONS
            dummy                       = 1
            error_operation             = 2
            no_backvalue                = 3
            feature_not_generated       = 4
            invalid_sign_in_funid       = 5
            field_in_report_tab_in_pe03 = 6
            OTHERS                      = 7.

        IF sy-subrc NE 0.
          CLEAR ls_return.
          ls_return-type = 'E'.
          ls_return-id   = 'ZHR_PA'.
          ls_return-number = '019'.
          ls_return-message_v1 = 'WEBMO'.
          ls_return-message_v2 = lv_pernr.
          APPEND ls_return TO lt_return.

          lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
            EXPORTING
              message_container = lo_message_container.
        ENDIF.

        lv_webmo = lv_back.

* группировка подраздела персонала
        SELECT SINGLE molga
          INTO lv_molga
          FROM t001p
          WHERE
            werks EQ ls_p0001-werks AND
            btrtl EQ ls_p0001-btrtl.

* выборка данных
        SELECT *
          INTO TABLE lt_t556a_web
          FROM t556a_web
          WHERE
            webmo EQ lv_webmo   AND
            mopgk EQ '1'        AND
            mozko EQ lv_molga   AND
            endda GE sy-datum   AND
            begda LE sy-datum.

        SELECT *
          INTO TABLE lt_t554s_web
          FROM t554s_web
          WHERE
            webmo EQ lv_webmo   AND
            moabw EQ lv_molga   AND
            endda GE sy-datum   AND
            begda LE sy-datum.

        SELECT *
          INTO TABLE lt_t7ru_556b
          FROM t7ru_556b.

        SELECT *
          INTO TABLE lt_t7ru_554t
          FROM t7ru_554t.

* результат
        CLEAR ls_deep_entity.

        LOOP AT lt_t556a_web INTO ls_t556a_web.
          CLEAR ls_limit.
          ls_limit-ktart = ls_t556a_web-ktart.
          LOOP AT lt_t7ru_556b INTO ls_t7ru_556b
            WHERE
              sprsl EQ 'R'      AND
              mopgk EQ '1'      AND
              mozko EQ lv_molga AND
              ktart EQ ls_t556a_web-ktart.
            EXIT.
          ENDLOOP.
          IF sy-subrc EQ 0.
            ls_limit-klong = ls_t7ru_556b-klong.
          ENDIF.
          APPEND ls_limit TO ls_deep_entity-navlimit.
        ENDLOOP.

        LOOP AT lt_t554s_web INTO ls_t554s_web.
          CLEAR ls_abs.
          ls_abs-awart = ls_t554s_web-subty.
          LOOP AT lt_t7ru_554t INTO ls_t7ru_554t
            WHERE
              sprsl EQ 'R'      AND
              moabw EQ lv_molga AND
              awart EQ ls_t554s_web-subty.
            EXIT.
          ENDLOOP.
          IF sy-subrc EQ 0.
            ls_abs-atext = ls_t7ru_554t-atext.
          ENDIF.
          APPEND ls_abs TO ls_deep_entity-navabs.
        ENDLOOP.


        APPEND VALUE #( ktart = `99` klong = `Ежегодный оплачиваемый отпуск` )  TO ls_deep_entity-navlimit.
        APPEND ls_deep_entity TO lt_deep_entity.

        copy_data_to_ref(
              EXPORTING
                is_data = lt_deep_entity
              CHANGING
                cr_data = er_entityset ).

    ENDCASE.
  ENDMETHOD.


  METHOD entityabsset_get_entityset.
    DATA:
      lo_message_container TYPE REF TO /iwbep/if_message_container,

      ls_deep_entity       TYPE zcl_zhr_ess_vac_get_li_mpc_ext=>ts_deep_entity,
      lt_deep_entity       TYPE TABLE OF zcl_zhr_ess_vac_get_li_mpc_ext=>ts_deep_entity,

      lv_property          TYPE string,
      lv_loginactdir       TYPE zlogin_act_dir,
      lv_pernr             TYPE pernr_d,
      lv_webmo             TYPE webmo,
      lv_back              TYPE string,
      ls_struc_content     TYPE pme95,
      lt_p0001             TYPE TABLE OF p0001,
      ls_p0001             TYPE p0001,
      lv_molga             TYPE molga,

      lt_return            TYPE bapiret2_t,
      ls_return            LIKE LINE OF lt_return,

      lt_t556a_web         TYPE TABLE OF t556a_web,
      ls_t556a_web         LIKE LINE OF lt_t556a_web,
      lt_t554s_web         TYPE TABLE OF t554s_web,
      ls_t554s_web         LIKE LINE OF lt_t554s_web,
      lt_t7ru_554t         TYPE TABLE OF t7ru_554t,
      ls_t7ru_554t         LIKE LINE OF lt_t7ru_554t,
      lt_t7ru_556b         TYPE TABLE OF t7ru_556b,
      ls_t7ru_556b         LIKE LINE OF lt_t7ru_556b,

      ls_limit             TYPE zvacation_limit_type,
      ls_abs               TYPE zvacation_abs_type,
      lr_abs_exclude       TYPE RANGE OF awart.

    FIELD-SYMBOLS:
      <filter> LIKE LINE OF it_filter_select_options,
      <range>  LIKE LINE OF <filter>-select_options.

    DATA(lo_assistent) = zcl_mss_data_assistent=>get_instance( ).

    lo_message_container = mo_context->get_message_container( ).

* исходные данные
    LOOP AT it_filter_select_options ASSIGNING <filter>.
      LOOP AT <filter>-select_options ASSIGNING <range>.
        lv_property = <filter>-property.
        TRANSLATE lv_property TO UPPER CASE.
        CASE lv_property.
          WHEN 'ENTRANCEUSER'.
            DATA(lv_entranceuser) = CONV string( <range>-low ).
            TRANSLATE lv_entranceuser TO UPPER CASE.
          WHEN 'MAINUSER'.
            lv_entranceuser = CONV string( <range>-low ).
            TRANSLATE lv_entranceuser TO UPPER CASE.
        ENDCASE.
        IF lv_entranceuser IS NOT INITIAL.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF lv_entranceuser IS NOT INITIAL.
        EXIT.
      ENDIF.
    ENDLOOP.

    lv_pernr = lo_assistent->get_pernr( iv_usrid = CONV #( lv_entranceuser ) ).

*        SELECT SINGLE pernr
*          FROM pa0105
*          INTO lv_pernr
*          WHERE subty = '0001'
*            AND usrid = lv_loginactdir.
*
*        IF sy-subrc NE 0.
    IF lv_pernr IS INITIAL.
      CLEAR ls_return.
      ls_return-type       = 'E'.
      ls_return-id         = 'ZHR_PA'.
      ls_return-number     = '015'.
      ls_return-message_v1 = lv_entranceuser.
      APPEND ls_return TO lt_return.

      lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.
    ENDIF.

* значение признака
    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
*       TCLAS           = 'A'
        pernr           = lv_pernr
        infty           = '0001'
        begda           = sy-datum
        endda           = sy-datum
        bypass_buffer   = 'X'
      TABLES
        infty_tab       = lt_p0001
      EXCEPTIONS
        infty_not_found = 1
        OTHERS          = 2.

    IF sy-subrc NE 0.
      CLEAR ls_return.
      ls_return-type      = 'E'.
      ls_return-id         = 'ZHR_PA'.
      ls_return-number     = '018'.
      ls_return-message_v1 = '0001'.
      ls_return-message_v2 = sy-datum.
      ls_return-message_v3 = lv_pernr.
      APPEND ls_return TO lt_return.

      lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.
    ENDIF.

    READ TABLE lt_p0001 INTO ls_p0001 INDEX 1.
    CLEAR ls_struc_content.
    MOVE-CORRESPONDING ls_p0001 TO ls_struc_content.

    CALL FUNCTION 'HR_FEATURE_BACKFIELD'
      EXPORTING
        feature                     = 'WEBMO'
        struc_content               = ls_struc_content
      IMPORTING
        back                        = lv_back
      EXCEPTIONS
        dummy                       = 1
        error_operation             = 2
        no_backvalue                = 3
        feature_not_generated       = 4
        invalid_sign_in_funid       = 5
        field_in_report_tab_in_pe03 = 6
        OTHERS                      = 7.

    IF sy-subrc NE 0.
      CLEAR ls_return.
      ls_return-type = 'E'.
      ls_return-id   = 'ZHR_PA'.
      ls_return-number = '019'.
      ls_return-message_v1 = 'WEBMO'.
      ls_return-message_v2 = lv_pernr.
      APPEND ls_return TO lt_return.

      lo_message_container->add_messages_from_bapi( it_bapi_messages = lt_return ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_message_container.
    ENDIF.

    lv_webmo = lv_back.

* группировка подраздела персонала
    SELECT SINGLE molga
      INTO lv_molga
      FROM t001p
      WHERE
        werks EQ ls_p0001-werks AND
        btrtl EQ ls_p0001-btrtl.

* выборка данных
    SELECT *
      INTO TABLE lt_t556a_web
      FROM t556a_web
      WHERE
        webmo EQ lv_webmo   AND
        mopgk EQ '1'        AND
        mozko EQ lv_molga   AND
        endda GE sy-datum   AND
        begda LE sy-datum.

    SELECT *
      INTO TABLE lt_t554s_web
      FROM t554s_web
      WHERE
        webmo EQ lv_webmo   AND
        moabw EQ lv_molga   AND
        endda GE sy-datum   AND
        begda LE sy-datum.

    SELECT *
      INTO TABLE lt_t7ru_556b
      FROM t7ru_556b.

    SELECT *
      INTO TABLE lt_t7ru_554t
      FROM t7ru_554t.

* результат
    CLEAR ls_deep_entity.

    LOOP AT lt_t556a_web INTO ls_t556a_web.
      CLEAR ls_limit.
      ls_limit-ktart = ls_t556a_web-ktart.
      LOOP AT lt_t7ru_556b INTO ls_t7ru_556b
        WHERE
          sprsl EQ 'R'      AND
          mopgk EQ '1'      AND
          mozko EQ lv_molga AND
          ktart EQ ls_t556a_web-ktart.
        EXIT.
      ENDLOOP.
      IF sy-subrc EQ 0.
        ls_limit-klong = ls_t7ru_556b-klong.
      ENDIF.
      APPEND ls_limit TO ls_deep_entity-navlimit.
    ENDLOOP.

    lr_abs_exclude = ZCL_HRPA_TVARVC=>read_range( i_name = 'ZHR_ESS_VAC_TYPE_EXCLUDE' ).

    LOOP AT lt_t554s_web INTO ls_t554s_web.
      CLEAR ls_abs.
      ls_abs-awart = ls_t554s_web-subty.
      LOOP AT lt_t7ru_554t INTO ls_t7ru_554t
        WHERE
          sprsl EQ 'R'      AND
          moabw EQ lv_molga AND
          awart EQ ls_t554s_web-subty.
        EXIT.
      ENDLOOP.
      IF sy-subrc EQ 0.
        ls_abs-atext = ls_t7ru_554t-atext.
      ENDIF.
      IF lr_abs_exclude IS NOT INITIAL AND ls_abs-awart IN lr_abs_exclude.
        CONTINUE.
      ENDIF.
      APPEND ls_abs TO et_entityset.
    ENDLOOP.
    ls_limit-ktart = `99`.
    ls_limit-klong = `Ежегодный оплачиваемый отпуск!`.
    APPEND ls_limit TO ls_deep_entity-navlimit.
  ENDMETHOD.
ENDCLASS.

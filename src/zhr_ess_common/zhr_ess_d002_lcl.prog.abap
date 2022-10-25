*&---------------------------------------------------------------------*
*&  Include           ZHR_ESS_D002_LCL
*&---------------------------------------------------------------------*
CLASS lcl_d002 DEFINITION.
  PUBLIC SECTION.
    TYPES: ty_awart TYPE RANGE OF p2001-awart
         , ty_pernr TYPE RANGE OF p2001-pernr
         .
    METHODS: constructor IMPORTING iv_begda TYPE begda     DEFAULT sy-datum
                                   iv_days  TYPE int2      OPTIONAL
                                   iv_awart TYPE ty_awart  OPTIONAL
                                   iv_pernr TYPE ty_pernr  OPTIONAL
           , run
           .
  PROTECTED SECTION.
    DATA: BEGIN OF ms_prop
        , begda TYPE begda
        , endda TYPE endda
        , awart TYPE ty_awart
        , pernr TYPE ty_pernr
        , days  TYPE int2
        , END OF ms_prop
        .
    METHODS: get_days_text IMPORTING iv_days         TYPE int2
                           RETURNING VALUE(rv_ltext) TYPE ltext
                           .
ENDCLASS.

CLASS lcl_d002 IMPLEMENTATION.
  METHOD constructor.
    ms_prop-begda = iv_begda.
    IF ms_prop-begda IS INITIAL.
      ms_prop-begda = sy-datum.
    ENDIF.


    ms_prop-endda = ms_prop-begda + iv_days.
    ms_prop-awart = iv_awart.
    ms_prop-pernr = iv_pernr.
    ms_prop-days  = iv_days.

    IF ms_prop-awart IS INITIAL.
      APPEND VALUE #( sign = `E` option = `CP` low = `*` ) TO ms_prop-awart.
    ENDIF.
  ENDMETHOD.

  METHOD run.
    SELECT pa2001~pernr
         , pa2001~awart
         , t7ru_554t~atext
           INTO TABLE @DATA(lt_pa2001)
           FROM pa2001
           LEFT OUTER JOIN t7ru_554t ON t7ru_554t~sprsl = @sy-langu AND t7ru_554t~moabw = `33` AND t7ru_554t~awart = pa2001~awart
           WHERE pa2001~pernr IN @ms_prop-pernr
             AND pa2001~awart IN @ms_prop-awart
             AND pa2001~begda  = @ms_prop-endda
             AND pa2001~sprps  = @abap_false.

    SORT lt_pa2001 BY pernr awart.

    DATA(lo_assist) = zcl_mss_data_assistent=>get_instance( ).

    DATA(lv_subject) = CONV so_obj_des( 'Уведомление об отпуске' ).
    DATA(lv_textname) = 'ZHR_NOTIFICATION'.

    DATA: lt_mapping TYPE zttmail_mapping
        , lt_mail_to TYPE bcsy_smtpa
        , lv_days    TYPE ltext
        .
    lv_days = get_days_text( iv_days = ms_prop-days ).

    LOOP AT lt_pa2001 ASSIGNING FIELD-SYMBOL(<lt_pa2001>).
      DATA(lv_email) = lo_assist->get_pernr_0105( iv_pernr = <lt_pa2001>-pernr iv_begda = ms_prop-begda ).

      IF lv_email IS INITIAL.
        lv_email = lo_assist->get_pernr_0105( iv_pernr = <lt_pa2001>-pernr iv_begda = ms_prop-begda iv_subty = `0010` ).
      ENDIF.

      CHECK lv_email IS NOT INITIAL.

      REFRESH: lt_mapping
             .
      APPEND VALUE #( name = `<name>`  value = CONV zsmail_mapping-value( lo_assist->get_pernr_fio( iv_pernr = <lt_pa2001>-pernr ) ) ) TO lt_mapping.
      APPEND VALUE #( name = `<awart>` value = <lt_pa2001>-atext  )                                                                    TO lt_mapping.
      APPEND VALUE #( name = `<days>`  value = lv_days )                                                                               TO lt_mapping.

      APPEND INITIAL LINE TO lt_mail_to ASSIGNING FIELD-SYMBOL(<lt_mail_to>).
      <lt_mail_to> = lv_email.

      zcl_send_email=>send_mail( EXPORTING itd_to          = lt_mail_to
                                           iv_textname     = CONV #( lv_textname )
                                           iv_subject      = lv_subject
                                           itd_mapping     = lt_mapping
                                           iv_immediately  = abap_false ).
      COMMIT WORK.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_days_text.
    DATA: wa_days1    TYPE int4
        , wa_days2    TYPE int4
        , wa_days     TYPE text100
        .
    wa_days1 = iv_days.

    wa_days2 = wa_days1 MOD 10.
    wa_days1 = wa_days1 DIV 10.

    CASE wa_days2.
      WHEN 1.
        CASE wa_days1.
          WHEN 1.
            wa_days = 'дней'.
          WHEN OTHERS.
            wa_days = 'день'.
        ENDCASE.
      WHEN 2 OR 3 OR 4.
        CASE wa_days1.
          WHEN 1.
            wa_days = 'дней'.
          WHEN OTHERS.
            wa_days = 'дня'.
        ENDCASE.
      WHEN OTHERS.
        wa_days = 'дней'.
    ENDCASE.

    rv_ltext = iv_days && ` ` && wa_days.
    CONDENSE rv_ltext.
  ENDMETHOD.
ENDCLASS.

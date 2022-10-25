class ZCLHR_ESS_INCOME_INFO_DPC_EXT definition
  public
  inheriting from ZCLHR_ESS_INCOME_INFO_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~UPDATE_STREAM
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_STREAM
    redefinition .
protected section.

  methods REVIEWSET_CREATE_ENTITY
    redefinition .
  methods REVIEWSET_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCLHR_ESS_INCOME_INFO_DPC_EXT IMPLEMENTATION.


  METHOD /iwbep/if_mgw_appl_srv_runtime~create_stream.
    DATA: ls_zthr_ess_sveddf TYPE zthr_ess_sveddf.
    TRY.

        DATA(lo_assistent) = zcl_mss_data_assistent=>get_instance( ).

        READ TABLE it_key_tab ASSIGNING FIELD-SYMBOL(<reid>) WITH KEY name = 'MAIN_USER'.
        IF sy-subrc = 0 AND <reid>-value IS INITIAL.
          READ TABLE it_key_tab ASSIGNING <reid> WITH KEY name = 'ENTRANCE_USER'.
        ENDIF.
        IF sy-subrc = 0.
          DATA(lv_pernr) = lo_assistent->get_pernr( iv_usrid = CONV #( <reid>-value ) ).

          READ TABLE it_key_tab ASSIGNING FIELD-SYMBOL(<fina>) WITH KEY name = 'YEAR'.
          IF sy-subrc = 0.
            DATA(lv_zyear) = CONV gjahr( <fina>-value ).

            SELECT MAX( seqnr ) FROM zthr_ess_sveddf INTO @DATA(lv_seqnr)
            WHERE pernr = @lv_pernr
              AND zyear = @lv_zyear.

            IF sy-subrc = 0.
              lv_seqnr = lv_seqnr + 1.
            ELSE.
              lv_seqnr = 1.
            ENDIF.

            ls_zthr_ess_sveddf-pernr = lv_pernr.
            ls_zthr_ess_sveddf-zyear = lv_zyear.
            ls_zthr_ess_sveddf-seqnr = lv_seqnr.
            ls_zthr_ess_sveddf-filename = |{ lv_pernr }_Сведения_о_доходах_{ lv_zyear }.doc|.
            ls_zthr_ess_sveddf-filedata = is_media_resource-value.

            CALL FUNCTION 'ZHR_ESS_SVEDDF_INS' IN UPDATE TASK
              EXPORTING
                is_zthr_ess_sveddf = ls_zthr_ess_sveddf.

            LOOP AT NEW zcl_argus_xsb( iv_x = is_media_resource-value )->parse( ) INTO DATA(ls_parse).
              IF ls_parse-base-ref_begda IS NOT INITIAL.
                NEW zcl_argus_parser_saver(
                  is_parsed = ls_parse
                  iv_source = is_media_resource-value
                  iv_filename = |{ lv_pernr }_Сведения_о_доходах_{ lv_zyear }.xsb|
                )->save(
                  iv_electro = abap_true
                  iv_paper = abap_false
                  iv_portal = abap_true
                ).
              ENDIF.
            ENDLOOP.

            COMMIT WORK AND WAIT.
          ENDIF.
        ENDIF.
      CATCH /iwbep/cx_mgw_busi_exception .
      CATCH /iwbep/cx_mgw_tech_exception .
    ENDTRY.
  ENDMETHOD.


  METHOD /iwbep/if_mgw_appl_srv_runtime~update_stream.
    DATA: ls_zthr_ess_sveddf TYPE zthr_ess_sveddf.
    TRY.

        DATA(lo_assistent) = zcl_mss_data_assistent=>get_instance( ).

        READ TABLE it_key_tab ASSIGNING FIELD-SYMBOL(<reid>) WITH KEY name = 'MainUser'.
        IF sy-subrc = 0.
          DATA(lv_pernr) = lo_assistent->get_pernr( iv_usrid = CONV #( <reid>-value ) ).

          READ TABLE it_key_tab ASSIGNING FIELD-SYMBOL(<fina>) WITH KEY name = 'Year'.
          IF sy-subrc = 0.
            DATA(lv_zyear) = CONV gjahr( <fina>-value ).

            SELECT MAX( seqnr ) FROM zthr_ess_sveddf INTO @DATA(lv_seqnr)
            WHERE pernr = @lv_pernr
              AND zyear = @lv_zyear.

            IF sy-subrc = 0.
              lv_seqnr = lv_seqnr + 1.
            ELSE.
              lv_seqnr = 1.
            ENDIF.

            ls_zthr_ess_sveddf-pernr = lv_pernr.
            ls_zthr_ess_sveddf-zyear = lv_zyear.
            ls_zthr_ess_sveddf-seqnr = lv_seqnr.
            ls_zthr_ess_sveddf-filename = |{ lv_pernr }_Сведения_о_доходах_{ lv_zyear }.doc|.
            ls_zthr_ess_sveddf-filedata = is_media_resource-value.

            CALL FUNCTION 'ZHR_ESS_SVEDDF_INS' IN UPDATE TASK
              EXPORTING
                is_zthr_ess_sveddf = ls_zthr_ess_sveddf.

            LOOP AT NEW zcl_argus_xsb( iv_x = is_media_resource-value )->parse( ) INTO DATA(ls_parse).
              IF ls_parse-base-ref_begda IS NOT INITIAL.
                NEW zcl_argus_parser_saver(
                  is_parsed = ls_parse
                  iv_source = is_media_resource-value
                  iv_filename = |{ lv_pernr }_Сведения_о_доходах_{ lv_zyear }.xsb|
                )->save(
                  iv_electro = abap_true
                  iv_paper = abap_false
                  iv_portal = abap_true
                ).
              ENDIF.
            ENDLOOP.

            COMMIT WORK AND WAIT.
          ENDIF.
        ENDIF.
      CATCH /iwbep/cx_mgw_busi_exception .
      CATCH /iwbep/cx_mgw_tech_exception .
    ENDTRY.
  ENDMETHOD.


METHOD reviewset_create_entity.

  DATA: ls_request_input_data TYPE zshr_ess_ws_incomeinfo_review,
        ls_output_data        TYPE zshr_ess_ws_incomeinfo_review,
        lv_pernr              TYPE persno,
        lv_plans              TYPE p0001-plans.

  io_data_provider->read_entry_data( IMPORTING es_data = ls_request_input_data ).

  zcl_ess_data_assistent=>income_info_submit( EXPORTING is_input_data  = ls_request_input_data
                                              IMPORTING es_output_data = ls_output_data ).

  DATA(lo_assistent) = zcl_mss_data_assistent=>get_instance( ).
  DATA: lt_mapping  TYPE zttmail_mapping
      , lt_mail_to TYPE bcsy_smtpa
      .


**  << берем ШД из stvarv
  zcl_hr_get_data=>read_stvarv_par( EXPORTING i_name  = 'ZHR_ESS_PLANS_MAIL'
                                    IMPORTING e_value = lv_plans ).
**  << отпраляем тому, кто на ней сидит сегодня
  lv_pernr = lo_assistent->get_pernr_by_plans( iv_plans = lv_plans ).

  DATA(lv_email) = lo_assistent->get_pernr_0105( iv_pernr = lv_pernr ).
  IF lv_email IS NOT INITIAL.
    APPEND INITIAL LINE TO lt_mail_to ASSIGNING FIELD-SYMBOL(<lt_mail_to>).
    <lt_mail_to> = lv_email.
    IF ls_request_input_data-entrance_user IS NOT INITIAL.
      lv_pernr = lo_assistent->get_pernr( iv_usrid = CONV #( ls_request_input_data-entrance_user ) ).
    ELSE.
      lv_pernr = lo_assistent->get_pernr( iv_usrid = CONV #( ls_request_input_data-main_user ) ).
    ENDIF.
    APPEND VALUE #( name = `<pernr>` value = CONV zsmail_mapping-value( lv_pernr && ` ` && lo_assistent->get_pernr_fio( iv_pernr = lv_pernr ) ) ) TO lt_mapping.
    zcl_send_email=>send_mail( EXPORTING itd_to          = lt_mail_to
                                         iv_textname     = 'ZHR_INCOME_CREATE'
                                         iv_subject      = 'Подача сведений о доходах'
                                         itd_mapping     = lt_mapping
                                         iv_immediately  = abap_false ).
  ENDIF.

  MOVE-CORRESPONDING ls_output_data TO er_entity.

ENDMETHOD.


METHOD reviewset_get_entityset.

  DATA: ls_input  TYPE zcl_ess_data_assistent=>gtys_input_data.

  "Получение фильтров
  zcl_ess_data_assistent=>get_input_filter( EXPORTING io_tech_request_context  = io_tech_request_context
                                                      it_filter_select_options = it_filter_select_options
                                            IMPORTING es_input_data            = ls_input ).

  "Получение данных
  zcl_ess_data_assistent=>income_info_review( EXPORTING is_input_data  = ls_input
                                              IMPORTING et_output_data = DATA(lt_output) ).

  MOVE-CORRESPONDING lt_output TO et_entityset.


ENDMETHOD.
ENDCLASS.

class ZCLHR_ESS_PERSONAL_DPC_EXT definition
  public
  inheriting from ZCLHR_ESS_PERSONAL_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_STREAM
    redefinition .
protected section.

  methods ADDRSET_GET_ENTITYSET
    redefinition .
  methods EMPLOYERSET_GET_ENTITY
    redefinition .
  methods FAMILYSET_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCLHR_ESS_PERSONAL_DPC_EXT IMPLEMENTATION.


METHOD /iwbep/if_mgw_appl_srv_runtime~get_stream.

  DATA: ls_stream         TYPE ty_s_media_resource,
        ls_lheader        TYPE ihttpnvp,
        ls_converted_keys TYPE zshr_ess_ws_personal_t2.

  io_tech_request_context->get_converted_keys( IMPORTING es_key_values = ls_converted_keys ).

  "Определение табельного номера
  DATA(lo_hr_mss) = NEW zcl_mss_data_assistent( ).

  IF ls_converted_keys-entrance_user IS NOT INITIAL.
    DATA(lv_pernr) = lo_hr_mss->get_pernr( iv_usrid = CONV #( ls_converted_keys-entrance_user ) ).
  ELSE.
    lv_pernr = lo_hr_mss->get_pernr( iv_usrid = CONV #( ls_converted_keys-main_user ) ).
  ENDIF.

  CASE to_upper( iv_entity_name ).
    WHEN 'T2'.
      "ls_stream-mime_type = 'application/vnd.ms-word'.
      ls_stream-mime_type = 'application/msword'.

      zcl_ess_data_assistent=>personal_get_t2_data( EXPORTING iv_pernr    = lv_pernr
                                                    IMPORTING ev_content  = ls_stream-value
                                                              ev_filename = DATA(lv_file) ).
      ls_lheader-name = 'Content-Disposition'.
      ls_lheader-value = 'inline; filename="' && lv_file && '";'.
      set_header( is_header = ls_lheader ).
  ENDCASE.
*
  copy_data_to_ref( EXPORTING is_data = ls_stream
                    CHANGING  cr_data = er_stream ).

ENDMETHOD.


METHOD addrset_get_entityset.

  DATA: ls_input        TYPE zcl_ess_data_assistent=>gtys_input_data,
        lr_user         TYPE RANGE OF string,
        lr_filter_proce TYPE RANGE OF t7rusen-proce,
        lo_filter       TYPE REF TO /iwbep/cl_mgw_req_filter.


  "Получение фильтров
  zcl_ess_data_assistent=>get_input_filter( EXPORTING io_tech_request_context  = io_tech_request_context
                                                      it_filter_select_options = it_filter_select_options
                                            IMPORTING es_input_data            = ls_input ).

  "--------------- Получение данных ------------------"
  "Определение табельного номера
  DATA(lo_hr_mss) = NEW zcl_mss_data_assistent( ).
  IF ls_input-entrance_user IS NOT INITIAL.
    DATA(lv_pernr) = lo_hr_mss->get_pernr( iv_usrid = CONV #( ls_input-entrance_user ) ).
  ELSE.
    lv_pernr = lo_hr_mss->get_pernr( iv_usrid = CONV #( ls_input-main_user ) ).
  ENDIF.

  "Получние основных данных
  zcl_ess_data_assistent=>personal_get_address_data( EXPORTING iv_pernr = lv_pernr
                                                     IMPORTING et_data  = DATA(lt_data) ).

  MOVE-CORRESPONDING lt_data TO et_entityset.

ENDMETHOD.


METHOD employerset_get_entity.

  DATA: ls_converted_keys LIKE er_entity,
        ls_data           TYPE zshr_ess_ws_personal_emp.

  io_tech_request_context->get_converted_keys( IMPORTING es_key_values = ls_converted_keys ).

  "Определение табельного номера
  DATA(lo_hr_mss) = NEW zcl_mss_data_assistent( ).
  IF ls_converted_keys-entrance_user IS NOT INITIAL.
    DATA(lv_pernr) = lo_hr_mss->get_pernr( iv_usrid = CONV #( ls_converted_keys-entrance_user ) ).
  ELSE.
    lv_pernr = lo_hr_mss->get_pernr( iv_usrid = CONV #( ls_converted_keys-main_user ) ).
  ENDIF.

  "Получние данных
  zcl_ess_data_assistent=>personal_get_employer_data( EXPORTING iv_pernr = lv_pernr
                                                      IMPORTING es_data  = ls_data ).

  MOVE-CORRESPONDING ls_data TO er_entity.


ENDMETHOD.


METHOD familyset_get_entityset.

  DATA: ls_input        TYPE zcl_ess_data_assistent=>gtys_input_data,
        lr_user         TYPE RANGE OF string,
        lr_filter_proce TYPE RANGE OF t7rusen-proce,
        lo_filter       TYPE REF TO /iwbep/cl_mgw_req_filter.


  "--------------- Получение фильтров ------------------"
  zcl_ess_data_assistent=>get_input_filter( EXPORTING io_tech_request_context  = io_tech_request_context
                                                      it_filter_select_options = it_filter_select_options
                                            IMPORTING es_input_data            = ls_input ).

  "--------------- Получение данных ------------------"
  "Определение табельного номера
  DATA(lo_hr_mss) = NEW zcl_mss_data_assistent( ).
  IF ls_input-entrance_user IS NOT INITIAL.
    DATA(lv_pernr) = lo_hr_mss->get_pernr( iv_usrid = CONV #( ls_input-entrance_user ) ).
  ELSE.
    lv_pernr = lo_hr_mss->get_pernr( iv_usrid = CONV #( ls_input-main_user ) ).
  ENDIF.

  "Получние основных данных
  zcl_ess_data_assistent=>personal_get_family_data( EXPORTING iv_pernr = lv_pernr
                                                    IMPORTING et_data  = DATA(lt_data) ).

  MOVE-CORRESPONDING lt_data TO et_entityset.

ENDMETHOD.
ENDCLASS.

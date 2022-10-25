class ZCLHR_ESS_EXPERIENCE_DPC_EXT definition
  public
  inheriting from ZCLHR_ESS_EXPERIENCE_DPC
  create public .

public section.
protected section.

  methods STANDINGSET_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCLHR_ESS_EXPERIENCE_DPC_EXT IMPLEMENTATION.


METHOD standingset_get_entityset.

  DATA: ls_input        TYPE zcl_ess_data_assistent=>gtys_input_data.

  "Получение фильтров
  zcl_ess_data_assistent=>get_input_filter( EXPORTING io_tech_request_context  = io_tech_request_context
                                                      it_filter_select_options = it_filter_select_options
                                            IMPORTING es_input_data            = ls_input ).

  zcl_ess_data_assistent=>experience_get_data( EXPORTING is_input_data  = ls_input
                                               IMPORTING et_output_data = DATA(lt_output) ).

  MOVE-CORRESPONDING lt_output TO et_entityset.

ENDMETHOD.
ENDCLASS.

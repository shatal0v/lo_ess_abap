class ZCL_HR_UTIL definition
  public
  create public .

public section.

  class-methods SEL_SCREEN_DATA_READ
    returning
      value(RT_PARAMS) type RSPARAMS_TT .
  class-methods SEL_SCREEN_FILL_STRUC
    importing
      !IV_SHIFT type INT1 optional
    exporting
      !ES_STRUC type ANY .
protected section.
private section.
ENDCLASS.



CLASS ZCL_HR_UTIL IMPLEMENTATION.


  METHOD sel_screen_data_read.
    CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
      EXPORTING
        curr_report     = sy-cprog
      TABLES
        selection_table = rt_params
      EXCEPTIONS
        not_found       = 1
        no_report       = 2
        OTHERS          = 3.
  ENDMETHOD.


  METHOD sel_screen_fill_struc.
    FIELD-SYMBOLS: <any> TYPE any
                 , <tab> TYPE table
                 .
    DATA: descr_ref TYPE REF TO cl_abap_structdescr
        .
    descr_ref ?= cl_abap_typedescr=>describe_by_data( es_struc ).

    DATA(lt_components) =  descr_ref->components.
    SORT lt_components BY name.

    DATA(lt_rsparams) = zcl_hr_util=>sel_screen_data_read( ).

    " только нужные нам пар-ры
    LOOP AT lt_rsparams ASSIGNING FIELD-SYMBOL(<lt_rsparams>).
      READ TABLE lt_components ASSIGNING FIELD-SYMBOL(<lt_components>) WITH KEY name = <lt_rsparams>-selname+iv_shift BINARY SEARCH.
      CHECK <lt_components> IS ASSIGNED AND sy-subrc = 0.

      CASE <lt_components>-type_kind.
        WHEN 'h'." таблица
          ASSIGN COMPONENT <lt_rsparams>-selname+iv_shift OF STRUCTURE es_struc TO <tab>.
          CHECK <tab> IS ASSIGNED AND sy-subrc = 0.
          APPEND INITIAL LINE TO <tab> ASSIGNING <any>.
          MOVE-CORRESPONDING <lt_rsparams> TO <any>.
          CHECK <any> IS INITIAL.
          DELETE <tab> INDEX sy-tabix.
        WHEN OTHERS.
          ASSIGN COMPONENT <lt_rsparams>-selname+iv_shift OF STRUCTURE es_struc TO <any>.
          CHECK <any> IS ASSIGNED AND sy-subrc = 0.
          <any> = <lt_rsparams>-low.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

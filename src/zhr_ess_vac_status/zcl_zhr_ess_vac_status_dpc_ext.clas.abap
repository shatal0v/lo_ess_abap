class ZCL_ZHR_ESS_VAC_STATUS_DPC_EXT definition
  public
  inheriting from ZCL_ZHR_ESS_VAC_STATUS_DPC
  create public .

public section.
protected section.

  methods STATU_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZHR_ESS_VAC_STATUS_DPC_EXT IMPLEMENTATION.


  METHOD statu_get_entityset.
    SELECT  domvalue_l AS status
          , ddtext     AS ltext
          FROM dd07t INTO TABLE @ET_ENTITYSET WHERE ddlanguage = @sy-langu
                                                AND domname    = `TIM_REQ_STATUS`
                                                AND as4local   = `A` ORDER BY status.
  ENDMETHOD.
ENDCLASS.

class ZCL_ZHR_ESS_VAC_COL_DPC_EXT definition
  public
  inheriting from ZCL_ZHR_ESS_VAC_COL_DPC
  create public .

public section.
protected section.

  methods ENTITYMAINSET_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZHR_ESS_VAC_COL_DPC_EXT IMPLEMENTATION.


  METHOD entitymainset_get_entityset.
    SELECT zhrt_awart_color~awart
         , zhrt_awart_color~status
         , zhrt_awart_color~color
         , t554t~atext
         , dd07t~ddtext AS statustext
      FROM zhrt_awart_color
      LEFT JOIN t554t ON t554t~awart = zhrt_awart_color~awart
      LEFT JOIN dd07t ON ddlanguage = @sy-langu AND dd07t~domname = `TIM_REQ_STATUS` AND dd07t~as4local   = `A`
      INTO CORRESPONDING FIELDS OF TABLE @et_entityset
      WHERE t554t~sprsl = 'RU' AND
            t554t~moabw = '33'."zcl_hr_data=>c_moabw_33.
  ENDMETHOD.
ENDCLASS.

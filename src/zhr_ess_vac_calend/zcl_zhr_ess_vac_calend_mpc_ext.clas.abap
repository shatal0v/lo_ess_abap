class ZCL_ZHR_ESS_VAC_CALEND_MPC_EXT definition
  public
  inheriting from ZCL_ZHR_ESS_VAC_CALEND_MPC
  create public .

public section.
 TYPES :
      BEGIN OF ts_deep_entity,
        owner_fio    TYPE zvacation_fio,
        owner_number TYPE zpernr_number,
**        begda        TYPE begda,
**        endda        TYPE endda,
*    abs_table:
        navabs       TYPE STANDARD TABLE OF ts_entityabs WITH DEFAULT KEY,
      END OF ts_deep_entity.
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZHR_ESS_VAC_CALEND_MPC_EXT IMPLEMENTATION.
ENDCLASS.

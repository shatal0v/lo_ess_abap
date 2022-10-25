CLASS zcl_zhr_ess_vac_get_li_mpc_ext DEFINITION
  PUBLIC
  INHERITING FROM zcl_zhr_ess_vac_get_li_mpc
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ts_deep_entity,
        navabs   TYPE STANDARD TABLE OF ts_entityabs WITH DEFAULT KEY,
        navlimit TYPE STANDARD TABLE OF ts_entitylimit WITH DEFAULT KEY,
      END OF ts_deep_entity.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ZHR_ESS_VAC_GET_LI_MPC_EXT IMPLEMENTATION.
ENDCLASS.

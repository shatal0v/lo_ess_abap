CLASS zcl_zhr_ess_vac_appl_mpc_ext DEFINITION
  PUBLIC
  INHERITING FROM zcl_zhr_ess_vac_appl_mpc
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ts_deep_entity,
        login_act_dir TYPE  zlogin_act_dir,
        begda         TYPE  zbegda_yyyymmdd,
        navappl       TYPE STANDARD TABLE OF zcl_zhr_ess_vac_appl_mpc=>ts_entityapplication WITH DEFAULT KEY,
***        naverr        TYPE STANDARD TABLE OF zcl_z_vacation_applica_mpc=>ts_entityerror WITH DEFAULT KEY,
      END OF ts_deep_entity.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ZHR_ESS_VAC_APPL_MPC_EXT IMPLEMENTATION.
ENDCLASS.

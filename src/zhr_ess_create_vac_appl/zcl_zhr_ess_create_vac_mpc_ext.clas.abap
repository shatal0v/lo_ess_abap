CLASS zcl_zhr_ess_create_vac_mpc_ext DEFINITION
  PUBLIC
  INHERITING FROM zcl_zhr_ess_create_vac_mpc
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ts_deep_entity
              ,request_id TYPE zrequest_id
              ,default_begda TYPE begda
              ,default_endda TYPE endda
              ,abstype TYPE n LENGTH 4
              ,approver_fio TYPE zvacation_fio
              ,approver_number TYPE persno
              ,approver_vis TYPE flag
              ,owner_fio TYPE zvacation_fio
              ,owner_number  TYPE persno
              ,owner_vis TYPE flag
              ,initiator_fio TYPE zvacation_fio
              ,initiator_number  TYPE persno
              ,abs_limit TYPE n LENGTH 4
              ,navabs TYPE STANDARD TABLE OF ts_entityabs WITH DEFAULT KEY
         , END OF ts_deep_entity.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ZHR_ESS_CREATE_VAC_MPC_EXT IMPLEMENTATION.
ENDCLASS.

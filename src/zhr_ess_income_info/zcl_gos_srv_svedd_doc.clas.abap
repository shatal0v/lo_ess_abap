class ZCL_GOS_SRV_SVEDD_DOC definition
  public
  inheriting from CL_GOS_SERVICE
  create public .

public section.

  methods EXECUTE
    redefinition .
protected section.

  methods CHECK_STATUS
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_GOS_SRV_SVEDD_DOC IMPLEMENTATION.


  METHOD CHECK_STATUS.
    CASE is_lporb-typeid.
      WHEN 'BUS1065'.
        ep_status = mp_status_active.
      WHEN OTHERS.
        ep_status = MP_STATUS_INVISIBLE.
    ENDCASE.
  ENDMETHOD.


  METHOD execute.
    DATA(lv_pernr) = CONV pernr_d( gs_lporb-instid ).


    CALL FUNCTION 'ZHR_ESS_SVEDDF_POPUP'
      EXPORTING
        iv_pernr = lv_pernr.

    RAISE container_ignored.

  ENDMETHOD.
ENDCLASS.

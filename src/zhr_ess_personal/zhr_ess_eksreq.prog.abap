REPORT zhr_ess_eksreq.

INCLUDE zhr_ess_eksreq_top.
INCLUDE zhr_ess_eksreq_cl1.
INCLUDE zhr_ess_eksreq_frm.
INCLUDE zhr_ess_eksreq_p01.
INCLUDE zhr_ess_eksreq_pa1.

INITIALIZATION.
  FREE: gt_att.
  gv_usrid = sy-uname.
  SELECT SINGLE pernr FROM pa0105 INTO gv_pernr WHERE subty = '0001' AND usrid = sy-uname.

  CALL FUNCTION 'DD_DOMVALUES_GET'
    EXPORTING
      domname        = 'ZDHR_ESS_EKSREQ_STATUS'
      text           = 'X'
      langu          = sy-langu
    TABLES
      dd07v_tab      = gt_idd07v
    EXCEPTIONS
      wrong_textflag = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
  ENDIF.

START-OF-SELECTION.
  PERFORM select_data.

END-OF-SELECTION.
  CALL SCREEN 100.

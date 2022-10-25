*&---------------------------------------------------------------------*
*&  Include           ZHR_ESS_EKSREQ_TOP
*&---------------------------------------------------------------------*
CONSTANTS:  cv_alv_structure TYPE dd02l-tabname VALUE 'ZSHR_ESS_EKSREQ_ALV',
            cs_stable        TYPE lvc_s_stbl    VALUE '11'.

DATA: gt_alv    TYPE TABLE OF zshr_ess_eksreq_alv,
      go_alv    TYPE REF TO cl_gui_alv_grid,
      go_cnt    TYPE REF TO cl_gui_custom_container,
      ok_code   TYPE sy-ucomm,
      gt_idd07v TYPE TABLE OF dd07v WITH HEADER LINE,
      gv_usrid  TYPE p0105-usrid,
      gv_pernr  TYPE p0105-pernr,
      gt_att    TYPE TABLE OF zthr_ess_eksreqf,
      gv_coment TYPE zehr_ess_eksreq_coment,
      gv_comment TYPE zehr_ess_docreq_comment,
      gv_hrcomment TYPE zehr_ess_docreq_comment,
      gv_selrow TYPE row_id,
      gv_status TYPE zehr_ess_eksreq_status.

DATA:
*   reference to wrapper class of control
  g_editor           TYPE REF TO cl_gui_textedit,
  g_editor_container TYPE REF TO cl_gui_custom_container.
DATA:
*   reference to wrapper class of control
  g_editor_new           TYPE REF TO cl_gui_textedit,
  g_editor_container_new TYPE REF TO cl_gui_custom_container.
DATA: BEGIN OF gtext OCCURS 200,
        line(72),
      END OF gtext.
DATA: BEGIN OF gtext_new OCCURS 200,
        line(72),
      END OF gtext_new.

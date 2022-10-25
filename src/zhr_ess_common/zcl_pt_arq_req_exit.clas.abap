class ZCL_PT_ARQ_REQ_EXIT definition
  public
  final
  create public .

*"* public components of class ZCL_PT_ARQ_REQ_EXIT
*"* do not include other source files here!!!
public section.
  type-pools PTGQD .
  type-pools TPTIM .

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_PT_ABS_REQ .

  data MESSAGE_HANDLER type ref to IF_PT_REQ_MESSAGE_HANDLER .

  methods CONSTRUCTOR .
protected section.
*"* protected components of class ZCL_PT_ARQ_REQ_EXIT
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_PT_ARQ_REQ_EXIT
*"* do not include other source files here!!!

  class-data PT_REQ_APPLICATION type ref to IF_PT_REQ_APPLICATION .
  class-data ACTOR_AGENT type ref to CA_PT_REQ_ACTOR .
  class-data SAVED_QUOTAS type PTARQ_BAPIABWKON_TAB .
  class-data SAVED_TIME_ACCOUNTS type PTARQ_TIME_ACCOUNTS_TAB .
  class-data AUTH_CHECKER type ref to IF_EX_HRPAD00AUTH_CHECK .
ENDCLASS.



CLASS ZCL_PT_ARQ_REQ_EXIT IMPLEMENTATION.


METHOD CONSTRUCTOR.
* XRK 20060922 Note983558 IT0130


  CLASS cl_pt_req_actor DEFINITION LOAD.
  actor_agent   = ca_pt_req_actor=>agent.

*---Create message handler singleton
  CLASS cl_pt_req_message_handler DEFINITION LOAD.
  CALL METHOD cl_pt_req_message_handler=>instance_get
    RECEIVING
      result = message_handler.

  CALL METHOD cl_pt_req_application=>get_instance
    RECEIVING
      result = pt_req_application.

*---Create authority checker object                         "Note983558
  IF auth_checker IS INITIAL.                               "Note983558
    CALL FUNCTION 'HR_GET_BUSINESS_ADD_IN'                  "Note983558
      EXPORTING                                             "Note983558
        exit_name = 'HRPAD00AUTH_CHECK'                     "Note983558
      CHANGING                                              "Note983558
        instance  = auth_checker.                           "Note983558
                                                            "Note983558
    IF auth_checker IS INITIAL.                             "Note983558
      CREATE OBJECT auth_checker                            "Note983558
               TYPE cl_hrpad00auth_check_std.               "Note983558
    ENDIF.                                                  "Note983558
  ENDIF.                                                    "Note983558

ENDMETHOD.


  method IF_EX_PT_ABS_REQ~ADD_ACCRUAL_TO_QUOTA_BAL.
     ADD_TO_QUOTA_BALANCE = abap_false.
     RETURN .
  endmethod.


METHOD IF_EX_PT_ABS_REQ~AUTH_CHECK_AND_ANONYMIZE_DATA.
* XRK 20100803 Note1165170 Authorization issue for POSTED records
  DATA:
      mode            TYPE pt_req_mode,
      constraints     TYPE ptarq_tconstr,
      no_authcheck    TYPE char1,
      owner_pernr     TYPE persno,
      approver_pernr  TYPE persno,
      uname_pernr     TYPE persno,
      message_wa      TYPE  LINE OF ptreq_message_tab,
      it_employees    TYPE pernr_us_tab,
      l_employee      TYPE LINE OF pernr_us_tab,
      message_handler TYPE REF TO if_pt_req_message_handler,
      lt_request_list TYPE ptreq_request_tab,               "Note1165170
      request_wa      TYPE REF TO if_pt_req_request,        "Note1165170
      all_versions_tab TYPE ptreq_request_tab_flat,         "Note1165170
      wa_all_versions LIKE LINE OF all_versions_tab,        "Note1165170
      infty           TYPE prelp-infty,
      subty           TYPE prelp-subty,
      begda           TYPE sy-datum,
      endda           TYPE sy-datum,
      ui_request      TYPE ptarq_uia_request,
      packed          TYPE ptreq_request_struc_flat,
      sel_reqlist     TYPE LINE OF ptarq_reqlist_tab_flat.
  DATA  status  TYPE tim_req_status.                        "ROY1353483

  FIELD-SYMBOLS:
        <abs>           TYPE ptarq_p2001_struc,
        <att>           TYPE ptarq_p2002_struc.

*---Read customizing for authority check
*  READ TABLE im_pernr_tab INDEX 1 INTO pernr.                    "DELROY_1620547
********************Begin of LAK1758910********************
*  LOOP AT im_pernr_tab INTO pernr.                                "ROY_1620547
*---Get personnel number of SY-UNAME (person requesting to see data)
  CALL FUNCTION 'HR_GET_EMPLOYEES_FROM_USER_DB'
    EXPORTING
      user   = sy-uname
      begda  = sy-datum
      endda  = sy-datum
    TABLES
      ee_tab = it_employees.
  READ TABLE it_employees INDEX 1 INTO l_employee.
  uname_pernr = l_employee-pernr.
  IF uname_pernr IS INITIAL.
*     Missing PA0105 customizing
    message_wa-message_v1 = sy-uname.
    IF 1 = 2.
*---Workaround for where-used list
      MESSAGE e075(hrtim_abs_req) WITH sy-uname.
    ENDIF.
*---Create message handler singleton
    CALL METHOD cl_pt_req_message_handler=>instance_get
      RECEIVING
        result = message_handler.
    CALL METHOD message_handler->add_message
      EXPORTING
        im_type       = 'E'
        im_cl         = 'HRTIM_ABS_REQ'
        im_number     = '075'
        im_par1       = message_wa-message_v1
        im_context    = 'REQLIST_GET'
        im_classname  = 'CL_PT_ARQ_BADI'
        im_methodname = 'AUTH_CHECK_AND_ANONYMIZE_DATA'.
  ENDIF.
********************End of LAK1758910********************
  CALL METHOD cl_pt_arq_customizing=>get_time_constraints
    EXPORTING
*      im_pernr       = pernr                              "LAK1758910
      im_pernr       = uname_pernr                         "LAK1758910
      im_date        = sy-datum
    IMPORTING
      ex_constraints = constraints
    EXCEPTIONS
      OTHERS         = 1.
  IF sy-subrc <> 0.
*---Perform authority check
    CLEAR no_authcheck.
  ELSE.
    CALL METHOD im_pt_arq_application->get_modus
      IMPORTING
        ex_modus = mode.
    CASE mode.
      WHEN 'R'.
*---ESS authority check
        IF NOT constraints-ess_no_authcheck IS INITIAL.
          no_authcheck = 'X'.
        ENDIF.
      WHEN 'A' OR 'T' OR 'O' OR 'C'.
*---MSS authority check
        IF NOT constraints-mss_no_authcheck IS INITIAL.
          no_authcheck = 'X'.
        ENDIF.
      WHEN OTHERS.
*---Perform authority check
        CLEAR no_authcheck.
    ENDCASE.
  ENDIF.
********************Begin of LAK1758910***************************
**---Get personnel number of SY-UNAME (person requesting to see data)
*  CALL FUNCTION 'HR_GET_EMPLOYEES_FROM_USER_DB'
*    EXPORTING
*      user   = sy-uname
*      begda  = sy-datum
*      endda  = sy-datum
*    TABLES
*      ee_tab = it_employees.
*  READ TABLE it_employees INDEX 1 INTO l_employee.
*  uname_pernr = l_employee-pernr.
*  IF uname_pernr IS INITIAL.
**     Missing PA0105 customizing
*    message_wa-message_v1 = sy-uname.
*    IF 1 = 2.
**---Workaround for where-used list
*      MESSAGE e075(hrtim_abs_req) WITH sy-uname.
*    ENDIF.
**---Create message handler singleton
*    CALL METHOD cl_pt_req_message_handler=>instance_get
*      RECEIVING
*        result = message_handler.
*    CALL METHOD message_handler->add_message
*      EXPORTING
*        im_type       = 'E'
*        im_cl         = 'HRTIM_ABS_REQ'
*        im_number     = '075'
*        im_par1       = message_wa-message_v1
*        im_context    = 'REQLIST_GET'
*        im_classname  = 'CL_PT_ARQ_BADI'
*        im_methodname = 'AUTH_CHECK_AND_ANONYMIZE_DATA'.
*  ENDIF.
********************End of LAK1758910************************
*---Perform authority check for each request/infotype record
*  LOOP AT ch_selected_reqlist INTO sel_reqlist.                                          "DELROY_1620547
*    LOOP AT ch_selected_reqlist INTO sel_reqlist WHERE version-owner-pernr = pernr.       "ROY_1620547
   LOOP AT ch_selected_reqlist INTO sel_reqlist.            "LAK1758910
    CLEAR approver_pernr.
*---Determine personnel number of request/infotype
    IF sel_reqlist-request_or_attabs EQ 'R'.
      owner_pernr = sel_reqlist-version-owner-pernr.
      approver_pernr = sel_reqlist-version-next_processor-pernr.
      "Begin insert note 1165170
*---Special processing for a POSTED request since its approver is blank
      IF sel_reqlist-version-status EQ cl_pt_req_const=>c_reqstat_posted AND
         approver_pernr IS INITIAL.
        CALL METHOD ca_pt_req_header=>sel_reqs_by_all_attribs
          EXPORTING
            im_request_id       = sel_reqlist-request_id
            im_request_type     = cl_pt_arq_const=>c_req_type
            im_sel_only_current = ' '
          IMPORTING
            ex_result_tab       = lt_request_list
          EXCEPTIONS
            actor_not_found     = 1
            OTHERS              = 2.
        IF sy-subrc <> 0.
*         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ELSE.
          LOOP AT lt_request_list INTO request_wa.
            CALL METHOD request_wa->get_all_attribs_for_all_vers
              IMPORTING
                ex_attribs_tab = all_versions_tab.
            LOOP AT all_versions_tab INTO wa_all_versions
              WHERE version-status EQ cl_pt_req_const=>c_reqstat_approved.
              IF NOT wa_all_versions-version-next_processor-pernr IS INITIAL.
                approver_pernr = wa_all_versions-version-next_processor-pernr.
                EXIT.
              ENDIF.
            ENDLOOP.
          ENDLOOP.
        ENDIF.
      ENDIF.
      "End insert note 1165170
    ELSEIF sel_reqlist-request_or_attabs EQ 'A'.
      LOOP AT sel_reqlist-absences ASSIGNING <abs>.
        CHECK NOT <abs>-owner-pernr IS INITIAL.
        owner_pernr = <abs>-owner-pernr.
        EXIT.
      ENDLOOP.
    ELSEIF sel_reqlist-request_or_attabs EQ 'P'.
      LOOP AT sel_reqlist-attendances ASSIGNING <att>.
        CHECK NOT <att>-owner-pernr IS INITIAL.
        owner_pernr = <att>-owner-pernr.
        EXIT.
      ENDLOOP.
    ENDIF.
*---Request initiator/owner can always see own request/infotype data
*   Request approver also needs to see data
    CHECK uname_pernr NE owner_pernr AND uname_pernr NE approver_pernr.
*---It's a different person --> retrieve relevant data from
*   request/infotype and check authorization
    PERFORM unpack_reqlist_row IN PROGRAM saplpt_arq_request_uia
    USING sel_reqlist
    CHANGING ui_request.
    IF NOT ui_request-ins_item IS INITIAL.
      infty = ui_request-ins_item-infotype.
      subty = ui_request-ins_item-subty.
      begda = ui_request-ins_item-begda.
      endda = ui_request-ins_item-endda.
    ELSEIF NOT ui_request-del_item IS INITIAL.
      infty = ui_request-del_item-infotype.
      subty = ui_request-del_item-subty.
      begda = ui_request-del_item-begda.
      endda = ui_request-del_item-endda.
    ENDIF.
    CALL FUNCTION 'HR_CHECK_AUTHORITY_INFTY'
      EXPORTING
        tclas            = 'A'
        pernr            = ui_request-owner-pernr
        infty            = infty
        subty            = subty
        begda            = begda
        endda            = endda
        level            = 'R'
        uname            = sy-uname
      EXCEPTIONS
        no_authorization = 1
        internal_error   = 2
        OTHERS           = 3.
    IF sy-subrc <> 0.
      IF no_authcheck IS INITIAL.
*---Remove data since it must not be shown
        DELETE ch_selected_reqlist.
      ELSE.
*---Anonymize data
        status = sel_reqlist-version-status.                "ROY1353483
        PERFORM remove_private_information IN PROGRAM saplpt_arq_request_uia
        CHANGING ui_request.
        PERFORM pack_request IN PROGRAM saplpt_arq_request_uia
        USING ui_request
        CHANGING packed.
        sel_reqlist-version = packed-version.
*          IF sel_reqlist-request_or_attabs = 'R'.                                                  "DEL Jain_1655501
*        IF sel_reqlist-request_or_attabs = 'R' OR sel_reqlist-request_or_attabs = 'P'.             "LAK2016454
        IF sel_reqlist-request_or_attabs = 'R' .                                                    "LAK2016454
          IF status = 'POSTED'.                             "ROY1353483
            sel_reqlist-version-status = status.            "ROY1353483
          ELSE.                                             "ROY1353483
            sel_reqlist-version-status = ui_request-status.
          ENDIF.
        ELSEIF sel_reqlist-request_or_attabs = 'P'.                                                  "LAK2016454
          sel_reqlist-version-status = status.                                                       "LAK2016454
          CLEAR status.                                                                              "LAK2016454
        ELSEIF sel_reqlist-request_or_attabs = 'A'.               "ROY_SPC_1472072
            sel_reqlist-version-status = status.                  "ROY_SPC_1472072
          CLEAR status.                                           "ROY1353483
        ENDIF.
        CLEAR sel_reqlist-first_subm_date.
        CLEAR sel_reqlist-first_subm_time.
        CLEAR sel_reqlist-deduction[].
        LOOP AT sel_reqlist-absences ASSIGNING <abs>.
          CLEAR <abs>-p2001-infty.
          CLEAR <abs>-p2001-subty.
          CLEAR <abs>-subty_description.
          CLEAR <abs>-p2001-objps.
          CLEAR <abs>-p2001-sprps.
          CLEAR <abs>-p2001-seqnr.
          CLEAR <abs>-p2001-abwtg.                               "Note1165170
          CLEAR <abs>-p2001-kaltg.                               "Note1165170
          CLEAR <abs>-p2001-alldf.                               "Note1165170
          CLEAR <abs>-p2001-abrtg.                               "Note1165170
          CLEAR <abs>-p2001-abrst.                               "Note1165170
          CLEAR <abs>-p2001-beguz.
          CLEAR <abs>-p2001-enduz.
          CLEAR <abs>-p2001-stdaz.
        ENDLOOP.
        LOOP AT sel_reqlist-attendances ASSIGNING <att>.
          CLEAR <att>-p2002-infty.
          CLEAR <att>-p2002-subty.
          CLEAR <att>-subty_description.
          CLEAR <att>-p2002-objps.
          CLEAR <att>-p2002-sprps.
          CLEAR <att>-p2002-seqnr.
          CLEAR <att>-p2002-abwtg.                               "Note1165170
          CLEAR <att>-p2002-kaltg.                               "Note1165170
          CLEAR <att>-p2002-alldf.                               "Note1165170
          CLEAR <att>-p2002-abrtg.                               "Note1165170
          CLEAR <att>-p2002-abrst.                               "Note1165170
          CLEAR <att>-p2002-beguz.
          CLEAR <att>-p2002-enduz.
          CLEAR <att>-p2002-stdaz.
        ENDLOOP.
*---Update internal table
        MODIFY ch_selected_reqlist FROM sel_reqlist.
      ENDIF.
    ENDIF.
    ENDLOOP.                                                       "ROY_1620547
*  ENDLOOP.                                                "LAK1758910

ENDMETHOD.


METHOD IF_EX_PT_ABS_REQ~CHECK_ATTACHMENT_MANDATORY.
ENDMETHOD.


  method IF_EX_PT_ABS_REQ~CHECK_DUPLICATED_APPROVERS.

* Check duplicated approvers
    " In current solution, if the approver list has duplicated approvers, it will return error.


    DATA: LT_APPROVER      TYPE PTREQ_APPROVER_TAB.
    DATA: ls_approver      LIKE LINE OF IT_APPROVER.
    DATA: ls_approver_tmp  LIKE LINE OF IT_APPROVER.

    clear ev_has_duplication.
    lt_approver = it_approver.
    sort lt_approver ASCENDING by pernr.
    Loop at lt_approver into ls_approver.
      if ls_approver_tmp is INITIAL or ls_approver_tmp-pernr <> ls_approver-pernr.
        ls_approver_tmp = ls_approver.
      else.
        EV_HAS_DUPLICATION = 'X'.
        RETURN.
      endif.
    ENDLOOP.
  endmethod.


METHOD IF_EX_PT_ABS_REQ~CHECK_TIME_CONSTRAINTS .
** XML 20041103 NOTE788234  New Time Constraints                         *
*                                                                    "BEGIN OF DEL NOTE788234
*  DATA:
*    custo_constraints TYPE ptarq_tconstr,
*    wa_attabs         TYPE ptarq_attabsdata_struc,
*    wa_abs_attribs    TYPE p2001,
*    wa_atts_attribs   TYPE p2002,
*    pernr             TYPE p_pernr,
*    wa_begda          TYPE sy-datum,
*    wa_endda          TYPE sy-datum,
*    max_date          TYPE sy-datum,
*    min_date          TYPE sy-datum,
*    ref_date          TYPE sy-datum,
*    first_week_day    TYPE sy-datum,
*    max_multiplier    TYPE i,
*    min_multiplier    TYPE i,
*    no_check_max      TYPE c,
*    no_check_min      TYPE c,
*    lcl_msgv1         TYPE sy-msgv1,
*    lcl_msgv2         TYPE sy-msgv1.
*
*
*
***TODO---
*  CONSTANTS: custo_first_day TYPE ptarq_weekday VALUE '0'.
*
*
*  LOOP AT im_attabs_tab INTO wa_attabs.
*
*    CHECK wa_attabs-operation = cl_pt_req_const=>c_item_ins.
*
*    IF NOT wa_attabs-abs_attribs IS INITIAL.
*
*      READ TABLE wa_attabs-abs_attribs INDEX 1
*      INTO wa_abs_attribs.
*
*      pernr    = wa_abs_attribs-pernr.
*      wa_begda = wa_abs_attribs-begda.
*      wa_endda = wa_abs_attribs-endda.
*
*
*    ELSEIF  NOT wa_attabs-atts_attribs IS INITIAL.
*
*      READ TABLE wa_attabs-atts_attribs INDEX 1
*      INTO wa_atts_attribs.
*
*      pernr    = wa_atts_attribs-pernr.
*      wa_begda = wa_atts_attribs-begda.
*      wa_endda = wa_atts_attribs-endda.
*    ENDIF.
*
*
*    CALL METHOD cl_pt_arq_badi=>get_time_constraints
*      EXPORTING
*        im_pernr         = pernr
*        im_date          = sy-datum
*      IMPORTING
*        ex_constraints   = custo_constraints
*      EXCEPTIONS
*        no_customizing   = 1
*        it0001_not_found = 2
*        it0008_not_found = 3
*        OTHERS           = 4.
*
*    CHECK sy-subrc = 0.
*
***** Set check_date
*
*    max_multiplier = custo_constraints-arq_poss_til.
*    min_multiplier = custo_constraints-arq_poss_snc.
*
*    IF
*      max_multiplier                     = cl_pt_arq_const=>c_arq_custo_fristen_max AND
*      custo_constraints-unit_arq_pos_til = cl_pt_arq_const=>c_arq_custo_year        AND
*      min_multiplier                     = cl_pt_arq_const=>c_arq_custo_fristen_max AND
*      custo_constraints-unit_arq_pos_snc = cl_pt_arq_const=>c_arq_custo_year.
*
*      EXIT.
*
*    ENDIF.
*
*    CASE custo_constraints-arq_poss_til_def.
** BEGIN "From the current date
*
*      WHEN cl_pt_req_const=>c_custo_from_current_date.
*
*        CALL METHOD cl_pt_arq_badi=>future_from_current_date
*          EXPORTING
*            im_unit           = custo_constraints-unit_arq_pos_til
*            im_max_multiplier = max_multiplier
*          IMPORTING
*            ex_max_date       = max_date.
*
** BEGIN "From the beginning of the curremt time unit
*
*      WHEN cl_pt_req_const=>c_custo_from_current_time_unit .
*
*        CALL METHOD cl_pt_arq_badi=>future_from_current_time_unit
*          EXPORTING
*            im_unit           = custo_constraints-unit_arq_pos_til
*            im_max_multiplier = max_multiplier
*            im_first_week_day = custo_first_day
*          IMPORTING
*            ex_max_date       = max_date.
*
** BEGIN "From the beginning of the current time period
*
*      WHEN cl_pt_req_const=>c_custo_from_current_period.
*
*        CALL METHOD cl_pt_arq_badi=>future_from_current_period
*          EXPORTING
*            im_permo          = custo_constraints-prmo_arq_pos_til
*            im_max_multiplier = max_multiplier
*          IMPORTING
*            ex_max_date       = max_date.
*
*
*    ENDCASE.
*
** Check for Errors for in Custo " Time Events nachträglich bearbeiten
**    IF wa_endda <= max_date.
***      OK
**    ELSE.
**
**      IF 1 = 2.
***---Workaround for where-used list
**        MESSAGE e064(hrtim_abs_req) WITH '&'.
**      ENDIF.
**
**      CALL METHOD message_handler->add_message
**        EXPORTING
**          im_type       = 'E'
**          im_cl         = 'HRTIM_ABS_REQ'
**          im_number     = '064'
**          im_par1       = 'CHECK_DATES_FOR_REQUEST'
**          im_par2       = 'IF_EX_PT_ABS_REQ~CHECK_DATES_FOR_REQUEST'
**          im_par3       = 'BLOP'
**          im_context    = 'LDATE'
**          im_subcontext = ''
**          im_classname  = 'CL_PT_ARQ_BLOP_ADAPTER'
**          im_methodname = 'CHECK_DATES_FOR_REQUEST'.
**
**    ENDIF.
*
**** END Check in the future
******************************
*
*    CLEAR ref_date.
*    CLEAR max_multiplier.
*
*
*
**** Check in the past
*****************************
*
*    CASE custo_constraints-arq_poss_snc_def.
**** BEGIN "From the current date
*      WHEN cl_pt_req_const=>c_custo_from_current_date.
*        CALL METHOD cl_pt_arq_badi=>past_from_current_date
*          EXPORTING
*            im_unit           = custo_constraints-unit_arq_pos_snc
*            im_min_multiplier = min_multiplier
*          IMPORTING
*            ex_min_date       = min_date.
*
** BEGIN "From the beginning of the curremt time unit
*      WHEN cl_pt_req_const=>c_custo_from_current_time_unit .
*
*        CALL METHOD cl_pt_arq_badi=>past_from_current_time_unit
*          EXPORTING
*            im_unit           = custo_constraints-unit_arq_pos_snc
*            im_min_multiplier = min_multiplier
*            im_first_week_day = custo_first_day
*          IMPORTING
*            ex_min_date       = min_date.
*
*** BEGIN "From the beginning of the current time period
*
*      WHEN cl_pt_req_const=>c_custo_from_current_period.
*        CALL METHOD cl_pt_arq_badi=>past_from_current_period
*          EXPORTING
*            im_permo          = custo_constraints-prmo_arq_pos_snc
*            im_min_multiplier = min_multiplier
*          IMPORTING
*            ex_min_date       = min_date.
*
*
*    ENDCASE.
*
*    IF wa_begda >= min_date and
*       wa_endda <= max_date.
**    OK
*    ELSE.
*
*      IF 1 = 2.
**---Workaround for where-used list
*        MESSAGE e063(hrtim_abs_req) WITH '&'.
*      ENDIF.
*
*      WRITE min_date TO lcl_msgv1.
*      WRITE max_date TO lcl_msgv2.
*
*      CALL METHOD message_handler->add_message
*        EXPORTING
*          im_type       = 'E'
*          im_cl         = 'HRTIM_ABS_REQ'
*          im_number     = '063'
*          im_par1       = lcl_msgv1
*          im_par2       = lcl_msgv2
**          im_par3       = 'BLOP'
*          im_context    = '(Begdate:Enddate)'
*          im_subcontext = ''
*          im_classname  = 'CL_PT_ARQ_BLOP_ADAPTER'
*          im_methodname = 'CHECK_DATES_FOR_REQUEST'.
*
*    ENDIF.
*
*  ENDLOOP.
*                                                                    "END OF DEL NOTE788234
ENDMETHOD.


METHOD IF_EX_PT_ABS_REQ~CHECK_TIME_CONSTR_FOR_SUBTY.
* XRK 20100107 Note983558 Consider IT0130 for authorization check
* XRK 20060922 Note983558 IT0130

** 5.00                                                                  *
** XML 20040603 NOTE742545  Coding Error                                 *
** XML 20040701 NOTE751154  Meldung Text                                 *
** XML 20041103 NOTE788234  New Time Constraints                         *

*                                                                       "BEGIN OF INS NOTE 20041103

  TYPES:
    BEGIN OF error_type,
      number TYPE sy-msgno,
      par1   TYPE sy-msgv1,
      par2    TYPE sy-msgv1,
    END OF error_type .

  TYPES error_type_tab TYPE STANDARD TABLE OF error_type.

  DATA:
     number_of_rows      TYPE i,
     wa_attabs           TYPE ptarq_attabsdata_struc,
     wa_abs_attribs      TYPE p2001,
     wa_atts_attribs     TYPE p2002,
     pernr               TYPE p_pernr,
     wa_begda            TYPE sy-datum,
     wa_endda            TYPE sy-datum,
     forbid_up_to        TYPE sy-datum,
     forbid_from         TYPE sy-datum,
     infty               TYPE infty,                        "Note983558
     subty               TYPE subty,
     subty_attribs_tab   TYPE attabs_attributes_tab,
     moabw               TYPE moabw,
     wa_is_authorized    TYPE boolean VALUE '-',            "Note983558
     wa_t584a            TYPE t584a,                        "Note983558
     p0130_w             TYPE pa0130,                       "Note983558
     wa_subty_attribs    TYPE attabs_attributes_struc,
     wa_error            TYPE error_type,
     error_tab           TYPE error_type_tab,
     error_unit          TYPE dd07v,
     error_qty           TYPE domvalue_l,
     tomorrow            TYPE sy-datum.

  DESCRIBE TABLE im_attabs_tab LINES number_of_rows.

* If the request has only one position, is an INS or DEL request
* If not, is a MOD request, and then, I need to read the position INS
  IF number_of_rows = 1.
    READ TABLE im_attabs_tab INTO wa_attabs INDEX 1.
  ELSE.
    READ TABLE im_attabs_tab INTO wa_attabs
    WITH KEY operation = cl_pt_req_const=>c_item_ins.
*Begin of note  SOW_1030261
*This code is added for the case when there are 2 DEL items.
    IF sy-subrc <> 0.
      READ TABLE im_attabs_tab INTO wa_attabs
      WITH KEY operation = cl_pt_req_const=>c_item_del.
    ENDIF.
*End of note SOW_1030261
  ENDIF.

* Can be Absences or Attendance
  IF NOT wa_attabs-abs_attribs IS INITIAL.

    READ TABLE wa_attabs-abs_attribs INDEX 1
    INTO wa_abs_attribs.

    pernr    = wa_abs_attribs-pernr.
    wa_begda = wa_abs_attribs-begda.
    wa_endda = wa_abs_attribs-endda.
    infty    = wa_abs_attribs-infty.                        "Note983558
    subty    = wa_abs_attribs-subty.

  ELSEIF NOT wa_attabs-atts_attribs IS INITIAL.

    READ TABLE wa_attabs-atts_attribs INDEX 1
    INTO wa_atts_attribs.

    pernr    = wa_atts_attribs-pernr.
    wa_begda = wa_atts_attribs-begda.
    wa_endda = wa_atts_attribs-endda.
    infty    = wa_atts_attribs-infty.                       "Note983558
    subty    = wa_atts_attribs-subty.
  ENDIF.

* Assume that no authorization exists and determine type(s) "Note983558
* of check CKTYP                                            "Note983558
* Note 1759157 - change to the other way, to fit to PA30.     "Note1759157
* Assume that authorization exists:                           "Note1759157
wa_is_authorized = 'X'.                                       "Note1759157
  SELECT * FROM t584a                                       "Note983558
           INTO wa_t584a                                    "Note983558
          WHERE infty = infty AND                           "Note983558
                subty = subty.                              "Note983558
* For an existing type of check, read its IT0130 record     "Note983558
    SELECT SINGLE * FROM pa0130                             "Note983558
                    INTO p0130_w                            "Note983558
                   WHERE pernr = pernr AND                  "Note983558
                         subty = wa_t584a-cktyp AND         "Note983558
                         reldt >= wa_begda.                 "Note983558
    IF sy-subrc EQ 0.                                       "Note983558
* Check infotype 0130 if absence/attendance can be entered at all "Note983558
      CALL METHOD auth_checker->check_authorization         "Note983558
        EXPORTING                                           "Note983558
          level                       = 'W'                 "Note983558
          pernr                       = pernr               "Note983558
          infty                       = '0130'              "Note983558
                subty                       = p0130_w-subty "Note983558
          begda                       = wa_begda            "Note983558
          endda                       = wa_endda            "Note983558
          process_only_partial_checks = 'X'                 "Note983558
        IMPORTING                                           "Note983558
          is_authorized               = wa_is_authorized    "Note983558
        EXCEPTIONS                                          "Note983558
          invalid                     = 1                   "Note983558
          internal_error              = 2                   "Note983558
          OTHERS                      = 3.                  "Note983558
      IF sy-subrc <> 0.                                     "Note983558
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO       "Note983558
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.    "Note983558
      ELSE.                                                 "Note983558
*        IF wa_is_authorized EQ 'X'.                         "Note983558     "DEL Note1759157
        IF wa_is_authorized NE 'X'.                                          "INS Note1759157
          EXIT.                                             "Note983558                                               "Note983558
        ENDIF.                                              "Note983558
      ENDIF.                                                "Note983558
*    ELSE.                                                   "Note983558     "DEL Note1759157
** No record for test procedure found --> no further checks required         "DEL Note1759157
*      wa_is_authorized = 'X'.                               "Note983558     "DEL Note1759157
*      EXIT.                                                 "Note983558     "DEL Note1759157
    ENDIF.                                                  "Note983558
  ENDSELECT.                                                "Note983558

  IF sy-subrc EQ 0 AND wa_is_authorized NE 'X'.             "Note983558
* This error is only relevant if a CKTYP has been defined   "Note983558
* No write authorization for IT0130 --> no more processing  "Note983558
    wa_error-number = '112'.                                "Note983558
    wa_error-par1 =''.                                      "Note983558
    wa_error-par2 =''.                                      "Note983558
    APPEND wa_error TO error_tab.                           "Note983558
  ENDIF.                                                    "Note983558

* Find de Customizing parameter MOABW
  CALL METHOD cl_pt_req_customizing=>get_modificators
    EXPORTING
      im_pernr = pernr
    IMPORTING
      ex_moabw = moabw.

  CHECK sy-subrc = 0.

* Find the Abs/Att Constrains
  CALL METHOD cl_pt_arq_customizing=>get_attabs_types_and_attribs
    EXPORTING
      im_pernr             = pernr
    IMPORTING
      ex_attabs_attributes = subty_attribs_tab
    EXCEPTIONS
      it0001_not_found     = 1
      it0008_not_found     = 2
      missing_customizing  = 3
      OTHERS               = 4.

  CHECK sy-subrc = 0.

* Find the Time Constrains for the specific Abs/Att type
  READ TABLE subty_attribs_tab
    INTO wa_subty_attribs
    WITH KEY moabw = moabw
             subty = subty  .

  CHECK sy-subrc = 0.

* 01--Check if Abs/Att are permitted in the future.***************
  IF wa_subty_attribs-begfut_permit = cl_pt_req_const=>c_true.
*   OK
  ELSEIF wa_subty_attribs-begfut_forbid = cl_pt_req_const=>c_true.
* If not, BEGDA must not be greater than SY-DATLO.
    IF wa_begda > sy-datlo.            "datum->datlo by note 1983589
      wa_error-number = '069'.
      wa_error-par1 =''.
      wa_error-par2 =''.
      APPEND wa_error TO error_tab.
    ENDIF.

  ELSEIF wa_subty_attribs-begfut_time_limit = cl_pt_req_const=>c_true.

    IF  wa_subty_attribs-begfut_minimal    = cl_pt_req_const=>c_true.
* Compute the period of time in which is BEGDA not permitted
      CLEAR tomorrow.
      tomorrow = sy-datlo + 1. "Today should not be considered  (datum->datlo by note 1983589)
      CALL METHOD cl_pt_req_time_constraints=>future_from_date
        EXPORTING
          im_date           = tomorrow
          im_unit           = wa_subty_attribs-begfut_time_unit
          im_max_multiplier = wa_subty_attribs-begfut_time_qty
        IMPORTING
          ex_max_date       = forbid_up_to.

* BEDDA must not be between sy-datlo + 1 and the variable "forbid_up_to"
      forbid_from = sy-datlo + 1.                 "datum->datlo by note 1983589
      IF wa_begda >= forbid_from AND
         wa_begda <= forbid_up_to.

        CLEAR error_unit.
        CLEAR error_qty.
        error_qty = wa_subty_attribs-begfut_time_unit.

        CALL FUNCTION 'DD_DOMVALUE_TEXT_GET'
          EXPORTING
            domname  = 'PTARQ_TIME_UNIT'
            value    = error_qty
          IMPORTING
            dd07v_wa = error_unit.
*
*        CONDENSE error_unit.
        wa_error-number = '070'.
        wa_error-par1 = wa_subty_attribs-begfut_time_qty.
        CONDENSE wa_error-par1.
        wa_error-par2 = error_unit-ddtext.
        APPEND wa_error TO error_tab.

      ENDIF.
    ENDIF.

    IF wa_subty_attribs-begfut_maximal    = cl_pt_req_const=>c_true.
* Compute the period of time in which is BEGDA not permitted
      CLEAR forbid_from.
      CLEAR tomorrow.

      tomorrow = sy-datlo + 1. "Today should not be considered      (datum->datlo by note 1983589)

      CALL METHOD cl_pt_req_time_constraints=>future_from_date
        EXPORTING
          im_date           = tomorrow
          im_unit           = wa_subty_attribs-begfut_max_unit
          im_max_multiplier = wa_subty_attribs-begfut_max_qty
        IMPORTING
          ex_max_date       = forbid_from.

* BEDDA must not be bigger than "forbid_from"
      IF wa_begda > forbid_from .

        CLEAR error_unit.
        CLEAR error_qty.
        error_qty = wa_subty_attribs-begfut_max_unit.
        CALL FUNCTION 'DD_DOMVALUE_TEXT_GET'
          EXPORTING
            domname  = 'PTARQ_TIME_UNIT'
            value    = error_qty
          IMPORTING
            dd07v_wa = error_unit.

        wa_error-par1 = wa_subty_attribs-begfut_max_qty.
        CONDENSE wa_error-par1.
        wa_error-par2 = error_unit-ddtext.

        wa_error-number = '091'.
        APPEND wa_error TO error_tab.

      ENDIF.
    ENDIF.
  ENDIF.

**02--Check if Abs/Att are permitted in the past.***************
  IF wa_subty_attribs-begpast_permit = cl_pt_req_const=>c_true.
*   OK
  ELSEIF wa_subty_attribs-begpast_forbid = cl_pt_req_const=>c_true.
* If not, BEGDA must not be smaller than SY-DATLO.
    IF wa_begda < sy-datlo.            "datum->datlo by note 1983589

      wa_error-number = '071'.
      wa_error-par1 =''.
      wa_error-par2 =''.
      APPEND wa_error TO error_tab.
    ENDIF.
  ELSEIF wa_subty_attribs-begpast_time_limit = cl_pt_req_const=>c_true.
* Compute the period of time in which is BEGDA not permitted
    CALL METHOD cl_pt_req_time_constraints=>past_from_current_date
      EXPORTING
        im_unit           = wa_subty_attribs-begpast_time_unit
        im_min_multiplier = wa_subty_attribs-begpast_time_qty
      IMPORTING
        ex_min_date       = forbid_from.
* BEDDA must not be between forbid_from and 01.01.1900"
    IF wa_begda < forbid_from.

      wa_error-number = '073'.
      WRITE forbid_from TO wa_error-par1.
      APPEND wa_error TO error_tab.

    ENDIF.
  ENDIF.

**03--Check if Abs/Att are permitted today.***************
  IF wa_subty_attribs-begcurr_permit = cl_pt_req_const=>c_true.
*   OK
  ELSEIF wa_subty_attribs-begcurr_forbid = cl_pt_req_const=>c_true.
* If not, BEGDA must not be greater than SY-DATLO.
    IF wa_begda = sy-datlo.            "datum->datlo by note 1983589

      wa_error-number = '074'.
      wa_error-par1 =''.
      wa_error-par2 =''.
      APPEND wa_error TO error_tab.
    ENDIF.
  ENDIF.

* Send Error Messages
  CLEAR wa_error.
  LOOP AT error_tab INTO wa_error.

    IF 1 = 2.
*---Workaround for where-used list
      MESSAGE e069(hrtim_abs_req).
      MESSAGE e070(hrtim_abs_req) WITH sy-msgv1 sy-msgv2.   "CHECKMAN
      MESSAGE e071(hrtim_abs_req).
      MESSAGE e073(hrtim_abs_req) WITH sy-msgv1.            "CHECKMAN
      MESSAGE e074(hrtim_abs_req).
      MESSAGE e091(hrtim_abs_req) WITH sy-msgv1 sy-msgv2.   "CHECKMAN
      MESSAGE e112(hrtim_abs_req).                          "Note983558

    ENDIF.

    CALL METHOD message_handler->add_message
      EXPORTING
        im_type       = 'E'
        im_cl         = 'HRTIM_ABS_REQ'
        im_number     = wa_error-number
        im_par1       = wa_error-par1
        im_par2       = wa_error-par2
        im_context    = 'CHECK TIME CONSTRAINTS'
        im_subcontext = ''
        im_classname  = 'IF_EX_PT_ABS_REQ'
        im_methodname = 'CHECK_TIME_CONSTR_FOR_SUBTY'.

  ENDLOOP.
*                                                                       *END OF INS NOTE 20041103
*                                                                       *BEGIN OF DEL NOTE 20041103
*  DATA:
*      wa_attabs           TYPE ptarq_attabsdata_struc,
*      wa_abs_attribs      TYPE p2001,
*      wa_atts_attribs     TYPE p2002,
*      pernr               TYPE p_pernr,
*      wa_begda            TYPE sy-datum,
*      wa_endda            TYPE sy-datum,
*      operation           TYPE tim_tmwopera,
*      subty               TYPE subty,
*      subty_attribs_tab   TYPE attabs_attributes_tab,
*      wa_subty_attribs    TYPE attabs_attributes_struc,
*      earliest_multiplier TYPE i,
*      earliest_date       TYPE sy-datum,
*      latest_multiplier   TYPE i,
*      latest_date         TYPE sy-datum,
*      moabw               TYPE moabw,
*      sign                TYPE i,
*      lcl_msgv1           TYPE sy-msgv1,
*      lcl_msgv2           TYPE sy-msgv1,
*      lcl_msgv3           TYPE sy-msgv1,
*      subty_text          TYPE abwtxt,                                                  "INS NOTE751154
*      number_of_rows      TYPE i.
*
*
*  DESCRIBE TABLE  im_attabs_tab
*    LINES number_of_rows.
*
*
*  LOOP AT im_attabs_tab INTO wa_attabs.
*
*    operation = wa_attabs-operation.
*
**** In a Modify Request, Check only OP INS
*    IF number_of_rows > 1.
*      CHECK operation = cl_pt_req_const=>c_item_ins.
*    ENDIF.
*
*    IF NOT wa_attabs-abs_attribs IS INITIAL.
*
*      READ TABLE wa_attabs-abs_attribs INDEX 1
*      INTO wa_abs_attribs.
*
*      pernr    = wa_abs_attribs-pernr.
*      wa_begda = wa_abs_attribs-begda.
*      wa_endda = wa_abs_attribs-endda.
*      subty    = wa_abs_attribs-subty.
*
*    ELSEIF  NOT wa_attabs-atts_attribs IS INITIAL.
*
*      READ TABLE wa_attabs-atts_attribs INDEX 1
*      INTO wa_atts_attribs.
*
*      pernr    = wa_atts_attribs-pernr.
*      wa_begda = wa_atts_attribs-begda.
*      wa_endda = wa_atts_attribs-endda.
**      subty    = wa_abs_attribs-subty.                                                 "DEL Note 742545
*      subty    = wa_atts_attribs-subty.                                                 "INS Note 742545
*    ENDIF.
*
*    CALL METHOD cl_pt_req_customizing=>get_modificators
*      EXPORTING
*        im_pernr = pernr
*      IMPORTING
*        ex_moabw = moabw.
*
*    CHECK sy-subrc = 0.
*
*
*    CALL METHOD cl_pt_arq_customizing=>get_attabs_types_and_attribs
*      EXPORTING
*        im_pernr             = pernr
*      IMPORTING
*        ex_attabs_attributes = subty_attribs_tab
*      EXCEPTIONS
*        it0001_not_found     = 1
*        it0008_not_found     = 2
*        missing_customizing  = 3
*        OTHERS               = 4.
*
*    CHECK sy-subrc = 0.
*
*    READ TABLE subty_attribs_tab
*      INTO wa_subty_attribs
*      WITH KEY moabw = moabw
*               subty = subty  .
*
*    CHECK sy-subrc = 0.
*
*    IF
*      wa_subty_attribs-earliest_new_bef   = cl_pt_arq_const=>c_arq_custo_fristen_subty_max AND
*      wa_subty_attribs-earliest_new_bef_u = cl_pt_arq_const=>c_arq_custo_year              AND
*      wa_subty_attribs-latest_new_aft     = cl_pt_arq_const=>c_arq_custo_fristen_subty_max AND
*      wa_subty_attribs-latest_new_aft_u   = cl_pt_arq_const=>c_arq_custo_year              AND
*      wa_subty_attribs-earliest_del_bef   = cl_pt_arq_const=>c_arq_custo_fristen_subty_max AND
*      wa_subty_attribs-earliest_del_bef_u = cl_pt_arq_const=>c_arq_custo_year              AND
*      wa_subty_attribs-latest_del_aft     = cl_pt_arq_const=>c_arq_custo_fristen_subty_max AND
*      wa_subty_attribs-latest_del_aft_u   = cl_pt_arq_const=>c_arq_custo_year.
*
*      EXIT.
*    ENDIF.
*
**** ex_ret_value. The check will be performed
*    ex_ret_value = cl_pt_req_const=>c_true.
*
*    CASE operation.
*
*      WHEN cl_pt_req_const=>c_item_ins.
*
**** earliest_day for op INS
*        earliest_multiplier = wa_subty_attribs-earliest_new_bef.
*        sign = SIGN( earliest_multiplier ).
*        earliest_multiplier = ABS( earliest_multiplier ).
*
*        IF sign > 0.
*
*          CALL METHOD cl_pt_arq_badi=>future_from_date
*            EXPORTING
*              im_date           = wa_begda
*              im_unit           = wa_subty_attribs-earliest_new_bef_u
*              im_max_multiplier = earliest_multiplier
*            IMPORTING
*              ex_max_date       = earliest_date.
*
*        ELSEIF sign < 0.
*
*          CALL METHOD cl_pt_arq_badi=>past_from_date
*            EXPORTING
*              im_date           = wa_begda
*              im_unit           = wa_subty_attribs-earliest_new_bef_u
*              im_min_multiplier = earliest_multiplier
*            IMPORTING
*              ex_min_date       = earliest_date.
*
*
*        ELSEIF sign = 0.
*
*          earliest_date = wa_begda.
*
*        ENDIF.
*
**** latest_date for op INS
*        latest_multiplier = wa_subty_attribs-latest_new_aft.
*        sign = SIGN( latest_multiplier ).
*        latest_multiplier = ABS( latest_multiplier ).
*
*        IF sign > 0.
*
*          CALL METHOD cl_pt_arq_badi=>future_from_date
*            EXPORTING
*              im_date           = wa_begda
*              im_unit           = wa_subty_attribs-latest_new_aft_u
*              im_max_multiplier = latest_multiplier
*            IMPORTING
*              ex_max_date       = latest_date.
*
*
*        ELSEIF sign < 0.
*
*          CALL METHOD cl_pt_arq_badi=>past_from_date
*            EXPORTING
*              im_date           = wa_begda
*              im_unit           = wa_subty_attribs-latest_new_aft_u
*              im_min_multiplier = latest_multiplier
*            IMPORTING
*              ex_min_date       = latest_date.
*
*        ELSEIF sign = 0.
*
*          latest_date = wa_begda.
*
*        ENDIF.
*
**** Check ERROR for op INS
*
**** Check 01
*        IF latest_date >= earliest_date .
**          OK.
*        ELSE.
*
*          IF 1 = 2.
**---Workaround for where-used list
*            MESSAGE e069(hrtim_abs_req) WITH '&'.
*          ENDIF.
*
*          WRITE latest_date   TO lcl_msgv1.
*          WRITE earliest_date TO lcl_msgv2.
*
*          CALL METHOD message_handler->add_message
*            EXPORTING
*              im_type       = 'E'
*              im_cl         = 'HRTIM_ABS_REQ'
*              im_number     = '069'
*              im_par1       = lcl_msgv1
*              im_par2       = lcl_msgv2
*              im_context    = 'latest_date & latest_date'
*              im_subcontext = ''
*              im_classname  = 'IF_EX_PT_ABS_REQ'
*              im_methodname = 'CHECK_TIME_CONSTR_FOR_SUBTY'.
*
*        ENDIF.
*
**** Check 02 INS
*        IF sy-datum  >= earliest_date.
**          OK.
*        ELSE.
***** Check 02.01 INS
**** Is an Insert request
*          IF number_of_rows = 1.
*            IF 1 = 2.
**---Workaround for where-used list
*              MESSAGE e070(hrtim_abs_req) WITH '&'.
*            ENDIF.
*
*            CALL METHOD cl_pt_arq_customizing=>get_subtype_description                  "INS NOTE751154
*              EXPORTING                                                                 "INS NOTE751154
*                im_attabs_type   = subty                                                "INS NOTE751154
*                im_pernr         = pernr                                                "INS NOTE751154
*              IMPORTING                                                                 "INS NOTE751154
*                ex_name          = subty_text                                           "INS NOTE751154
*              EXCEPTIONS                                                                "INS NOTE751154
*                it0001_not_found = 1                                                    "INS NOTE751154
*                OTHERS           = 2.                                                   "INS NOTE751154
*            IF sy-subrc <> 0.                                                           "INS NOTE751154
*              lcl_msgv1 = subty.                                                        "INS NOTE751154
*            ENDIF.                                                                      "INS NOTE751154
*
*            lcl_msgv1 = subty_text.                                                     "INS NOTE751154
**            lcl_msgv1 = subty.                                                         "DEL NOTE751154
*            WRITE earliest_date TO lcl_msgv2.
*
*            CALL METHOD message_handler->add_message
*              EXPORTING
*                im_type       = 'E'
*                im_cl         = 'HRTIM_ABS_REQ'
*                im_number     = '070'
*                im_par1       = lcl_msgv1
*                im_par2       = lcl_msgv2
*                im_context    = 'earliest_date & endda'
*                im_subcontext = ''
*                im_classname  = 'IF_EX_PT_ABS_REQ'
*                im_methodname = 'CHECK_TIME_CONSTR_FOR_SUBTY'.
*
*
*          ELSE.
***** Check 02.02 INS
**** Is an Modify request.  number_of_rows = 2
*            IF 1 = 2.
**---Workaround for where-used list
*              MESSAGE e073(hrtim_abs_req) WITH '&'.
*            ENDIF.
*
*            CALL METHOD cl_pt_arq_customizing=>get_subtype_description                  "INS NOTE751154
*              EXPORTING                                                                 "INS NOTE751154
*                im_attabs_type   = subty                                                "INS NOTE751154
*                im_pernr         = pernr                                                "INS NOTE751154
*              IMPORTING                                                                 "INS NOTE751154
*                ex_name          = subty_text                                           "INS NOTE751154
*              EXCEPTIONS                                                                "INS NOTE751154
*                it0001_not_found = 1                                                    "INS NOTE751154
*                OTHERS           = 2.                                                   "INS NOTE751154
*            IF sy-subrc <> 0.                                                           "INS NOTE751154
*              lcl_msgv1 = subty.                                                        "INS NOTE751154
*            ENDIF.                                                                      "INS NOTE751154
*
*            lcl_msgv1 = subty_text.                                                     "INS NOTE751154
**            lcl_msgv1 = subty.                                                         "DEL NOTE751154
*            WRITE earliest_date TO lcl_msgv2.
*
*            CALL METHOD message_handler->add_message
*              EXPORTING
*                im_type       = 'E'
*                im_cl         = 'HRTIM_ABS_REQ'
*                im_number     = '073'
*                im_par1       = lcl_msgv1
*                im_par2       = lcl_msgv2
*                im_context    = 'earliest_date & endda'
*                im_subcontext = ''
*                im_classname  = 'IF_EX_PT_ABS_REQ'
*                im_methodname = 'CHECK_TIME_CONSTR_FOR_SUBTY'.
*
*          ENDIF.
*        ENDIF.
**** Check 03
*        IF sy-datum <= latest_date.
**          OK.
*        ELSE.
**** Check 03.01
**** Is an Insert request
*          IF number_of_rows = 1.
*            IF 1 = 2.
**---Workaround for where-used list
*              MESSAGE e071(hrtim_abs_req) WITH '&'.
*            ENDIF.
*
*            CALL METHOD cl_pt_arq_customizing=>get_subtype_description                  "INS NOTE751154
*              EXPORTING                                                                 "INS NOTE751154
*                im_attabs_type   = subty                                                "INS NOTE751154
*                im_pernr         = pernr                                                "INS NOTE751154
*              IMPORTING                                                                 "INS NOTE751154
*                ex_name          = subty_text                                           "INS NOTE751154
*              EXCEPTIONS                                                                "INS NOTE751154
*                it0001_not_found = 1                                                    "INS NOTE751154
*                OTHERS           = 2.                                                   "INS NOTE751154
*            IF sy-subrc <> 0.                                                           "INS NOTE751154
*              lcl_msgv1 = subty.                                                        "INS NOTE751154
*            ENDIF.                                                                      "INS NOTE751154
*
*            lcl_msgv1 = subty_text.                                                     "INS NOTE751154
**            lcl_msgv1 = subty.                                                         "DEL NOTE751154
*            WRITE latest_date   TO lcl_msgv2.
*
*            CALL METHOD message_handler->add_message
*              EXPORTING
*                im_type       = 'E'
*                im_cl         = 'HRTIM_ABS_REQ'
*                im_number     = '071'
*                im_par1       = lcl_msgv1
*                im_par2       = lcl_msgv2
*                im_context    = 'earliest_date & endda'
*                im_subcontext = ''
*                im_classname  = 'IF_EX_PT_ABS_REQ'
*                im_methodname = 'CHECK_TIME_CONSTR_FOR_SUBTY'.
*
*          ELSE.
**** Check 03.02
**** Is an Modify request.  number_of_rows = 2
*            IF 1 = 2.
**---Workaround for where-used list
*              MESSAGE e074(hrtim_abs_req) WITH '&'.
*            ENDIF.
*
*            CALL METHOD cl_pt_arq_customizing=>get_subtype_description                  "INS NOTE751154
*              EXPORTING                                                                 "INS NOTE751154
*                im_attabs_type   = subty                                                "INS NOTE751154
*                im_pernr         = pernr                                                "INS NOTE751154
*              IMPORTING                                                                 "INS NOTE751154
*                ex_name          = subty_text                                           "INS NOTE751154
*              EXCEPTIONS                                                                "INS NOTE751154
*                it0001_not_found = 1                                                    "INS NOTE751154
*                OTHERS           = 2.                                                   "INS NOTE751154
*            IF sy-subrc <> 0.                                                           "INS NOTE751154
*              lcl_msgv1 = subty.                                                        "INS NOTE751154
*            ENDIF.                                                                      "INS NOTE751154
*
*            lcl_msgv1 = subty_text.                                                     "INS NOTE751154
**            lcl_msgv1 = subty.                                                         "DEL NOTE751154
*            WRITE earliest_date TO lcl_msgv2.
*
*            CALL METHOD message_handler->add_message
*              EXPORTING
*                im_type       = 'E'
*                im_cl         = 'HRTIM_ABS_REQ'
*                im_number     = '074'
*                im_par1       = lcl_msgv1
*                im_par2       = lcl_msgv2
*                im_context    = 'earliest_date & endda'
*                im_subcontext = ''
*                im_classname  = 'IF_EX_PT_ABS_REQ'
*                im_methodname = 'CHECK_TIME_CONSTR_FOR_SUBTY'.
*
*          ENDIF.
*        ENDIF.
*
*      WHEN cl_pt_req_const=>c_item_del.
**** earliest_day for op DEL
*
*        earliest_multiplier = wa_subty_attribs-earliest_del_bef.
*        sign = SIGN( earliest_multiplier ).
*        earliest_multiplier = ABS( earliest_multiplier ).
*
*        IF sign > 0.
*
*          CALL METHOD cl_pt_arq_badi=>future_from_date
*            EXPORTING
*              im_date           = wa_begda
*              im_unit           = wa_subty_attribs-earliest_del_bef_u
*              im_max_multiplier = earliest_multiplier
*            IMPORTING
*              ex_max_date       = earliest_date.
*
*        ELSEIF sign < 0.
*
*          CALL METHOD cl_pt_arq_badi=>past_from_date
*            EXPORTING
*              im_date           = wa_begda
*              im_unit           = wa_subty_attribs-earliest_del_bef_u
*              im_min_multiplier = earliest_multiplier
*            IMPORTING
*              ex_min_date       = earliest_date.
*
*        ELSEIF sign = 0.
*
*          earliest_date = wa_begda.
*
*        ENDIF.
*
**** latest_date for op DEL
*        latest_multiplier = wa_subty_attribs-latest_del_aft.
*        sign = SIGN( latest_multiplier ).
*        latest_multiplier = ABS( latest_multiplier ).
*
*        IF sign > 0.
*
*          CALL METHOD cl_pt_arq_badi=>future_from_date
*            EXPORTING
*              im_date           = wa_begda
*              im_unit           = wa_subty_attribs-latest_del_aft_u
*              im_max_multiplier = latest_multiplier
*            IMPORTING
*              ex_max_date       = latest_date.
*
*        ELSEIF sign < 0.
*
*          CALL METHOD cl_pt_arq_badi=>past_from_date
*            EXPORTING
*              im_date           = wa_begda
*              im_unit           = wa_subty_attribs-latest_del_aft_u
*              im_min_multiplier = latest_multiplier
*            IMPORTING
*              ex_min_date       = latest_date.
*
*        ELSEIF sign = 0.
*
*          latest_date = wa_begda.
*
*        ENDIF.
*
**** Check ERROR for op DEL
*
**** Check 01
*        IF latest_date >= earliest_date .
**          OK.
*        ELSE.
*
*          IF 1 = 2.
**---Workaround for where-used list
*            MESSAGE e069(hrtim_abs_req) WITH '&'.
*          ENDIF.
*
*          WRITE earliest_date   TO lcl_msgv2.
*          WRITE latest_date     TO lcl_msgv1.
*
*          CALL METHOD message_handler->add_message
*            EXPORTING
*              im_type       = 'E'
*              im_cl         = 'HRTIM_ABS_REQ'
*              im_number     = '069'
*              im_par1       = lcl_msgv1
*              im_par2       = lcl_msgv2
*              im_context    = 'latest_date & latest_date'
*              im_subcontext = ''
*              im_classname  = 'IF_EX_PT_ABS_REQ'
*              im_methodname = 'CHECK_TIME_CONSTR_FOR_SUBTY'.
*
*        ENDIF.
*
**** Check 02
*        IF sy-datum  >= earliest_date..
**          OK.
*        ELSE.
*
*          IF 1 = 2.
**---Workaround for where-used list
*            MESSAGE e073(hrtim_abs_req) WITH '&'.
*          ENDIF.
*
*          CALL METHOD cl_pt_arq_customizing=>get_subtype_description                  "INS NOTE751154
*            EXPORTING                                                                 "INS NOTE751154
*              im_attabs_type   = subty                                                "INS NOTE751154
*              im_pernr         = pernr                                                "INS NOTE751154
*            IMPORTING                                                                 "INS NOTE751154
*              ex_name          = subty_text                                           "INS NOTE751154
*            EXCEPTIONS                                                                "INS NOTE751154
*              it0001_not_found = 1                                                    "INS NOTE751154
*              OTHERS           = 2.                                                   "INS NOTE751154
*          IF sy-subrc <> 0.                                                           "INS NOTE751154
*            lcl_msgv1 = subty.                                                        "INS NOTE751154
*          ENDIF.                                                                      "INS NOTE751154
*
*          lcl_msgv1 = subty_text.                                                     "INS NOTE751154
**            lcl_msgv1 = subty.                                                       "DEL NOTE751154
*          WRITE earliest_date TO lcl_msgv2.
*
*          CALL METHOD message_handler->add_message
*            EXPORTING
*              im_type       = 'E'
*              im_cl         = 'HRTIM_ABS_REQ'
*              im_number     = '073'
*              im_par1       = lcl_msgv1
*              im_par2       = lcl_msgv2
*              im_context    = 'earliest_date & endda'
*              im_subcontext = ''
*              im_classname  = 'IF_EX_PT_ABS_REQ'
*              im_methodname = 'CHECK_TIME_CONSTR_FOR_SUBTY'.
*
*        ENDIF.
*
**** Check 03
*        IF sy-datum <= latest_date.
**          OK.
*        ELSE.
*
*          IF 1 = 2.
**---Workaround for where-used list
*            MESSAGE e074(hrtim_abs_req) WITH '&'.
*          ENDIF.
*
*          CALL METHOD cl_pt_arq_customizing=>get_subtype_description                  "INS NOTE751154
*            EXPORTING                                                                 "INS NOTE751154
*              im_attabs_type   = subty                                                "INS NOTE751154
*              im_pernr         = pernr                                                "INS NOTE751154
*            IMPORTING                                                                 "INS NOTE751154
*              ex_name          = subty_text                                           "INS NOTE751154
*            EXCEPTIONS                                                                "INS NOTE751154
*              it0001_not_found = 1                                                    "INS NOTE751154
*              OTHERS           = 2.                                                   "INS NOTE751154
*          IF sy-subrc <> 0.                                                           "INS NOTE751154
*            lcl_msgv1 = subty.                                                        "INS NOTE751154
*          ENDIF.                                                                      "INS NOTE751154
*
*          lcl_msgv1 = subty_text.                                                     "INS NOTE751154
**            lcl_msgv1 = subty.                                                        "DEL NOTE751154
*          WRITE latest_date   TO lcl_msgv2.
*
*          CALL METHOD message_handler->add_message
*            EXPORTING
*              im_type       = 'E'
*              im_cl         = 'HRTIM_ABS_REQ'
*              im_number     = '074'
*              im_par1       = lcl_msgv1
*              im_par2       = lcl_msgv2
*              im_context    = 'earliest_date & endda'
*              im_subcontext = ''
*              im_classname  = 'IF_EX_PT_ABS_REQ'
*              im_methodname = 'CHECK_TIME_CONSTR_FOR_SUBTY'.
*
*        ENDIF.
*
*    ENDCASE.
*
*  ENDLOOP.
*                                                                       *END OF DEL NOTE 20041103
ENDMETHOD.


METHOD IF_EX_PT_ABS_REQ~CONVERT_FILE_FORMAT.
ENDMETHOD.


METHOD IF_EX_PT_ABS_REQ~GET_ATTATCHMENT_CONFIG.
ENDMETHOD.


method IF_EX_PT_ABS_REQ~GET_FIELD_SELECTION_ATTRIBS.
endmethod.


METHOD IF_EX_PT_ABS_REQ~GET_IT0005_QUOTAS.
* XRK 20060817 Note915561 Quota begin and end dates are missing
* no standard coding, if IT0005 is used coding has to be activated
  IF 1 = 2.
    CONSTANTS: high_date TYPE d VALUE '99991231'.
    TYPES:  BEGIN OF ursel_t,
              sign(1),
              option(2),
              low  TYPE p0005-uar01,
              high TYPE p0005-uar01,
           END OF ursel_t.
    DATA: i0001 TYPE tim_p0001_tab,
          i0003 TYPE tim_p0003_tab.
    DATA: ursel TYPE TABLE OF ursel_t.
    DATA: xansp TYPE TABLE OF tptim_xan,
          xansp_line LIKE LINE OF xansp.
    DATA: xansp2 TYPE TABLE OF tptim_xan,
          xansp2_line LIKE LINE OF xansp2.
    DATA: t0005 TYPE TABLE OF p0005,
          t0005_line LIKE LINE OF t0005.
    DATA: timequota LIKE LINE OF ex_time_quota.

* CDATUM = SY-DATUM (HIGHDATE would read entries in future as well)
* Note:
*     UREDU = All deducted days in the past + future (on key date)
*     URECH = All deducted + compensated days (on evaluation date (NOT key date))
*     UGELT = All compensated days in the past + future (on key date)
* German version:
*     UREDU = alle abgetragenen Tage Vergangenheit + Zukunft (Stichtag)
*     URECH = alle abgetragenen + abgegoltenen Tage (Abrechnungsdatum
*                                                     (NICHT Stichtag!)
*     UGELT = alle abgegoltenen Tage Vergangenheit + Zukunft (Stichtag)


    CALL FUNCTION 'HR_GET_LEAVE_DATA'
      EXPORTING
        pernr                    = im_pernr
        cdatum                   = sy-datum
        ubegd                    = im_begda
        uendd                    = im_endda
        begda_0005               = im_sel_begda             "INS Note915561
        endda_0005               = im_sel_endda             "INS Note915561
      TABLES
        xansp                    = xansp2
        urart_sel                = ursel
        i0001                    = i0001
        i0003                    = i0003
      EXCEPTIONS
        infty_0005_not_defined   = 1
        infty_0003_not_defined   = 2
        infty_0083_not_defined   = 3
        missing_authority        = 4
        old_ne_new_pc_version_nr = 5
        OTHERS                   = 6.

    IF sy-subrc NE 0.
*    RAISE error.
    ENDIF.

    CALL FUNCTION 'HR_GET_LEAVE_DATA'
      EXPORTING
        pernr                    = im_pernr
        cdatum                   = high_date
        ubegd                    = im_begda
        uendd                    = im_endda
        begda_0005               = im_sel_begda             "INS Note915561
        endda_0005               = im_sel_endda             "INS Note915561
      TABLES
        xansp                    = xansp
        urart_sel                = ursel
        i0001                    = i0001
        i0003                    = i0003
      EXCEPTIONS
        infty_0005_not_defined   = 1
        infty_0003_not_defined   = 2
        infty_0083_not_defined   = 3
        missing_authority        = 4
        old_ne_new_pc_version_nr = 5
        OTHERS                   = 6.

    IF sy-subrc EQ 0.

      CALL FUNCTION 'HR_READ_INFOTYPE'
        EXPORTING
          pernr           = im_pernr
          infty           = '0005'
          begda           = im_sel_begda                    "INS Note915561
          endda           = im_sel_endda                    "INS Note915561
        TABLES
          infty_tab       = t0005
        EXCEPTIONS
          infty_not_found = 1
          OTHERS          = 2.
      IF sy-subrc <> 0.
      ENDIF.

      LOOP AT xansp INTO xansp_line.
*        CLEAR ex_time_quota.                               "DEL 1792864
        CLEAR timequota.                                    "INS 1792864
        LOOP AT xansp2 INTO xansp2_line
            WHERE urart = xansp_line-urart
            AND   uabeg = xansp_line-uabeg
            AND   uaend = xansp_line-uaend
            AND   urljj = xansp_line-urljj
            AND   objps = xansp_line-objps.
          timequota-leavetype = xansp_line-urart.
          timequota-quotabeg  = xansp_line-uabeg.
          timequota-quotatext = xansp_line-urtxt.
          timequota-entitle   = xansp_line-uansp.
          timequota-deduct    = xansp2_line-uredu + xansp2_line-ugelt.
          timequota-ordered   = xansp_line-uredu + xansp_line-ugelt -
                                  timequota-deduct.
          timequota-quotaend  = xansp_line-uaend.
          timequota-quotanum  = xansp_line-uansp -
                                  xansp_line-uredu - xansp_line-ugelt.
          timequota-time_unit = xansp_line-uunit.
          timequota-tiunitext = xansp_line-etext.

          LOOP AT t0005 INTO t0005_line
              WHERE      objps = xansp_line-objps
              AND   (    uab01 = xansp_line-uabeg
                      OR uab02 = xansp_line-uabeg
                      OR uab03 = xansp_line-uabeg
                      OR uab04 = xansp_line-uabeg
                      OR uab05 = xansp_line-uabeg
                      OR uab06 = xansp_line-uabeg )
              AND   (    uan01 = xansp_line-uaend
                      OR uan02 = xansp_line-uaend
                      OR uan03 = xansp_line-uaend
                      OR uan04 = xansp_line-uaend
                      OR uan05 = xansp_line-uaend
                      OR uan06 = xansp_line-uaend ).
            timequota-begda = t0005_line-begda.
            timequota-endda = t0005_line-endda.
          ENDLOOP.

          APPEND timequota TO ex_time_quota.
        ENDLOOP.
      ENDLOOP.

    ELSE.
*    RAISE error.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD IF_EX_PT_ABS_REQ~GET_MOD_AND_QTYPE.

* please read documentation of function 'HR_GET_QUOTA_DATA'.
* Changes can cause performance problems!!!!

  ex_mod   = 'B'.
  ex_qtype = 3.

ENDMETHOD.


  METHOD IF_EX_PT_ABS_REQ~GET_MULTIPLE_APPROVERS.
    DATA: lv_has_manager TYPE boolean,
          lv_has_hrbp TYPE boolean,
          lv_has_hr_admin TYPE boolean,
          ls_approver TYPE ptreq_approver_struc,
          lv_approver_seqnr TYPE int4,
          lo_actor_agent    TYPE REF TO ca_pt_req_actor,
          lo_owner                TYPE REF TO if_pt_req_a_wf,
          lo_approver             TYPE REF TO if_pt_req_a_wf.

    "Init
    lv_approver_seqnr = 0.
    CLEAR: lv_has_manager,lv_has_hrbp,lv_has_hr_admin,
           et_approvers, ev_approver_level,
           ev_no_implementation_flag,ev_ess_add_dele_approver_flag.

*****default: disable multiple approver scenario.
    ev_no_implementation_flag = 'X'.

*************************************************************************************************************
******           Enable the Remember Function    ************************************
******           This functinality will be enabled when you
******                   add implementation to get approvers from remember table(database table zhressapprover2) here
******                   add implementation to record approvers into remember table in BAdI method UPDATE_MULTIPLE_APPROVERS
*****            Please check the BAdI method description and method UPDATE_MULTIPLE_APPROVER for details
*************************************************************************************************************
** If remember function returns approvers, the line manager & HRBP won't be used.
** Init
*    CLASS ca_pt_req_actor DEFINITION LOAD.
*    lo_actor_agent = ca_pt_req_actor=>agent.
*
*    CALL METHOD lo_actor_agent->create_actor
*      EXPORTING
*        im_actor_type = im_actortype
*        im_otype      = im_otype
*        im_objid      = im_objid
*      IMPORTING
*        ex_actor      = lo_owner.
*
*    DATA: lt_last_input_list TYPE TABLE OF zhressapprover2,
*          ls_last_input TYPE zhressapprover2,
*          lv_approver_pernr TYPE objektid.
*
*
*    SELECT * FROM  zhressapprover2  INTO  TABLE lt_last_input_list
*        WHERE  inituser  = lo_owner->user AND req_type = im_subtype.
*
*    LOOP AT lt_last_input_list INTO ls_last_input.
*      CLEAR ls_approver.
*      CLEAR lv_approver_pernr.
*      CALL FUNCTION 'BAPI_USR01DOHR_GETEMPLOYEE'
*        EXPORTING
*          id             = ls_last_input-approver
*          begindate      = sy-datum
*          enddate        = sy-datum
*        IMPORTING
*          employeenumber = lv_approver_pernr.
*
*      CALL METHOD actor_agent->create_actor
*        EXPORTING
*          im_actor_type      = 'P'
*          im_otype           = 'P'
*          im_objid           = lv_approver_pernr
*        IMPORTING
*          ex_actor           = lo_approver
*        EXCEPTIONS
*          missing_parameter  = 1
*          pernr_not_existing = 2
*          application_error  = 3
*          OTHERS             = 4.
*
*      ls_approver-seqnr  = ls_last_input-sequence.
*      ls_approver-status = 'T'.
*      ls_approver-pernr  = lv_approver_pernr.
*      ls_approver-sys_user   = ls_last_input-approver.
*      ls_approver-name       = lo_approver->name.
*      APPEND ls_approver TO et_approvers.
*    ENDLOOP.
*
*    IF et_approvers IS NOT INITIAL.
*      ev_no_implementation_flag = abap_false.
*      DESCRIBE TABLE et_approvers LINES ev_approver_level.
*      RETURN.
*    ENDIF.


*************************************************************************************************************
******        Choose Direct Line Manager as approver        ************************************
*************************************************************************************************************
*    CLEAR: ls_approver.
*    CALL FUNCTION 'BAPI_GET_LINE_MANAGER'
*      EXPORTING
*        im_objid       = im_objid
*      IMPORTING
*        es_approver    = ls_approver
*        ev_has_manager = lv_has_manager.
*
*    IF lv_has_manager = abap_true.
*      lv_approver_seqnr = lv_approver_seqnr + 1.
*      ls_approver-seqnr = lv_approver_seqnr.
*      APPEND ls_approver TO et_approvers.
*    ENDIF.

**************************************************************************************************End


*************************************************************************************************************
******        Choose HRBP from Relationship 741 as approver        ************************************
*************************************************************************************************************
*    CLEAR ls_approver.
*    CALL FUNCTION 'BAPI_GET_HRBP'
*      EXPORTING
*        im_objid    = im_objid
*      IMPORTING
*        es_approver = ls_approver
*        ev_has_hrbp = lv_has_hrbp.
*
*    IF lv_has_hrbp = abap_true.
*      lv_approver_seqnr =  lv_approver_seqnr + 1.
*      ls_approver-seqnr =  lv_approver_seqnr.
*      APPEND ls_approver TO et_approvers.
*    ENDIF.
**************************************************************************************************


*************************************************************************************************************
******        Choose HR Admin from PA0001 as approver        ************************************
*************************************************************************************************************
*    CLEAR ls_approver.
*    CALL FUNCTION 'BAPI_GET_HR_ADMIN'
*      EXPORTING
*        im_objid        = im_objid
*        im_date         = im_date
*      IMPORTING
*        es_approver     = ls_approver
*        ev_has_hr_admin = lv_has_hr_admin.
*
*    IF lv_has_hr_admin = abap_true.
*      lv_approver_seqnr =  lv_approver_seqnr + 1.
*      ls_approver-seqnr = lv_approver_seqnr.
*      APPEND ls_approver TO et_approvers.
*    ENDIF.

*    ev_no_implementation_flag = abap_false.
*    DESCRIBE TABLE et_approvers LINES ev_approver_level.

  ENDMETHOD.


METHOD IF_EX_PT_ABS_REQ~GET_TIMETYPES_AS_QUOTAS.
* XRK 20120328 Note1701664 Projected quota not considered

* example coding, deactivate for standard implementation
  IF 1 = 2.

    DATA: lcl_time_results           TYPE TABLE OF ptm_time_results,
          lcl_time_results_wa        LIKE LINE OF lcl_time_results,
          ch_quotas_wa               TYPE ptarq_time_accounts_struc,
          saldo                      TYPE pc2b5,
          b1_data                    TYPE TABLE OF ptm_cluster_b1,
          b1_data_wa                 TYPE ptm_cluster_b1,
          t555b_tab                  TYPE TABLE OF t555b,
          i555b                      TYPE t555b,
          i0003                      TYPE TABLE OF p0003,
          i0003_wa                   LIKE LINE OF i0003,
          i2001                      TYPE TABLE OF p2001,
          i2001_wa                   LIKE LINE OF i2001,
          lcl_msgvar                 TYPE sy-msgv1.
    DATA: quotas                     TYPE ptarq_time_accounts_tab,
          quotas_wa                  TYPE LINE OF ptarq_time_accounts_tab,
          lcl_tabix                  TYPE sy-tabix.
    DATA: lt_tbuff                   TYPE TABLE OF ptr_tbuff,          "Note1701664
          lt_buffer_dir              TYPE TABLE OF ptr_buffer_dir.     "Note1701664
    DATA: BEGIN OF buff_key,                                           "Note1701664
            repid                    TYPE trdir-name VALUE 'RPTIME00', "Note1701664
            pernr                    TYPE pernr-pernr,                 "Note1701664
          END OF buff_key.                                             "Note1701664

    IMPORT buffer_dir TO lt_buffer_dir                                 "Note1701664
           tbuff      TO lt_tbuff                                      "Note1701664
    FROM MEMORY ID buff_key.                                           "Note1701664

    SELECT        * FROM  t555b INTO TABLE t555b_tab
           WHERE  sprsl  = sy-langu
           AND    mobde  = im_mobde.

    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        pernr           = im_pernr
        infty           = '0003'
      TABLES
        infty_tab       = i0003
      EXCEPTIONS
        infty_not_found = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
    ENDIF.

    CALL FUNCTION 'HR_TIME_CLUSTER_B1_GET'
      EXPORTING
        int_pernr             = im_pernr
        no_authority_check    = 'X'
      TABLES
        get_tbuff             = lt_tbuff                               "Note1701664
        get_buffer_dir        = lt_buffer_dir                          "Note1701664
        int_cluster_b1        = b1_data
      EXCEPTIONS
        wrong_cluster_version = 1
        no_read_authority     = 2
        OTHERS                = 3.
    IF sy-subrc <> 0.
    ENDIF.


* add non transferred accruals
    IF lcl_time_results IS INITIAL.

      CALL FUNCTION 'HR_TIME_RESULTS_IN_INTERVAL'
        EXPORTING
          int_pernr                   = im_pernr
          int_begda                   = im_begda
          int_endda                   = im_endda
*         INT_CLTYP             = '1'
        TABLES
          get_tbuff             = lt_tbuff                         "Note1701664
          get_buffer_dir        = lt_buffer_dir                    "Note1701664
          int_time_results            = lcl_time_results
       EXCEPTIONS
         no_period_specified         = 1
         wrong_cluster_version       = 2
         no_read_authority           = 3
         cluster_archived            = 4
         technical_error             = 5
          OTHERS                = 6.
      IF sy-subrc <> 0.
      ENDIF.

    ENDIF.

    SORT lcl_time_results BY pabrj pabrp DESCENDING.

    LOOP AT lcl_time_results INTO lcl_time_results_wa.

      CLEAR quotas_wa.
      LOOP AT lcl_time_results_wa-saldo INTO saldo
               WHERE ztart = '0005'.

        quotas_wa-entitle  = saldo-anzhl.
*        quotas_wa-QUOTATYPE =
*        quotas_wa-quotabeg = saldo-begda.
*        quotas_wa-quotaend = saldo-endda.
        quotas_wa-pernr = im_pernr.
        quotas_wa-seqnr = '999'.          "put these entries to the end
        READ TABLE t555b_tab INTO i555b WITH KEY ztart = saldo-ztart.
        quotas_wa-timetype  = saldo-ztart.
        quotas_wa-quotatext = i555b-ztext.
        quotas_wa-time_unit = '001'.
        CALL FUNCTION 'HRMS_BIW_TIME_UNITS'
          EXPORTING
            zeinh = '001'
            langu = sy-langu
          IMPORTING
            etext = quotas_wa-tiunitext.

* if last evaluated day is still valid and no irrelevant action has changed
* retroactive calc. date to a date too early
* we can assume that absences of type 0900 after that date
* will deduct from time type 0005
* according to SAP standard in Schema TM00, rule TP20 which looks like
* 2 S        D OUTTPABTYP
* 2 S *
* 2 S ****
* 2 S 0900     ADDDB0005-ADDDB0007-
* 2 S 0910     ADDDB0410-ADDDB0407-

        READ TABLE b1_data INTO b1_data_wa INDEX 1.
        READ TABLE i0003 INTO i0003_wa INDEX 1.
        IF i0003_wa-bderr = b1_data_wa-qt-dneva.
          CALL FUNCTION 'HR_READ_INFOTYPE'
            EXPORTING
              pernr                 = im_pernr
              infty                 = '2001'
             begda                 =  b1_data_wa-qt-dneva
*             ENDDA           = '99991231'
            TABLES
              infty_tab             = i2001
           EXCEPTIONS
             infty_not_found       = 1
              OTHERS          = 2.
          IF sy-subrc <> 0.
          ENDIF.
          LOOP AT i2001 INTO i2001_wa WHERE subty = '0900'.
            quotas_wa-deducted&reduced_fut  = i2001_wa-abrst.
            quotas_wa-rest_used             =                        "Rest verbr
              quotas_wa-entitle - quotas_wa-deducted&reduced.
            quotas_wa-rest_posted           =                        "Rest gepl u gen
              quotas_wa-entitle - quotas_wa-deducted&reduced
                                - quotas_wa-deducted&reduced_fut.
* one could do the same thing with requested absences 0900, see GET_SIMULATED_DEDUCTION
            quotas_wa-rest_posted&requested = quotas_wa-rest_posted. "Rest geplant
          ENDLOOP.
        ELSE.
          IF 1 = 2.
            MESSAGE e043(hrtim_abs_req) WITH quotas_wa-quotatext.
          ELSE.
            lcl_msgvar = quotas_wa-quotatext.
            CALL METHOD message_handler->add_message
              EXPORTING
                im_type       = 'I'
                im_cl         = 'HRTIM_ABS_REQ'
                im_number     = '043'
                im_par1       = lcl_msgvar
                im_context    = 'REQUEST'
                im_subcontext = 'QUOTAS'
                im_funcname   = ' '
                im_classname  = 'CL_PT_ARQ_TIME_ACCOUNTS'
                im_methodname = 'IF_PT_ARQ_TIME_ACCOUNTS~GET_TIME_ACCOUNTS'.
          ENDIF.
        ENDIF.

        APPEND quotas_wa TO quotas.
      ENDLOOP.
      EXIT.        "only the latest result/month
    ENDLOOP.
    IF sy-subrc <> 0.
    ENDIF.

    LOOP AT quotas INTO quotas_wa.
      READ TABLE ch_quotas INTO ch_quotas_wa
      WITH KEY timetype  = quotas_wa-timetype
               pernr     = quotas_wa-pernr.
      lcl_tabix = sy-tabix.
      IF sy-subrc = 0.
*     replace as only absolute entitlements are being calculated
        MOVE-CORRESPONDING quotas_wa TO ch_quotas_wa.
        MODIFY ch_quotas FROM ch_quotas_wa INDEX lcl_tabix.
      ELSE.
        MOVE-CORRESPONDING quotas_wa TO ch_quotas_wa.
        APPEND ch_quotas_wa TO ch_quotas.
      ENDIF.
    ENDLOOP.

    SORT ch_quotas BY seqnr.

  ENDIF.


ENDMETHOD.


METHOD IF_EX_PT_ABS_REQ~GET_WORKFLOW_ATTRIBS.
* XRK 20100921 Note1510867 Insufficient error message

  DATA: attabs_data_wa TYPE ptarq_attabsdata_struc,
        awart          TYPE awart,
        pernr          TYPE persno,
        p2001_wa       TYPE p2001,
        p2002_wa       TYPE p2002.
  DATA: lcl_msgvar     TYPE sy-msgv1.
  DATA: begda          LIKE sy-datum ,                      "LAK2068372
        endda          LIKE sy-datum .                      "LAK2068372
* Find entry for which the customizing should be read
  READ TABLE im_attabsdata_tab INTO attabs_data_wa WITH KEY operation = 'INS'.
  IF sy-subrc <> 0.
    READ TABLE im_attabsdata_tab INDEX 1 INTO attabs_data_wa.
  ENDIF.

* Determine awart and pernr out of attabs_data
  IF NOT attabs_data_wa-abs_attribs IS INITIAL.
    READ TABLE attabs_data_wa-abs_attribs INDEX 1 INTO p2001_wa.
    pernr = p2001_wa-pernr.
    awart = p2001_wa-subty.
    begda = p2001_wa-begda.                                 "LAK2068372
    endda = p2001_wa-endda.                                 "LAK2068372
  ELSEIF NOT attabs_data_wa-atts_attribs IS INITIAL.
    READ TABLE attabs_data_wa-atts_attribs INDEX 1 INTO p2002_wa.
    pernr = p2002_wa-pernr.
    awart = p2002_wa-subty.
    begda = p2002_wa-begda.                                 "LAK2068372
    endda = p2002_wa-endda.                                 "LAK2068372
  ENDIF.

* Read customizing
  IF NOT pernr IS INITIAL AND NOT awart IS INITIAL.
    CALL METHOD cl_pt_arq_badi=>get_workflow_settings
      EXPORTING
        im_pernr            = pernr
        im_begda            = begda                       "LAK2068372
        im_endda            = endda                       "LAK2068372
        im_awart            = awart
      IMPORTING
        ex_wf_attribs       = ex_wf_attribs
      EXCEPTIONS
        it0001_not_found    = 1
        it0008_not_found    = 2
        missing_customizing = 3
        OTHERS              = 4.
    IF ( ex_wf_attribs IS INITIAL AND sy-msgno IS INITIAL ) "Note1510867
      OR sy-subrc = 4.                                      "Note1510867
*   No customizing has been determined                      "Note1510867
      lcl_msgvar = pernr.
      IF 1 = 2.
*       Workaround for where-used list
        MESSAGE e056(hrtim_abs_req) WITH '&'.
      ENDIF.
      CALL METHOD message_handler->add_message
        EXPORTING
          im_type       = 'W'
          im_cl         = 'HRTIM_ABS_REQ'
          im_number     = '056'
          im_par1       = lcl_msgvar
          im_context    = 'EXIT'
          im_subcontext = 'WORKFLOW'
          im_classname  = 'IF_EX_PT_ABS_REQ'
          im_methodname = 'GET_WORKFLOW_ATTRIBS'.
    ELSEIF sy-subrc = 1 OR sy-subrc = 2 OR sy-subrc = 3.    "Note1510867
*   Infotype IT0001 or IT0008 not found; missing T554S_WEB  "Note1510867
*   customizing entry                                       "Note1510867
      IF 1 = 2.
*       Workaround for where-used list
        MESSAGE e090(hrtim_abs_req) WITH '&' '&' '&' '&'.
        MESSAGE e087(hrtim_abs_req) WITH '&' '&' '&' '&'.
      ENDIF.
      CALL METHOD message_handler->add_message
        EXPORTING
          im_type       = 'E'
          im_cl         = sy-msgid
          im_number     = sy-msgno
          im_par1       = sy-msgv1
          im_par2       = sy-msgv2
          im_par3       = sy-msgv3
          im_par4       = sy-msgv4
          im_context    = 'EXIT'
          im_subcontext = 'WORKFLOW'
          im_classname  = 'IF_EX_PT_ABS_REQ'
          im_methodname = 'GET_WORKFLOW_ATTRIBS'.
      RAISE missing_customizing.
    ENDIF.
  ENDIF.

ENDMETHOD.


method IF_EX_PT_ABS_REQ~INDIRECT_SUBST_APPR.

 IF cl_hrmss_wda_switch_check=>hrmss_sfws_wda_ui_02( ) EQ abap_true.
  EX_ACTIV_FLAG = 'X'.
 ELSE.
  CLEAR EX_ACTIV_FLAG .
 ENDIF.

endmethod.


METHOD if_ex_pt_abs_req~post_via_blop .
* YTZ 20060308 note 929948

  DATA: lt_blop_ins TYPE tim_tmw_blprequest_tab,
        lt_blop_del TYPE tim_tmw_blprequest_tab,
        lt_blop_tab TYPE tim_tmw_blprequest_tab,
        l_test      TYPE flag.
  DATA: blop_tab_del_wa TYPE tim_tmw_blprequest_entry,                 "HEGDEPR 2053774
        blop_tab_ins_wa TYPE tim_tmw_blprequest_entry.                 "HEGDEPR 2053774

* { goncharov 22.08.2017 11:52:0
  DATA: lv_dont_post_del TYPE flag
      .
* не надо ничего делать для мероприятия изменения
  IMPORT lv_dont_post_del TO lv_dont_post_del FROM MEMORY ID 'ZDONT_POST_DEL'.
  IF lv_dont_post_del EQ abap_true.
    RETURN.
  ENDIF.
* } goncharov 22.08.2017 11:52:02

*---Anything to do?
  CHECK NOT im_attabs_tab IS INITIAL.

*---Fill BLoP structure with INSert and DELete data records
  CALL METHOD cl_pt_arq_badi=>convert_attabs_to_blop_ins
    EXPORTING
      im_attabs_tab = im_attabs_tab
    IMPORTING
      ex_blop_tab   = lt_blop_ins.

  CALL METHOD cl_pt_arq_badi=>convert_attabs_to_blop_del
    EXPORTING
      im_attabs_tab = im_attabs_tab
    IMPORTING
      ex_blop_tab   = lt_blop_del.

* Begin of changes: HEGDEPR 2053774
* Retaining the text if maintained via transaction PA30
  IF lt_blop_del IS NOT INITIAL AND lt_blop_ins IS NOT INITIAL.
    READ TABLE lt_blop_del INTO blop_tab_del_wa INDEX 1.
    IF blop_tab_del_wa-data-it-text IS NOT INITIAL.
      READ TABLE lt_blop_ins INTO blop_tab_ins_wa INDEX 1.
      blop_tab_ins_wa-data-it-text = blop_tab_del_wa-data-it-text.
      MODIFY TABLE lt_blop_ins FROM blop_tab_ins_wa TRANSPORTING data-it-text.
    ENDIF.
  ENDIF.
* End of changes: HEGDEPR 2053774

  APPEND LINES OF lt_blop_ins TO lt_blop_tab.
  APPEND LINES OF lt_blop_del TO lt_blop_tab.

*---Call BLoP to fill TMW buffer and add messages to message handler
  CALL METHOD cl_pt_arq_badi=>blop_maintain_timedata
    EXPORTING
      im_check_mode       = 'X'
      im_collect_messages = 'X'
    IMPORTING
      ex_cannot_post      = ex_cannot_post
    CHANGING
      ch_blop_tab         = lt_blop_tab.

*---Free TMW buffer if data could not be posted            "note 929948
  IF NOT ex_cannot_post IS INITIAL.                        "note 929948
    CALL FUNCTION 'HR_TMW_BUFFER_FREE'. "note 929948
  ENDIF.                                                   "note 929948

*---No sense to continue if data could not be posted
  CHECK ex_cannot_post IS INITIAL.

  CLEAR l_test.                                            "Begin of Note ANK 1014628
  IMPORT update TO l_test FROM MEMORY ID 'UPD_FLG'.
  IF NOT sy-subrc IS INITIAL OR l_test IS INITIAL.
*---Save TMW buffer to database
    CALL METHOD cl_pt_arq_badi=>blop_save_timedata
      IMPORTING
        ex_cannot_post = ex_cannot_post.
  ENDIF.                                                   "End of Note ANK 1014628

*---Free TMW buffer
  CALL FUNCTION 'HR_TMW_BUFFER_FREE'.

*---No sense to continue if data could not be posted
  CHECK ex_cannot_post IS INITIAL.

*---Convert BLoP structure into infotype structure
  CALL METHOD cl_pt_arq_badi=>convert_blop_to_attabs
    EXPORTING
      im_attabs_tab = im_attabs_tab
      im_blop_tab   = lt_blop_tab
    IMPORTING
      ex_attabs_tab = ex_attabs_tab.

ENDMETHOD.


  METHOD IF_EX_PT_ABS_REQ~PROCESS_MESSAGES.
* XRK 20141215 Note 2109287 Error message processing
* XRK 20120620 Note1732458 Process messages via a customer exit
* Messages are imported via two tables:
*  IM_MESSAGE_TAB     contains messages of request to be checked
*  IM_MESSAGE_DEP_TAB contains messages of dependent records

    DATA:          context          TYPE tim_req_msg_context.
    FIELD-SYMBOLS: <lcl_message_wa> TYPE tim_tmw_message_entry.

    IF NOT im_collect_messages IS INITIAL AND NOT im_message_tab IS INITIAL.
*     Collect all messages pertaining to the entered request
      LOOP AT im_message_tab ASSIGNING <lcl_message_wa>.
        context = <lcl_message_wa>-msgcon-msgcontext-context.
*       Hand-over messages to message handler
        CALL METHOD message_handler->add_message
          EXPORTING
            im_type       = <lcl_message_wa>-msgdat-msgty
            im_cl         = <lcl_message_wa>-msgdat-msgid
            im_number     = <lcl_message_wa>-msgdat-msgno
            im_par1       = <lcl_message_wa>-msgdat-msgv1
            im_par2       = <lcl_message_wa>-msgdat-msgv2
            im_par3       = <lcl_message_wa>-msgdat-msgv3
            im_par4       = <lcl_message_wa>-msgdat-msgv4
            im_context    = context
            im_classname  = 'CL_PT_ARQ_REQ_EXIT'
            im_methodname = 'PROCESS_MESSAGES'.
      ENDLOOP.
    ENDIF.

    IF ( im_blop_retcd EQ cl_pt_tmw_tdm_const=>retcd_e          "Note 2109287
      OR im_blop_retcd EQ cl_pt_tmw_tdm_const=>retcd_a ) AND    "Note 2109287
        im_message_tab IS INITIAL.                              "Note 2109287
*   Collect all error messages for dependent records in case the record cannot
*   be posted and there is no error message for the record to be checked
      LOOP AT im_message_dep_tab ASSIGNING <lcl_message_wa>
      WHERE msgdat-msgty CA 'EAX'.
        context = <lcl_message_wa>-msgcon-msgcontext-context.
*     Hand-over messages to message handler
        CALL METHOD message_handler->add_message
          EXPORTING
            im_type       = <lcl_message_wa>-msgdat-msgty
            im_cl         = <lcl_message_wa>-msgdat-msgid
            im_number     = <lcl_message_wa>-msgdat-msgno
            im_par1       = <lcl_message_wa>-msgdat-msgv1
            im_par2       = <lcl_message_wa>-msgdat-msgv2
            im_par3       = <lcl_message_wa>-msgdat-msgv3
            im_par4       = <lcl_message_wa>-msgdat-msgv4
            im_context    = context
            im_classname  = 'CL_PT_ARQ_REQ_EXIT'
            im_methodname = 'PROCESS_MESSAGES'.
      ENDLOOP.
    ENDIF.                                                      "Note 2109287
  ENDMETHOD.


method IF_EX_PT_ABS_REQ~SET_CALENDAR_COLOR.
endmethod.


method IF_EX_PT_ABS_REQ~SET_CALENDAR_LEGEND_COLORS.
endmethod.


method IF_EX_PT_ABS_REQ~SET_CALENDAR_OVERLAPPING_COLOR.
endmethod.


method IF_EX_PT_ABS_REQ~SET_OVERVIEW_LEGEND_COLORS.
endmethod.


METHOD if_ex_pt_abs_req~simulate_via_blop .
* XRK 20131114 Note1940733 Not all required quota records read
* XRK 20130627 Note1877895 'Negative quota' for DEL-requests
* XRK 20070403 Note1043502 Disable authority check for DEL records during simulation
* XRK 20060103 Note912250 Disable authority check for simulation

  TYPES: BEGIN OF p2006_recuid,
           include      TYPE p2006,
           quota_recuid TYPE tim_tmwrecuid,
         END OF p2006_recuid.
  TYPES: BEGIN OF p2007_recuid,
           include      TYPE p2007,
           quota_recuid TYPE tim_tmwrecuid,
         END OF p2007_recuid.
  TYPES: BEGIN OF it_quota,
           include TYPE ptarq_bapiabwkon,
           docnr   TYPE ptm_docnr,
         END OF it_quota.
  TYPES: BEGIN OF i_docnr,                                            "Note1877895
           docnr TYPE ptm_docnr,                                      "Note1877895
         END OF i_docnr.                                                     "Note1877895

  DATA: lt_blop_tab      TYPE tim_tmw_blprequest_tab,
        blop_wa          TYPE LINE OF tim_tmw_blprequest_tab,
        lt_blop_temp     TYPE tim_tmw_blprequest_tab,
        lt_match_tab     TYPE ptarq_attabsdata_tab,
        lt_attabs_tab    TYPE ptarq_attabsdata_tab,
        attabs_deduct_wa TYPE LINE OF ptarq_attabs_deduct_tab,
        attabs_temp      TYPE LINE OF ptarq_attabsdata_tab,
        lt_quota_tab     TYPE TABLE OF it_quota,
        quota_wa         TYPE it_quota.
  DATA: l_selcat         TYPE tim_tmw_selcattype_entry,
        lt_selcat_tab    TYPE tim_tmw_selcattype_tab,
        lt_timedata_tab  TYPE tim_tmw_blprequest_tab,
        lt_pergrp_tab    TYPE tim_tmw_pernr_group_tab,
        l_timedata_cmdwa TYPE tim_tmw_commtab_entry,
        quotaded_tab     TYPE tim_quotaded_tab,
        lt_quotaded_tab  TYPE tim_quotaded_tab,
        l_ptquoded       TYPE ptquoded,
        lt_docnr         TYPE TABLE OF i_docnr,                           "Note1877895
        l_p2001          TYPE p2001,
        p2001_tab        TYPE TABLE OF p2001,
        l_p2002          TYPE p2002,
        p2002_tab        TYPE TABLE OF p2002,
        l_p2006          TYPE p2006,
        l_p2006_recuid   TYPE p2006_recuid,
        p2006_tab        TYPE TABLE OF p2006_recuid,
        l_p2007          TYPE p2007,
        l_p2007_recuid   TYPE p2007_recuid,
        p2007_tab        TYPE TABLE OF p2007_recuid,
        l_retcd          TYPE sysubrc,
        l_556b           TYPE t556b,
        l_556q           TYPE t556q,
        l_538t           TYPE t538t,
        sel_begda        TYPE d,
        sel_endda        TYPE d.

  FIELD-SYMBOLS: <timedata_blpwa> TYPE tim_tmw_blprequest_entry,
                 <ptquoded_wa>    TYPE ptquoded.            "Note1877895

  lt_match_tab = im_attabs_tab.

*---Fill BLoP structure and add RECUID to match table
  LOOP AT lt_match_tab INTO attabs_temp.
    CLEAR lt_attabs_tab[].
    CLEAR lt_blop_temp[].
    APPEND attabs_temp TO lt_attabs_tab.
    IF attabs_temp-operation EQ cl_pt_tmw_tdm_const=>op_ins.
      CALL METHOD cl_pt_arq_badi=>convert_attabs_to_blop_ins
        EXPORTING
          im_attabs_tab = lt_attabs_tab
        IMPORTING
          ex_blop_tab   = lt_blop_temp.
    ELSE.
      CALL METHOD cl_pt_arq_badi=>convert_attabs_to_blop_del
        EXPORTING
          im_attabs_tab = lt_attabs_tab
        IMPORTING
          ex_blop_tab   = lt_blop_temp.
    ENDIF.
    READ TABLE lt_blop_temp INDEX 1 INTO blop_wa TRANSPORTING control-recuid.
    attabs_temp-tmw_recuid = blop_wa-control-recuid.
    MODIFY lt_match_tab FROM attabs_temp.
*---Use special authority status '3' for all records to disable       "INS Note1043502
*   authority check during simulation                                 "INS Note1043502
    blop_wa-control-authstat = 3.                                   "INS Note912250
    MODIFY lt_blop_temp INDEX 1 FROM blop_wa                        "INS Note1043502
                        TRANSPORTING control-authstat.              "INS Note1043502
*    MODIFY lt_blop_temp FROM blop_wa TRANSPORTING control-authstat   "DEL Note1043502
*     WHERE control-authstat IS INITIAL.         "INS Note912250      "DEL Note1043502
    APPEND LINES OF lt_blop_temp TO lt_blop_tab.
  ENDLOOP.

*---Get item id of record to be checked (always in first line of table)
  CLEAR attabs_temp.
  READ TABLE lt_match_tab INDEX 1 INTO attabs_temp TRANSPORTING tmw_recuid.

*---Sort LT_BLOP_TAB so that record to be checked is at first index position
  READ TABLE lt_blop_tab WITH KEY control-recuid = attabs_temp-tmw_recuid INTO blop_wa.
  IF sy-subrc EQ 0.
    DELETE lt_blop_tab INDEX sy-tabix.
    INSERT blop_wa INTO lt_blop_tab INDEX 1.
  ENDIF.

*---Call BLoP to fill TMW buffer and - if requested - add messages to message handler
  CALL METHOD cl_pt_arq_badi=>blop_maintain_timedata
    EXPORTING
      im_check_mode       = space
      im_collect_messages = im_collect_messages
    CHANGING
      ch_blop_tab         = lt_blop_tab.

*---Fill table of PERNRGROUPs and selection time period with data from checked record
  LOOP AT lt_blop_tab INTO blop_wa WHERE control-prstat NE 'ERR'.
    INSERT blop_wa-control-pernrgroup INTO TABLE lt_pergrp_tab.
    IF sel_begda IS INITIAL OR blop_wa-control-begda < sel_begda.
      sel_begda = blop_wa-control-begda.
    ENDIF.
    IF sel_endda IS INITIAL OR blop_wa-control-endda > sel_endda.
      sel_endda = blop_wa-control-endda.
    ENDIF.
  ENDLOOP.
*---Consider previous-day record                                      "Note1940733
  sel_begda = sel_begda - 1.                                          "Note1940733
*---Fill table of selection categories
*---Quota deduction
  l_selcat-category                  = cl_pt_tmw_tdm_const=>cat_deduct.
  l_selcat-type                      = cl_pt_tmw_tdm_const=>type_quotaded.
  APPEND l_selcat TO lt_selcat_tab.
*---Absences
  l_selcat-category                  = cl_pt_tmw_tdm_const=>cat_infty.
  l_selcat-type                      = '2001'.
  APPEND l_selcat TO lt_selcat_tab.
*---Attendances
  l_selcat-category                  = cl_pt_tmw_tdm_const=>cat_infty.
  l_selcat-type                      = '2002'.
  APPEND l_selcat TO lt_selcat_tab.
*---Get all specified data records
  CALL FUNCTION 'HR_BLP_READ_TIMEDATA'
    EXPORTING
      pernrgroup    = lt_pergrp_tab
      selcattype    = lt_selcat_tab
      record_status = 0                                               "Note1940733
      fromdate      = sel_begda
      todate        = sel_endda
      get_dependent = space                                           "Note1940733
      no_authcheck  = 'X'
    IMPORTING
      time_data     = lt_timedata_tab
      retcd         = l_retcd
    EXCEPTIONS
      OTHERS        = 1.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
* Begin of note 1940733
*---Get all quota records and append them to LT_TIMEDATA_TAB
    CLEAR lt_selcat_tab[].
    CLEAR lt_blop_temp[].
*---Absence quota
    l_selcat-category                  = cl_pt_tmw_tdm_const=>cat_infty.
    l_selcat-type                      = '2006'.
    APPEND l_selcat TO lt_selcat_tab.
*---Attendance quota
    l_selcat-category                  = cl_pt_tmw_tdm_const=>cat_infty.
    l_selcat-type                      = '2007'.
    APPEND l_selcat TO lt_selcat_tab.
    CALL FUNCTION 'HR_BLP_READ_TIMEDATA'
      EXPORTING
*       RECUIDLIST    =
        pernrgroup    = lt_pergrp_tab
        selcattype    = lt_selcat_tab
        fromdate      = '18000101'
        todate        = '99991231'
*       CONTEXT_TAB   =
        get_dependent = space
        record_status = 0                                             "Note1940733
        no_authcheck  = 'X'
      IMPORTING
        time_data     = lt_blop_temp
*       MESSAGETAB    =
        retcd         = l_retcd
      EXCEPTIONS
        OTHERS        = 1.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    APPEND LINES OF lt_blop_temp TO lt_timedata_tab.
* End of note 1940733

*---Append absence/attendance deletion requests                       "Note1877895
    LOOP AT lt_blop_tab ASSIGNING <timedata_blpwa>                    "Note1877895
      WHERE control-opera EQ cl_pt_req_const=>c_item_del AND          "Note1877895
            control-category EQ 'IT' AND (                            "Note1877895
            control-type EQ '2001' OR                                 "Note1877895
            control-type EQ '2002' ).                                 "Note1877895
      APPEND <timedata_blpwa> TO lt_timedata_tab.                     "Note1877895
    ENDLOOP.                                                          "Note1877895
*---Fill internal tables containing quotas, absences and attendances
    LOOP AT lt_timedata_tab ASSIGNING <timedata_blpwa>.
      CLEAR l_timedata_cmdwa.
      MOVE-CORRESPONDING <timedata_blpwa>-control TO l_timedata_cmdwa.
      l_timedata_cmdwa-data = <timedata_blpwa>-data.

      CASE l_timedata_cmdwa-type.
        WHEN 'QUOTADED'.
          CALL FUNCTION 'HR_TMW_QLDDATA_FROM_COMMTAB'
            EXPORTING
              commtab_entry   = l_timedata_cmdwa
            IMPORTING
              quoded_day      = l_ptquoded
            EXCEPTIONS
              wrong_category  = 1
              parameter_error = 2
              no_entry_found  = 3
              OTHERS          = 4.
          IF sy-subrc EQ 0.
            APPEND l_ptquoded TO quotaded_tab.
          ENDIF.
        WHEN '2001'.
          CALL FUNCTION 'HR_TMW_ITDATA_FROM_BLPREQUEST'
            EXPORTING
              blprequest_entry = <timedata_blpwa>
            IMPORTING
              infty_record     = l_p2001
            EXCEPTIONS
              wrong_category   = 1
              parameter_error  = 2
              no_entry_found   = 3
              OTHERS           = 4.
          IF sy-subrc EQ 0.
            APPEND l_p2001 TO p2001_tab.
*---Collect DOCNRs of absence deletion requests                       "Note1877895
            IF l_timedata_cmdwa-opera EQ cl_pt_req_const=>c_item_del. "Note1877895
              APPEND l_p2001-docnr TO lt_docnr.                       "Note1877895
            ENDIF.                                                    "Note1877895
          ENDIF.
        WHEN '2002'.
          CALL FUNCTION 'HR_TMW_ITDATA_FROM_BLPREQUEST'
            EXPORTING
              blprequest_entry = <timedata_blpwa>
            IMPORTING
              infty_record     = l_p2002
            EXCEPTIONS
              wrong_category   = 1
              parameter_error  = 2
              no_entry_found   = 3
              OTHERS           = 4.
          IF sy-subrc EQ 0.
            APPEND l_p2002 TO p2002_tab.
*---Collect DOCNRs of attendance deletion requests                    "Note1877895
            IF l_timedata_cmdwa-opera EQ cl_pt_req_const=>c_item_del. "Note1877895
              APPEND l_p2002-docnr TO lt_docnr.                       "Note1877895
            ENDIF.                                                    "Note1877895
          ENDIF.
        WHEN '2006'.
          CALL FUNCTION 'HR_TMW_ITDATA_FROM_BLPREQUEST'
            EXPORTING
              blprequest_entry = <timedata_blpwa>
            IMPORTING
              infty_record     = l_p2006
            EXCEPTIONS
              wrong_category   = 1
              parameter_error  = 2
              no_entry_found   = 3
              OTHERS           = 4.
          IF sy-subrc EQ 0.
            MOVE-CORRESPONDING l_p2006 TO l_p2006_recuid-include.
            l_p2006_recuid-quota_recuid = <timedata_blpwa>-control-recuid.
            APPEND l_p2006_recuid TO p2006_tab.
          ENDIF.
        WHEN '2007'.
          CALL FUNCTION 'HR_TMW_ITDATA_FROM_BLPREQUEST'
            EXPORTING
              blprequest_entry = <timedata_blpwa>
            IMPORTING
              infty_record     = l_p2007
            EXCEPTIONS
              wrong_category   = 1
              parameter_error  = 2
              no_entry_found   = 3
              OTHERS           = 4.
          IF sy-subrc EQ 0.
            MOVE-CORRESPONDING l_p2007 TO l_p2007_recuid-include.
            l_p2007_recuid-quota_recuid = <timedata_blpwa>-control-recuid.
            APPEND l_p2007_recuid TO p2007_tab.
          ENDIF.

      ENDCASE.

    ENDLOOP.

    IF lt_docnr[] IS NOT INITIAL.                                     "Note1877895
*---Determine "negative quota" for DEL requests                       "Note1877895
      SELECT *                                                        "Note1877895
        INTO TABLE lt_quotaded_tab                                    "Note1877895
        FROM ptquoded                                                 "Note1877895
        FOR ALL ENTRIES IN lt_docnr                                   "Note1877895
        WHERE docnr = lt_docnr-docnr.                                 "Note1877895
      LOOP AT lt_quotaded_tab ASSIGNING <ptquoded_wa>.                "Note1877895
*---Change sign of quota                                              "Note1877895
        <ptquoded_wa>-quode = - <ptquoded_wa>-quode.                  "Note1877895
        APPEND <ptquoded_wa> TO quotaded_tab.                         "Note1877895
      ENDLOOP.                                                        "Note1877895
    ENDIF.                                                            "Note1877895

*---Compose deduction tables
*---Process absences
    SORT p2001_tab.                                                   "Note1940733
    DELETE ADJACENT DUPLICATES FROM p2001_tab.                        "Note1940733
    LOOP AT p2001_tab INTO l_p2001.
      CLEAR lt_quotaded_tab.
*---For each absence, fill table PTQUODED for each day
      LOOP AT quotaded_tab INTO l_ptquoded WHERE docnr EQ l_p2001-docnr.
        APPEND l_ptquoded TO lt_quotaded_tab.
      ENDLOOP.

*     Note 1924123: ensure sort for binary search access
      SORT lt_quotaded_tab.
      LOOP AT lt_quotaded_tab INTO l_ptquoded.

        AT NEW quonr.
*---Initialize deduction workarea
          CLEAR quota_wa.
        ENDAT.

        LOOP AT p2006_tab INTO l_p2006_recuid WHERE include-quonr EQ l_ptquoded-quonr.
          IF l_ptquoded-datum > sy-datum.
*---Planned quota for the future
            ADD l_ptquoded-quode TO quota_wa-include-ordered.
          ELSE.
*---Planned quota up to (and including) today
            ADD l_ptquoded-quode TO quota_wa-include-deduct.
          ENDIF.
        ENDLOOP.

        quota_wa-include-time_unit = l_ptquoded-quoun.

        AT END OF quonr.
          quota_wa-include-quotaend = l_p2006_recuid-include-deend.
          quota_wa-include-quotabeg = l_p2006_recuid-include-desta.
          quota_wa-include-entitle = l_p2006_recuid-include-anzhl.
          quota_wa-include-quotanum = l_p2006_recuid-include-anzhl - quota_wa-include-ordered - quota_wa-include-deduct.
          quota_wa-include-begda = l_p2001-begda.
          quota_wa-include-endda = l_p2001-endda.
          quota_wa-include-qtype = 'A'.
          quota_wa-include-leavetype = quota_wa-include-quotatype = l_p2006_recuid-include-ktart.
*---Retrieve absence quota text and time unit text
          IF NOT quota_wa IS INITIAL.
            CALL METHOD cl_pt_arq_customizing=>get_abwko_customizing
              EXPORTING
                im_pernr      = l_p2006_recuid-include-pernr
                im_abwko      = quota_wa-include-quotatype
              IMPORTING
                ex_556b       = l_556b
                ex_538t       = l_538t
              EXCEPTIONS
                error_occured = 1
                OTHERS        = 2.
            IF sy-subrc EQ 0.
              quota_wa-include-quotatext = l_556b-ktext.
              quota_wa-include-tiunitext = l_538t-etext.
            ENDIF.
            quota_wa-docnr = l_p2001-docnr.
*            APPEND quota_wa TO lt_quota_tab.                         "Note900648
            COLLECT quota_wa INTO lt_quota_tab.             "Note900648
          ENDIF.
        ENDAT.

      ENDLOOP.
    ENDLOOP.

*---Process attendances
    SORT p2002_tab.                                                   "Note1940733
    DELETE ADJACENT DUPLICATES FROM p2002_tab.                        "Note1940733
    LOOP AT p2002_tab INTO l_p2002.
      CLEAR lt_quotaded_tab.
*---For each attendance, fill table PTQUODED for each day
      LOOP AT quotaded_tab INTO l_ptquoded WHERE docnr EQ l_p2002-docnr.
        APPEND l_ptquoded TO lt_quotaded_tab.
      ENDLOOP.

*     Note 1924123: ensure sort for binary search access
      SORT lt_quotaded_tab.
      LOOP AT lt_quotaded_tab INTO l_ptquoded.

        AT NEW quonr.
*---Initialize deduction workarea
          CLEAR quota_wa.
        ENDAT.

        LOOP AT p2007_tab INTO l_p2007_recuid WHERE include-quonr EQ l_ptquoded-quonr.
          IF l_ptquoded-datum > sy-datum.
*---Planned quota for the future
            ADD l_ptquoded-quode TO quota_wa-include-ordered.
          ELSE.
*---Planned quota up to (and including) today
            ADD l_ptquoded-quode TO quota_wa-include-deduct.
          ENDIF.
        ENDLOOP.

        quota_wa-include-time_unit = l_ptquoded-quoun.

        AT END OF quonr.
          quota_wa-include-quotaend = l_p2007_recuid-include-deend.
          quota_wa-include-quotabeg = l_p2007_recuid-include-desta.
          quota_wa-include-entitle = l_p2007_recuid-include-anzhl.
          quota_wa-include-quotanum = l_p2007_recuid-include-anzhl - quota_wa-include-ordered - quota_wa-include-deduct.
          quota_wa-include-begda = l_p2002-begda.
          quota_wa-include-endda = l_p2002-endda.
          quota_wa-include-qtype = 'P'.
          quota_wa-include-leavetype = quota_wa-include-quotatype = l_p2007_recuid-include-ktart.
*---Retrieve attendance quota text and time unit text
          IF NOT quota_wa IS INITIAL.
            CALL METHOD cl_pt_arq_customizing=>get_anwko_customizing
              EXPORTING
                im_pernr      = l_p2007_recuid-include-pernr
                im_anwko      = quota_wa-include-quotatype
              IMPORTING
                ex_556q       = l_556q
                ex_538t       = l_538t
              EXCEPTIONS
                error_occured = 1
                OTHERS        = 2.
            IF sy-subrc EQ 0.
              quota_wa-include-quotatext = l_556q-ktext.
              quota_wa-include-tiunitext = l_538t-etext.
            ENDIF.
            quota_wa-docnr = l_p2002-docnr.
*            APPEND quota_wa TO lt_quota_tab.                         "Note900648
            COLLECT quota_wa INTO lt_quota_tab.             "Note900648
          ENDIF.
        ENDAT.

      ENDLOOP.
    ENDLOOP.

  ENDIF.

*---TMW buffer no longer relevant
  CALL FUNCTION 'HR_TMW_BUFFER_FREE'.

*---Convert BLoP structure into infotype structure
  CALL METHOD cl_pt_arq_badi=>convert_blop_to_attabs
    EXPORTING
      im_attabs_tab = lt_match_tab
      im_blop_tab   = lt_blop_tab
    IMPORTING
      ex_attabs_tab = lt_attabs_tab.

*---Fill EX_ATTABS_DEDUCT_TAB
  LOOP AT lt_attabs_tab INTO attabs_temp.
* attabs_deduct_wa-REQUEST_ID
    CLEAR attabs_deduct_wa.
    MOVE-CORRESPONDING attabs_temp TO attabs_deduct_wa.
    IF NOT attabs_deduct_wa-abs_attribs IS INITIAL.
      READ TABLE attabs_deduct_wa-abs_attribs INDEX 1 INTO l_p2001 TRANSPORTING docnr.
      LOOP AT lt_quota_tab INTO quota_wa WHERE docnr = l_p2001-docnr.
        APPEND quota_wa TO attabs_deduct_wa-deduct_tab.
      ENDLOOP.
    ENDIF.
    IF NOT attabs_deduct_wa-atts_attribs IS INITIAL.
      READ TABLE attabs_deduct_wa-atts_attribs INDEX 1 INTO l_p2002 TRANSPORTING docnr.
      LOOP AT lt_quota_tab INTO quota_wa WHERE docnr = l_p2002-docnr.
        APPEND quota_wa TO attabs_deduct_wa-deduct_tab.
      ENDLOOP.
    ENDIF.
    APPEND attabs_deduct_wa TO ex_attabs_deduct_tab.
  ENDLOOP.


* { goncharov 14.09.2017 16:55:12
***  LOOP AT ex_attabs_deduct_tab ASSIGNING FIELD-SYMBOL(<fs_attabs>).
***    LOOP AT <fs_attabs>-deduct_tab ASSIGNING FIELD-SYMBOL(<fs_abs_attribs>)
***        WHERE ( deduct NE 0 OR ordered NE 0 ) AND
***              quotanum NE 0 AND " лимит не полностью израсходован
***              quotatype IN zcl_vacation_appl=>mr_ktart.
***      MESSAGE e001(zhr_pa) WITH 'Отсутствие должно полностью расходовать лимит.'(001)
***        INTO zcl_vacation_appl=>mv_dummy.
***      message_handler->add_message(
***        im_type       = sy-msgty
***        im_cl         = sy-msgid
***        im_number     = sy-msgno
***        im_par1       = sy-msgv1
***        im_par2       = sy-msgv2
***        im_par3       = sy-msgv3
***        im_par4       = sy-msgv4
***        im_classname  = 'ZCL_PT_ARQ_REQ_EXIT'
***        im_methodname = 'SIMULATE_VIA_BLOP' ).
***      RETURN.
***    ENDLOOP.
***  ENDLOOP.
* } goncharov 14.09.2017 16:55:12
ENDMETHOD.


METHOD IF_EX_PT_ABS_REQ~TEAM_CALE_ENRICHMENT.

* Method TEAM_CALE_ENRICHMENT implementation example
**********************************************************
*  DATA: rowlabel_wa TYPE LINE OF ptarq_uia_rowlabel_tab,
*        calendar_wa TYPE LINE OF ptarq_uia_weekcale_tab,
*        request_wa TYPE LINE OF ptreq_uia_request_id_tab.
*
*  CASE im_modus.
*    WHEN 'R'.
*      rowlabel_wa-row_id = '00999999'.
*      rowlabel_wa-rowlabel = 'All Colleagues'.
*      APPEND rowlabel_wa TO ch_rowlabel.
*
*      calendar_wa-row_id = '00999999'.
*      calendar_wa-begda = sy-datum.
*      calendar_wa-endda = sy-datum + 3.
*      calendar_wa-legendid = 'GROUP_LEVEL2'.
*      calendar_wa-barlabel = 'Excursion'.
*      APPEND calendar_wa TO ch_calendar_tab.
*
*    WHEN 'A'.
*      rowlabel_wa-row_id = '00999998'.
*      rowlabel_wa-rowlabel = 'All Employees'.
*      APPEND rowlabel_wa TO ch_rowlabel.
*
*      calendar_wa-row_id = '00999998'.
*      calendar_wa-begda = sy-datum.
*      calendar_wa-endda = sy-datum + 3.
*      calendar_wa-legendid = 'GROUP_LEVEL2'.
*      calendar_wa-barlabel = 'Excursion'.
*      APPEND calendar_wa TO ch_calendar_tab.
*
*    WHEN OTHERS.
*  ENDCASE.

ENDMETHOD.


  METHOD IF_EX_PT_ABS_REQ~UPDATE_MULTIPLE_APPROVERS.
*************************************************************************************************************
******    Update Multiple Level Remember Table ************************************
* If customers want to enable the remember functionality for multiple approver. He should follow below steps:
*   1. Create an application table to record the last input approvers based on leave type (and leave period)
*   2. Add implementation into BAdI method UPDATE_MULTIPLE_APPROVERS to record approver into table created in Step 1
*   3. Add implemenation into BAdI method GET_MULTIPLE_APPROVERS to get approvers from table created in Step 1
*
*Below code shows how to update the remember table accoridng to leave type. HRESSAPPROVER2 is an customzing
*table. It contains the mandt, leave type(subtype), employee user name(inituser), approver sequence(seqnr), approver user name(approver).
*
* For details, please check the BAdI method description.
*
*************************************************************************************************************

*    DATA: ls_approver        LIKE LINE OF it_approver,
*          ls_last_input      TYPE  zhressapprover2,
*          lt_last_input_list TYPE TABLE OF zhressapprover2.
*    "update remember table
*    DELETE FROM zhressapprover2 WHERE inituser = sy-uname and subtype = im_subtype.
*    LOOP AT it_approver INTO ls_approver.
*      CLEAR ls_last_input.
*      ls_last_input-mandt = sy-mandt.
*      ls_last_input-req_type = im_subtype.
*      ls_last_input-inituser = sy-uname.
*      ls_last_input-seqnr = ls_approver-seqnr.
*      ls_last_input-approver = ls_approver-sys_user.
*      ls_last_input-subtype =  im_subtype.
*      APPEND ls_last_input TO lt_last_input_list.
*    ENDLOOP.
*    INSERT hressapprover2 FROM TABLE lt_last_input_list.
  ENDMETHOD.


METHOD IF_EX_PT_ABS_REQ~VALIDATE_AND_SIMULATE .
* XRK 20040630 Note750630 Method obsolete

*  DATA:   lcl_req_header  TYPE REF TO if_pt_req_header,
*          l_selector_tab  TYPE ptreq_selector_tab,
*          l_selector_wa   TYPE LINE OF ptreq_selector_tab,
*          time_data_tab   TYPE tim_tmw_blprequest_tab,
*          time_data_wa    TYPE LINE OF tim_tmw_blprequest_tab,
*          l_messages      TYPE bapiret2_t,
*          l_messages_wa   TYPE LINE OF bapiret2_t,
*          l_e_message_sw  TYPE boole_d,
*          blop_tab        TYPE ptarq_attabsdata_tab,
*          lt_attabs_tab   TYPE ptarq_attabsdata_tab.
*
**---Convert request into attabs
*  CALL METHOD cl_pt_arq_processor=>convert_request_into_attabs
*    EXPORTING
*      im_request    = request
*    IMPORTING
*      ex_attabs_tab = lt_attabs_tab.
*
**---Check provided request
*  CALL METHOD me->if_ex_pt_abs_req~check_time_constraints
*    EXPORTING
*      im_attabs_tab = lt_attabs_tab
*    IMPORTING
*      ex_ret_value  = ex_ret_value.
*
*  lcl_req_header ?= request->workarea_version.
**---Fill TIME_DATA_TAB with infotype data from request positions
*  CALL METHOD lcl_req_header->get_item_selector_tab
*    RECEIVING
*      result = l_selector_tab.
*
*  LOOP AT l_selector_tab INTO l_selector_wa.
**---Read infotype data into master data buffer via selector method
*    CALL METHOD l_selector_wa-selector->read_it_data_into_timedata
*      EXPORTING
*        im_item            = l_selector_wa-item
*        im_req_header      = lcl_req_header
*      IMPORTING
*        ex_time_data_entry = time_data_wa.
*    APPEND time_data_wa TO time_data_tab.
*  ENDLOOP.
*
**---Convert BLoP data into infotype data
*  CALL METHOD cl_pt_arq_badi=>convert_blop_to_attabs
*    EXPORTING
*      im_attabs_tab = lt_attabs_tab
*      im_blop_tab   = time_data_tab
*    IMPORTING
*      ex_attabs_tab = blop_tab.
*
*  CLEAR saved_quotas[].
*  CLEAR saved_time_accounts[].
**---Simulate and always add messages to message handler
**  CALL METHOD me->if_ex_pt_abs_req~simulate_via_blop
**    EXPORTING
**      im_attabs_tab       = blop_tab
**      im_collect_messages = cl_pt_req_const=>c_true
**    IMPORTING
**      ex_attdeduction_tab    = saved_quotas.
*
*  GET REFERENCE OF saved_quotas        INTO ex_simulated_data.
**  GET REFERENCE OF saved_time_accounts INTO ex_additional_data.
*
**---Add all messages
*  LOOP AT l_messages INTO l_messages_wa.
*    IF l_messages_wa-type = 'E'.
*      l_e_message_sw = 'X'.
*    ENDIF.
*    CALL METHOD message_handler->add_message
*      EXPORTING
*        im_type       = l_messages_wa-type
*        im_cl         = l_messages_wa-id
*        im_number     = l_messages_wa-number
*        im_par1       = l_messages_wa-message_v1
*        im_par2       = l_messages_wa-message_v2
*        im_par3       = l_messages_wa-message_v3
*        im_par4       = l_messages_wa-message_v4
*        im_context    = 'REQUEST'
*        im_subcontext = 'WORKFLOW'
*        im_funcname   = ' '
*        im_classname  = 'CL_PT_REQ_REQUEST'
*        im_methodname = 'IF_PT_REQ_REQUEST~VALIDATE_AND_SIMULATE'.
*  ENDLOOP.
*  IF sy-subrc = 0 AND NOT l_e_message_sw IS INITIAL.
*    ex_ret_value = 25.
*  ENDIF.
*
*
ENDMETHOD.
ENDCLASS.

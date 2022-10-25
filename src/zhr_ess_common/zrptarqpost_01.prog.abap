*&---------------------------------------------------------------------*
*&  Include  ZRPTARQPOST_01
*&---------------------------------------------------------------------*

* переработка стандарта для создания мероприятия

FORM zz_process_all_requests
  USING
    appid TYPE pt_application_id.

  DATA:
    sorted_req_tab  TYPE request_process_by_oper_tab,
    wa_sorted_req   TYPE request_process_by_oper_typ,
    wa_command      TYPE tim_req_xfer_event,
    found           TYPE xfeld,
    lcl_header      TYPE REF TO cl_pt_req_header,
    lt_itemtab      TYPE ptreq_items_tab,
    lt_itemtab_tmp  TYPE ptreq_items_tab,
    lcl_item_wa     TYPE ptreq_items_struc,
    lcl_item        TYPE REF TO cl_pt_req_attabsdata,
    lcl_pa03_status TYPE vwsta,                                   "INS Note1128385
    lcl_pa03_endda  TYPE t549q-endda,                              "INS Note1128385
    wa_p2001        TYPE pa2001,                            "#EC NEEDED
    wa_p2002        TYPE pa2002,                            "#EC NEEDED
    wa_pernr        TYPE persno,
    wa_subty        TYPE subty,
    wa_begda        TYPE begda,
    wa_endda        TYPE endda,
    wa_begtm        TYPE begti,
    wa_opera        TYPE ioper,                                    "INS Note1135709
    wa_stdaz        TYPE abstd,                                    "INS Note1135709
    found_ins       TYPE xfeld,                                    "INS Note1135709
    found_del       TYPE xfeld,                                    "INS Note1135709
    ins_tabix       TYPE sy-tabix,                                 "INS Note1135709
    del_tabix       TYPE sy-tabix,                                 "INS Note1135709
    modif_item_tab  TYPE xfeld,                                    "INS Note1135709
    item_obj_tab    TYPE ptreq_item_object_tab,                    "INS Note1135709
    timestamp_match TYPE boolean VALUE c_true,                  "INS NOTE 999620
    wa_endtm        TYPE endti,
    space6(6)       VALUE '      '
  , lv_subrc        TYPE sysubrc
  .
  FIELD-SYMBOLS: <fs_req> TYPE LINE OF request_process_by_oper_tab
               .

  PERFORM sort_request_objects
    USING
      req_tab
    CHANGING
      sorted_req_tab.

*  SORT sorted_req_tab STABLE BY operation.                    " INS ROY 0001301458   "DEL Jain_1500028
*  SORT sorted_req_tab BY operation.                          " INS ROY 0001301458
  SORT sorted_req_tab STABLE BY operation req_date.                                  "INS Jain_1500028

  IF NOT update IS INITIAL.                                    "Note ANK 1014628
    EXPORT update TO MEMORY ID 'UPD_FLG'.                      "Note ANK 1014628
  ENDIF.                                                       "Note ANK 1014628

  LOOP AT sorted_req_tab INTO wa_sorted_req WHERE operation IS NOT INITIAL.
    ASSIGN wa_sorted_req-request TO <request>.

*    index = index + 1.      "DEL Note 1802506 - do the checks first and increase afterwards.
*   Always try to post a request
    wa_command = c_req_post.
    PERFORM free_messages.

*Begin of note 1802506
* Checks if posting is possible:
*  ------------------- (Check 1) Payroll lock  -------------------------------
*Determine PA03 status
    PERFORM get_pa03_status
      USING
        <request>
      CHANGING
        lcl_pa03_status
        lcl_pa03_endda.

    IF lcl_pa03_status EQ 1 OR lcl_pa03_status EQ 4.
*     Processing payroll - infotype data cannot be changed and
*     request needs to be re-processed in a future posting run
*     if its begin date occurs on or before the last payroll day.
      lcl_header ?= <request>->workarea_version.
      CALL METHOD lcl_header->get_item_tab
        RECEIVING
          result = lt_itemtab.
*     Check request items if any occur in the payroll past
      CLEAR found.
      LOOP AT lt_itemtab INTO lcl_item_wa.
        lcl_item ?= lcl_item_wa-item.
        IF lcl_item->get_begda( ) LE lcl_pa03_endda.
          found = 'X'. EXIT.
        ENDIF.
      ENDLOOP.
      IF found EQ 'X'.
        DELETE sorted_req_tab.
        CONTINUE.
      ENDIF.
    ENDIF.

*  ------------------- (Check 2) Changed Request  -------------------------------
    PERFORM check_timestamp
      USING
        <request>
        CHANGING
        timestamp_match.

    IF timestamp_match NE c_true.
* Request have changed can not be processed
      DELETE sorted_req_tab.
      CONTINUE.
    ENDIF.

*  ------------------- (Check 3) Number of items  -------------------------------
*   Get item table for the request
    lcl_header ?= <request>->workarea_version.
    CALL METHOD lcl_header->get_item_tab
      RECEIVING
        result = lt_itemtab.
    IF lines( lt_itemtab ) GT 2.
*     Request has more than two items --> cannot be processed
      DELETE sorted_req_tab.
      CONTINUE.
    ENDIF.
* { goncharov 22.08.2017 10:54:42
    DATA: lv_dont_post_del TYPE flag.

    IF lines( lt_itemtab ) EQ 2.
      lv_dont_post_del = abap_true.
      EXPORT lv_dont_post_del FROM lv_dont_post_del TO MEMORY ID 'ZDONT_POST_DEL'.
    ELSE.
      FREE MEMORY ID 'ZDONT_POST_DEL'.
    ENDIF.
* } goncharov 22.08.2017 10:54:42

*  ------------------- (Check 4) Lock possible  ----------------------------------
    CALL FUNCTION 'ENQUEUE_EPTREQ'
      EXPORTING
        mode_ptreq_header = 'E'
        request_id        = <request>->request_id
        _scope            = '1'
      EXCEPTIONS
        foreign_lock      = 1
        system_failure    = 2
        OTHERS            = 3.

    IF sy-subrc NE 0.
*   Request cannot be locked --> no further processing
      DELETE sorted_req_tab.
      CONTINUE.
    ENDIF.
* --------------------------- End of Checks  -----------------------------------------
* Still in the loop => increase index and start posting
    index = index + 1.
*End of note 1802506

*   IF NOT matchit IS INITIAL AND wa_status EQ c_req_error.       "Note1124775
    IF NOT matchit IS INITIAL.                                    "Note1124775
*   Special processing for requests in status ERROR or APPROVED
*     CLEAR found.                                                "DEL Note1135709
      CLEAR: found_ins, found_del, modif_item_tab.                "INS Note1135709
*     Get item table for the request
      lcl_header ?= <request>->workarea_version.
      CALL METHOD lcl_header->get_item_tab
        RECEIVING
          result = lt_itemtab.
*     Process items of request until a first infotype match has been found
      LOOP AT lt_itemtab INTO lcl_item_wa.
        lcl_item ?= lcl_item_wa-item.
        wa_pernr = lcl_item->get_pernr( ).
        wa_subty = lcl_item->get_subty( ).
        wa_begda = lcl_item->get_begda( ).
        wa_endda = lcl_item->get_endda( ).
        wa_opera = lcl_item->get_operation( ).                    "INS Note1135709
        wa_stdaz = lcl_item->get_attabs_hours( ).                 "INS Note1135709
        wa_begtm = lcl_item->get_begin_time( ).
*       IF wa_begtm EQ '000000'. MOVE space6 TO wa_begtm. ENDIF.  "DEL Note1135709
        wa_endtm = lcl_item->get_end_time( ).
*       IF wa_endtm EQ '000000'. MOVE space6 TO wa_endtm. ENDIF.  "DEL Note1135709

        CASE lcl_item->get_infotype( ).
          WHEN '2001'.
            IF NOT wa_begtm IS INITIAL OR NOT wa_endtm IS INITIAL."INS Note1135709
              SELECT * FROM  pa2001
            INTO wa_p2001
                   WHERE  pernr EQ wa_pernr
                   AND    subty = wa_subty
                   AND    endda  = wa_endda
                   AND    begda  = wa_begda
                   AND    beguz  = wa_begtm
                   AND    enduz  = wa_endtm.
              ENDSELECT.
            ELSE.                                                 "INS Note1135709
              SELECT * FROM  pa2001                               "INS Note1135709
              INTO wa_p2001                                       "INS Note1135709
                     WHERE  pernr EQ wa_pernr                     "INS Note1135709
                     AND    subty = wa_subty                      "INS Note1135709
                     AND    endda  = wa_endda                     "INS Note1135709
                     AND    begda  = wa_begda                     "INS Note1135709
                     AND    stdaz  = wa_stdaz.                    "INS Note1135709
              ENDSELECT.                                          "INS Note1135709
            ENDIF.                                                "INS Note1135709
*           IF sy-subrc EQ 0. found = 'X'. EXIT. ENDIF.           "DEL Note1135709

          WHEN '2002'.
            IF NOT wa_begtm IS INITIAL OR NOT wa_endtm IS INITIAL."INS Note1135709
              SELECT * FROM  pa2002
              INTO wa_p2002
                     WHERE  pernr EQ wa_pernr
                     AND    subty = wa_subty
                     AND    endda  = wa_endda
                     AND    begda  = wa_begda
                     AND    beguz  = wa_begtm
                     AND    enduz  = wa_endtm.
              ENDSELECT.
            ELSE.                                                 "INS Note1135709
              SELECT * FROM  pa2002                               "INS Note1135709
              INTO wa_p2002                                       "INS Note1135709
                     WHERE  pernr EQ wa_pernr                     "INS Note1135709
                     AND    subty = wa_subty                      "INS Note1135709
                     AND    endda  = wa_endda                     "INS Note1135709
                     AND    begda  = wa_begda                     "INS Note1135709
                     AND    stdaz  = wa_stdaz.                    "INS Note1135709
              ENDSELECT.                                          "INS Note1135709
            ENDIF.                                                "INS Note1135709
*           IF sy-subrc EQ 0. found = 'X'. EXIT. ENDIF.           "DEL Note1135709

          WHEN OTHERS.

        ENDCASE.

        IF wa_opera EQ 'INS'.                                     "INS Note1135709
          ins_tabix = sy-tabix.                                   "INS Note1135709
          CHECK sy-dbcnt GT 0.                                    "INS Note1135709
          found_ins = 'X'.                                        "INS Note1135709
        ENDIF.                                                    "INS Note1135709
        IF wa_opera EQ 'DEL'.                                     "INS Note1135709
          del_tabix = sy-tabix.                                   "INS Note1135709
          CHECK sy-dbcnt GT 0.                                    "INS Note1135709
          found_del = 'X'.                                        "INS Note1135709
        ENDIF.                                                    "INS Note1135709

      ENDLOOP.

      CASE lines( lt_itemtab ).                                   "INS Note1135709
        WHEN 1.  "INSert or DELete operation                      "INS Note1135709
          IF wa_opera EQ 'INS' AND found_ins = 'X'.               "INS Note1135709
*           INSert operation and a matching infotype record       "INS Note1135709
*           exists --> do a manual POST                           "INS Note1135709
            wa_command = c_req_manual_post.                       "INS Note1135709
          ENDIF.                                                  "INS Note1135709
          IF wa_opera EQ 'DEL' AND found_del IS INITIAL.          "INS Note1135709
*           DELete operation and no matching infotype record      "INS Note1135709
*           --> do a manual POST                                  "INS Note1135709
            wa_command = c_req_manual_post.                       "INS Note1135709
          ENDIF.                                                  "INS Note1135709
        WHEN 2.  "MODify operation consist. of DELete and INSert  "INS Note1135709
          CALL METHOD lcl_header->get_item_list_object_tab        "INS Note1135709
            RECEIVING                                             "INS Note1135709
              result = item_obj_tab.                              "INS Note1135709
          IF found_ins = 'X'.                                     "INS Note1135709
            IF found_del = 'X'.                                   "INS Note1135709
*             INSert record already exists - remove it            "INS Note1135709
*             and POST remaining DELete record                    "INS Note1135709
*              DELETE lt_itemtab INDEX ins_tabix.                  "INS Note1135709    "DEL Jain_1357956
*              DELETE item_obj_tab INDEX ins_tabix.                "INS Note1135709    "DEL Jain_1357956
*              modif_item_tab = 'X'.                               "INS Note1135709    "DEL Jain_1357956
              wa_command = c_req_manual_post.                                            "INS Jain_1357956
            ELSE.                                                 "INS Note1135709
*             DELete record already removed - remove it           "INS Note1135709
*             and MAN_POST remaining INSert record                "INS Note1135709
              DELETE lt_itemtab INDEX del_tabix.                  "INS Note1135709
              DELETE item_obj_tab INDEX del_tabix.                "INS Note1135709
              wa_command = c_req_manual_post.                     "INS Note1135709
              modif_item_tab = 'X'.                               "INS Note1135709
            ENDIF.                                                "INS Note1135709
          ELSE.                                                   "INS Note1135709
            IF found_del = 'X'.                                   "INS Note1135709
*             Normal POST operation - nothing special to do       "INS Note1135709
            ELSE.                                                 "INS Note1135709
*             DELete record already removed - remove it           "INS Note1135709
*             and POST remaining INSert record                    "INS Note1135709
              DELETE lt_itemtab INDEX del_tabix.                  "INS Note1135709
              DELETE item_obj_tab INDEX del_tabix.                "INS Note1135709
              modif_item_tab = 'X'.                               "INS Note1135709
            ENDIF.                                                "INS Note1135709
          ENDIF.                                                  "INS Note1135709
      ENDCASE.                                                    "INS Note1135709

      IF modif_item_tab EQ 'X'.                                   "INS Note1135709
*       Update changed item table in request object               "INS Note1135709
*       There's always only one item left                         "INS Note1135709
        CALL METHOD lcl_header->set_number_of_items               "INS Note1135709
          EXPORTING                                               "INS Note1135709
            i_number_of_items = 1.                                "INS Note1135709
*       With only one item, the item list number must equal '1'   "INS Note1135709
        lcl_item_wa-item_list_no = 1.                             "INS Note1135709
        MODIFY lt_itemtab INDEX 1                                 "INS Note1135709
          FROM lcl_item_wa                                        "INS Note1135709
          TRANSPORTING item_list_no.                              "INS Note1135709
        CALL METHOD lcl_header->set_item_tab                      "INS Note1135709
          EXPORTING                                               "INS Note1135709
            i_item_tab = lt_itemtab.                              "INS Note1135709
        CALL METHOD lcl_header->set_item_list_object_tab          "INS Note1135709
          EXPORTING                                               "INS Note1135709
            i_item_list_object_tab = item_obj_tab.                "INS Note1135709
      ENDIF.                                                      "INS Note1135709

*     IF found EQ 'X'.                                            "DEL Note1135709
**      Matching infotype found --> do a manual POST              "DEL Note1135709
*       wa_command = c_req_manual_post.                           "DEL Note1135709
*     ENDIF.                                                      "DEL Note1135709

    ENDIF.


*** { goncharov 06.09.2017 17:35:47
***   для мероприятия переноса отпуска нужна вторая заявка
**    CLEAR: lt_itemtab_tmp.
**    UNASSIGN: <fs_req>.
**    IF lines( lt_itemtab ) EQ 2.
**      zcl_vacation_appl_renault=>get_requests_with_divided(
**        EXPORTING
**          i_req_id = <request>->request_id    " Ид. документа
**        IMPORTING
**          e_req   = DATA(lv_another_req) ).
**      IF lv_another_req IS NOT INITIAL.
**        LOOP AT sorted_req_tab ASSIGNING <fs_req>
**            WHERE request->request_id EQ lv_another_req.
**          CLEAR: <fs_req>-operation.
**          EXIT.
**        ENDLOOP.
**        IF sy-subrc NE 0. " не нашли связанную заявку!!
**          DELETE sorted_req_tab.
**          CONTINUE.
**        ENDIF.
**
**        CALL FUNCTION 'ENQUEUE_EPTREQ'
**          EXPORTING
**            mode_ptreq_header = 'E'
**            request_id        = <fs_req>-request->request_id
**            _scope            = '1'
**          EXCEPTIONS
**            foreign_lock      = 1
**            system_failure    = 2
**            OTHERS            = 3.
**
**        IF sy-subrc NE 0.
***       Request cannot be locked --> no further processing
**          DELETE sorted_req_tab.
**          CONTINUE.
**        ENDIF.
**
**        lcl_header ?= <fs_req>-request->workarea_version.
**        lt_itemtab_tmp = lcl_header->get_item_tab( ).
**      ENDIF.
**    ENDIF.
*** } goncharov 06.09.2017 17:35:47

    CLEAR: subrc.
    IF p_zz01 EQ 'X'.
      PERFORM zz_add_massn USING lt_itemtab lt_itemtab_tmp CHANGING subrc.
    ENDIF.
    IF subrc EQ 0.
*     Do either POST or manual POST
      CALL METHOD <request>->initiate_state_transition
        EXPORTING
          im_transfer_event = wa_command
          im_external_call  = abap_true "add Shibkova (ИГ не изменена, поэтому тут не нужно вставлять запись в 2001 ИТ)
        IMPORTING
          ex_ret_code       = subrc.

      IF <fs_req> IS ASSIGNED.
        CALL METHOD <fs_req>-request->initiate_state_transition
          EXPORTING
            im_transfer_event = wa_command
          IMPORTING
            ex_ret_code       = lv_subrc.
        IF lv_subrc = 0.
          success_count = success_count + 1.
        ELSE.
          error_count = error_count + 1.
        ENDIF.
*   Fill protocol
        IF protlist IS NOT INITIAL.
          PERFORM fill_prot_tab USING appid <fs_req>-request
                                CHANGING prot_tab error_count locked_count.
        ENDIF.

        IF lv_subrc > subrc.
          subrc = lv_subrc.
        ENDIF.
      ENDIF.
    ENDIF.

    IF subrc = 0.
      success_count = success_count + 1.
    ELSE.
      error_count = error_count + 1.
    ENDIF.

*   Fill protocol
    IF protlist IS NOT INITIAL.
      PERFORM fill_prot_tab USING appid <request>
                            CHANGING prot_tab error_count locked_count.
    ENDIF.

*   Update DB
    IF index >= max_upd.                                         "Note 1802506  change = to >= just to be sure
      IF NOT update IS INITIAL.
        ROLLBACK WORK.                                           "Note ANK 1014628
        index = 0.
      ELSE.
        TRY.
            COMMIT WORK AND WAIT.
            index = 0.
          CATCH cx_os_db_insert.
            ROLLBACK WORK.
            ASSERT 1 = 2.  "DUMP
        ENDTRY.
      ENDIF.
    ENDIF.

**   Dequeue request object                                          "DEL Note 999620
*    CALL FUNCTION 'DEQUEUE_EPTREQ'                                  "DEL Note 999620
*      EXPORTING                                                     "DEL Note 999620
*        request_id = <request>->request_id.                         "DEL Note 999620

  ENDLOOP.

  FREE MEMORY ID 'UPD_FLG'.                                    "Note ANK 1014628

  IF index < max_upd AND index NE 0.
    IF NOT update IS INITIAL.
      ROLLBACK WORK.
      index = 0.
    ELSE.
      TRY.
*         COMMIT WORK.                                              "Note1470672
          COMMIT WORK AND WAIT.                                     "Note1470672
          index = 0.
        CATCH cx_os_db_insert.
          ROLLBACK WORK.
          ASSERT 1 = 2.  "DUMP
      ENDTRY.
    ENDIF.
  ENDIF.

  LOOP AT sorted_req_tab INTO wa_sorted_req.                         "INS Note 999620
    ASSIGN wa_sorted_req-request TO <request>.                       "INS Note 999620
*   Dequeue request object                                           "INS Note 999620
    CALL FUNCTION 'DEQUEUE_EPTREQ'                                   "INS Note 999620
      EXPORTING                                                      "INS Note 999620
        request_id = <request>->request_id                           "INS Note 999620
        _scope     = '1'.                                            "Note1470672
  ENDLOOP.                                                           "INS Note 999620

* convert from ui to fieldcat-logic
  IF showguid = 'X'.
    noout = ' '.
  ELSE.
    noout = 'X'.
    show_guid = space.
  ENDIF.

ENDFORM.                    " process_all_requests

FORM zz_add_massn USING it_itemtab    TYPE ptreq_items_tab
                        it_itemtab_2  TYPE ptreq_items_tab
                  CHANGING cv_subrc TYPE sy-subrc.
  DEFINE add_val.
    APPEND INITIAL LINE TO proposed_values ASSIGNING <proposed_values>.
    <proposed_values>-infty = &1.
    <proposed_values>-fname = &2.
    <proposed_values>-fval  = &3.
  END-OF-DEFINITION.

  TYPES:
    BEGIN OF ty_pernr,
      pernr TYPE pernr_d,
      massn TYPE p0298-massn,
      massg TYPE p0298-massg,
      actio TYPE pspar-actio,
      infty TYPE pspar-infty,
      subty TYPE pspar-subty,
      objps TYPE p0298-objps,
      begda TYPE p0298-begda,
      endda TYPE p0298-endda,
    END OF ty_pernr.

  TYPES:
    BEGIN OF ty_massn_data,
      subty     TYPE subty,
      edq_begda TYPE begda,
      edq_endda TYPE endda,
      begda     TYPE begda,  " первый период
      endda     TYPE endda,
      begda_2   TYPE begda,  " второй период
      endda_2   TYPE endda,
    END OF ty_massn_data .

  DATA:
        ls_zthr_awart_massn  TYPE zthr_awart_massn
      , ls_pernr             TYPE ty_pernr
      , return               TYPE bapireturn
      , return1              TYPE bapireturn1
      , hr_return            TYPE hrhrmm_msg
      , proposed_values      TYPE TABLE OF pprop

      , dialog_mode          VALUE '0'

      , lo_item              TYPE REF TO cl_pt_req_attabsdata
      , ex_attribs_tab       TYPE name2value_table

      , ls_ret               TYPE bapireturn1
      , ls_p0000             TYPE p0000
      , ls_p0302             TYPE p0302
      , ls_pa0302            TYPE pa0302
      , lv_begda             TYPE datum
      , lv_endda             TYPE datum
      , lv_change_infogr     TYPE flag VALUE abap_true
      , lv_opera             TYPE string
      , lv_tmp             TYPE string
      , lv_ins               TYPE flag
      , lv_del               TYPE flag

      , lt_p2001_old         TYPE TABLE OF p2001
      , lv_actio             TYPE pspar-actio

      , lv_old_begda          TYPE begda
      , lv_old_endda          TYPE begda

      , lv_begda_2            TYPE begda
      , lv_endda_2            TYPE endda

      , ls_massn_data   TYPE ty_massn_data
      .

  FIELD-SYMBOLS: <proposed_values>  LIKE LINE OF proposed_values
               .

  LOOP AT it_itemtab ASSIGNING FIELD-SYMBOL(<t_itemtab>).
    lo_item ?= <t_itemtab>-item.
    lv_tmp = lo_item->get_operation( ).
    CASE lv_tmp.
      WHEN 'INS'.
        lv_ins = abap_true.
        lv_begda = lo_item->get_begda( ).
        lv_endda = lo_item->get_endda( ).
      WHEN 'DEL'.
        lv_del = abap_true.
        ls_pernr-begda = lo_item->get_begda( ).
        ls_pernr-endda = lo_item->get_endda( ).
    ENDCASE.
    ls_pernr-infty = lo_item->get_infotype( ).
    ls_pernr-pernr = lo_item->get_pernr( ).
    ls_pernr-subty = lo_item->get_subty( ).
  ENDLOOP.

  CHECK ls_pernr-infty EQ '2001'.

  IF lv_ins EQ abap_true AND
    lv_del EQ abap_true.
    lv_opera = 'MOD'.
  ELSEIF lv_ins EQ abap_true.
    lv_opera = 'INS'.
    ls_pernr-begda = lv_begda.
    ls_pernr-endda = lv_endda.
  ELSE.
    lv_opera = 'DEL'.
  ENDIF.

  LOOP AT it_itemtab_2 ASSIGNING <t_itemtab>.
    lo_item ?= <t_itemtab>-item.
    lv_tmp = lo_item->get_operation( ).
    CASE lv_tmp.
      WHEN 'INS'.
        lv_begda_2 = lo_item->get_begda( ).
        lv_endda_2 = lo_item->get_endda( ).
      WHEN 'DEL'.
        lv_old_begda = lo_item->get_begda( ).
        lv_old_endda = lo_item->get_endda( ).
    ENDCASE.
  ENDLOOP.

* при переносе у них должна быть общая запись для удаления, иначе ошибка(((
  IF lv_opera EQ 'MOD' AND
    ( lv_begda_2 IS NOT INITIAL AND (
        lv_old_begda NE ls_pernr-begda OR
          lv_old_endda NE ls_pernr-endda ) ).
    cv_subrc = 4.
    RETURN.
  ENDIF.


  IF lv_opera NE 'DEL'.
    SELECT SINGLE * FROM zthr_awart_massn
    INTO ls_zthr_awart_massn
    WHERE awart EQ ls_pernr-subty AND
          opera EQ lv_opera.

    CHECK sy-subrc EQ 0.

    ls_pernr-massn = ls_zthr_awart_massn-massn.
    ls_pernr-massg = ls_zthr_awart_massn-massg.
  ENDIF.

  IF lv_opera EQ 'MOD' OR
      lv_opera EQ 'DEL'.
    CALL FUNCTION 'HR_EMPLOYEE_ENQUEUE'
      EXPORTING
        number = ls_pernr-pernr
      IMPORTING
        return = return1.
    IF return1-type EQ 'E' OR
          return1-type EQ 'A'.
      cv_subrc = 4.
      RETURN.
    ENDIF.

*   нужно заблокировать старую запись
    CALL FUNCTION 'HR_READ_SUBTYPE'
      EXPORTING
        pernr           = ls_pernr-pernr
        infty           = '2001'
        subty           = ls_pernr-subty
        sprps           = abap_false
        begda           = ls_pernr-begda
        endda           = ls_pernr-endda
        no_auth_check   = 'X'
      TABLES
        infty_tab       = lt_p2001_old
      EXCEPTIONS
        infty_not_found = 1
        invalid_input   = 2
        OTHERS          = 3.
    READ TABLE lt_p2001_old INDEX 1 ASSIGNING FIELD-SYMBOL(<fs_p2001>).
    IF sy-subrc EQ 0.
      IF p_nodel EQ abap_true.
        lv_actio = 'EDQ'.
      ELSE.
        lv_actio = 'DEL'.
      ENDIF.

      CALL FUNCTION 'HR_INFOTYPE_OPERATION'
        EXPORTING
          infty         = <fs_p2001>-infty
          number        = <fs_p2001>-pernr
          subtype       = <fs_p2001>-subty
          objectid      = <fs_p2001>-objps
          lockindicator = <fs_p2001>-sprps
          validityend   = <fs_p2001>-endda
          validitybegin = <fs_p2001>-begda
          recordnumber  = <fs_p2001>-seqnr
          record        = <fs_p2001>
          operation     = lv_actio
          dialog_mode   = dialog_mode
          nocommit      = abap_true
*         VIEW_IDENTIFIER        =
*         SECONDARY_RECORD       =
        IMPORTING
          return        = return1.

      CALL FUNCTION 'HR_EMPLOYEE_DEQUEUE'
        EXPORTING
          number = <fs_p2001>-pernr.

      IF return1-type EQ 'E' OR
          return1-type EQ 'A'.
        cv_subrc = 4.
        RETURN.
      ENDIF.
    ENDIF.

    IF lv_opera EQ 'DEL'. " мероприятие проводить не нужно
      RETURN.
    ENDIF.
  ENDIF.

  add_val '0000' 'P0000-MASSN' ls_pernr-massn.
  add_val '0000' 'P0000-MASSG' ls_pernr-massg.
  add_val '0000' 'P0000-PERNR' ls_pernr-pernr.
  add_val '0000' 'P0000-BEGDA' ls_pernr-begda.

  IF lv_opera EQ 'MOD'.
    ls_massn_data-edq_begda = ls_pernr-begda.
    ls_massn_data-edq_endda = ls_pernr-endda.
    ls_massn_data-subty     = ls_pernr-subty.

    ls_massn_data-begda   = lv_begda.
    ls_massn_data-endda   = lv_endda.

    ls_massn_data-begda_2 = lv_begda_2.
    ls_massn_data-endda_2 = lv_endda_2.
  ENDIF.

  add_val '0298' 'P0298-MASSN' ls_pernr-massn.
  add_val '0298' 'P0298-MASSG' ls_pernr-massg.
  add_val '0298' 'P0298-BEGDA' ls_pernr-begda.
  add_val '0298' 'P0298-ORDDT' ls_pernr-begda.

**<<< в проде нет изменения ИГ, так чт добавим данные о 2001 ИТ

  add_val '2001' 'P2001-BEGDA' ls_pernr-begda.
  add_val '2001' 'P2001-ENDDA' ls_pernr-endda.
  add_val '2001' 'P2001-SUBTY' ls_pernr-subty.
  add_val '2001' 'P2001-AWART' ls_pernr-subty.

  IF ls_pernr-massn EQ 'RR'.
    add_val '0298' 'P0298-SUBTY' space.
    add_val '0298' 'P0298-MASSS' space.
  ENDIF.


*  EXPORT lv_change_infogr FROM lv_change_infogr TO MEMORY ID 'ZCHANGE_INFOGR'.
*  EXPORT ls_massn_data FROM ls_massn_data TO MEMORY ID zcl_vacation_appl_renault=>c_memory_rr_id.
  CALL FUNCTION 'HR_MAINTAIN_MASTERDATA'
    EXPORTING
      pernr           = ls_pernr-pernr
      massn           = ls_pernr-massn
      begda           = ls_pernr-begda
      actio           = 'INS'
      dialog_mode     = dialog_mode " '2'
      luw_mode        = '0'  " без commit и rollback, выполнится ниже
    IMPORTING
      return          = return
      return1         = return1
      hr_return       = hr_return
    TABLES
      proposed_values = proposed_values.

*  FREE MEMORY ID zcl_vacation_appl_renault=>c_memory_rr_id.
*  FREE MEMORY ID 'ZCHANGE_INFOGR'.
  IF return1 IS INITIAL  OR
   ( return1-type NE 'E' AND
     return1-type NE 'A' ).
    CLEAR: cv_subrc.
  ELSE.
    cv_subrc = 4.
  ENDIF.
ENDFORM.

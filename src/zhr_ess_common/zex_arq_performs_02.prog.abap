*&---------------------------------------------------------------------*
*&  Include           ZEX_ARQ_PERFORMS_02
*&---------------------------------------------------------------------*
FORM zz_check_application_status
  USING
    pernr   TYPE p_pernr
    modus   TYPE pt_arq_mode.
  CLASS:
    cl_pt_arq_application DEFINITION LOAD.

  DATA:
    application        TYPE REF TO cl_pt_arq_application,
    app_modus          TYPE pt_arq_mode,
    app_pernr          TYPE p_pernr,
    initiator          TYPE REF TO if_pt_req_a_wf,
    initiator_pernr    TYPE pernr_us_tab,                   "LAK1830604
    wa_initiator_pernr TYPE pernr_us.                       "LAK1830604
  IF g_app_running = c_false.
*   Start the application
*    CALL METHOD cl_os_system=>init_and_set_modes  "DEL NOTE 826551
*      EXPORTING                                   "DEL NOTE 826551
*        i_external_commit = oscon_false           "DEL NOTE 826551
*        i_update_mode     = oscon_dmode_default.  "DEL NOTE 826551

*   required to make sure, the actors are written to the database
*   PERFORM transaction_start.                     "DEL NOTE 826551
    PERFORM free_messages.
*   get reference to the application
    application = cl_pt_arq_application=>get_instance( ).
**********************Begin of LAK1830604****************
    CALL FUNCTION 'HR_GET_EMPLOYEES_FROM_USER'
      EXPORTING
        user              = sy-uname
        iv_with_authority = ' '               "LAK1938465
      TABLES
        ee_tab            = initiator_pernr.
*     READ TABLE initiator_pernr INTO wa_initiator_pernr INDEX 1.
*Begin of changes: HEGDEPR 2030538
*Check for an active PERNR
    LOOP AT initiator_pernr INTO wa_initiator_pernr.
      CALL FUNCTION 'RP_CHECK_PERNR'
        EXPORTING
          beg               = sy-datum
          pnr               = wa_initiator_pernr-pernr
        EXCEPTIONS
          data_fault        = 1
          person_not_active = 2
          person_unknown    = 3
          exit_fault        = 4
          pernr_missing     = 5
          date_missing      = 6
          OTHERS            = 7.
      IF sy-subrc EQ 0.
        EXIT.
      ELSE.
        CONTINUE.
      ENDIF.
    ENDLOOP.
*End of changes: HEGDEPR 2030538
**********************End of LAK1830604***********************
    CALL METHOD application->initialize
      EXPORTING
        im_modus           = modus
*       im_initiator_objid = pernr.                      "LAK1830604
        im_initiator_objid = wa_initiator_pernr-pernr     "LAK1830604
        im_owner_objid     = pernr.                      "LAK1830604


    g_app_running = c_true.
  ELSE.
*   Get application initiator pernr
*   get reference to the application
    application = cl_pt_arq_application=>get_instance( ).
    CALL METHOD application->if_pt_req_application~get_initiator
      IMPORTING
        ex_initiator = initiator.

*    app_pernr = initiator->pernr.                                  "RAG2335741

    CALL METHOD application->if_pt_req_application~get_modus
      IMPORTING
        ex_modus = app_modus.

    IF app_modus NE 'A' AND initiator IS BOUND.
      app_pernr = initiator->pernr.                         "RAG2335741
    ENDIF.                                                  "RAG2335741

*******************************"LAK1830604***********************************
*    IF app_pernr = pernr AND app_modus = modus.
**   Application is Consistent
*    ELSE.
**   Application is NOT Consistent, then DUMP
*      MESSAGE x999(53).  "#EC *
*    ENDIF.
*     IF modus = 'A' AND app_pernr <> pernr.                  "LAK1865665
**   Application is NOT Consistent, then DUMP                 "LAK1865665
*      MESSAGE x999(53).  "#EC *                              "LAK1865665
*     ENDIF.                                                  "LAK1865665
*******************************"LAK1830604***********************************
  ENDIF.


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""$"$\SE:(5) FORM CHECK_APPLICATION_STATUS, Выход                                                                                                              A
ENDFORM.                    " check_application_status

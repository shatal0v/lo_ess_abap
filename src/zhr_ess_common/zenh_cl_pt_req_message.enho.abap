CLASS lcl_zenh_cl_pt_req_message DEFINITION DEFERRED.
CLASS cl_pt_req_message_handler DEFINITION LOCAL FRIENDS lcl_zenh_cl_pt_req_message.
CLASS lcl_zenh_cl_pt_req_message DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA obj TYPE REF TO lcl_zenh_cl_pt_req_message.  "#EC NEEDED
    DATA core_object TYPE REF TO cl_pt_req_message_handler . "#EC NEEDED
 INTERFACES  IOW_ZENH_CL_PT_REQ_MESSAGE.
    METHODS:
      constructor IMPORTING core_object
                              TYPE REF TO cl_pt_req_message_handler OPTIONAL.
ENDCLASS.
CLASS lcl_zenh_cl_pt_req_message IMPLEMENTATION.
  METHOD constructor.
    me->core_object = core_object.
  ENDMETHOD.

  METHOD iow_zenh_cl_pt_req_message~get_messages.
*"------------------------------------------------------------------------*
*" Declaration of Overwrite-method, do not insert any comments here please!
*"
*"methods GET_MESSAGES
*"  importing
*"    !IM_TRANSFORM_MSG type XFELD
*"    !IM_PERNR type PERNR_D optional
*"    !IM_APPID type PT_APPLICATION_ID
*"  exporting
*"    !EX_MESSAGE_TAB type PTREQ_MESSAGE_TAB
*"  exceptions
*"    WEBMO_MISSING
*"    PERNR_MISSING .
*"------------------------------------------------------------------------*
*
* Get all messages and transform them as an option.
*

    DATA: l_message     TYPE bapiret2,
          message_wa    TYPE ptreq_message_struc,
          lcl_initiator TYPE REF TO if_pt_req_a_wf,
          lcl_pernr     TYPE pernr_d,
          lcl_webmo     TYPE webmo.

    DATA:
      exit_gen          TYPE REF TO pt_gen_req.

    DATA: app_modus    TYPE pt_arq_mode.                    "RAG2335741

    ex_message_tab = core_object->message_tab.

*---Transform any messages?
    CHECK im_transform_msg IS NOT INITIAL AND ex_message_tab IS NOT INITIAL AND sy-batch NE 'X'. "RAOSHRU0 2124062

*---Determine pernr to transform messages
    IF im_pernr IS INITIAL.

      CALL METHOD core_object->application->get_initiator
        IMPORTING
          ex_initiator = lcl_initiator.

*---> Start of RAG2335741
      CALL METHOD core_object->application->if_pt_req_application~get_modus
        IMPORTING
          ex_modus = app_modus.

      IF app_modus NE 'A'.
        IF lcl_initiator IS BOUND.
          lcl_pernr = lcl_initiator->pernr.
        ENDIF.
        IF lcl_pernr IS INITIAL.
*  ---ERROR: PERNR not available --> f.e. Admin --> ok
          RAISE pernr_missing.
        ENDIF.
      ENDIF.
*<--- End of RAG2335741
    ELSE.
      lcl_pernr = im_pernr.
    ENDIF.

    CALL METHOD cl_pt_req_customizing=>get_cust_messages
      EXPORTING
        im_pernr            = lcl_pernr
        im_appid            = im_appid
      CHANGING
        ch_message_tab      = ex_message_tab
      EXCEPTIONS
        it0001_not_found    = 1
        it0008_not_found    = 2
        missing_customizing = 3
        OTHERS              = 4.
    IF sy-subrc <> 0.
*   do nothing, wrong customizing
    ENDIF.

* call exit and read customizing
    exit_gen = core_object->application->functional_exit_gen.

* <Customer Exit> Filter Next Processor Tab

    CALL BADI exit_gen->modify_application_messages
      EXPORTING
        im_appid       = im_appid
      CHANGING
        ch_message_tab = ex_message_tab.



  ENDMETHOD.
ENDCLASS.

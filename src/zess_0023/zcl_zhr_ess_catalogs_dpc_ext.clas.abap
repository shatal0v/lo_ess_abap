class ZCL_ZHR_ESS_CATALOGS_DPC_EXT definition
  public
  inheriting from ZCL_ZHR_ESS_CATALOGS_DPC
  create public .

public section.
protected section.

  methods ADDEDUS_GET_ENTITYSET
    redefinition .
  methods DLINES_GET_ENTITYSET
    redefinition .
  methods EDUFORMS_GET_ENTITYSET
    redefinition .
  methods EFFECTS_GET_ENTITYSET
    redefinition .
  methods GOALS_GET_ENTITYSET
    redefinition .
  methods REASONEDUSET_GET_ENTITYSET
    redefinition .
  methods TIMEUNITS_GET_ENTITYSET
    redefinition .
  methods EDUPROGSET_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZHR_ESS_CATALOGS_DPC_EXT IMPLEMENTATION.


  method ADDEDUS_GET_ENTITYSET.
**TRY.
*CALL METHOD SUPER->ADDEDUS_GET_ENTITYSET
*  EXPORTING
*    IV_ENTITY_NAME           =
*    IV_ENTITY_SET_NAME       =
*    IV_SOURCE_NAME           =
*    IT_FILTER_SELECT_OPTIONS =
*    IS_PAGING                =
*    IT_KEY_TAB               =
*    IT_NAVIGATION_PATH       =
*    IT_ORDER                 =
*    IV_FILTER_STRING         =
*    IV_SEARCH_STRING         =
**    io_tech_request_context  =
**  IMPORTING
**    et_entityset             =
**    es_response_context      =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.

 DATA:
       lt_aedu TYPE TABLE OF zess_0023_t_aedu.
    FIELD-SYMBOLS:
      <ls_aedu>      LIKE LINE OF lt_aedu,
      <ls_entityset> LIKE LINE OF et_entityset.

    SELECT * INTO TABLE lt_aedu FROM zess_0023_t_aedu.
    LOOP AT  lt_aedu ASSIGNING <ls_aedu>.
      APPEND INITIAL LINE TO et_entityset ASSIGNING <ls_entityset>.
      <ls_entityset>-addeduid =  <ls_aedu>-addeduid.
      <ls_entityset>-addedu_text = <ls_aedu>-addedu_text.
    ENDLOOP.



  endmethod.


  method DLINES_GET_ENTITYSET.
**TRY.
*CALL METHOD SUPER->DLINES_GET_ENTITYSET
*  EXPORTING
*    IV_ENTITY_NAME           =
*    IV_ENTITY_SET_NAME       =
*    IV_SOURCE_NAME           =
*    IT_FILTER_SELECT_OPTIONS =
*    IS_PAGING                =
*    IT_KEY_TAB               =
*    IT_NAVIGATION_PATH       =
*    IT_ORDER                 =
*    IV_FILTER_STRING         =
*    IV_SEARCH_STRING         =
**    io_tech_request_context  =
**  IMPORTING
**    et_entityset             =
**    es_response_context      =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.

 DATA:
           lt_dline TYPE TABLE OF ZTHR_DLINET.
    FIELD-SYMBOLS:
      <ls_dline>   LIKE LINE OF lt_dline,
      <ls_entityset> LIKE LINE OF et_entityset.

    SELECT * INTO TABLE lt_dline
      FROM ZTHR_DLINET
      WHERE SPRAS EQ 'RU'.

    LOOP AT lt_dline ASSIGNING <ls_dline>.
      APPEND INITIAL LINE TO et_entityset ASSIGNING <ls_entityset>.
      <ls_entityset>-dline =  <ls_dline>-dline.
      <ls_entityset>-stext = <ls_dline>-stext.
    ENDLOOP.

  endmethod.


  METHOD eduforms_get_entityset.
**TRY.
*CALL METHOD SUPER->EDUFORMS_GET_ENTITYSET
*  EXPORTING
*    IV_ENTITY_NAME           =
*    IV_ENTITY_SET_NAME       =
*    IV_SOURCE_NAME           =
*    IT_FILTER_SELECT_OPTIONS =
*    IS_PAGING                =
*    IT_KEY_TAB               =
*    IT_NAVIGATION_PATH       =
*    IT_ORDER                 =
*    IV_FILTER_STRING         =
*    IV_SEARCH_STRING         =
**    io_tech_request_context  =
**  IMPORTING
**    et_entityset             =
**    es_response_context      =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.

    DATA:
           lt_eduform TYPE TABLE OF t7ruokin.
    FIELD-SYMBOLS:
      <ls_eduform>   LIKE LINE OF lt_eduform,
      <ls_entityset> LIKE LINE OF et_entityset.

    SELECT * INTO TABLE lt_eduform
      FROM t7ruokin
      WHERE molga EQ 33 AND facet = 33.

    LOOP AT lt_eduform ASSIGNING <ls_eduform>.
      APPEND INITIAL LINE TO et_entityset ASSIGNING <ls_entityset>.
      <ls_entityset>-ccode =  <ls_eduform>-ccode.
      <ls_entityset>-cname = <ls_eduform>-cname.
    ENDLOOP.

  ENDMETHOD.


  METHOD eduprogset_get_entityset.
**TRY.
*CALL METHOD SUPER->EDUPROGSET_GET_ENTITYSET
*  EXPORTING
*    IV_ENTITY_NAME           =
*    IV_ENTITY_SET_NAME       =
*    IV_SOURCE_NAME           =
*    IT_FILTER_SELECT_OPTIONS =
*    IS_PAGING                =
*    IT_KEY_TAB               =
*    IT_NAVIGATION_PATH       =
*    IT_ORDER                 =
*    IV_FILTER_STRING         =
*    IV_SEARCH_STRING         =
**    io_tech_request_context  =
**  IMPORTING
**    et_entityset             =
**    es_response_context      =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.



    DATA:  lt_prog TYPE TABLE OF zthr_edu_prograt .
    FIELD-SYMBOLS:
      <ls_prog>      LIKE LINE OF lt_prog,
      <ls_entityset> LIKE LINE OF et_entityset.

    SELECT zt~id zt~name INTO CORRESPONDING FIELDS OF TABLE lt_prog
      FROM zthr_edu_prograt AS zt
      INNER JOIN zthr_edu_program AS zm ON zt~id = zm~id
     WHERE zt~lang = sy-langu
       AND ( zm~is_old IS NULL OR zm~is_old = ' ').

    IF sy-subrc = 0.

      LOOP AT  lt_prog ASSIGNING <ls_prog>.
        APPEND INITIAL LINE TO et_entityset ASSIGNING <ls_entityset>.
        <ls_entityset>-eduprogid =  <ls_prog>-id.
        <ls_entityset>-eduprogtxt = <ls_prog>-name.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD effects_get_entityset.
**TRY.
*CALL METHOD SUPER->EFFECTS_GET_ENTITYSET
*  EXPORTING
*    IV_ENTITY_NAME           =
*    IV_ENTITY_SET_NAME       =
*    IV_SOURCE_NAME           =
*    IT_FILTER_SELECT_OPTIONS =
*    IS_PAGING                =
*    IT_KEY_TAB               =
*    IT_NAVIGATION_PATH       =
*    IT_ORDER                 =
*    IV_FILTER_STRING         =
*    IV_SEARCH_STRING         =
**    io_tech_request_context  =
**  IMPORTING
**    et_entityset             =
**    es_response_context      =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.
    DATA:
       lt_efct TYPE TABLE OF zess_0023_t_efct.
    FIELD-SYMBOLS:
      <ls_efct>      LIKE LINE OF lt_efct,
      <ls_entityset> LIKE LINE OF et_entityset.

    SELECT * INTO TABLE lt_efct FROM zess_0023_t_efct.
    LOOP AT  lt_efct ASSIGNING <ls_efct>.
      APPEND INITIAL LINE TO et_entityset ASSIGNING <ls_entityset>.
      <ls_entityset>-efctid =  <ls_efct>-efctid.
      <ls_entityset>-efct_text = <ls_efct>-efct_text.
    ENDLOOP.
  ENDMETHOD.


  method GOALS_GET_ENTITYSET.
**TRY.
*CALL METHOD SUPER->GOALS_GET_ENTITYSET
*  EXPORTING
*    IV_ENTITY_NAME           =
*    IV_ENTITY_SET_NAME       =
*    IV_SOURCE_NAME           =
*    IT_FILTER_SELECT_OPTIONS =
*    IS_PAGING                =
*    IT_KEY_TAB               =
*    IT_NAVIGATION_PATH       =
*    IT_ORDER                 =
*    IV_FILTER_STRING         =
*    IV_SEARCH_STRING         =
**    io_tech_request_context  =
**  IMPORTING
**    et_entityset             =
**    es_response_context      =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.
DATA:
       lt_goal TYPE TABLE OF zess_0023_t_goal.
    FIELD-SYMBOLS:
      <ls_goal>      LIKE LINE OF lt_goal,
      <ls_entityset> LIKE LINE OF et_entityset.

    SELECT * INTO TABLE lt_goal FROM zess_0023_t_goal.
    LOOP AT  lt_goal ASSIGNING <ls_goal>.
      APPEND INITIAL LINE TO et_entityset ASSIGNING <ls_entityset>.
      <ls_entityset>-goalid =  <ls_goal>-goalid.
      <ls_entityset>-subty =  <ls_goal>-subty.
      <ls_entityset>-goal_text = <ls_goal>-goal_text.
    ENDLOOP.

  endmethod.


  METHOD reasoneduset_get_entityset.
**TRY.
*CALL METHOD SUPER->REASONEDUSET_GET_ENTITYSET
*  EXPORTING
*    IV_ENTITY_NAME           =
*    IV_ENTITY_SET_NAME       =
*    IV_SOURCE_NAME           =
*    IT_FILTER_SELECT_OPTIONS =
*    IS_PAGING                =
*    IT_KEY_TAB               =
*    IT_NAVIGATION_PATH       =
*    IT_ORDER                 =
*    IV_FILTER_STRING         =
*    IV_SEARCH_STRING         =
**    io_tech_request_context  =
**  IMPORTING
**    et_entityset             =
**    es_response_context      =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.


    DATA:
        lt_reas TYPE TABLE OF zess_t_reas_txt .
    FIELD-SYMBOLS:
      <ls_reas>      LIKE LINE OF lt_reas,
      <ls_entityset> LIKE LINE OF et_entityset.

    SELECT * INTO TABLE lt_reas FROM zess_t_reas_txt
      WHERE lang = sy-langu.
    IF sy-subrc = 0.
      LOOP AT  lt_reas ASSIGNING <ls_reas>.
        APPEND INITIAL LINE TO et_entityset ASSIGNING <ls_entityset>.
        <ls_entityset>-reasonid =  <ls_reas>-reason_id.
        <ls_entityset>-reasontxt = <ls_reas>-text.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD timeunits_get_entityset.
**TRY.
*CALL METHOD SUPER->TIMEUNITS_GET_ENTITYSET
*  EXPORTING
*    IV_ENTITY_NAME           =
*    IV_ENTITY_SET_NAME       =
*    IV_SOURCE_NAME           =
*    IT_FILTER_SELECT_OPTIONS =
*    IS_PAGING                =
*    IT_KEY_TAB               =
*    IT_NAVIGATION_PATH       =
*    IT_ORDER                 =
*    IV_FILTER_STRING         =
*    IV_SEARCH_STRING         =
**    io_tech_request_context  =
**  IMPORTING
**    et_entityset             =
**    es_response_context      =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.

    DATA:
           lt_timeunit TYPE TABLE OF t538t.
    FIELD-SYMBOLS:
      <ls_timeunit>  LIKE LINE OF lt_timeunit,
      <ls_entityset> LIKE LINE OF et_entityset.

    SELECT * INTO TABLE lt_timeunit
      UP TO 5 ROWS
      FROM t538t
      WHERE sprsl EQ 'RU'
        AND zeinh EQ '001'.

    LOOP AT lt_timeunit ASSIGNING <ls_timeunit>.
      APPEND INITIAL LINE TO et_entityset ASSIGNING <ls_entityset>.
      <ls_entityset>-zeinh =  <ls_timeunit>-zeinh.
      <ls_entityset>-etext = <ls_timeunit>-etext.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.

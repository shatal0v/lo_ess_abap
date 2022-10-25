class ZESS_0023_CL_HRUTILS definition
  public
  final
  create public .

public section.

  class-methods GET_ORGEH
    importing
      !_IV_PERNR type PERNR_D
      !_IV_PLVAR type PLVAR default '01'
      !_IV_DATE type DATUM default SY-DATUM
    exporting
      !_EV_ORGEH type ANY
      !_EV_DESCR type ANY
    exceptions
      NOT_FOUND
      NOT_FOUND_TEXT .
  class-methods GET_OBJEC_TEXT
    importing
      !IM_PLVAR type PLVAR default '01'
      !IM_OTYPE type OTYPE
      !IM_OBJID type PD_OBJID_R
      !IM_ISTAT type ISTAT_D default '1'
      !IM_DATE type SY-DATUM default SY-DATUM
      !IM_SUBTY type SUBTY default '0001'
      !IM_NO_1000 type FLAG default ABAP_FALSE
      !IM_STEXT_ONLY type FLAG default ABAP_FALSE
      value(NO_AUTH) type FLAG default ' '
      !NO_CONDENSE type FLAG default ABAP_FALSE
    exporting
      !EX_DESCRIPTION type ANY
      !EX_SHORT type ANY
      !EX_STEXT type ANY
    exceptions
      NOT_FOUND .
  class-methods GET_RELATION
    importing
      !IM_PLVAR type PLVAR default '01'
      !IM_OTYPE type OTYPE
      !IM_OBJID type ANY
      !IM_RELATION type SUBTY optional
      !IM_OTYPE_REL type OTYPE optional
      !IM_BEGDA type BEGDA optional
      !IM_ENDDA type ENDDA optional
    exporting
      !EX_OTYPE type OTYPE
      !EX_OBJID type ANY
      !ET_OBJEC type OBJEC_T
      !ES_1001 type P1001
      !ET_1001 type P1001_T
      !EX_BEGDA_OBJ type DATS
      !EX_ENDDA_OBJ type DATS .
protected section.
private section.
ENDCLASS.



CLASS ZESS_0023_CL_HRUTILS IMPLEMENTATION.


  method GET_OBJEC_TEXT.
    DATA:
    lf_stext_only TYPE flag,
    lt_text TYPE TABLE OF pt1002,
    lt_1000 TYPE TABLE OF p1000.
  FIELD-SYMBOLS: <fs_1000_line> LIKE LINE OF lt_1000,
                 <fs_1002_line> LIKE LINE OF lt_text.

  CLEAR:
    ex_description.

  IF im_subty EQ 'STXT'.
    lf_stext_only = abap_true.
  ELSE.
    lf_stext_only = im_stext_only.
  ENDIF.

  IF lf_stext_only = abap_false.

    IF no_auth = abap_true.
      CALL FUNCTION 'RH_AUTHORITY_CHECK_OFF'.
    ENDIF.

    CALL FUNCTION 'RH_OBJECT_DESCRIPTION_READ'
      EXPORTING
        plvar    = im_plvar
        otype    = im_otype
        objid    = im_objid
        sttag    = im_date
        subty    = im_subty
      TABLES
        ptxt1002 = lt_text
      EXCEPTIONS
        OTHERS   = 1.

    IF no_auth = abap_true.
      CALL FUNCTION 'RH_AUTHORITY_CHECK_ON'.
    ENDIF.

    IF lt_text[] IS NOT INITIAL.
      LOOP AT lt_text ASSIGNING <fs_1002_line>.
        CONCATENATE ex_description <fs_1002_line>-tline INTO ex_description SEPARATED BY space.
      ENDLOOP.
      CONDENSE: ex_description.
      IF ex_short IS NOT SUPPLIED AND ex_stext IS NOT SUPPLIED.
        RETURN.
      ENDIF.
    ELSEIF im_no_1000 = abap_true.
      RAISE not_found.
    ENDIF.
  ENDIF.

*CHECK ex_description IS INITIAL.


  IF no_auth = abap_true.
    no_auth = abap_false.
  ELSE.
    no_auth = abap_true.
  ENDIF.

  CALL FUNCTION 'RH_READ_INFTY_1000'
    EXPORTING
      plvar  = im_plvar
      otype  = im_otype
      objid  = im_objid
      istat  = im_istat
      begda  = im_date
      endda  = im_date

      with_stru_auth = no_auth

    TABLES
      i1000  = lt_1000
    EXCEPTIONS
      OTHERS = 1.
  IF sy-subrc EQ 0.

    READ TABLE lt_1000 INDEX 1 ASSIGNING <fs_1000_line>.
    ex_short = <fs_1000_line>-short.
    ex_stext = <fs_1000_line>-stext.
    IF lf_stext_only EQ abap_true OR ex_description IS INITIAL.
      ex_description = <fs_1000_line>-stext.
    ENDIF.
  ELSE.
    RAISE not_found.
  ENDIF.
  endmethod.


  METHOD get_orgeh.
    DATA: lt_0001 TYPE TABLE OF p0001.
    FIELD-SYMBOLS: <ls_0001> LIKE LINE OF lt_0001.
    CLEAR: _ev_orgeh,
           _ev_descr.
    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        pernr           = _iv_pernr
        infty           = '0001'
        begda           = _iv_date
        endda           = _iv_date
      TABLES
        infty_tab       = lt_0001
      EXCEPTIONS
        infty_not_found = 1
        OTHERS          = 2.
    IF sy-subrc = 0 AND lt_0001 IS NOT INITIAL.
      READ TABLE lt_0001
        ASSIGNING <ls_0001>
        INDEX 1.
      IF _ev_orgeh IS SUPPLIED.
        _ev_orgeh = <ls_0001>-orgeh.
      ENDIF.
      IF _ev_descr IS SUPPLIED.
        CALL METHOD get_objec_text(
          EXPORTING
            im_plvar       = _iv_plvar
            im_otype       = 'O'
            im_objid       = <ls_0001>-orgeh
            im_date        = _iv_date
          IMPORTING
            ex_description = _ev_descr
          EXCEPTIONS
            not_found      = 1
            OTHERS         = 2
                             ).
        IF sy-subrc <> 0.
          RAISE not_found_text.
        ENDIF.

      ENDIF.
    ENDIF.

  ENDMETHOD.


  method GET_RELATION.
     DATA:
    lf_begda TYPE d,
    lf_endda TYPE d,
    lf_objid TYPE hrobjid,
    lt_1001 TYPE TABLE OF p1001.

  FIELD-SYMBOLS:
    <objec> TYPE objec,
    <p1001> LIKE LINE OF lt_1001.

  CHECK im_objid IS NOT INITIAL.
  IF im_begda IS SUPPLIED.

    lf_begda = im_begda.

    IF im_endda IS NOT SUPPLIED.
      lf_endda = lf_begda.
    ELSE.
      lf_endda = im_endda.
    ENDIF.
  ELSE.
    lf_begda = '18000101'.
    lf_endda = '99991231'.
  ENDIF.

  lf_objid = im_objid.

  CALL FUNCTION 'RH_READ_INFTY_1001'
    EXPORTING
      plvar = im_plvar
      otype = im_otype
      objid = lf_objid
      subty = im_relation
      begda = lf_begda
      endda = lf_endda
    TABLES
      i1001 = lt_1001
    EXCEPTIONS
      others = 1.

  CLEAR: ex_otype,
         ex_objid,
         et_objec,
         es_1001,
         et_1001.

  CHECK sy-subrc EQ 0.

  DATA:
    lf_first.

  lf_first = 'X'.

  IF et_1001 IS REQUESTED.
    et_1001[] = lt_1001[].
  ENDIF.

  LOOP AT lt_1001 ASSIGNING <p1001>.

    IF im_otype_rel IS SUPPLIED.
      CHECK <p1001>-sclas EQ im_otype_rel.
    ENDIF.

    IF lf_first EQ 'X'.
      CLEAR lf_first.
      ex_otype = <p1001>-sclas.
      ex_objid = <p1001>-sobid.
      IF es_1001 IS REQUESTED.
        es_1001 = <p1001>.
      ENDIF.

      IF ex_begda_obj IS SUPPLIED OR ex_endda_obj IS SUPPLIED.
        CALL FUNCTION 'RH_READ_OBJECT'
          EXPORTING
            objid           = ex_objid
            otype           = ex_otype
            plvar           = im_plvar
            check_stru_auth = ' '
            read_db         = 'X'
          IMPORTING
            obeg            = ex_begda_obj
            oend            = ex_endda_obj
          EXCEPTIONS
            OTHERS          = 1.
      ENDIF.

    ENDIF.

    IF et_objec IS REQUESTED.
      APPEND INITIAL LINE TO et_objec ASSIGNING <objec>.
      <objec>-plvar = im_plvar.
      <objec>-otype = <p1001>-sclas.
      <objec>-objid = <p1001>-sobid.
    ENDIF.

  ENDLOOP.
  endmethod.
ENDCLASS.

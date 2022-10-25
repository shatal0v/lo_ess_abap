*&---------------------------------------------------------------------*
*&  Include           ZHR_PA014_LCL
*&---------------------------------------------------------------------*

CLASS lcl DEFINITION.
  PUBLIC SECTION.

    CONSTANTS: c_dar_01     TYPE p0041-dar01 VALUE '01'
             , c_otype_o    TYPE otype       VALUE 'O'
             , c_otype_s    TYPE otype       VALUE 'S'
             , c_otype_p    TYPE otype       VALUE 'P'
             , c_begda_low  TYPE begda       VALUE '19010101'
             , c_begda_high TYPE endda       VALUE '99991231'
             , c_wegid_psoo TYPE wegid       VALUE 'P-S-O-O'
             , c_infty_1000 TYPE infty       VALUE '1000'
             , c_ktart_01   TYPE p2006-ktart VALUE '01'
             , c_ktart_02   TYPE p2006-ktart VALUE '02'
             , c_ktart_03   TYPE p2006-ktart VALUE '03'
             , c_ktart_04   TYPE p2006-ktart VALUE '04'
             , c_ktart_05   TYPE p2006-ktart VALUE '05'
             , c_ktart_10   TYPE p2006-ktart VALUE '10'
             .

    TYPES: BEGIN OF ty_line
         , pernr     TYPE pernr_d
         , fio       TYPE text255
         , orgehtxt  TYPE text255
         , orgeh     TYPE p0001-orgeh
         , planstxt  TYPE text255
         , plans     TYPE p0001-plans
         , dtype     TYPE text20
         , pdate     TYPE text10
         , anzhlm    TYPE anzhl
         , anzhlo    TYPE anzhl
         , anzhlmtxt TYPE text10
         , anzhlotxt TYPE text10
***      << add Shibkova 28.09.2017
         , anzhl_01  TYPE anzhl
         , anzhl_02  TYPE anzhl
         , anzhl_03  TYPE anzhl
         , anzhl_04  TYPE anzhl
         , anzhl_05  TYPE anzhl
         , anzhl_10  TYPE anzhl
         , anzhl_01txt TYPE text10
         , anzhl_02txt TYPE text10
         , anzhl_03txt TYPE text10
         , anzhl_04txt TYPE text10
         , anzhl_05txt TYPE text10
         , anzhl_10txt TYPE text10
***      >> end
         ,   END OF ty_line
         , BEGIN OF ty_main
         , field01 TYPE text10
         ,   END OF ty_main
         , BEGIN OF ty_limi
         , pernr TYPE p2006-pernr
         , tabix TYPE sy-tabix
         , ktart TYPE p2006-ktart
         , quonr TYPE p2006-quonr
         , kverb TYPE p2006-kverb
         , anzhl TYPE p2006-anzhl
         , END OF ty_limi
         , BEGIN OF ty_pa2001
         , pernr TYPE p2001-pernr
         , begda TYPE p2001-begda
         , docnr TYPE p2001-docnr
         , sppe1 TYPE p2001-sppe1
         , END OF ty_pa2001
         .

    DATA: t_line  TYPE TABLE OF ty_line
        , s_main  TYPE ty_main
        , t_t547s TYPE TABLE OF t547s
        , t_limi  TYPE SORTED TABLE OF ty_limi WITH NON-UNIQUE KEY pernr tabix

        , zzpa014 TYPE REF TO zclhr_pa014

        , a_plvar TYPE plvar
        , a_begda TYPE begda
        , lo_hr_om TYPE REF TO zcl_hr_get_data
        .

    METHODS: constructor
           , get_data
           , end_of_selection
           , print
           , check_orgeh    RETURNING VALUE(subrc) TYPE sy-subrc
           , get_objid_txt  IMPORTING i_otype TYPE otype
                                     i_objid TYPE hrobjid
                            RETURNING VALUE(text)  TYPE text255
           , get_anzhl      IMPORTING i_p2006 TYPE p2006
                            RETURNING VALUE(anzhl) TYPE anzhl
           , month_end      IMPORTING i_begda TYPE begda
                            RETURNING VALUE(endda) TYPE endda
           , next_month_beg IMPORTING i_begda TYPE begda
                            RETURNING VALUE(endda) TYPE endda
           , get_month_days IMPORTING i_begda TYPE begda
                                      i_endda TYPE endda
                            RETURNING VALUE(anzhl) TYPE anzhl

           .

ENDCLASS.                    "lcl DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl IMPLEMENTATION.
  METHOD constructor.
    lo_hr_om = NEW zcl_hr_get_data( ).

    SELECT * FROM t547s INTO TABLE t_t547s WHERE sprsl EQ sy-langu.

    a_begda = pn-begda.

    pn-begda = c_begda_low.
    pn-endda = c_begda_high.

    WRITE a_begda TO s_main-field01 DD/MM/YYYY.

    CALL FUNCTION 'RH_GET_ACTIVE_WF_PLVAR'
      IMPORTING
        act_plvar       = a_plvar
      EXCEPTIONS
        no_active_plvar = 1
        OTHERS          = 2.

    IF zz_new IS NOT INITIAL.
      CREATE OBJECT zzpa014.
    ENDIF.
  ENDMETHOD.                    "constructor

  METHOD get_data.
    DATA: dar TYPE p0041-dar01
        , dat TYPE p0041-dat01
        .
    FIELD-SYMBOLS: <t_line>  LIKE LINE OF t_line
                 , <p0001>   LIKE LINE OF p0001[]
                 , <p0002>   LIKE LINE OF p0002[]
                 , <p0016>   LIKE LINE OF p0016[]
                 , <p0041>   LIKE LINE OF p0041[]
                 , <p2006>   LIKE LINE OF p2006[]
                 , <t_t547s> LIKE LINE OF t_t547s
                 .

    LOOP AT p0001 ASSIGNING <p0001> WHERE begda <= a_begda
                                      AND endda >= a_begda.

      IF NOT <p0001>-orgeh IN pnpobjid[].
        CHECK me->check_orgeh( ) EQ 0.
      ENDIF.

      APPEND INITIAL LINE TO t_line ASSIGNING <t_line>.
      <t_line>-pernr = peras-pernr.
      <t_line>-orgeh = <p0001>-orgeh.
      <t_line>-plans = <p0001>-plans.
      <t_line>-orgehtxt = get_objid_txt( i_otype = c_otype_o i_objid = <p0001>-orgeh ).
      <t_line>-planstxt = get_objid_txt( i_otype = c_otype_s i_objid = <p0001>-plans ).
    ENDLOOP.

    CHECK <t_line> IS ASSIGNED.

    LOOP AT p0002 ASSIGNING <p0002> WHERE begda <= a_begda
                                      AND endda >= a_begda .

      <t_line>-fio = <p0002>-nachn && ` ` && <p0002>-vorna && ` ` && <p0002>-midnm.
    ENDLOOP.

    LOOP AT p0016 ASSIGNING <p0016> WHERE begda <= a_begda
                                      AND endda >= a_begda.

      READ TABLE t_t547s ASSIGNING <t_t547s> WITH KEY cttyp = <p0016>-cttyp.
      CHECK sy-subrc EQ 0 AND <t_t547s> IS ASSIGNED.
      <t_line>-dtype = <t_t547s>-cttxt.
    ENDLOOP.

    LOOP AT p0041 ASSIGNING <p0041> WHERE begda <= a_begda
                                      AND endda >= a_begda.

      DO 12 TIMES VARYING dar FROM <p0041>-dar01 NEXT <p0041>-dar02
                  VARYING dat FROM <p0041>-dat01 NEXT <p0041>-dat02.

        CHECK dar EQ c_dar_01.
        WRITE dat TO <t_line>-pdate DD/MM/YYYY.
      ENDDO.
    ENDLOOP.

    IF zzpa014 IS BOUND.
      CLEAR zzpa014->s_limi.
      LOOP AT p2006 ASSIGNING <p2006> WHERE sprps <> abap_true
                                        AND ktart IN s_ktart
                                        AND desta <= a_begda
                                        AND deend >= a_begda.
        zzpa014->add_limi( i_p2006 = <p2006> i_norm = p_norm i_begda = a_begda i_anzhl = p_anzhl ).
      ENDLOOP.
      EXIT.
    ENDIF.

    TRY.
        LOOP AT p2006 ASSIGNING <p2006> WHERE sprps <> abap_true
                                          AND ktart IN s_ktart
                                          AND desta <= a_begda
                                          AND deend >= a_begda.

          CASE <p2006>-ktart.
            WHEN c_ktart_01.
              <t_line>-anzhlm = <t_line>-anzhlm + me->get_anzhl( <p2006> ).
***      <<< add Shibkova 28.09.2017
              <t_line>-anzhl_01 = <t_line>-anzhl_01 + me->get_anzhl( <p2006> ).
            WHEN c_ktart_02.
              <t_line>-anzhlo = <t_line>-anzhlo + me->get_anzhl( <p2006> ).
              <t_line>-anzhl_02 = <t_line>-anzhl_02 + me->get_anzhl( <p2006> ).
            WHEN c_ktart_03.
              <t_line>-anzhlo = <t_line>-anzhlo + me->get_anzhl( <p2006> ).
              <t_line>-anzhl_03 = <t_line>-anzhl_03 + me->get_anzhl( <p2006> ).
            WHEN c_ktart_04.
              <t_line>-anzhlo = <t_line>-anzhlo + me->get_anzhl( <p2006> ).
              <t_line>-anzhl_04 = <t_line>-anzhl_04 + me->get_anzhl( <p2006> ).
            WHEN c_ktart_05.
              <t_line>-anzhlo = <t_line>-anzhlo + me->get_anzhl( <p2006> ).
              <t_line>-anzhl_05 = <t_line>-anzhl_05 + me->get_anzhl( <p2006> ).
            WHEN c_ktart_10.
              <t_line>-anzhlo = <t_line>-anzhlo + me->get_anzhl( <p2006> ).
              <t_line>-anzhl_10 = <t_line>-anzhl_10 + me->get_anzhl( <p2006> ).
***     >>> end
            WHEN OTHERS.
              <t_line>-anzhlo = <t_line>-anzhlo + me->get_anzhl( <p2006> ).
          ENDCASE.
        ENDLOOP.


        IF <t_line>-anzhlm <> 0.
          <t_line>-anzhlmtxt = <t_line>-anzhlmtxt + <t_line>-anzhlm.
        ENDIF.

        IF <t_line>-anzhlo <> 0.
          <t_line>-anzhlotxt = <t_line>-anzhlotxt + <t_line>-anzhlo.
        ENDIF.
***      <<< add Shibkova 28.09.2017
        IF <t_line>-anzhl_01 <> 0.
          <t_line>-anzhl_01txt = <t_line>-anzhl_01txt + <t_line>-anzhl_01.
        ENDIF.
        IF <t_line>-anzhl_02 <> 0.
          <t_line>-anzhl_02txt = <t_line>-anzhl_02txt + <t_line>-anzhl_02.
        ENDIF.
        IF <t_line>-anzhl_03 <> 0.
          <t_line>-anzhl_03txt = <t_line>-anzhl_03txt + <t_line>-anzhl_03.
        ENDIF.
        IF <t_line>-anzhl_04 <> 0.
          <t_line>-anzhl_04txt = <t_line>-anzhl_04txt + <t_line>-anzhl_04.
        ENDIF.
        IF <t_line>-anzhl_05 <> 0.
          <t_line>-anzhl_05txt = <t_line>-anzhl_05txt + <t_line>-anzhl_05.
        ENDIF.
        IF <t_line>-anzhl_10 <> 0.
          <t_line>-anzhl_10txt = <t_line>-anzhl_10txt + <t_line>-anzhl_10.
        ENDIF.

        IF <t_line>-anzhl_01txt CP '*-'.
          REPLACE ALL OCCURRENCES OF '-' IN <t_line>-anzhl_01txt WITH ' '.
          CONCATENATE '-' <t_line>-anzhl_01txt INTO <t_line>-anzhl_01txt.
        ENDIF.
        IF <t_line>-anzhl_02txt CP '*-'.
          REPLACE ALL OCCURRENCES OF '-' IN <t_line>-anzhl_02txt WITH ' '.
          CONCATENATE '-' <t_line>-anzhl_02txt INTO <t_line>-anzhl_02txt.
        ENDIF.
        IF <t_line>-anzhl_03txt CP '*-'.
          REPLACE ALL OCCURRENCES OF '-' IN <t_line>-anzhl_03txt WITH ' '.
          CONCATENATE '-' <t_line>-anzhl_03txt INTO <t_line>-anzhl_03txt.
        ENDIF.
        IF <t_line>-anzhl_04txt CP '*-'.
          REPLACE ALL OCCURRENCES OF '-' IN <t_line>-anzhl_04txt WITH ' '.
          CONCATENATE '-' <t_line>-anzhl_04txt INTO <t_line>-anzhl_04txt.
        ENDIF.
        IF <t_line>-anzhl_05txt CP '*-'.
          REPLACE ALL OCCURRENCES OF '-' IN <t_line>-anzhl_05txt WITH ' '.
          CONCATENATE '-' <t_line>-anzhl_05txt INTO <t_line>-anzhl_05txt.
        ENDIF.
        IF <t_line>-anzhl_10txt CP '*-'.
          REPLACE ALL OCCURRENCES OF '-' IN <t_line>-anzhl_10txt WITH ' '.
          CONCATENATE '-' <t_line>-anzhl_10txt INTO <t_line>-anzhl_10txt.
        ENDIF.
***     >>> end

        IF <t_line>-anzhlmtxt CP '*-'.
          REPLACE ALL OCCURRENCES OF '-' IN <t_line>-anzhlmtxt WITH ' '.
          CONCATENATE '-' <t_line>-anzhlmtxt INTO <t_line>-anzhlmtxt.
        ENDIF.
        IF <t_line>-anzhlotxt CP '*-'.
          REPLACE ALL OCCURRENCES OF '-' IN <t_line>-anzhlotxt WITH ' '.
          CONCATENATE '-' <t_line>-anzhlotxt INTO <t_line>-anzhlotxt.
        ENDIF.
        CONDENSE <t_line>-anzhlmtxt NO-GAPS.
        CONDENSE <t_line>-anzhlotxt NO-GAPS.
        CONDENSE <t_line>-anzhl_01txt NO-GAPS.
        CONDENSE <t_line>-anzhl_02txt NO-GAPS.
        CONDENSE <t_line>-anzhl_03txt NO-GAPS.
        CONDENSE <t_line>-anzhl_04txt NO-GAPS.
        CONDENSE <t_line>-anzhl_05txt NO-GAPS.
        CONDENSE <t_line>-anzhl_10txt NO-GAPS.
      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.                    "get_data

  METHOD end_of_selection.
    IF zzpa014 IS BOUND.
      zzpa014->minus_ptquoded( a_begda ).

      LOOP AT t_line ASSIGNING FIELD-SYMBOL(<t_line>).
        LOOP AT zzpa014->t_limi ASSIGNING FIELD-SYMBOL(<t_limi>) WHERE pernr = <t_line>-pernr.
          CASE <t_limi>-ktart.
            WHEN c_ktart_01.
              <t_line>-anzhlm = <t_line>-anzhlm + <t_limi>-anzhl - <t_limi>-kverb.
***      <<< add Shibkova 28.09.2017
              <t_line>-anzhl_01 = <t_line>-anzhl_01 + <t_limi>-anzhl - <t_limi>-kverb.
            WHEN c_ktart_02.
              <t_line>-anzhlo = <t_line>-anzhlo + <t_limi>-anzhl - <t_limi>-kverb.
              <t_line>-anzhl_02 = <t_line>-anzhl_02 + <t_limi>-anzhl - <t_limi>-kverb.
            WHEN c_ktart_03.
              <t_line>-anzhlo = <t_line>-anzhlo + <t_limi>-anzhl - <t_limi>-kverb.
              <t_line>-anzhl_03 = <t_line>-anzhl_03 + <t_limi>-anzhl - <t_limi>-kverb.
            WHEN c_ktart_04.
              <t_line>-anzhlo = <t_line>-anzhlo + <t_limi>-anzhl - <t_limi>-kverb.
              <t_line>-anzhl_04 = <t_line>-anzhl_04 + <t_limi>-anzhl - <t_limi>-kverb.
            WHEN c_ktart_05.
              <t_line>-anzhlo = <t_line>-anzhlo + <t_limi>-anzhl - <t_limi>-kverb.
              <t_line>-anzhl_05 = <t_line>-anzhl_05 + <t_limi>-anzhl - <t_limi>-kverb.
            WHEN c_ktart_10.
              <t_line>-anzhlo = <t_line>-anzhlo + <t_limi>-anzhl - <t_limi>-kverb.
              <t_line>-anzhl_10 = <t_line>-anzhl_10 + <t_limi>-anzhl - <t_limi>-kverb.
***     >>> end
            WHEN OTHERS.
              <t_line>-anzhlo = <t_line>-anzhlo + <t_limi>-anzhl - <t_limi>-kverb.
          ENDCASE.
        ENDLOOP.

        IF <t_line>-anzhlm <> 0.
          <t_line>-anzhlmtxt = <t_line>-anzhlmtxt + <t_line>-anzhlm.
        ENDIF.
        IF <t_line>-anzhlo <> 0.
          <t_line>-anzhlotxt = <t_line>-anzhlotxt + <t_line>-anzhlo.
        ENDIF.
        IF <t_line>-anzhl_01 <> 0.
          <t_line>-anzhl_01txt = <t_line>-anzhl_01txt + <t_line>-anzhl_01.
        ENDIF.
        IF <t_line>-anzhl_02 <> 0.
          <t_line>-anzhl_02txt = <t_line>-anzhl_02txt + <t_line>-anzhl_02.
        ENDIF.
        IF <t_line>-anzhl_03 <> 0.
          <t_line>-anzhl_03txt = <t_line>-anzhl_03txt + <t_line>-anzhl_03.
        ENDIF.
        IF <t_line>-anzhl_04 <> 0.
          <t_line>-anzhl_04txt = <t_line>-anzhl_04txt + <t_line>-anzhl_04.
        ENDIF.
        IF <t_line>-anzhl_05 <> 0.
          <t_line>-anzhl_05txt = <t_line>-anzhl_05txt + <t_line>-anzhl_05.
        ENDIF.
        IF <t_line>-anzhl_10 <> 0.
          <t_line>-anzhl_10txt = <t_line>-anzhl_10txt + <t_line>-anzhl_10.
        ENDIF.

        IF <t_line>-anzhl_01txt CP '*-'.
          REPLACE ALL OCCURRENCES OF '-' IN <t_line>-anzhl_01txt WITH ' '.
          CONCATENATE '-' <t_line>-anzhl_01txt INTO <t_line>-anzhl_01txt.
        ENDIF.
        IF <t_line>-anzhl_02txt CP '*-'.
          REPLACE ALL OCCURRENCES OF '-' IN <t_line>-anzhl_02txt WITH ' '.
          CONCATENATE '-' <t_line>-anzhl_02txt INTO <t_line>-anzhl_02txt.
        ENDIF.
        IF <t_line>-anzhl_03txt CP '*-'.
          REPLACE ALL OCCURRENCES OF '-' IN <t_line>-anzhl_03txt WITH ' '.
          CONCATENATE '-' <t_line>-anzhl_03txt INTO <t_line>-anzhl_03txt.
        ENDIF.
        IF <t_line>-anzhl_04txt CP '*-'.
          REPLACE ALL OCCURRENCES OF '-' IN <t_line>-anzhl_04txt WITH ' '.
          CONCATENATE '-' <t_line>-anzhl_04txt INTO <t_line>-anzhl_04txt.
        ENDIF.
        IF <t_line>-anzhl_05txt CP '*-'.
          REPLACE ALL OCCURRENCES OF '-' IN <t_line>-anzhl_05txt WITH ' '.
          CONCATENATE '-' <t_line>-anzhl_05txt INTO <t_line>-anzhl_05txt.
        ENDIF.
        IF <t_line>-anzhl_10txt CP '*-'.
          REPLACE ALL OCCURRENCES OF '-' IN <t_line>-anzhl_10txt WITH ' '.
          CONCATENATE '-' <t_line>-anzhl_10txt INTO <t_line>-anzhl_10txt.
        ENDIF.
        IF <t_line>-anzhlmtxt CP '*-'.
          REPLACE ALL OCCURRENCES OF '-' IN <t_line>-anzhlmtxt WITH ' '.
          CONCATENATE '-' <t_line>-anzhlmtxt INTO <t_line>-anzhlmtxt.
        ENDIF.
        IF <t_line>-anzhlotxt CP '*-'.
          REPLACE ALL OCCURRENCES OF '-' IN <t_line>-anzhlotxt WITH ' '.
          CONCATENATE '-' <t_line>-anzhlotxt INTO <t_line>-anzhlotxt.
        ENDIF.
        CONDENSE <t_line>-anzhlmtxt NO-GAPS.
        CONDENSE <t_line>-anzhlotxt NO-GAPS.
        CONDENSE <t_line>-anzhl_01txt NO-GAPS.
        CONDENSE <t_line>-anzhl_02txt NO-GAPS.
        CONDENSE <t_line>-anzhl_03txt NO-GAPS.
        CONDENSE <t_line>-anzhl_04txt NO-GAPS.
        CONDENSE <t_line>-anzhl_05txt NO-GAPS.
        CONDENSE <t_line>-anzhl_10txt NO-GAPS.
      ENDLOOP.
    ENDIF.



    SORT t_line BY fio.
  ENDMETHOD.                    "END_OF_SELECTION

  METHOD print.
    DATA: xml_out     TYPE xstring
        , filename    TYPE string
        , return(255) TYPE c
        , t_data_xml  TYPE swxmlcont
        .
* { goncharov 04.09.2017 17:21:30
    IF p_explim EQ abap_true.
      EXPORT t_line FROM t_line TO MEMORY ID 'ZLIM_PA_014'.
      RETURN.
    ENDIF.
* } goncharov 04.09.2017 17:21:30

    TRY.
        CASE PNPBUKRS-LOW.
          WHEN '0072'.
            CALL TRANSFORMATION ztrhr_pa014_0072
                 SOURCE root = t_line
                        main = s_main
                 RESULT XML t_data_xml.
          WHEN OTHERS.
            CALL TRANSFORMATION ztrhr_pa014
                 SOURCE root = t_line
                        main = s_main
                 RESULT XML t_data_xml.
        ENDCASE.
      CATCH cx_st_error.
    ENDTRY.

    CALL FUNCTION 'GUI_GET_DESKTOP_INFO'
      EXPORTING
        type   = 4  "TmpDir
      CHANGING
        return = return
      EXCEPTIONS
        OTHERS = 99.

    IF return IS INITIAL.
      return = 'C:\TEMP'.
    ENDIF.

    filename = return && `\` && sy-repid && `_` && sy-datum && `_` && sy-uzeit && `.xml`.

    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        filename         = filename
        filetype         = 'BIN'
      CHANGING
        data_tab         = t_data_xml
      EXCEPTIONS
        file_write_error = 1
        OTHERS           = 99.

    CHECK sy-subrc EQ 0.

    CONCATENATE '"' filename '"' INTO filename.

    CALL METHOD cl_gui_frontend_services=>execute
      EXPORTING
        application = 'Excel'
        parameter   = filename
        operation   = 'OPEN'.
  ENDMETHOD.                    "print

  METHOD check_orgeh.
    DATA: t_objec TYPE objec_t.

    CALL FUNCTION 'RH_STRUC_GET'
      EXPORTING
        act_otype       = c_otype_p
        act_objid       = peras-pernr
        act_wegid       = c_wegid_psoo
        act_plvar       = a_plvar
        act_begda       = a_begda
        act_endda       = a_begda
        act_tflag       = abap_false
        act_vflag       = abap_false
        authority_check = abap_false
      TABLES
        result_objec    = t_objec
      EXCEPTIONS
        no_plvar_found  = 1
        no_entry_found  = 2
        OTHERS          = 3.

    DELETE t_objec WHERE NOT ( otype EQ c_otype_o AND objid IN pnpobjid[] ).

    CASE lines( t_objec ).
      WHEN 0.
        subrc = 4.
      WHEN OTHERS.
        "
    ENDCASE.

    FREE: t_objec.
  ENDMETHOD.                    "check_orgeh

  METHOD get_objid_txt.
    DATA: t_p1000  TYPE TABLE OF p1000
        , t_object TYPE hrobject_t

        .

    FIELD-SYMBOLS: <t_p1000>  LIKE LINE OF t_p1000
                 , <t_object> LIKE LINE OF t_object
                 .

    APPEND INITIAL LINE TO t_object ASSIGNING <t_object>.
    <t_object>-plvar = a_plvar.
    <t_object>-otype = i_otype.
    <t_object>-objid = i_objid.


    CALL METHOD lo_hr_om->read_om_infty
      EXPORTING
        i_infty     = c_infty_1000
        i_object    = t_object
        i_endda     = a_begda
        i_begda     = a_begda
        i_authority = abap_false
        i_sauth     = abap_false
      IMPORTING
        e_pnnnn       = t_p1000.

    LOOP AT t_p1000 ASSIGNING <t_p1000> WHERE langu EQ sy-langu.
      text = <t_p1000>-stext.
    ENDLOOP.

    FREE: t_p1000.
  ENDMETHOD.                    "get_objid_txt

  METHOD get_anzhl.
    DATA: wa_anzhl(7) TYPE p DECIMALS 3
        , wa_month    TYPE anzhl
        .

    IF i_p2006-endda <= a_begda OR p_norm <> abap_true.
      anzhl = i_p2006-anzhl - i_p2006-kverb.
    ELSE.
      CASE i_p2006-ktart.
        WHEN c_ktart_01.
          wa_anzhl = p_anzhl.
          wa_month = 1.
        WHEN OTHERS.
          wa_anzhl = i_p2006-anzhl.
          wa_month = me->get_month_days( i_begda = i_p2006-begda i_endda = i_p2006-endda ).
      ENDCASE.

      wa_anzhl = wa_anzhl / wa_month.

      anzhl = wa_anzhl * me->get_month_days( i_begda = i_p2006-begda i_endda = a_begda ) -  i_p2006-kverb.
    ENDIF.
  ENDMETHOD.                    "get_anzhl

  METHOD month_end.
    endda = me->next_month_beg( i_begda ) - 1.
  ENDMETHOD.                    "month_end

  METHOD next_month_beg.
    endda = i_begda.
    endda+6(2) = `01`.
    endda = endda + 32.
    endda+6(2) = `01`.
  ENDMETHOD.                    "next_month_beg

  METHOD get_month_days.
    DATA: wa_begda TYPE begda
        , wa_endda TYPE endda
        , wa_days  TYPE anzhl.

    CALL FUNCTION 'RE_ADD_MONTH_TO_DATE'
      EXPORTING
        months  = 12
        olddate = i_begda
      IMPORTING
        newdate = wa_endda.

    IF wa_endda EQ i_endda.
      anzhl = 12.
      EXIT.
    ENDIF.

    IF i_begda+6(2) <> '01'.
      wa_days  = me->month_end( i_begda ) - i_begda + 1.
      wa_begda = me->next_month_beg( i_begda ).
      IF wa_days >= 15.
        ADD 1 TO anzhl.
      ENDIF.
    ELSE.
      wa_begda = i_begda+0(6) && `01`.
    ENDIF.

    WHILE wa_begda <= i_endda.

      CASE wa_begda+0(6).
        WHEN i_endda+0(6).
          IF me->month_end( wa_begda ) EQ i_endda.
            ADD 1 TO anzhl.
          ELSE.
            wa_days  = i_endda - wa_begda + 1.
            IF wa_days >= 15.
              ADD 1 TO anzhl.
            ENDIF.
          ENDIF.
        WHEN OTHERS.
          ADD 1 TO anzhl.
      ENDCASE.
      wa_begda = me->next_month_beg( wa_begda ).
    ENDWHILE.
  ENDMETHOD.                    "get_month_days
ENDCLASS.                    "lcl IMPLEMENTATION

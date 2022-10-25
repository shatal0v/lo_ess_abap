class ZCLHR_PA014 definition
  public
  create public .

public section.

  types:
    BEGIN OF ty_limi
           , pernr TYPE p2006-pernr
           , tabix TYPE sy-tabix
           , ktart TYPE p2006-ktart
           , quonr TYPE p2006-quonr
           , kverb TYPE p2006-kverb
           , anzhl TYPE p2006-anzhl
           , END OF ty_limi .
  types:
    BEGIN OF ty_pa2001
           , pernr TYPE p2001-pernr
           , begda TYPE p2001-begda
           , docnr TYPE p2001-docnr
           , sppe1 TYPE p2001-sppe1
           , END OF ty_pa2001 .

  data:
    T_LIMI TYPE SORTED TABLE OF ty_limi WITH NON-UNIQUE KEY pernr tabix .
  data:
    s_limi LIKE LINE OF t_limi .

  methods ADD_LIMI
    importing
      !I_P2006 type P2006
      !I_BEGDA type BEGDA
      !I_NORM type FLAG default `X`
      !I_ANZHL type ANZHL default `2.33` .
  methods GET_ANZHL
    importing
      !I_P2006 type P2006
      !I_BEGDA type BEGDA default SY-DATUM
      !I_NORM type FLAG
      !I_ANZHL type ANZHL default `2.33`
    returning
      value(ANZHL) type P2006-ANZHL .
  methods GET_MONTH_DAYS
    importing
      !I_BEGDA type P2006-BEGDA
      !I_ENDDA type P2006-ENDDA
    returning
      value(ANZHL) type P2006-ANZHL .
  methods NEXT_MONTH_BEG
    importing
      !I_BEGDA type BEGDA
    returning
      value(ENDDA) type ENDDA .
  methods MONTH_END
    importing
      !I_BEGDA type BEGDA
    returning
      value(ENDDA) type ENDDA .
  methods MINUS_PTQUODED
    importing
      !I_BEGDA type BEGDA
      !I_SIMPLE type FLAG default ABAP_TRUE .
  methods ROUNDING_NUMBER
    importing
      !I_ANZHL type P2006-ANZHL
      !I_RULE type RDTYP
    returning
      value(ANZHL) type P2006-ANZHL .
protected section.
private section.
ENDCLASS.



CLASS ZCLHR_PA014 IMPLEMENTATION.


  METHOD add_limi.
    s_limi-pernr = i_p2006-pernr.
    s_limi-ktart = i_p2006-ktart.
    s_limi-quonr = i_p2006-quonr.
    ADD 1 TO s_limi-tabix.
    s_limi-kverb = i_p2006-kverb.
    s_limi-anzhl = me->get_anzhl( i_p2006 = i_p2006
                                  i_begda = i_begda
                                  i_norm  = i_norm
                                  i_anzhl = i_anzhl ).
*    << add Shibkova 19.05.2017 (округление)
    CASE i_p2006-anzhl.
      WHEN 28.
        s_limi-anzhl = rounding_number( i_anzhl = s_limi-anzhl i_rule = '33' ).
      WHEN 31.
        s_limi-anzhl = rounding_number( i_anzhl = s_limi-anzhl i_rule = 'Z5' ).
    ENDCASE.
*    >> end
    INSERT s_limi INTO TABLE t_limi.

  ENDMETHOD.


  METHOD get_anzhl.
    DATA: wa_anzhl(7) TYPE p DECIMALS 3
       , wa_month    TYPE anzhl
       .

    IF i_p2006-endda <= i_begda OR i_norm <> abap_true.
      anzhl = i_p2006-anzhl.
    ELSE.
      CASE i_p2006-ktart.
        WHEN `01`.
          wa_anzhl = i_anzhl.
          wa_month = 1.
        WHEN OTHERS.
          wa_anzhl = i_p2006-anzhl.
          wa_month = me->get_month_days( i_begda = i_p2006-begda i_endda = i_p2006-endda ).
      ENDCASE.

      wa_anzhl = wa_anzhl / wa_month.

      DATA: BEGIN OF ls_perio
          , begda TYPE begda
          , endda TYPE endda
          , empty TYPE flag
          , anzhl TYPE p0267-betrg
          , END OF ls_perio

          , lt_perio  LIKE TABLE OF ls_perio
          , lt_perio2 LIKE lt_perio
          , lt_p2001  TYPE TABLE OF p2001
          , ls_p2001  LIKE LINE OF lt_p2001

          , lv_perio_valid
          , lv_p2001_valid

          .
      FIELD-SYMBOLS: <lt_perio2> LIKE LINE OF lt_perio2
                   .
      CASE i_p2006-ktart.
        WHEN `01`.
          IF i_p2006-begda <= i_begda.
            ls_perio-begda = i_p2006-begda.
            ls_perio-endda = i_begda.
            COLLECT ls_perio INTO lt_perio.

            SORT lt_perio BY begda.

            zcl_hr_data=>read_pa_infty( EXPORTING i_pernr = i_p2006-pernr
                                                  i_infty = `2001`
                                                  i_begda = ls_perio-begda
                                                  i_endda = ls_perio-endda
                                        IMPORTING e_pnnnn = lt_p2001 ).

            SORT lt_p2001 BY begda.

            DELETE lt_p2001 WHERE NOT ( ( subty BETWEEN '2031' AND '2034' OR subty = '1091' )
                                  AND sprps = abap_false ).

            TRY.
                PROVIDE FIELDS * FROM lt_perio INTO ls_perio VALID lv_perio_valid
                                 BOUNDS begda AND endda
                        FIELDS * FROM lt_p2001 INTO ls_p2001 VALID lv_p2001_valid
                                 BOUNDS begda AND endda
                                 BETWEEN i_p2006-begda AND i_begda
                                 INCLUDING GAPS.

                  ls_perio-empty = lv_p2001_valid.

                  DATA(lv_endda) = ls_perio-begda.
                  lv_endda = lv_endda - 1.

                  IF <lt_perio2> IS ASSIGNED AND lv_endda = <lt_perio2>-endda AND ls_perio-empty = <lt_perio2>-empty.
                    <lt_perio2>-endda = ls_perio-endda.
                  ELSE.
                    APPEND ls_perio TO lt_perio2 ASSIGNING <lt_perio2>.
                  ENDIF.
                ENDPROVIDE.
              CATCH cx_sy_provide_interval_overlap.
            ENDTRY.

            LOOP AT lt_perio2 ASSIGNING <lt_perio2> WHERE empty = abap_false.
              <lt_perio2>-anzhl = wa_anzhl * me->get_month_days( i_begda = <lt_perio2>-begda i_endda = <lt_perio2>-endda ).
              ADD <lt_perio2>-anzhl TO anzhl.
            ENDLOOP.
          ELSE.
            anzhl = wa_anzhl * me->get_month_days( i_begda = i_p2006-begda i_endda = i_begda ).
          ENDIF.
        WHEN OTHERS.
          anzhl = wa_anzhl * me->get_month_days( i_begda = i_p2006-begda i_endda = i_begda ).
      ENDCASE.
    ENDIF.
  ENDMETHOD.


  METHOD get_month_days.
    DATA: wa_begda TYPE begda
  , wa_endda TYPE endda
  , wa_days  TYPE anzhl
  , wa_datum TYPE datum
  .

    CALL FUNCTION 'HR_HCP_ADD_MONTH_TO_DATE'
      EXPORTING
        im_monthcount = 12
        im_date       = i_begda
      IMPORTING
        ex_date       = wa_endda.

*    CALL FUNCTION 'RE_ADD_MONTH_TO_DATE'
*      EXPORTING
*        months  = 12
*        olddate = i_begda
*      IMPORTING
*        newdate = wa_endda.

    IF wa_endda EQ i_endda.
      anzhl = 12.
      EXIT.
    ENDIF.

*    IF i_begda+6(2) <> '01'.
*      wa_days  = me->month_end( i_begda ) - i_begda + 1.
*      wa_begda = me->next_month_beg( i_begda ).
*      IF wa_days >= 15.
*        ADD 1 TO anzhl.
*      ENDIF.
*    ELSE.
*      wa_begda = i_begda+0(6) && `01`.
*    ENDIF.
*
*    WHILE wa_begda <= i_endda.
*      CASE wa_begda+0(6).
*        WHEN i_endda+0(6).
*          IF me->month_end( wa_begda ) EQ i_endda.
*            ADD 1 TO anzhl.
*          ELSE.
*            wa_days  = i_endda - wa_begda + 1.
*            IF wa_days >= 15.
*              ADD 1 TO anzhl.
*            ENDIF.
*          ENDIF.
*        WHEN OTHERS.
*          ADD 1 TO anzhl.
*      ENDCASE.
*      wa_begda = me->next_month_beg( wa_begda ).
*    ENDWHILE.


    wa_begda = i_begda.
    wa_endda = i_endda.

    wa_days = 0.

    wa_datum = wa_begda.

    WHILE wa_begda <= wa_endda.
      wa_datum = wa_begda.

      CALL FUNCTION 'HR_HCP_ADD_MONTH_TO_DATE'
        EXPORTING
          im_monthcount = '1'
          im_date       = wa_begda
        IMPORTING
          ex_date       = wa_begda.

      CHECK wa_begda <= wa_endda.
      ADD 1 TO anzhl.
    ENDWHILE.

    wa_days = wa_endda - wa_datum.

    IF wa_days >= 15.
      ADD 1 TO anzhl.
    ENDIF.
  ENDMETHOD.


  METHOD minus_ptquoded.

    DATA: t_ptquoded TYPE SORTED TABLE OF ptquoded  WITH NON-UNIQUE KEY quonr datum
      , q_ptquoded TYPE SORTED TABLE OF ptquoded  WITH NON-UNIQUE KEY pernr docnr
      , t_pa2001   TYPE SORTED TABLE OF ty_pa2001 WITH NON-UNIQUE KEY pernr begda docnr sppe1
      .

    CHECK t_limi IS NOT INITIAL.

    SELECT * FROM ptquoded INTO TABLE t_ptquoded FOR ALL ENTRIES IN t_limi WHERE quonr =  t_limi-quonr
                                                                             AND datum >= i_begda.

    IF NOT i_simple = abap_true.
      " таблица тех отсутсвий, котор вычитать не нужно
      " 1. отстутсвие уже есть на дату отчёта
      LOOP AT t_ptquoded ASSIGNING FIELD-SYMBOL(<t_ptquoded>) WHERE datum = i_begda.
        LOOP AT t_ptquoded ASSIGNING FIELD-SYMBOL(<z_ptquoded>) WHERE pernr = <t_ptquoded>-pernr
                                                                  AND docnr = <t_ptquoded>-docnr.
          LOOP AT q_ptquoded TRANSPORTING NO FIELDS WHERE pernr = <z_ptquoded>-pernr
                                                      AND docnr = <z_ptquoded>-docnr.
          ENDLOOP.
          CHECK NOT sy-subrc = 0.
          INSERT <z_ptquoded> INTO TABLE q_ptquoded.
        ENDLOOP.
      ENDLOOP.
      "2. может есть связанные отстутвия: первое заканчивается endda > pn-endda, а второе начинается begda > pn-endda
      " только для тех, отсутсвий, котор уже попали в исключения
      IF q_ptquoded IS NOT INITIAL.
        SELECT pernr begda docnr sppe1 FROM pa2001 INTO TABLE t_pa2001 FOR ALL ENTRIES IN q_ptquoded WHERE pernr = q_ptquoded-pernr.
        LOOP AT t_pa2001 ASSIGNING FIELD-SYMBOL(<t_pa2001>) WHERE sppe1 < i_begda
                                                              AND sppe1 > '19000101'.
          " ищем основное отсутсвие
          LOOP AT t_pa2001 ASSIGNING FIELD-SYMBOL(<q_pa2001>) WHERE pernr = <t_pa2001>-pernr
                                                                AND begda = <t_pa2001>-sppe1.
            " попало ли в исключения основное отсутсвие?
            LOOP AT q_ptquoded TRANSPORTING NO FIELDS WHERE pernr = <q_pa2001>-pernr
                                                        AND docnr = <t_ptquoded>-docnr.
            ENDLOOP.
            " если попало, то и продолжение добавим
            CHECK sy-subrc = 0.
            LOOP AT t_ptquoded ASSIGNING <t_ptquoded> WHERE docnr = <t_pa2001>-docnr
                                                        AND pernr = <t_pa2001>-pernr.
              " есть уже в исключениях?
              LOOP AT q_ptquoded TRANSPORTING NO FIELDS WHERE pernr = <t_ptquoded>-pernr
                                                          AND docnr = <t_ptquoded>-docnr.
              ENDLOOP.
              CHECK NOT sy-subrc = 0.
              " добавим, если нет
              INSERT <t_ptquoded> INTO TABLE q_ptquoded.
            ENDLOOP.
          ENDLOOP.
        ENDLOOP.
      ENDIF.
    ENDIF.

    "3. вычитаем то, что было использовано после даты отчёта
    LOOP AT t_limi ASSIGNING FIELD-SYMBOL(<t_limi>).
      LOOP AT t_ptquoded ASSIGNING <t_ptquoded> WHERE quonr = <t_limi>-quonr
                                                  AND pernr = <t_limi>-pernr
                                                  AND datum > i_begda.
        " проверка на исключение
        LOOP AT q_ptquoded TRANSPORTING NO FIELDS WHERE pernr = <t_ptquoded>-pernr
                                                    AND docnr = <t_ptquoded>-docnr.
        ENDLOOP.
        CHECK NOT sy-subrc = 0.
        <t_limi>-kverb = <t_limi>-kverb - <t_ptquoded>-quode.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  method MONTH_END.
    endda = me->next_month_beg( i_begda ) - 1.
  endmethod.


  method NEXT_MONTH_BEG.
    endda = i_begda.
    endda+6(2) = `01`.
    endda = endda + 32.
    endda+6(2) = `01`.
  endmethod.


  METHOD rounding_number.
    DATA: t_t559r TYPE TABLE OF t559r
    .
    SELECT *
      FROM t559r
      INTO TABLE @t_t559r
      WHERE rdtyp = @i_rule.

    LOOP AT t_t559r ASSIGNING FIELD-SYMBOL(<t_t559r>)
                     WHERE loval <= i_anzhl
                       AND hival >= i_anzhl.
      anzhl = <t_t559r>-rdval.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

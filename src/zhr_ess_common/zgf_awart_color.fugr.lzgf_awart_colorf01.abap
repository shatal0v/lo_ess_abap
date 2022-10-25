*----------------------------------------------------------------------*
***INCLUDE LZGF_AWART_COLORF01.
*----------------------------------------------------------------------*

FORM ZZCHECK_NOT_EMPTY.
    LOOP AT TOTAL ASSIGNING FIELD-SYMBOL(<TOTAL>).
      ASSIGN COMPONENT 'COLOR' OF STRUCTURE <TOTAL> TO FIELD-SYMBOL(<COLOR>).
      IF <COLOR> IS ASSIGNED AND <COLOR> IS INITIAL.
            MESSAGE 'Цвет не может быть пустым' TYPE 'E'.
      ENDIF.
    ENDLOOP.
ENDFORM.

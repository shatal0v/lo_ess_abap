*&---------------------------------------------------------------------*
*& Report  ZHR_PA014
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZHR_PA014.

INCLUDE: zhr_pa014_top
       , zhr_pa014_lcl
       .

START-OF-SELECTION.

  CREATE OBJECT l_lcl.

GET peras.

  l_lcl->get_data( ).

END-OF-SELECTION.

  CHECK l_lcl IS BOUND.

  l_lcl->end_of_selection( ).

  l_lcl->print( ).

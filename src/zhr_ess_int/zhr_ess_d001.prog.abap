REPORT zhr_ess_d001.

INCLUDE: zhr_ess_d001_top
       , zhr_ess_d001_lcl
       .

START-OF-SELECTION.

    DATA(lo_lcl) = NEW lcl_ess_d001( iv_folder = p_folder ).

    CHECK lo_lcl IS BOUND.
    lo_lcl->get_data( ).
    lo_lcl->end( ).

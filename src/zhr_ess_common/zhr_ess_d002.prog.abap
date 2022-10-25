REPORT zhr_ess_d002.

INCLUDE: zhr_ess_d002_top
       , zhr_ess_d002_lcl
       .

START-OF-SELECTION.

  DATA(l_lcl) = NEW lcl_d002( iv_days  = p_days
                              iv_begda = p_begda
                              iv_awart = s_awart[]
                              iv_pernr = s_pernr[] ).

  l_lcl->run( ).

END-OF-SELECTION.

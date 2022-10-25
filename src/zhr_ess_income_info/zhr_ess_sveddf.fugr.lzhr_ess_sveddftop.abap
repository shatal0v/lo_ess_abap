FUNCTION-POOL zhr_ess_sveddf.               "MESSAGE-ID ..

* INCLUDE LZHR_ESS_SVEDDFD...                " Local class definition


DATA: ok_code      TYPE sy-ucomm
    , lo_container TYPE REF TO cl_gui_custom_container
    , lo_grid      TYPE REF TO cl_gui_alv_grid
    , lt_data      TYPE TABLE OF zshr_ess_sveddf
    , lo_assistent TYPE REF TO zcl_mss_data_assistent
    .

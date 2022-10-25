class ZCL_ZESS_0023_CATALOGS_DPC_EXT definition
  public
  inheriting from ZCL_ZESS_0023_CATALOGS_DPC
  create public .

public section.
protected section.

  methods EFFECTS_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZESS_0023_CATALOGS_DPC_EXT IMPLEMENTATION.


  method EFFECTS_GET_ENTITYSET.
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
  endmethod.
ENDCLASS.

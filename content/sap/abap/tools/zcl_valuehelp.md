---
title: ValueHelp ABAP Handler
---

Sample request:

```
Get?Name='%2FFGLR%2FSH_SHIP_TO_PARTY'&Type='SH'&Filters='%5B%7B%22name%22%3A%22PARTNERIN%22%2C%22params%22%3A%5B%7B%22sign%22%3A%22I%22%2C%22option%22%3A%22EQ%22%2C%22low%22%3A%221000031%22%7D%5D%7D%2C%7B%22name%22%3A%22PARTNER_FCT%22%2C%22params%22%3A%5B%7B%22sign%22%3A%22I%22%2C%22option%22%3A%22EQ%22%2C%22low%22%3A%22ZFIT0002%22%7D%5D%7D%2C%7B%22name%22%3A%22PKIND%22%2C%22params%22%3A%5B%7B%22sign%22%3A%22I%22%2C%22option%22%3A%22EQ%22%2C%22low%22%3A%2202%22%7D%5D%7D%5D'&MaxResult=500&RespectConversion=true
```

```abap
class /FGLC/VALUE_HELP definition
  public
  final
  create public .

public section.

  class-methods DESCRIBE
    importing
      !IV_NAME type SHLPNAME
      !IV_TYPE type DDSHLPTYP default 'SH'
      !IV_FIELDNAME type FIELDNAME default ''
      !IV_FILTER type STRING default '[]'
    exporting
      !ER_PROCESSOR type ref to IF_DSH_TYPE_AHEAD_PROCESSOR
    returning
      value(RT_FIELDS) type /FGLC/VALUE_HELP_META_TT
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  class-methods GET
    importing
      !IV_NAME type SHLPNAME
      !IV_TYPE type DDSHLPTYP default 'SH'
      !IV_FIELDS type STRING default ''
      !IV_MAX_RESULT type INT4 default 1000
      !IV_FILTER type STRING default '[]'
      !IV_RESPECT_CONVERSION type ABAP_BOOL default ''
    returning
      value(RV_JSON) type STRING
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION .
protected section.
private section.
ENDCLASS.



CLASS /FGLC/VALUE_HELP IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method /FGLC/VALUE_HELP=>DESCRIBE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NAME                        TYPE        SHLPNAME
* | [--->] IV_TYPE                        TYPE        DDSHLPTYP (default ='SH')
* | [--->] IV_FIELDNAME                   TYPE        FIELDNAME (default ='')
* | [--->] IV_FILTER                      TYPE        STRING (default ='[]')
* | [<---] ER_PROCESSOR                   TYPE REF TO IF_DSH_TYPE_AHEAD_PROCESSOR
* | [<-()] RT_FIELDS                      TYPE        /FGLC/VALUE_HELP_META_TT
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD describe.
    DATA ls_param TYPE if_dsh_type_ahead_processor=>ty_s_additional_parameters.
    DATA lv_extnd_so TYPE abap_bool VALUE abap_true.
    DATA lv_force_type_ahead TYPE abap_bool VALUE abap_true.
    DATA ls_textid TYPE scx_t100key.
    DATA lv_dummy ##NEEDED.
    DATA lv_final_type TYPE ddshlptyp.
    DATA lv_final_name TYPE shlpname.
    DATA lv_data_element TYPE rollname.

    /ui2/cl_json=>deserialize(
                 EXPORTING json = iv_filter
                 CHANGING data = ls_param-field_values ).

    lv_final_type = iv_type.
    lv_final_name = iv_name.

    IF iv_type = 'DE'.
      SELECT SINGLE domname
        INTO @lv_final_name
        FROM dd04l
        WHERE rollname = @iv_name
          AND as4local = 'A'
          AND as4vers = '0000'.

      IF sy-subrc IS INITIAL.
        lv_final_type = 'FV'.
        lv_data_element = iv_name.
      ELSE.
        MESSAGE e001(/fglc/fiori) WITH iv_name INTO lv_dummy.
        ls_textid-msgid = sy-msgid.
        ls_textid-msgno = sy-msgno.
        ls_textid-attr1 = sy-msgv1.

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
          EXPORTING
            textid = ls_textid.
      ENDIF.
    ENDIF.

    TRY.
        er_processor = cl_dsh_type_ahead_processor=>create_instance_for_shlp(
          i_search_help_name             = lv_final_name
          i_search_help_type             = lv_final_type
          i_data_element_for_fixed_value = lv_data_element
          i_referenced_field_name        = iv_fieldname
          i_additional_parameters        = ls_param
          i_use_extended_select_options  = lv_extnd_so
          i_force_type_ahead             = lv_force_type_ahead
        ).

        IF er_processor->is_type_ahead_supported( ) = abap_false.
          MESSAGE e006(fins_gen_utils) INTO lv_dummy.
          ls_textid-msgid = sy-msgid.
          ls_textid-msgno = sy-msgno.

          RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
            EXPORTING
              textid = ls_textid.
        ENDIF.

        DATA(ls_meta) = er_processor->get_search_help_metadata( ).
        MOVE-CORRESPONDING ls_meta-fielddescr TO rt_fields[].
        LOOP AT rt_fields[] ASSIGNING FIELD-SYMBOL(<ls_fields>).
          DATA(ls_prop) = ls_meta-fieldprop[ fieldname = <ls_fields>-fieldname ].

          <ls_fields>-shlpselpos = ls_prop-shlpselpos.
          <ls_fields>-shlplispos = ls_prop-shlplispos.
        ENDLOOP.


      CATCH cx_root ##CATCH_ALL.
        CLEAR rt_fields.
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method /FGLC/VALUE_HELP=>GET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NAME                        TYPE        SHLPNAME
* | [--->] IV_TYPE                        TYPE        DDSHLPTYP (default ='SH')
* | [--->] IV_FIELDS                      TYPE        STRING (default ='')
* | [--->] IV_MAX_RESULT                  TYPE        INT4 (default =1000)
* | [--->] IV_FILTER                      TYPE        STRING (default ='[]')
* | [--->] IV_RESPECT_CONVERSION          TYPE        ABAP_BOOL (default ='')
* | [<-()] RV_JSON                        TYPE        STRING
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get.
    DATA lr_processor TYPE REF TO if_dsh_type_ahead_processor.
    DATA lr_type TYPE REF TO cl_abap_tabledescr.
    DATA lr_data TYPE REF TO data.
    DATA lv_req TYPE if_dsh_type_ahead_processor=>ty_type_ahead_request.
    FIELD-SYMBOLS <values> TYPE STANDARD TABLE.

    describe(
      EXPORTING
        iv_name      = iv_name
        iv_type      = iv_type
        iv_filter    = iv_filter
      IMPORTING
        er_processor = lr_processor
    ).

    lr_type = lr_processor->get_result_descriptor( ).
    CREATE DATA lr_data TYPE HANDLE lr_type.
    ASSIGN lr_data->* TO <values>.

    TRY.
      lr_processor->get_type_ahead_values(
        EXPORTING i_type_ahead_request = lv_req
                  i_max_results        = iv_max_result
        IMPORTING e_type_ahead_values  = <values> ).

      CATCH cx_root ##CATCH_ALL.
        rv_json = '{"error":"An error occured in /fglc/value_help=>get()"}'.
    ENDTRY.

    DELETE ADJACENT DUPLICATES FROM <values>.

    IF <values> IS NOT INITIAL.
      rv_json = /ui2/cl_json=>serialize( data = <values> conversion_exits = iv_respect_conversion ).
    ELSE.
      rv_json = '[]'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
```

```abap
class /FGLC/CL_COMMON_VALUE__DPC_EXT definition
  public
  inheriting from /FGLC/CL_COMMON_VALUE__DPC
  create public .

public section.

  methods FUNCTION_IMPORT_DESCRIBE
    importing
      !IT_PARAMETER type /IWBEP/T_MGW_NAME_VALUE_PAIR optional
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_FUNC_IMPORT optional
    exporting
      !ER_DATA type ref to DATA
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods FUNCTION_IMPORT_GET
    importing
      !IT_PARAMETER type /IWBEP/T_MGW_NAME_VALUE_PAIR optional
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_FUNC_IMPORT optional
    exporting
      !ER_DATA type ref to DATA
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~EXECUTE_ACTION
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS /FGLC/CL_COMMON_VALUE__DPC_EXT IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method /FGLC/CL_COMMON_VALUE__DPC_EXT->/IWBEP/IF_MGW_APPL_SRV_RUNTIME~EXECUTE_ACTION
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ACTION_NAME                 TYPE        STRING(optional)
* | [--->] IT_PARAMETER                   TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR(optional)
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_FUNC_IMPORT(optional)
* | [<---] ER_DATA                        TYPE REF TO DATA
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /iwbep/if_mgw_appl_srv_runtime~execute_action.

    DATA(lv_function_name) = |FUNCTION_IMPORT_{ to_upper( iv_action_name ) }|.

    CALL METHOD me->(lv_function_name)
      EXPORTING
        it_parameter            = it_parameter
        io_tech_request_context = io_tech_request_context
      IMPORTING
        er_data                 = er_data.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method /FGLC/CL_COMMON_VALUE__DPC_EXT->FUNCTION_IMPORT_DESCRIBE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_PARAMETER                   TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR(optional)
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_FUNC_IMPORT(optional)
* | [<---] ER_DATA                        TYPE REF TO DATA
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD function_import_describe.
    DATA lv_name TYPE shlpname.
    DATA lv_type TYPE ddshlptyp.
    DATA ls_parameter TYPE /iwbep/s_mgw_name_value_pair.

    FIELD-SYMBOLS <lt_fields> TYPE any.
    CREATE DATA er_data TYPE /fglc/value_help_meta_tt.
    ASSIGN er_data->* TO <lt_fields>.

    lv_name = it_parameter[ name = 'Name' ]-value.

    READ TABLE it_parameter INTO ls_parameter WITH KEY name = 'Type'.
    lv_type = SWITCH #( sy-subrc WHEN 0 THEN ls_parameter-value ).

    <lt_fields> = /fglc/value_help=>describe( iv_name = lv_name iv_type = lv_type ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method /FGLC/CL_COMMON_VALUE__DPC_EXT->FUNCTION_IMPORT_GET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_PARAMETER                   TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR(optional)
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_FUNC_IMPORT(optional)
* | [<---] ER_DATA                        TYPE REF TO DATA
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD FUNCTION_IMPORT_GET.
    DATA lv_max_result TYPE i.
    DATA lv_name TYPE shlpname.
    DATA lv_type TYPE ddshlptyp.
    DATA lv_fields TYPE string.
    DATA lv_filter TYPE string.
    DATA lv_respect_conversion TYPE abap_bool.
    DATA ls_parameter TYPE /iwbep/s_mgw_name_value_pair.

    FIELD-SYMBOLS <ls_result> TYPE /fglc/cl_common_value__mpc_ext=>result.

    lv_name = it_parameter[ name = 'Name' ]-value.

    READ TABLE it_parameter INTO ls_parameter WITH KEY name = 'Type'.
    lv_type = SWITCH #( sy-subrc WHEN 0 THEN ls_parameter-value ).

    READ TABLE it_parameter INTO ls_parameter WITH KEY name = 'Fields'.
    lv_fields = SWITCH #( sy-subrc WHEN 0 THEN ls_parameter-value ).

    READ TABLE it_parameter INTO ls_parameter WITH KEY name = 'MaxResult'.
    lv_max_result = SWITCH #( sy-subrc WHEN 0 THEN ls_parameter-value ELSE 500 ).

    READ TABLE it_parameter INTO ls_parameter WITH KEY name = 'Filters'.
    lv_filter = SWITCH #( sy-subrc WHEN 0 THEN ls_parameter-value ).

    READ TABLE it_parameter INTO ls_parameter WITH KEY name = 'RespectConversion'.
    lv_respect_conversion = SWITCH #( sy-subrc WHEN 0 THEN ls_parameter-value ).

    CREATE DATA er_data LIKE <ls_result>.
    ASSIGN er_data->* TO <ls_result>.

    <ls_result>-items = /fglc/value_help=>get(
      iv_name = lv_name
      iv_type = lv_type
      iv_fields = lv_fields
      iv_max_result = lv_max_result
      iv_filter = lv_filter
      iv_respect_conversion = lv_respect_conversion
    ).
  ENDMETHOD.
ENDCLASS.
```

```abap
class /FGLC/CL_COMMON_VALUE__MPC_EXT definition
  public
  inheriting from /FGLC/CL_COMMON_VALUE__MPC
  create public .

public section.

  methods DEFINE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS /FGLC/CL_COMMON_VALUE__MPC_EXT IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method /FGLC/CL_COMMON_VALUE__MPC_EXT->DEFINE
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] /IWBEP/CX_MGW_MED_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD define.
    super->define( ).

    DATA: lo_property    TYPE REF TO /iwbep/if_mgw_odata_property.
    DATA: lo_action      TYPE REF TO /iwbep/if_mgw_odata_action.

    lo_action = model->get_action( iv_action_name = 'Get' ).

    lo_property = lo_action->get_input_parameter( iv_name = 'Type' ).
    IF lo_property IS NOT INITIAL.
      lo_property->set_nullable( ).
    ENDIF.

    lo_property = lo_action->get_input_parameter( iv_name = 'Fields' ).
    IF lo_property IS NOT INITIAL.
      lo_property->set_nullable( ).
    ENDIF.

    lo_property = lo_action->get_input_parameter( iv_name = 'MaxResult' ).
    IF lo_property IS NOT INITIAL.
      lo_property->set_nullable( ).
    ENDIF.

    lo_property = lo_action->get_input_parameter( iv_name = 'Filters' ).
    IF lo_property IS NOT INITIAL.
      lo_property->set_nullable( ).
    ENDIF.

    lo_property = lo_action->get_input_parameter( iv_name = 'RespectConversion' ).
    IF lo_property IS NOT INITIAL.
      lo_property->set_nullable( ).
    ENDIF.


    lo_action = model->get_action( iv_action_name = 'Describe' ).

    lo_property = lo_action->get_input_parameter( iv_name = 'Type' ).
    IF lo_property IS NOT INITIAL.
      lo_property->set_nullable( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
```

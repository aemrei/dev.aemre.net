---
title: Selection Screen Value Help
metaTitle: Add value help to selection screen
metaDescription: Selecting data and showing it as value help in SAP.
---

## Add field to your screen

Add a field to screen.

```abap
SELECT-OPTIONS: s_stat  FOR crm_jest-stat NO INTERVALS.
```

Register a handler for value help request.

```abap
AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_stat-low .
  PERFORM stat_searchhelp.
```

Implement handler for that

```
FORM stat_searchhelp.
  DATA(lv_stsma) = SWITCH J_STSMA( p_cont WHEN 'X' THEN 'VALUE1' ELSE 'VALUE2'  ).

  SELECT estat, txt30
    FROM TJ30T
    INTO TABLE @DATA(lt_stats)
    WHERE stsma = @lv_stsma
      AND spras = @sy-langu.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'ESTAT'
      dynpprog        = sy-cprog
      dynpnr          = sy-dynnr
      dynprofield     = 'P_CATEG'
      value_org       = 'S'
    TABLES
      value_tab       = lt_stats[]
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF. "   IF sy-subrc <> 0.

ENDFORM.
```
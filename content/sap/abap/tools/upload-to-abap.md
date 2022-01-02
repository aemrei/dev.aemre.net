---
title: Upload excel to ABAP program
---

You can use functions:

* `ALSM_EXCEL_TO_INTERNAL_TABLE`
* `KCD_EXCEL_OLE_TO_INT_CONVERT`

Sample program

```abap
 FORM upload_file_into_table.
* Data Declarations.......................................
  DATA : L_INTERN TYPE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE.
  DATA : L_INDEX TYPE I.
  DATA : L_START_COL TYPE I VALUE '1',
         L_START_ROW TYPE I VALUE '1',
         L_END_COL TYPE I VALUE '256',
         L_END_ROW TYPE I VALUE '65536'.
* Field Symbols...........................................
  FIELD-SYMBOLS : <FS>.
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
       EXPORTING
            FILENAME = P_PCFILE
            I_BEGIN_COL = L_START_COL
            I_BEGIN_ROW = L_START_ROW
            I_END_COL = L_END_COL
            I_END_ROW = L_END_ROW
       TABLES
            INTERN = L_INTERN
       EXCEPTIONS
            INCONSISTENT_PARAMETERS = 1
            UPLOAD_OLE = 2
            OTHERS = 3.
  IF sy-subrc > 0.
    message i000 with 'Error opening PC file ... RC=' sy-subrc.
    EXIT.
  ENDIF.
  IF L_INTERN[] IS INITIAL.
    message i051 with 'No data uploaded!'.
    EXIT.
  ELSE.
    SORT L_INTERN BY ROW COL.
    LOOP AT L_INTERN.
      MOVE L_INTERN-COL TO L_INDEX.
      ASSIGN COMPONENT L_INDEX OF STRUCTURE inrec TO <FS>.
      if sy-subrc = 0. " Incase there are more xls columns than fields
        MOVE L_INTERN-VALUE TO <FS>.
      endif.
      AT END OF ROW.
        APPEND inrec.
        CLEAR inrec.
      ENDAT.
    ENDLOOP.
  ENDIF.
ENDFORM. " upload_file_into_table
```

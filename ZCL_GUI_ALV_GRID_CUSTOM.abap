class ZCL_GUI_ALV_GRID_CUSTOM definition
  public
  inheriting from CL_GUI_ALV_GRID
  create public .

public section.

  methods SET_GRAND_TOTAL_TEXT
    importing
      !P_FIELDNAME type LVC_FNAME
      !P_TOTALS_TEXT type LVC_TXTCOL .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_GUI_ALV_GRID_CUSTOM IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_GUI_ALV_GRID_CUSTOM->SET_GRAND_TOTAL_TEXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] P_FIELDNAME                    TYPE        LVC_FNAME
* | [--->] P_TOTALS_TEXT                  TYPE        LVC_TXTCOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_grand_total_text.
*   Set text in the grand total line, this class extends SAP default CL_GUI_ALV_GRID adding this feature. 
    FIELD-SYMBOLS:
      <lt_ct00>   TYPE STANDARD TABLE,
      <fs_struct> TYPE any,
      <fs_field>  TYPE any.

    ASSIGN mt_ct00->* TO <lt_ct00>.
    READ TABLE <lt_ct00> ASSIGNING <fs_struct> INDEX 1.
    IF sy-subrc = 0.
      ASSIGN COMPONENT p_fieldname OF STRUCTURE <fs_struct> TO <fs_field>.
      IF sy-subrc = 0.
        <fs_field> = p_totals_text.
      ENDIF.
    ENDIF.

    refresh_table_display( i_soft_refresh = abap_true ).
  ENDMETHOD.
ENDCLASS.
*&---------------------------------------------------------------------*
*& Report  ZPFIR_ALV_TEMPLATE
*& Exemplo de report ALV com campos editaveis
*&---------------------------------------------------------------------*
*& Autor   Marcos M. Meneses
*&---------------------------------------------------------------------*
REPORT zpfir_alv_template.

TABLES:
  bkpf, tgsb.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_bukrs TYPE bkpf-bukrs OBLIGATORY DEFAULT '1000',
            p_gjahr TYPE bkpf-gjahr OBLIGATORY,
            p_monat TYPE bkpf-monat OBLIGATORY.
SELECT-OPTIONS: s_gsber FOR tgsb-gsber.
SELECTION-SCREEN END OF BLOCK b1.

** custom ALV
DATA: BEGIN OF gt_alv OCCURS 0.
        INCLUDE STRUCTURE fagl_doc_line_alv.
DATA: ic_status TYPE char6,
      celltab   TYPE lvc_t_styl.
DATA: END OF gt_alv.

DATA:
  g_grid TYPE REF TO cl_gui_alv_grid.

INITIALIZATION.
  DATA:
    lv_datum TYPE datum.
  lv_datum = sy-datum(6) && '01'.
  lv_datum = lv_datum - 1.
  p_gjahr = lv_datum(4).
  p_monat = lv_datum+4(2).

START-OF-SELECTION.
  CALL SCREEN 9001.

MODULE status_9001 OUTPUT.
  SET PF-STATUS 'ST_9001'.
  SET TITLEBAR 'TB_001'.
  PERFORM f_monta_alv.
  PERFORM f_seleciona_dados.
ENDMODULE.

MODULE user_command_9001 INPUT.
  CASE sy-ucomm.
    WHEN 'CANC' OR 'BACK' OR 'EXIT'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

CLASS lcl_events DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no,
      handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.
ENDCLASS.

CLASS lcl_events IMPLEMENTATION.
  METHOD handle_hotspot_click .
    IF e_row_id-index IS INITIAL.
      EXIT.
    ENDIF.
    FIELD-SYMBOLS <fs_alv> LIKE LINE OF gt_alv.
    READ TABLE gt_alv ASSIGNING <fs_alv> INDEX e_row_id-index.
    PERFORM f_abrir_registro USING <fs_alv>.
  ENDMETHOD.

  METHOD handle_toolbar.
    DATA: ls_toolbar      TYPE stb_button.

    CLEAR ls_toolbar.
    MOVE 'ZREF' TO ls_toolbar-function.
    MOVE icon_refresh TO ls_toolbar-icon.
    MOVE 'Atualizar' TO ls_toolbar-quickinfo.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    CLEAR ls_toolbar.
    MOVE 3 TO ls_toolbar-butn_type.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    CLEAR ls_toolbar.
    MOVE 'ZPROC' TO ls_toolbar-function.
    MOVE icon_okay TO ls_toolbar-icon.
    MOVE 'Processar' TO ls_toolbar-text.
    MOVE 'Processar' TO ls_toolbar-quickinfo.
    APPEND ls_toolbar TO e_object->mt_toolbar.

  ENDMETHOD.

  METHOD handle_user_command.
    CASE e_ucomm.
      WHEN 'ZREF'.
        PERFORM f_seleciona_dados.
      WHEN 'ZPROC'.
        PERFORM f_processar.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

FORM f_monta_alv.
  DATA:
    lo_custom_container TYPE REF TO cl_gui_custom_container,
    ls_events           TYPE REF TO lcl_events,
    ls_stable           TYPE lvc_s_stbl,
    lt_exclude          TYPE ui_functions,
    lt_f4               TYPE lvc_t_f4 WITH HEADER LINE,
    lw_fieldcat         TYPE lvc_t_fcat,
    ls_fieldcat         TYPE lvc_s_fcat,
    lw_sort             TYPE lvc_t_sort,
    ls_layout           TYPE lvc_s_layo.
  FIELD-SYMBOLS:
    <fs_fieldcat> TYPE lvc_s_fcat.

  IF g_grid IS NOT INITIAL.
    EXIT.
  ENDIF.

  CREATE OBJECT lo_custom_container
    EXPORTING
      container_name = 'CC_ALV'.

  CREATE OBJECT g_grid
    EXPORTING
      i_parent = lo_custom_container.

  IF ls_events IS INITIAL.
    CREATE OBJECT ls_events.
    SET HANDLER ls_events->handle_hotspot_click FOR g_grid.
    SET HANDLER ls_events->handle_toolbar FOR g_grid.
    SET HANDLER ls_events->handle_user_command FOR g_grid.
  ENDIF.

  "PERFORM f_exclude_tb_functions CHANGING lt_exclude.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'FAGL_DOC_LINE_ALV'
    CHANGING
      ct_fieldcat            = lw_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  LOOP AT lw_fieldcat ASSIGNING <fs_fieldcat>.
    CASE <fs_fieldcat>-fieldname.
      WHEN 'BELNR'.
        <fs_fieldcat>-hotspot = abap_true.
      WHEN 'KOSTL' OR 'PRCTR'.
        <fs_fieldcat>-edit = abap_true.
    ENDCASE.
  ENDLOOP.

  " coluna icone status
  ls_fieldcat-fieldname = 'IC_STATUS'.
  ls_fieldcat-scrtext_l = 'Status'.
  ls_fieldcat-col_id = 1.
  ls_fieldcat-just = 'C'.
  ls_fieldcat-outputlen = 6.
  INSERT ls_fieldcat INTO lw_fieldcat INDEX 1.

  ls_layout-col_opt = abap_true.
  ls_layout-zebra = abap_true.

  CALL METHOD g_grid->set_table_for_first_display
    EXPORTING
      is_layout            = ls_layout
      it_toolbar_excluding = lt_exclude
    CHANGING
      it_fieldcatalog      = lw_fieldcat
      it_outtab            = gt_alv[]
      it_sort              = lw_sort.
ENDFORM.

FORM f_atualiza_alv.
  DATA ls_stable   TYPE lvc_s_stbl.
  ls_stable-row   = abap_true.
  ls_stable-col   = abap_true.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable      = ls_stable
      i_soft_refresh = abap_true.
ENDFORM.

FORM f_seleciona_dados.
  DATA:
    ls_alv   LIKE LINE OF gt_alv.

*** BAPI ou QUERY de consulta aqui
  ls_alv-ic_status = icon_okay.
  ls_alv-bukrs = '1000'.
  ls_alv-belnr = '1000000001'.
  ls_alv-gjahr = sy-datum(4).
  APPEND ls_alv TO gt_alv.


  PERFORM f_atualiza_alv.
ENDFORM.

FORM f_abrir_registro USING p_alv LIKE LINE OF gt_alv.
  SET PARAMETER ID 'BUK' FIELD p_bukrs.
  SET PARAMETER ID 'BLN' FIELD p_alv-belnr.
  SET PARAMETER ID 'GJR' FIELD p_gjahr.
  CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
ENDFORM.

FORM f_processar.
  DATA:
    lw_rows    TYPE lvc_t_row,
    ls_rows    TYPE lvc_s_row,
    ls_item    TYPE ztbfi_057,
    lv_sucesso TYPE flag,
    ls_return  TYPE bapiret2.

  FIELD-SYMBOLS:
    <fs_alv> LIKE LINE OF gt_alv.

  g_grid->get_selected_rows(
     IMPORTING
         et_index_rows = lw_rows ).

  IF lw_rows[] IS INITIAL.
    MESSAGE 'Selecione as linhas para processar.' TYPE 'I' DISPLAY LIKE 'W'.
    EXIT.
  ENDIF.

  DATA r_ans TYPE c.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      text_question = 'Confirma a operação?'
    IMPORTING
      answer        = r_ans.
  IF sy-subrc <> 0 OR r_ans <> '1'.
    EXIT.
  ENDIF.

  LOOP AT lw_rows INTO ls_rows.
    READ TABLE gt_alv INDEX ls_rows-index ASSIGNING <fs_alv>.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.


*** BAPI de lançamento aqui


    IF lv_sucesso IS INITIAL.
      MESSAGE ID ls_return-id TYPE 'S' NUMBER ls_return-number
         WITH ls_return-message_v1 ls_return-message_v2
              ls_return-message_v3 ls_return-message_v4
         DISPLAY LIKE ls_return-type.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF lv_sucesso = abap_true.
    COMMIT WORK AND WAIT.
    PERFORM f_atualiza_alv.
    MESSAGE 'Processo concluído com sucesso!' TYPE 'S'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.
ENDFORM.

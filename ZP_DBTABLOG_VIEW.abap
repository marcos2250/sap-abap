*&---------------------------------------------------------------------*
*& Report  ZP_DBTABLOG_VIEW
*& Visualizar log DBTABLOG
*&---------------------------------------------------------------------*
*& Autor   Marcos M. Meneses
*&---------------------------------------------------------------------*
REPORT ZP_DBTABLOG_VIEW.

TABLES:
  dbtablog.

DATA:
  gt_dbtablog TYPE TABLE OF dbtablog.

SELECTION-SCREEN BEGIN OF BLOCK 01 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_tbnam FOR dbtablog-tabname OBLIGATORY, "MATCHCODE OBJECT zshai_tab_audit,
                s_ldate FOR dbtablog-logdate OBLIGATORY,
                s_ltime FOR dbtablog-logtime,
                s_tcode FOR dbtablog-tcode,
                s_uname FOR dbtablog-username.
PARAMETERS:     p_max TYPE i DEFAULT 1000.
SELECTION-SCREEN END OF BLOCK 01.

INITIALIZATION.
  REFRESH s_ldate.
  s_ldate-low = sy-datum - 30.
  s_ldate-high = sy-datum.
  APPEND s_ldate.

START-OF-SELECTION.
  PERFORM f_buscar_dados.
  PERFORM f_mostra_alv.

FORM f_buscar_dados.
  FIELD-SYMBOLS:
     <fs_log> TYPE dbtablog.

  SELECT logdate logtime logid tabname logkey hostname username tcode progname optype
    FROM dbtablog UP TO p_max ROWS
    INTO CORRESPONDING FIELDS OF TABLE gt_dbtablog
    WHERE tabname IN s_tbnam
      AND logdate IN s_ldate
      AND logtime IN s_ltime
      AND tcode IN s_tcode
      AND username IN s_uname
      AND optype IN ('I','D','U')
   %_HINTS ORACLE 'INDEX("DBTABLOG" "DBTABLOG~TAB")'.


  IF sy-subrc NE 0.
    MESSAGE 'Registros não encontrados!' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
ENDFORM.

FORM f_mostra_alv.
  DATA:
    lt_fieldcat TYPE slis_t_fieldcat_alv,
    ls_sort     TYPE slis_sortinfo_alv,
    lt_sort     TYPE slis_t_sortinfo_alv.
  FIELD-SYMBOLS:
    <fs_fieldcat> TYPE slis_fieldcat_alv.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'DBTABLOG'
    CHANGING
      ct_fieldcat            = lt_fieldcat[]
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  LOOP AT lt_fieldcat ASSIGNING <fs_fieldcat>.
    IF <fs_fieldcat>-fieldname = 'LOGID' OR <fs_fieldcat>-fieldname = 'VERSNO' OR <fs_fieldcat>-fieldname = 'LANGUAGE'
      OR <fs_fieldcat>-fieldname = 'DATALN' OR <fs_fieldcat>-fieldname = 'LOGDATA'.
      <fs_fieldcat>-no_out = abap_true.
    ENDIF.
    IF <fs_fieldcat>-fieldname = 'LOGKEY'.
      <fs_fieldcat>-reptext_ddic = <fs_fieldcat>-seltext_s = <fs_fieldcat>-seltext_m = <fs_fieldcat>-seltext_l = 'Detalhe'.
      <fs_fieldcat>-hotspot = abap_true.
      <fs_fieldcat>-col_pos = 9.
      <fs_fieldcat>-outputlen = 25.
    ENDIF.
    IF <fs_fieldcat>-fieldname = 'OPTYPE'.
      <fs_fieldcat>-outputlen = 3.
    ENDIF.
  ENDLOOP.

  ls_sort-fieldname = 'LOGDATE'.
  APPEND ls_sort TO lt_sort.
  ls_sort-fieldname = 'LOGTIME'.
  APPEND ls_sort TO lt_sort.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = sy-repid
      i_callback_user_command = 'F01_ALV_EVENT_USER_COMMAND'
      it_fieldcat             = lt_fieldcat[]
      it_sort                 = lt_sort
    TABLES
      t_outtab                = gt_dbtablog
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.
ENDFORM.

FORM f01_alv_event_user_command USING r_ucomm     LIKE sy-ucomm
                                      rs_selfield TYPE slis_selfield.
  DATA ls_alv TYPE dbtablog.
  IF r_ucomm = '&IC1' AND rs_selfield-fieldname = 'LOGKEY'.
    READ TABLE gt_dbtablog INTO ls_alv INDEX rs_selfield-tabindex.
    IF sy-subrc = 0.
      PERFORM zf_detalhe_item USING ls_alv.
    ENDIF.
  ENDIF.
ENDFORM.

FORM zf_detalhe_item USING ps_alv TYPE dbtablog.
  DATA:
*    t_dblog    TYPE  /bev2/ed_dblog,
*    t_tables   TYPE  /bev2/ed_tables,
    lt_detalhe TYPE TABLE OF /bev2/ed_fields_s,
    ls_detalhe TYPE /bev2/ed_fields_s.

* Mostra valores antes e depois, porem leva uma eternidade para carregar...
*  APPEND ps_alv-tabname TO t_tables.
*  TRY.
*      CALL FUNCTION '/BEV2/ED_READ_DBLOG'
*        EXPORTING
*          from_day       = ps_alv-logdate
*          from_time      = ps_alv-logtime
*          to_day         = ps_alv-logdate
*          to_time        = ps_alv-logtime
*        TABLES
*          t_dblog        = t_dblog
*          t_tables       = t_tables
*        EXCEPTIONS
*          data_not_found = 1
*          tab_not_found  = 2
*          OTHERS         = 3.
*      IF sy-subrc <> 0.
*        EXIT.
*      ENDIF.
*  ENDTRY.

  PERFORM zf_decodifica_dbtablog
    TABLES lt_detalhe
    USING ps_alv-tabname ps_alv-logid ps_alv-logdate ps_alv-logtime.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_structure_name      = '/BEV2/ED_FIELDS_S'
      i_screen_start_column = 10
      i_screen_start_line   = 5
      i_screen_end_column   = 180
      i_screen_end_line     = 30
    TABLES
      t_outtab              = lt_detalhe "t_dblog-fields
    EXCEPTIONS
      program_error         = 1
      OTHERS                = 2.

ENDFORM.

FORM zf_decodifica_dbtablog
  TABLES t_detalhe STRUCTURE /bev2/ed_fields_s
  USING p_tabname p_logid TYPE logid p_logdate p_logtime.

  DATA:
    it_tab_struc TYPE STANDARD TABLE OF dfies,
    lv_dataln    TYPE dbtablog-dataln,
    lv_logdata   TYPE dbtablog-logdata,
    wa_fields    TYPE /bev2/ed_fields_s,
    lt_history   TYPE ddnthisttb,
    lv_date      TYPE ddrefstruc-dddate,
    lv_time      TYPE ddrefstruc-ddtime.
  FIELD-SYMBOLS:
    <logdata>      TYPE any,
    <wa_tab_struc> TYPE dfies,
    <fs_history>   TYPE ddnthist,
    <fs_fields>    TYPE x031l.

  SELECT SINGLE dataln, logdata INTO (@lv_dataln,@lv_logdata)
    FROM dbtablog
    WHERE logdate = @p_logdate
      AND logtime = @p_logtime
      AND logid = @p_logid.

  CHECK sy-subrc = 0.
  lv_logdata = lv_logdata+1. "remover 1 byte

** Buscar estrutura atual
  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      tabname        = p_tabname
      langu          = sy-langu
    TABLES
      dfies_tab      = it_tab_struc
    EXCEPTIONS
      not_found      = 1
      internal_error = 2
      OTHERS         = 3.

** Buscar estrutura historica
  lv_date = p_logdate.
  lv_time = p_logtime.

  CALL FUNCTION 'DD_NTAB_HIST_GET'
    EXPORTING
      date_high         = lv_date
      time_high         = lv_time
      tabname           = p_tabname
    TABLES
      nthist            = lt_history
    EXCEPTIONS
      not_found         = 1
      invalid_parameter = 2
      OTHERS            = 3.

  IF sy-subrc = 0 AND lt_history[] IS NOT INITIAL.
    READ TABLE lt_history ASSIGNING <fs_history> INDEX lines( lt_history ).
    IF sy-subrc = 0 AND p_logdate && p_logtime < <fs_history>-crtimestmp.
      UNASSIGN <fs_history>.
    ENDIF.
  ENDIF.

  LOOP AT it_tab_struc ASSIGNING <wa_tab_struc>.
    CLEAR wa_fields.

    wa_fields-fieldname = <wa_tab_struc>-fieldname.
    wa_fields-fieldtext = <wa_tab_struc>-scrtext_l.

    IF <wa_tab_struc>-inttype = 'I'.
      CONTINUE. " FIXME: evitar erro INT4
    ENDIF.

    IF <fs_history> IS ASSIGNED.
      " layout historico
      READ TABLE <fs_history>-fields ASSIGNING <fs_fields> WITH KEY fieldname = <wa_tab_struc>-fieldname.
      IF sy-subrc NE 0.
        CONTINUE. " campo nao existia na epoca
      ENDIF.
      IF <fs_fields>-exid = 'P'.
        ASSIGN: lv_logdata+<fs_fields>-offset(<fs_fields>-dblength)
                TO <logdata> TYPE <fs_fields>-exid
                DECIMALS <fs_fields>-decimals.
      ELSE.
        ASSIGN: lv_logdata+<fs_fields>-offset(<fs_fields>-dblength)
                TO <logdata> TYPE <fs_fields>-exid.
      ENDIF.
    ELSE.
      " layout atual
      IF <wa_tab_struc>-inttype = 'P'.
        ASSIGN: lv_logdata+<wa_tab_struc>-offset(<wa_tab_struc>-intlen)
                TO <logdata> TYPE <wa_tab_struc>-inttype
                DECIMALS <wa_tab_struc>-decimals.
      ELSE.
        ASSIGN: lv_logdata+<wa_tab_struc>-offset(<wa_tab_struc>-intlen)
                TO <logdata> TYPE <wa_tab_struc>-inttype.
      ENDIF.
    ENDIF.

    IF <logdata> IS NOT ASSIGNED.
      CONTINUE.
    ENDIF.

    IF <wa_tab_struc>-inttype = 'D'.
      IF NOT <logdata> IS INITIAL.
        WRITE <logdata> TO wa_fields-field_new.
      ELSE.
        CLEAR wa_fields-field_new.
      ENDIF.
    ELSE.
      wa_fields-field_new = <logdata>.
    ENDIF.

    APPEND wa_fields TO t_detalhe.
  ENDLOOP.
ENDFORM.

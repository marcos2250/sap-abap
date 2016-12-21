*&------------------------------------------------------------------------------*
*& Report  ZSRMP_UPDATE_CONTRACTS
*& Uses BBP_PROCDOC_UPDATE to batch update SRM contracts from a given CSV file.
*&------------------------------------------------------------------------------*
REPORT ZSRMP_ALTERA_CONTRATOS.

CONSTANTS:
  c_msgid       TYPE sy-msgid                    VALUE 'ZSRM',
  c_msgty_e     TYPE sy-msgty                    VALUE 'E',
  c_stype_erp   TYPE bbp_logsys_keys-sys_type    VALUE 'ERP',
  c_stype_local TYPE bbp_logsys_keys-sys_type    VALUE 'LOCAL',
  c_numeros(10) TYPE c                           VALUE '1234567890',
  S_VERMELHO(4) TYPE c VALUE '@0A@',
  S_AMARELO(4)  TYPE c VALUE '@09@',
  S_VERDE(4)    TYPE c VALUE '@08@',
  S_ATENCAO(4)  TYPE c VALUE '@1A@'.

TYPES:
  BEGIN OF ty_arq, "CSV file line data
    object_id            TYPE bbp_pds_ctr_header_ic-OBJECT_ID, "numero SAP do contrato
    description          TYPE bbp_pds_ctr_header_ic-description, "Descrição
    currency             TYPE bbp_pds_ctr_header_ic-currency, "Moeda
    vper_start           TYPE bbp_pds_ctr_header_ic-vper_start, "Data Inicio
    zzdata_assinatura(8) TYPE c, "Data assinatura do contrato
    vper_end             TYPE bbp_pds_ctr_header_ic-vper_end, "Data Fim
    pmnttrms             TYPE bbp_pds_ctr_header_ic-pmnttrms, "Cond Pagto
    incoterm_key         TYPE bbp_pds_ctr_header_ic-incoterm_key, "Cond Frete
    incoterm_loc         TYPE bbp_pds_ctr_header_ic-incoterm_loc, "Texto Cond Frete
    co_code              TYPE bbp_pds_ctr_header_ic-co_code, "empresa
    total_value          TYPE char20, "bbp_pds_ctr_header_ic-total_value, "Valor cabeçalho
    zzprorrogavel        TYPE bbp_pds_ctr_header_ic-zzprorrogavel, "Prorrogável até
    zzobjeto_contratado  TYPE bbp_pds_ctr_header_ic-zzobjeto_contratado, "Desc. obj. contratado
    tar_hval             TYPE char17, "bbp_pds_ctr_header_ic-tar_hval, "Alerta valor cabeçalho
    expiry_days          TYPE bbp_pds_ctr_header_ic-expiry_days, " Alerta dias
    process_type         TYPE bbp_pds_ctr_header_ic-process_type, " Tipo de contrato (ZADM, etc)
    zznumero_contrato    TYPE bbp_pds_ctr_header_ic-zznumero_contrato, " Numero contrato antigo
    ps_guarant_min_po    TYPE char20, "TYPE bbp_pds_ctr_header_ic-ps_guarant_min_po, "Min garantido
    description_estr     TYPE bbp_pds_ctr_item_icu-description, "em branco

    quantity             TYPE char17, "TYPE bbp_pds_ctr_item_icu-quantity, "item - quantidade
    ordered_prod         TYPE bbp_pds_ctr_item_icu-ordered_prod, " codigo do material
    price                TYPE char16, "TYPE bbp_pds_ctr_item_icu-price, "preço unitário
    value                TYPE char20, "TYPE bbp_pds_ctr_item_icu-value, "valor fixado
    guaranteed_min       TYPE char20, "TYPE bbp_pds_ctr_item_icu-guaranteed_min, "min. garantido
    icc_br_taxcode       TYPE bbp_pds_ctr_item_icu-icc_br_taxcode, "IVA
    tar_ival             TYPE char17, "TYPE bbp_pds_ctr_item_icu-tar_ival, "Alerta consumidor
    tar_qty              TYPE char17, "TYPE bbp_pds_ctr_item_icu-tar_qty, "Alerta quantidade
    partner_id_forn      TYPE bbp_pds_partner-partner_id, "Codigo fornecedor
    partner_id_cenc      TYPE bbp_pds_partner-partner_id, "Centro cabeçalho
    partner_id_rece      TYPE bbp_pds_partner-partner_id, "Recebedor
    partner_id_empr      TYPE bbp_pds_partner-partner_id, "Empregado
    partner_id_orga      TYPE bbp_pds_partner-partner_id, "Organiz. compras
    partner_id_gest      TYPE bbp_pds_partner-partner_id, "Gestor do contrato
    partner_id_merc      TYPE bbp_pds_partner-partner_id, "Fornecedor mercadoria
    partner_id_ceni      TYPE bbp_pds_partner-partner_id, "Centro item
    partner_id_fisc      TYPE bbp_pds_partner-partner_id, "Fiscal contrato
    partner_id_gere      TYPE bbp_pds_partner-partner_id, "Gerente resp.
    partner_id_emis      TYPE bbp_pds_partner-partner_id, "Emissor da fatura
    grupo_comp           TYPE char10, "Grupo comprador (ex. B01, G03, 001)
    organ_comp           TYPE char10, "Organização de compras - 0001
    distr_perc           TYPE char6, "TYPE bbp_pds_acc-distr_perc, "Distribuição percentual

    acc_cat              TYPE bbp_pds_acc-acc_cat, "Cat. class. contábil - AC ou CC
    g_l_acct             TYPE bbp_pds_acc-g_l_acct, "Conta razão
    cost_ctr             TYPE bbp_pds_acc-cost_ctr, "Centro de custo Atual
    cost_ctr_alt         TYPE bbp_pds_acc-cost_ctr, "Centro de custo ALTERADO ***
    asset_no             TYPE bbp_pds_acc-asset_no, "Ativo fixo Atual
    asset_no_alt         TYPE bbp_pds_acc-asset_no, "Ativo fixo ALTERADO ***
    order_no             TYPE bbp_pds_acc-order_no, "Ordem Atual
    order_no_alt         TYPE bbp_pds_acc-order_no, "Ordem ALTERADO ***
    wbs_elem_e           TYPE bbp_pds_acc-wbs_elem_e, "Elemento PEP ?
    network              TYPE bbp_pds_acc-network, "Diagrama rede ?
    dist_quan            TYPE char17, "Distrib. quantidade ?
    dist_value           TYPE char17, "Distrib. valor
    dist_ind             TYPE bbp_dist_ind, "Indicador distrib. V ou vazio
    bus_area             TYPE bbp_pds_acc-bus_area, "Divisão
    ref_date             TYPE bbp_pds_acc-ref_date, "Dt. referência
    funds_ctr            TYPE fistl,                 "Centro financeiro
    cmmt_item            TYPE fm_fipex,              "Item financeiro - nr. conta razão
    dist_quan_2          TYPE char17, "Quantidade ano+1
    dist_value_2         TYPE char17, "Valor ano+1
    dist_quan_3          TYPE char17, "Quantidade ano+2
    dist_value_3         TYPE char17, "Valor ano+2
    dist_quan_4          TYPE char17, "Quantidade ano+3
    dist_value_4         TYPE char17, "Valor ano+3
    dist_quan_5          TYPE char17, "Quantidade ano+4
    dist_value_5         TYPE char17, "Valor ano+4
    indice               TYPE syst-tabix,
  END OF ty_arq,


  BEGIN OF ty_tcurc,
    waers TYPE tcurc-waers,
  END OF ty_tcurc,

  BEGIN OF ty_msg_log,
    msgid TYPE sy-msgid,
    msgty TYPE sy-msgty,
    msgno TYPE sy-msgno,
    msgv1 TYPE sy-msgv1,
    msgv2 TYPE sy-msgv2,
    msgv3 TYPE sy-msgv3,
    msgv4 TYPE sy-msgv4,
  END OF ty_msg_log,

  BEGIN OF ty_log,
    ind     TYPE char4,
    msgid   TYPE sy-msgid,
    message TYPE char250,
  END OF ty_log,


  BEGIN OF key_idx_badi,
    idx_est_material TYPE int4,
    idx_material     TYPE int4,
    idx_acc_conta    TYPE int4,
    idx_exsnr        TYPE int4,
    ps_handle_itm    TYPE bbp_pds_ctr_item_icu-ps_handle_itm,
    idx_linha_ini    TYPE int4,
    idx_linha_fim    TYPE int4,
  END OF key_idx_badi,

  BEGIN OF ty_guids_contas,
    guid_conta type BBP_GUID,
    guid_item type BBP_GUID,
    ordered_prod type bbp_pds_ctr_item_icu-ordered_prod,
    COST_CTR type bbp_pds_acc-COST_CTR,
    ASSET_NO type bbp_pds_acc-ASSET_NO,
    ORDER_NO type bbp_pds_acc-ORDER_NO,
    ano type i,
  END OF ty_guids_contas.


DATA:
  gt_log_ret TYPE TABLE OF ty_log,
  gt_arq     TYPE TABLE OF ty_arq,
  gt_curc    TYPE TABLE OF ty_tcurc,
  gt_cont_i  TYPE BBPT_PD_ITEM, "bbp_pds_ctr_item_icu,
  "gt_cont_i  TYPE TABLE OF bbp_pds_ctr_item_icu,
  gt_acc     TYPE TABLE OF bbp_pds_acc,
  gt_partner TYPE TABLE OF bbp_pds_partner,
  gt_orgdata TYPE TABLE OF bbp_pds_org,
  gt_bapimsg TYPE TABLE OF bbp_pds_messages,
  gt_msglog  TYPE TABLE OF ty_msg_log,
  gt_eitem   TYPE TABLE OF bbp_pds_ctr_item_d,
  gt_conditions type BBPT_PD_CND,

  gt_ld_contas type table of ty_guids_contas,
  gt_ld_contas_comp type table of BBP_PDS_ACC,
  gt_ld_itens type table of BBP_PDS_CTR_ITEM_D,
  gt_ld_partners type table of BBP_PDS_PARTNER,
  gt_ld_orgdata type table of BBP_PDS_ORG,
  gt_ld_conditions type BBPT_PD_CND_D.


DATA:
  gs_log_ret  TYPE ty_log,
  gs_log      TYPE bal_s_log,
  gs_arq      TYPE ty_arq,
  gs_cont_h   TYPE BBP_PDS_HEADER,
  gs_cont_i   LIKE LINE OF gt_cont_i,
  gs_acc      LIKE LINE OF gt_acc,
  gs_partner  LIKE LINE OF gt_partner,
  gs_orgdata  LIKE LINE OF gt_orgdata,
  gs_bapimsg  LIKE LINE OF gt_bapimsg,
  gs_eitem    LIKE LINE OF gt_eitem,
  gs_key_badi TYPE key_idx_badi.

DATA: gv_data(8)       TYPE c,
      gv_erro          TYPE c,
      gv_linha_arquivo TYPE i.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
  SELECTION-SCREEN SKIP.
  PARAMETER: p_file(1024) TYPE c OBLIGATORY.
  SELECTION-SCREEN SKIP.
  PARAMETER: p_teste AS CHECKBOX DEFAULT abap_true.
SELECTION-SCREEN END   OF BLOCK b1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f4_arquivo.


*-------------------------------------------------------------------------
*	START-OF-SELECTION
*-------------------------------------------------------------------------
START-OF-SELECTION.

  PERFORM le_arquivo.
  PERFORM cria_contratos.
  perform log.



FORM le_arquivo.
  DATA: lt_arq_raw TYPE TABLE OF string. "Dados do arquivo na importação
  DATA: ls_arq_raw LIKE LINE OF lt_arq_raw.
  DATA: lv_extensao(1024) TYPE c,      "Extensão do arquivo
        lv_filename       TYPE string. "Caminho do arquivo

  FREE gt_arq.

  CALL FUNCTION 'TRINT_FILE_GET_EXTENSION'
    EXPORTING
      filename  = p_file
    IMPORTING
      extension = lv_extensao.

  IF lv_extensao NE 'CSV'.
    MESSAGE s002(zsrm) DISPLAY LIKE 'E'.
    gv_erro = abap_true.
    EXIT.
  ENDIF.

  lv_filename = p_file.

  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename                = lv_filename
    CHANGING
      data_tab                = lt_arq_raw
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      not_supported_by_gui    = 17
      error_no_gui            = 18
      OTHERS                  = 19.
  IF sy-subrc EQ 0.

    "*** REMOVE CSV HEADERS *** 
    DELETE lt_arq_raw INDEX 1. 
    DELETE lt_arq_raw INDEX 1.

    SELECT waers FROM tcurc INTO TABLE gt_curc.
    IF sy-subrc EQ 0.
      SORT gt_curc BY waers.
    ENDIF.

    CLEAR gv_linha_arquivo.
    LOOP AT lt_arq_raw INTO ls_arq_raw.

      gv_linha_arquivo = gv_linha_arquivo + 1.
      CLEAR gs_arq.
      SPLIT ls_arq_raw AT ';' INTO
        gs_arq-OBJECT_ID
        gs_arq-description
        gs_arq-currency
        gs_arq-vper_start
        gs_arq-zzdata_assinatura
        gs_arq-vper_end
        gs_arq-pmnttrms
        gs_arq-incoterm_key
        gs_arq-incoterm_loc
        gs_arq-co_code
        gs_arq-total_value
        gs_arq-zzprorrogavel
        gs_arq-zzobjeto_contratado
        gs_arq-tar_hval
        gs_arq-expiry_days
        gs_arq-process_type
        gs_arq-zznumero_contrato
        gs_arq-ps_guarant_min_po
        gs_arq-description_estr
        gs_arq-quantity
        gs_arq-ordered_prod
        gs_arq-price
        gs_arq-value
        gs_arq-guaranteed_min
        gs_arq-icc_br_taxcode
        gs_arq-tar_ival
        gs_arq-tar_qty
        gs_arq-partner_id_forn
        gs_arq-partner_id_cenc
        gs_arq-partner_id_rece
        gs_arq-partner_id_empr
        gs_arq-partner_id_orga
        gs_arq-partner_id_gest
        gs_arq-partner_id_merc
        gs_arq-partner_id_ceni
        gs_arq-partner_id_fisc
        gs_arq-partner_id_gere
        gs_arq-partner_id_emis
        gs_arq-grupo_comp
        gs_arq-organ_comp
        gs_arq-distr_perc
        gs_arq-acc_cat
        gs_arq-g_l_acct
        gs_arq-cost_ctr
        gs_arq-cost_ctr_alt
        gs_arq-asset_no
        gs_arq-asset_no_alt
        gs_arq-order_no
        gs_arq-order_no_alt
        gs_arq-wbs_elem_e
        gs_arq-network
        gs_arq-dist_quan
        gs_arq-dist_value
        gs_arq-dist_ind
        gs_arq-bus_area
        gs_arq-ref_date
        gs_arq-funds_ctr
        gs_arq-cmmt_item
        gs_arq-dist_quan_2
        gs_arq-dist_value_2
        gs_arq-dist_quan_3
        gs_arq-dist_value_3
        gs_arq-dist_quan_4
        gs_arq-dist_value_4
        gs_arq-dist_quan_5
        gs_arq-dist_value_5.

      REPLACE ';' IN gs_arq-network WITH ''.
      gs_arq-indice = gv_linha_arquivo.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = gs_arq-icc_br_taxcode
        IMPORTING
          output = gs_arq-icc_br_taxcode.

      APPEND gs_arq TO gt_arq.
    ENDLOOP.

  ELSE.
    MESSAGE text-e01 TYPE 'S' DISPLAY LIKE 'E'.
    gv_erro = abap_true.
  ENDIF.
ENDFORM.

FORM f4_arquivo .
  DATA: lt_filetab     TYPE filetable,
        ls_filetab     LIKE LINE OF lt_filetab,
        lv_file_filter TYPE string,
        lv_rc          TYPE i.

  CONCATENATE cl_gui_frontend_services=>filetype_excel '' INTO lv_file_filter.
  REPLACE '|*.' IN lv_file_filter WITH '|*.CSV;*.'.
  lv_file_filter = '*.CSV'.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      file_filter             = lv_file_filter
    CHANGING
      file_table              = lt_filetab
      rc                      = lv_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF sy-subrc EQ 0. "Obtém o caminho do arquivo
    READ TABLE lt_filetab INTO ls_filetab INDEX 1.
    p_file = ls_filetab-filename.
  ELSE.
    MESSAGE text-e02 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.


FORM cria_contratos .
  PERFORM limpa_tabelas_badi.

  SORT gt_arq BY indice.

  LOOP AT gt_arq INTO gs_arq.
    gs_key_badi-idx_linha_fim = gs_arq-indice.

    IF gs_arq-currency IS NOT INITIAL OR gs_arq-co_code IS NOT INITIAL
       OR gs_arq-process_type IS NOT INITIAL .
          IF sy-tabix > 1.
            PERFORM cria_contrato.
          ENDIF.
    ENDIF.

    PERFORM set_header_data.

    if gv_erro is initial.
        PERFORM set_item_data.
        PERFORM set_account_data.
        PERFORM set_partner_data.
        PERFORM set_org_data.
    endif.

  ENDLOOP.

*** last one
  IF gs_cont_h IS NOT INITIAL.
    PERFORM cria_contrato.
    PERFORM limpa_tabelas_badi.
    CLEAR: gv_erro.
  ENDIF.

ENDFORM.


FORM converte_data  USING VALUE(p_campo) VALUE(p_data_raw) p_data_ret TYPE sy-datum.
  IF p_data_raw is initial or p_data_raw = ''.
    RETURN.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_SDATE_INPUT'
    EXPORTING
      input  = p_data_raw
    IMPORTING
      output = p_data_ret.

  IF p_data_ret IS INITIAL.
    PERFORM write_msg USING:
    006 gv_linha_arquivo gs_arq-indice p_campo text-e09.
  ENDIF.
ENDFORM.


FORM write_msg  USING VALUE(p_msgno) VALUE(p_msgv1) VALUE(p_msgv2) VALUE(p_msgv3) VALUE(p_msgv4).
  DATA: lv_msg TYPE string.

  IF gv_erro  IS INITIAL AND "Se for o primeiro erro encontrado
     sy-batch IS INITIAL.    "E somente na execução online do programa
    WRITE: / text-e03.
  ENDIF.

  MESSAGE ID c_msgid TYPE c_msgty_e NUMBER p_msgno
     WITH p_msgv1 p_msgv2 p_msgv3 p_msgv4
     INTO lv_msg.

  WRITE: / lv_msg.

* Atribui erro
  gv_erro = abap_true.

ENDFORM.

FORM valida_valor  USING    p_input p_campo p_posic p_cadec CHANGING p_output.
  DATA: lv_strlen TYPE i,      "Tamanho de string
        lv_sub    TYPE i,      "Nro para subtrair do STRLEN
        lv_msg    TYPE string, "Mensagem para exibição
        lv_input  TYPE string. "Input string para manipulação de texto

  CHECK p_input IS NOT INITIAL.

  lv_input = p_input.
  lv_strlen = strlen( lv_input ).
  lv_sub = p_cadec + 1.

  SUBTRACT lv_sub FROM lv_strlen.
  IF lv_strlen LT 0.
    lv_strlen = 0.
  ENDIF.

  IF lv_input+lv_strlen(1) EQ ',' OR lv_input+lv_strlen(1) CO c_numeros.
    CALL FUNCTION 'CHAR_FLTP_CONVERSION'
      EXPORTING
        string             = lv_input
        maxdec             = p_cadec
        maxexp             = p_posic
      IMPORTING
        flstr              = p_output
      EXCEPTIONS
        exponent_too_big   = 1
        exponent_too_small = 2
        string_not_fltp    = 3
        too_many_decim     = 4
        OTHERS             = 5.

  IF sy-subrc <> 0.
      lv_msg = text-e10.
      REPLACE '&1' IN lv_msg WITH p_posic.
      REPLACE '&2' IN lv_msg WITH p_cadec.
      PERFORM write_msg USING: 006 gv_linha_arquivo gs_arq-indice p_campo lv_msg.
    ENDIF.
  ELSE.
    lv_msg = text-e10.
    REPLACE '&1' IN lv_msg WITH p_posic.
    REPLACE '&2' IN lv_msg WITH p_cadec.
    PERFORM write_msg USING:
    006 gv_linha_arquivo gs_arq-indice p_campo lv_msg.
  ENDIF.
ENDFORM.


FORM set_header_data.

* Verifica se a linha descreve um header
    IF gs_arq-OBJECT_ID is initial.
    "IF gs_arq-description is initial.
        return.
    ENDIF.

    PERFORM limpa_tabelas_badi.

    gs_key_badi-idx_linha_ini = gs_arq-indice.

* Gera o GUID
    perform carrega_contrato.

* Busca o sistema lógico
    "PERFORM get_logsys USING c_stype_erp CHANGING gs_cont_h-logsys_fi.

    if gs_arq-currency is not initial.
        READ TABLE gt_curc TRANSPORTING NO FIELDS WITH KEY waers = gs_arq-currency.
        IF sy-subrc EQ 0.
          gs_cont_h-currency = gs_arq-currency.
        ENDIF.
    endif.

    if gs_arq-vper_start <> ''.
        gv_data = gs_arq-vper_start.
        CONDENSE gv_data NO-GAPS.
        UNPACK gv_data TO gv_data.
        gs_arq-vper_start = gv_data.
    endif.

    if gs_arq-vper_end <> ''.
        gv_data = gs_arq-vper_end.
        CONDENSE gv_data NO-GAPS.
        UNPACK gv_data TO gv_data.
        gs_arq-vper_end = gv_data.
    endif.

    if gs_arq-zzprorrogavel <> ''.
        gv_data = gs_arq-zzprorrogavel.
        CONDENSE gv_data NO-GAPS.
        UNPACK gv_data TO gv_data.
        gs_arq-zzprorrogavel = gv_data.
    endif.

    PERFORM converte_data USING:
          '' gs_arq-vper_start    gs_cont_h-vper_start,
          '' gs_arq-vper_end      gs_cont_h-vper_end,
          '' gs_arq-zzprorrogavel gs_cont_h-zzprorrogavel,
          '' gs_arq-zzdata_assinatura gs_cont_h-zzdata_assinatura.

    if gs_arq-total_value is not initial.
        REPLACE ALL OCCURRENCES OF '.' IN gs_arq-total_value WITH ''.
        CONDENSE gs_arq-total_value NO-GAPS.
        PERFORM valida_valor USING gs_arq-total_value 'TOTAL_VALUE' '15' '2' CHANGING gs_cont_h-total_value.
    endif.

    if gs_arq-expiry_days > 0.
        MOVE gs_arq-expiry_days TO gs_cont_h-expiry_days.
        UNPACK gs_cont_h-expiry_days TO gs_cont_h-expiry_days.
    endif.

*   Mínimo garantido Cabeçalho
    IF NOT gs_arq-ps_guarant_min_po IS INITIAL.
      gs_cont_h-ps_gm_level = 'HDR'.
      REPLACE ALL OCCURRENCES OF '.' IN gs_arq-ps_guarant_min_po WITH ''.
      CONDENSE gs_arq-ps_guarant_min_po NO-GAPS.
      PERFORM valida_valor USING gs_arq-ps_guarant_min_po 'MIN_GARANTEED_PO' '15' '2' CHANGING gs_cont_h-guaranteed_min.
    ENDIF.

* Transfere os valores dos demais campos
    perform nullSafeSet using gs_arq-description          changing gs_cont_h-description.
    perform nullSafeSet using gs_arq-pmnttrms             changing gs_cont_h-pmnttrms.
    perform nullSafeSet using gs_arq-incoterm_key         changing gs_cont_h-incoterm_key.
    perform nullSafeSet using gs_arq-incoterm_loc         changing gs_cont_h-incoterm_loc.
    perform nullSafeSet using gs_arq-co_code              changing gs_cont_h-co_code.
    perform nullSafeSet using gs_arq-zzobjeto_contratado  changing gs_cont_h-zzobjeto_contratado.
    perform nullSafeSet using gs_arq-process_type         changing gs_cont_h-process_type.
    perform nullSafeSet using gs_arq-zznumero_contrato    changing gs_cont_h-zznumero_contrato.
    perform nullSafeSet using gs_arq-tar_hval             changing gs_cont_h-tar_hval.

    "gs_cont_h-sk_country          = 'BR'.
    "gs_cont_h-posting_date        = sy-datum.
ENDFORM.

FORM get_logsys  USING p_systype TYPE bbp_logsys_keys-sys_type
              CHANGING p_logsys  TYPE bbp_logsys_keys-log_sys.
  DATA: lt_logsyss TYPE  bbpt_logsys_keys,
        ls_logsyss LIKE LINE OF lt_logsyss.

  CALL FUNCTION 'BBP_GET_LOGSYSS_F4'
    TABLES
      et_logsyss = lt_logsyss.

  LOOP AT lt_logsyss INTO ls_logsyss.
    IF ls_logsyss-sys_type CS p_systype.
      p_logsys = ls_logsyss-log_sys.
      EXIT.
    ENDIF.
  ENDLOOP.
ENDFORM.


FORM set_item_data.
    DATA: lt_prod     TYPE bbpt_product_keys,
          ls_prod     LIKE LINE OF lt_prod,
          lv_prod_id  TYPE comt_product_id,
          lv_idx_mate TYPE int4 VALUE 1.

* Verifica se linha descreve item de contrato
    IF gs_arq-ordered_prod IS INITIAL.
          return.
    endif.

    CLEAR gs_cont_i.

* *** Copia valores originais
    perform atualiza_guid_item.
    data itemOriginal type BBP_PDS_CTR_ITEM_D.
    read table gt_ld_itens into itemOriginal with key GUID = gs_cont_i-GUID.
    CLEAR gs_cont_i.
    MOVE-CORRESPONDING itemOriginal TO gs_cont_i.

*  **** Verifica se o tipo e estrutura
    IF gs_arq-description_estr IS NOT INITIAL.
        perform logErro using 'Item do tipo - estrutura - não é suportado' gs_cont_h-DESCRIPTION.
    ENDIF.

    "IF gs_cont_h-ps_gm_level <> 'HDR'. "inclusao
    "  CLEAR gv_acc_no.
    "ENDIF.

    gs_key_badi-idx_material = gs_key_badi-idx_material + 1.
    gs_key_badi-idx_exsnr    = gs_key_badi-idx_exsnr + 1.

*  Dados basicos
    gs_cont_i-guid = itemOriginal-GUID.
    gs_cont_i-parent = gs_cont_h-guid.
    gs_cont_i-number_int = gs_key_badi-idx_material.
    gs_cont_i-ordered_prod = gs_arq-ordered_prod.

* Converte o valor para entrar na função que busca as informações do produto
    CALL FUNCTION 'CONVERSION_EXIT_PRID1_INPUT'
       EXPORTING
         input  = gs_cont_i-ordered_prod
       IMPORTING
         output = lv_prod_id.

* Transfere as informações do produto
    CALL FUNCTION 'BBP_GET_PRODUCTS_F4'
       EXPORTING
         iv_product_id               = lv_prod_id
       TABLES
         et_products                 = lt_prod
       EXCEPTIONS
         error_reading_hierarchy     = 1
         error_reading_fragment_type = 2
         customizing_error           = 3
         OTHERS                      = 4.

     IF sy-subrc EQ 0 AND lt_prod IS NOT INITIAL.
       CLEAR ls_prod.
       READ TABLE lt_prod INTO ls_prod INDEX 1.
       gs_cont_i-product      = ls_prod-product_guid.
     ENDIF.

*  Transfere as informações de sistema lógico (somente inclusao)
"    PERFORM get_logsys:
"       USING c_stype_erp    CHANGING gs_cont_i-src_log_sys,
"       USING c_stype_local  CHANGING gs_cont_i-product_src_sys,
"       USING c_stype_erp    CHANGING gs_cont_i-logsys_fi.

    if gs_arq-price is not initial.
        REPLACE ALL OCCURRENCES OF '.' IN gs_arq-price WITH ''.
        CONDENSE gs_arq-price NO-GAPS.
        PERFORM valida_valor USING gs_arq-price 'PRICE' '13' '2' CHANGING gs_cont_i-price.
    endif.

    if gs_arq-guaranteed_min is not initial.
        REPLACE ALL OCCURRENCES OF '.' IN gs_arq-guaranteed_min WITH ''.
        CONDENSE gs_arq-guaranteed_min NO-GAPS.
        PERFORM valida_valor USING gs_arq-guaranteed_min 'GUARANTEED_MIN' '15' '2' CHANGING gs_cont_i-guaranteed_min.
    endif.

    if gs_arq-value is not initial.
        REPLACE ALL OCCURRENCES OF '.' IN gs_arq-value WITH ''.
        CONDENSE gs_arq-value NO-GAPS.
        PERFORM valida_valor USING gs_arq-value 'VALUE' '15' '2' CHANGING gs_cont_i-value.
    endif.

    if gs_arq-quantity is not initial.
        PERFORM valida_valor USING gs_arq-quantity 'QUANTITY' '15' '2' CHANGING gs_cont_i-quantity.
    endif.

*  ** Transfere as informações dos demais campos
    perform nullSafeSet using gs_arq-icc_br_taxcode changing gs_cont_i-icc_br_taxcode.
    perform nullSafeSet using gs_arq-tar_ival changing gs_cont_i-tar_ival.
    perform nullSafeSet using gs_arq-tar_ival changing gs_cont_i-tar_qty.

    gs_cont_i-itm_released      = abap_true.
    gs_cont_i-co_code           = gs_cont_h-co_code.
    gs_cont_i-currency          = gs_cont_h-currency.
    gs_cont_i-ps_hl_item        = gs_key_badi-ps_handle_itm.
    gs_cont_i-ps_exsnr          = gs_key_badi-idx_exsnr.
*    gs_cont_i-product_type      = '01'.
*    gs_cont_i-ps_ctrl_key       = '0002'.
*    gs_cont_i-ps_ipt            = 'MAT_ITM'.
*    gs_cont_i-item_process_typ  = 'MATL'.
    gs_cont_i-ps_handle_itm = itemOriginal-PS_HANDLE_ITM.

    APPEND gs_cont_i TO gt_cont_i.

ENDFORM.


FORM set_account_data.
    DATA: lv_data(8) TYPE c.

*   Verifica se linha descreve uma classificação contábil.
    IF gs_arq-COST_CTR is initial and gs_arq-ASSET_NO is initial and gs_arq-ORDER_NO is initial.
        return.
    endif.

    if gs_arq-COST_CTR_ALT is not initial and gs_arq-COST_CTR_ALT ne gs_arq-FUNDS_CTR.
        perform logErro using 'Centro de Custo a alterar deve ser igual Centro Financeiro' gs_arq-ORDERED_PROD.
        exit.
    endif.

    clear gs_acc.

* Gera o GUID
    DATA: lv_ano(4) TYPE n.
    lv_ano = gs_cont_h-vper_start(4).
    PERFORM atualiza_guid_conta USING lv_ano CHANGING gs_acc-GUID.

* Copia valores originais
    read table gt_ld_contas_comp into gs_acc with key GUID = gs_acc-GUID.

* Transfere as informações dos demais campos
    perform nullSafeSet using gs_arq-distr_perc changing gs_acc-distr_perc.
    perform nullSafeSet using gs_arq-acc_cat changing gs_acc-acc_cat.
    perform nullSafeSet using gs_arq-g_l_acct changing gs_acc-g_l_acct.
    perform nullSafeSet using gs_arq-bus_area changing gs_acc-bus_area.
    perform nullSafeSet using gs_arq-dist_ind changing gs_acc-dist_ind.
    perform nullSafeSet using gs_arq-funds_ctr changing gs_acc-funds_ctr.

* Reatribui o numero sequencial
    if gs_acc-acc_no is initial.
        gs_acc-acc_no = lines( gt_acc ) + 1.
    endif.

    if gs_arq-COST_CTR_ALT is not initial.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING input  = gs_arq-cost_ctr_alt IMPORTING output = gs_acc-cost_ctr.
    endif.

    if gs_arq-ORDER_NO_ALT is not initial.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING input  = gs_arq-order_no_alt IMPORTING output = gs_acc-order_no.
    endif.

    if gs_arq-ASSET_NO_ALT is not initial.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING input  = gs_arq-asset_no_alt IMPORTING output = gs_acc-asset_no.
    endif.

    if gs_arq-network is not initial.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING input  = gs_arq-network IMPORTING output = gs_acc-network.
    endif.

    if gs_arq-wbs_elem_e is not initial.
        CALL FUNCTION 'CONVERSION_EXIT_ABPSN_INPUT'
          EXPORTING input  = gs_arq-wbs_elem_e IMPORTING output = gs_acc-wbs_elem_e.
    endif.

    if gs_arq-cmmt_item is not initial.
        CALL FUNCTION 'CONVERSION_EXIT_FMCIL_INPUT'
          EXPORTING input  = gs_arq-cmmt_item IMPORTING output = gs_acc-cmmt_item.
    endif.

*   Quantidade em UM do Pedido
    if gs_arq-dist_quan is not initial.
        REPLACE ALL OCCURRENCES OF '.' IN gs_arq-dist_quan WITH ''.
        CONDENSE gs_arq-dist_quan NO-GAPS.
        PERFORM: valida_valor USING gs_arq-dist_quan 'DIST_QUAN' '13' '3' CHANGING gs_acc-dist_quan.
    endif.

*   Valor Líquido
    if gs_arq-dist_value is not initial.
        REPLACE ALL OCCURRENCES OF '.' IN gs_arq-dist_value WITH ''.
        CONDENSE gs_arq-dist_value NO-GAPS.
        PERFORM: valida_valor USING gs_arq-dist_value 'DIST_VALUE' '15' '2' CHANGING gs_acc-dist_value.
    endif.

    IF NOT gs_arq-ref_date IS INITIAL.
        lv_data = gs_arq-ref_date.
        CONDENSE lv_data NO-GAPS.
        UNPACK lv_data TO gv_data.
        gs_arq-ref_date = lv_data.
        PERFORM converte_data USING '' gs_arq-ref_date  gs_acc-ref_date.
    ENDIF.

    APPEND gs_acc TO gt_acc.

*   Exercícios posteriores para a mesma classificação contábil
    PERFORM set_additional_acc USING gs_acc  gs_arq-dist_quan_2 gs_arq-dist_value_2.
    PERFORM set_additional_acc USING gs_acc  gs_arq-dist_quan_3 gs_arq-dist_value_3.
    PERFORM set_additional_acc USING gs_acc  gs_arq-dist_quan_4 gs_arq-dist_value_4.
    PERFORM set_additional_acc USING gs_acc  gs_arq-dist_quan_5 gs_arq-dist_value_5.

ENDFORM.


FORM set_partner_data.
* Dados parceiros vem na mesma linha de item de contrato
    IF gs_arq-quantity IS INITIAL and gs_arq-ordered_prod IS INITIAL
       and gs_arq-price IS INITIAL and gs_arq-value IS INITIAL.
            exit.
    endif.

  if gs_arq-partner_id_cenc is not initial or gs_arq-partner_id_ceni is not initial.
      perform logErro using 'Requer alteração manual - Centro Cabeçalho e Centro Item!' gs_cont_h-DESCRIPTION.
      exit.
  endif.

* Adiciona os dados do parceiro de negócio do tipo Empregado ...
  PERFORM manter_parceiro  USING gs_arq-partner_id_forn '00000019'.
  PERFORM manter_parceiro  USING gs_arq-partner_id_cenc '00000075'.
  PERFORM manter_parceiro  USING gs_arq-partner_id_rece '00000020'.
  PERFORM manter_parceiro  USING gs_arq-partner_id_empr '00000026'.
  PERFORM manter_parceiro  USING gs_arq-partner_id_orga '00000051'.
  PERFORM manter_parceiro  USING gs_arq-partner_id_gest '00000700'.
  PERFORM manter_parceiro  USING gs_arq-partner_id_merc '00000712'.
  PERFORM manter_parceiro  USING gs_arq-partner_id_ceni '00000075'.
  PERFORM manter_parceiro  USING gs_arq-partner_id_fisc '00000714'.
  PERFORM manter_parceiro  USING gs_arq-partner_id_gere '00000713'.
  PERFORM manter_parceiro  USING gs_arq-partner_id_emis '00000029'.

ENDFORM.



FORM set_org_data .
  CLEAR: gs_orgdata.

  if gt_orgdata is not initial.
      return.
  endif.

  if gt_ld_orgdata is initial.
      perform logErro using 'Dados da Organizacao nao encontrados!' gs_cont_h-DESCRIPTION.
      return.
  elseif lines( gt_ld_orgdata ) > 2.
      perform logErro using 'Há mais de uma Organizacao no contrato!' gs_cont_h-DESCRIPTION.
      return.
  endif.
  read table gt_ld_orgdata index 1 into gs_orgdata.

  " há alteração em um dos dois valores?

  if gs_arq-organ_comp is not initial.
      DATA ls_search_ekorg TYPE bbps_om_purch_orgx.
      data ls_purch_org    TYPE hrobject.

      ls_search_ekorg-logsys = gs_cont_h-logsys_fi.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = gs_arq-organ_comp
        IMPORTING
          output = ls_search_ekorg-ekorg.

      " Obter o ID do objeto organização de compras
      CALL FUNCTION 'BBP_OM_READ_PURCH_ORG_ID'
        EXPORTING
          is_search_ekorg = ls_search_ekorg
        IMPORTING
          es_purch_org    = ls_purch_org
        EXCEPTIONS
          internal_error  = 1
          no_authority    = 2
          nothing_found   = 3
          OTHERS          = 4.
      IF sy-subrc <> 0.
        PERFORM append_msg USING
        sy-msgid sy-msgno sy-msgty sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      gs_orgdata-proc_org_id = ls_purch_org-objid.
      gs_orgdata-proc_org_ot = ls_purch_org-otype.
   endif.

   if gs_arq-grupo_comp is not initial.
      DATA: ls_search_ekgrp TYPE bbps_om_purch_grpx,
            ls_purch_grp    TYPE hrobject.

      ls_search_ekgrp-logsys = gs_cont_h-logsys_fi.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = gs_arq-grupo_comp
        IMPORTING
          output = ls_search_ekgrp-ekgrp.

      " Obter o ID do objeto grupo de compras
      CALL FUNCTION 'BBP_OM_READ_PURCH_GRP_ID'
        EXPORTING
          is_search_ekgrp = ls_search_ekgrp
        IMPORTING
          es_purch_grp    = ls_purch_grp
        EXCEPTIONS
          internal_error  = 1
          no_authority    = 2
          nothing_found   = 3
          OTHERS          = 4.
      IF sy-subrc <> 0.
        PERFORM append_msg USING
        sy-msgid sy-msgno sy-msgty sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      gs_orgdata-proc_org_resp_id = ls_purch_grp-objid.
      gs_orgdata-proc_org_resp_ot = ls_purch_grp-otype.
      gs_orgdata-proc_group_id = ls_purch_grp-objid.
      gs_orgdata-proc_group_ot = ls_purch_grp-otype.

  ENDIF.

  "gs_orgdata-client = sy-mandt.
  "gs_orgdata-guid = organizacao-GUID. "GUID da organizacao
  "gs_orgdata-p_guid = gs_cont_h-guid. "Associa GUID do cabeçalho
  "gs_orgdata-del_ind = space.

  APPEND gs_orgdata TO gt_orgdata.
ENDFORM.


FORM cria_contrato.
  DATA: lv_idx_linha_ini     TYPE char10,
        lv_idx_linha_fim     TYPE char10,
        lv_idx_linha_ini_aux TYPE int4,
        lv_idx_linha_fim_aux TYPE int4.

  DATA ls_acc                TYPE bbp_pds_acc.
  DATA ls_item               TYPE LINE OF BBPT_PD_ITEM.
  "data ls_item              type bbp_pds_ctr_item_icu. "Itens do contrato
  DATA lt_acc_main           TYPE TABLE OF bbps_budget_filter.
  DATA ls_acc_main           TYPE bbps_budget_filter.
  DATA lt_account            TYPE TABLE OF bbp_pds_acc.
  DATA lt_budget             TYPE TABLE OF bbp_budget_pi.
  DATA ls_budget             TYPE bbp_budget_pi.
  DATA lt_return             TYPE TABLE OF bapiret2.
  DATA lv_budget_open        TYPE bbp_bucurr_pi.
  DATA lv_budget_saldo       TYPE bbp_bucurr_pi.
  DATA lv_check_value        TYPE bbp_bucurr_pi.
  DATA lv_char_buffer        TYPE string.
  DATA lv_msgv1              TYPE symsgv.
  DATA lv_msgv2              TYPE symsgv.
  DATA lv_msgv3              TYPE symsgv.
  DATA lv_msgv4              TYPE symsgv.
  DATA lv_char(16)           TYPE c.

  TYPES: BEGIN OF ly_budget_aggr,
           year(4)     TYPE n,             "Exercício
           cmmt_item   TYPE fm_fipex,      "Item financeiro
           funds_ctr   TYPE fistl,         "Centro financeiro
           budget_open TYPE bbp_bucurr_pi,
           check_value TYPE bbp_bucurr_pi,
         END OF ly_budget_aggr.
  DATA: lt_budget_aggr TYPE STANDARD TABLE OF ly_budget_aggr,
        ls_budget_aggr TYPE ly_budget_aggr.

  DATA: lt_cobl        TYPE bbpcobl_eci OCCURS 0 WITH HEADER LINE.
  DATA: lt_new_cobl    TYPE bbpcobl_eci OCCURS 0 WITH HEADER LINE.
  DATA: lt_return_2    LIKE bapireturn OCCURS 0 WITH HEADER LINE.
  DATA: lt_ctrl_record LIKE bbp_control_record OCCURS 0 WITH HEADER LINE.

  if gv_erro eq abap_true.
      perform logErro using 'Dados informados do contrato contém erros!' gs_cont_h-DESCRIPTION.
      return.
  endif.

* Copia os itens que não foram alterados
  data itemOriginal type BBP_PDS_CTR_ITEM_D.
  loop at gt_ld_itens into itemOriginal.
      read table gt_cont_i with key GUID = itemOriginal-GUID transporting no fields.
      if sy-subrc ne 0.
          "incluir o item inteiro
          clear gs_cont_i.
          MOVE-CORRESPONDING itemOriginal TO gs_cont_i.
          append gs_cont_i to gt_cont_i.
      endif.
  endloop.

* Inclui qualquer outra conta que ficou fora
  loop at gt_ld_contas_comp into gs_acc.
      read table gt_acc with key GUID = gs_acc-GUID transporting no fields.
      if sy-subrc ne 0.
          append gs_acc to gt_acc.
      endif.
  endloop.

* Inclui qualquer outro parceiro que ficou fora
  loop at gt_ld_partners into gs_partner.
      read table gt_partner with key PARTNER_GUID = gs_partner-PARTNER_GUID transporting no fields.
      if sy-subrc ne 0.
          append gs_partner to gt_partner.
      endif.
  endloop.

* Atualiza valores das condições
  data gs_condition type BBP_PDS_CND_D.
  data condicao type BBP_PDS_CND.
  loop at gt_ld_conditions into gs_condition.
      move-corresponding gs_condition to condicao.
      read table gt_cont_i with key GUID = condicao-P_GUID into gs_cont_i.
      if sy-subrc = 0 and condicao-COND_RATE ne gs_cont_i-PRICE.
          condicao-COND_RATE = gs_cont_i-PRICE.
          "perform gera_guid changing condicao-GUID.
      endif.
      append condicao to gt_conditions.
  endloop.

  lv_idx_linha_ini_aux = gs_key_badi-idx_linha_ini + 2.
  lv_idx_linha_fim_aux = gs_key_badi-idx_linha_fim + 2.

  MOVE:  lv_idx_linha_ini_aux TO lv_idx_linha_ini,
         lv_idx_linha_fim_aux TO lv_idx_linha_fim.

  CONDENSE: lv_idx_linha_ini NO-GAPS,
            lv_idx_linha_fim NO-GAPS.

  FREE: lt_budget_aggr, ls_budget_aggr, lt_ctrl_record, ls_acc_main.

* Verificar orçamento
  LOOP AT gt_acc INTO ls_acc.

    FREE: ls_item, ls_acc,
          lt_acc_main, lt_account, lt_budget,
          lt_ctrl_record, lt_return, lt_return_2, lt_cobl, lt_new_cobl.

    lv_check_value = ls_acc-dist_value.

    READ TABLE gt_cont_i INTO ls_item WITH KEY guid = ls_acc-p_guid.

    if ls_item is not initial. "IF sy-subrc EQ 0.
      IF ls_acc-distr_perc > 0.
        lv_check_value = ( ls_acc-distr_perc * ls_item-value ) / 100.
      ELSEIF ls_acc-dist_quan > 0.
        lv_check_value = ls_acc-dist_quan * ls_item-price.
      ENDIF.
    ENDIF.

    IF lv_check_value  = 0.
        IF gs_cont_h-ps_gm_level = 'HDR'.
            lv_check_value = gs_cont_h-guaranteed_min.
        ELSE.
          if ls_item is not initial.
              lv_check_value = ls_item-value.
          ENDIF.
        ENDIF.
    ENDIF.

    REFRESH: lt_cobl, lt_new_cobl, lt_return_2, lt_ctrl_record.

    lt_cobl-comp_code  = ls_item-co_code.
    lt_cobl-pstng_date = gs_cont_h-posting_date.
    lt_cobl-doc_date   = gs_cont_h-posting_date.
    lt_cobl-asval_date = gs_cont_h-posting_date.
    lt_cobl-co_area    = ls_item-co_code.
    lt_cobl-bus_area   = ls_acc-bus_area.
    lt_cobl-gl_account = ls_acc-g_l_acct.
    lt_cobl-gl_trans_t = 'RMBE'.
    lt_cobl-obj_type   = 'PCON'.
    lt_cobl-cmmt_item  = ls_acc-cmmt_item.
    lt_cobl-funds_ctr  = ls_acc-funds_ctr.
    lt_cobl-ref_date   = ls_acc-ref_date.
    lt_cobl-acc_no     = ls_acc-acc_no.
    APPEND lt_cobl.

*   Verificar derivação Centro Financeiro
    CALL FUNCTION 'META_ACCSERV_CHECKACCASSIGNMT'
      EXPORTING
        logical_system = ls_item-logsys_fi
      TABLES
        bbp_cobl       = lt_cobl
        exp_cobl       = lt_new_cobl
        return         = lt_return_2
        control_record = lt_ctrl_record.

    IF NOT lt_new_cobl[] IS INITIAL.
      READ TABLE lt_new_cobl INDEX 1.
      IF NOT lt_new_cobl-cmmt_item IS INITIAL.
        ls_acc-cmmt_item = lt_new_cobl-cmmt_item.
      ENDIF.
      IF NOT lt_new_cobl-funds_ctr IS INITIAL.
        ls_acc-funds_ctr = lt_new_cobl-funds_ctr.
      ENDIF.
    ENDIF.

    MOVE-CORRESPONDING ls_acc TO ls_acc_main.
    MOVE ls_acc-ref_date  TO ls_acc_main-deliv_date.
    MOVE gs_cont_h-co_code TO ls_acc_main-co_code.
    APPEND ls_acc_main TO lt_acc_main.
    APPEND ls_acc      TO lt_account.

    CALL FUNCTION 'META_BUDGET_READ'
      EXPORTING
        budget_log_sys = gs_cont_h-logsys_fi
        budget_co_code = gs_cont_h-co_code
      TABLES
        ct_acc_main    = lt_acc_main
        it_acc         = lt_account
        et_budget      = lt_budget
        return         = lt_return.
*       CONTROL_RECORD =

    CLEAR lv_budget_open.
    READ TABLE lt_budget INTO ls_budget INDEX 1.
    IF sy-subrc EQ 0.
      lv_budget_open = ls_budget-bdgt_curr - ls_budget-bdgt_assgn.

      IF lv_budget_open < lv_check_value.
        gv_erro = abap_true.

        gs_log_ret-ind     = S_ATENCAO.
        CONCATENATE 'Carga Contrato || Linha inicial: ' lv_idx_linha_ini ' Linha final: ' lv_idx_linha_fim 
            INTO gs_log_ret-message RESPECTING BLANKS.
        gs_log_ret-msgid   = 'CONTRATO'.
        APPEND gs_log_ret TO gt_log_ret.

        lv_budget_saldo = abs( lv_budget_open - lv_check_value ).
        lv_msgv1 = ls_item-number_int.
        MOVE ls_acc-funds_ctr TO lv_char_buffer.
        lv_msgv2 = lv_char_buffer.
        MOVE ls_acc-cmmt_item TO lv_char_buffer.
        lv_msgv3 = lv_char_buffer.
        WRITE lv_budget_saldo TO lv_char.
        SHIFT lv_char LEFT DELETING LEADING space.
        CONCATENATE lv_char gs_cont_h-currency INTO lv_msgv4 SEPARATED BY space.
        gs_log_ret-ind = S_VERMELHO.
        gs_log_ret-msgid = 'PSSRM_GM_ACC'.
        MESSAGE e037(pssrm_gm_acc) WITH lv_msgv1 lv_msgv2 lv_msgv3 lv_msgv4 INTO gs_log_ret-message.
        APPEND gs_log_ret TO gt_log_ret.
      ENDIF.
    ENDIF.

    READ TABLE lt_budget_aggr INTO ls_budget_aggr
               WITH KEY year      = ls_acc_main-deliv_date(4)
                        cmmt_item = ls_acc_main-cmmt_item
                        funds_ctr = ls_acc_main-funds_ctr.
    IF sy-subrc EQ 0.
      ls_budget_aggr-check_value = lv_check_value.
      CLEAR ls_budget_aggr-budget_open.
    ELSE.
      ls_budget_aggr-year        = ls_acc_main-deliv_date(4).
      ls_budget_aggr-cmmt_item   = ls_acc_main-cmmt_item.
      ls_budget_aggr-funds_ctr   = ls_acc_main-funds_ctr.
      ls_budget_aggr-check_value = lv_check_value.
      ls_budget_aggr-budget_open = lv_budget_open.
    ENDIF.
    COLLECT ls_budget_aggr INTO lt_budget_aggr.

  ENDLOOP.


  IF gv_erro IS INITIAL.
    LOOP AT lt_budget_aggr INTO ls_budget_aggr.
      IF ls_budget_aggr-budget_open < ls_budget_aggr-check_value.
        gv_erro = abap_true.
        lv_budget_saldo = abs( ls_budget_aggr-budget_open - ls_budget_aggr-check_value ).
        CONCATENATE 'Exercício' ls_budget_aggr-year INTO lv_char_buffer SEPARATED BY space.
        lv_msgv1 = lv_char_buffer.
        MOVE ls_budget_aggr-funds_ctr TO lv_char_buffer.
        lv_msgv2 = lv_char_buffer.
        MOVE ls_budget_aggr-cmmt_item TO lv_char_buffer.
        lv_msgv3 = lv_char_buffer.
        WRITE lv_budget_saldo TO lv_char.
        SHIFT lv_char LEFT DELETING LEADING space.
        CONCATENATE lv_char gs_cont_h-currency INTO lv_msgv4 SEPARATED BY space.
        gs_log_ret-ind = S_VERMELHO.
        gs_log_ret-msgid = 'PSSRM_GM_ACC'.
        MESSAGE e037(pssrm_gm_acc) WITH lv_msgv1 lv_msgv2 lv_msgv3 lv_msgv4 INTO gs_log_ret-message.
        APPEND gs_log_ret TO gt_log_ret.
      ENDIF.
    ENDLOOP.

  ENDIF.

  if gv_erro eq abap_true.
      perform logErro using 'Dados contábeis informados com problemas!' gs_cont_h-DESCRIPTION.
      return.
  endif.

  if gs_cont_h-OBJECT_TYPE is initial.
      gs_cont_h-OBJECT_TYPE = /sapsrm/if_pdo_obj_types_c=>gc_pdo_contract.
  endif.

  sort gt_acc by ACC_NO ascending.

  data lv_changed type XFELD.
  data es_header type BBP_PDS_HEADER.
  move-corresponding gs_cont_h to es_header.

* Simular alteração
  CALL FUNCTION 'BBP_PROCDOC_RESET_BUFFER'.
  CALL FUNCTION 'BBP_PROCDOC_UPDATE'
    EXPORTING
      i_header                      = gs_cont_h
      I_TESTRUN                     = abap_true
      i_save                        = abap_true
      it_conditions                 = gt_conditions
    IMPORTING
      ES_HEADER                     = es_header
    TABLES
      i_item                        = gt_cont_i
      i_account                     = gt_acc
      i_partner                     = gt_partner
      i_orgdata                     = gt_orgdata
      e_messages                    = gt_bapimsg
    CHANGING
      e_changed                     = lv_changed.

    clear es_header.
    move-corresponding gs_cont_h to es_header.

* Alterar valendo se estiver tudo ok
    READ TABLE gt_bapimsg INTO gs_bapimsg WITH KEY msgty = 'A'.
    IF sy-subrc ne 0 and p_teste is initial.
        CALL FUNCTION 'BBP_PROCDOC_RESET_BUFFER'.
        CALL FUNCTION 'BBP_PROCDOC_UPDATE'
          EXPORTING
            i_header                      = gs_cont_h
            it_conditions                 = gt_conditions
            "IV_USE_GROUP_CONDITIONS       = abap_true
            "IV_CREATE_CHANGE_VERSION      = abap_true
            "IV_UPDATE_FROM_CHANGE_VERSION = abap_true
          IMPORTING
            ES_HEADER                     = es_header
          TABLES
            i_item                        = gt_cont_i
            i_account                     = gt_acc
            i_partner                     = gt_partner
            i_orgdata                     = gt_orgdata
            e_messages                    = gt_bapimsg
          CHANGING
            e_changed                     = lv_changed.
        CALL FUNCTION 'BBP_PROCDOC_SAVE'
          EXPORTING
            iv_header_guid         = gs_cont_h-GUID
            iv_object_type         = gs_cont_h-OBJECT_TYPE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.
  endif.

  CALL FUNCTION 'BBP_PROCDOC_RESET_BUFFER'.

  CLEAR gs_log_ret.

  gs_log_ret-ind     = S_ATENCAO.
  CONCATENATE 'Carga Contrato || Linha inicial: ' lv_idx_linha_ini ' Linha final: ' lv_idx_linha_fim 
      INTO gs_log_ret-message RESPECTING BLANKS.
  gs_log_ret-msgid   = 'CONTRATO'.
  APPEND gs_log_ret TO gt_log_ret.

  READ TABLE gt_bapimsg INTO gs_bapimsg WITH KEY msgty = 'A'.
  IF sy-subrc = 0.
      gs_log_ret-ind     = S_VERMELHO.
      CONCATENATE 'Contrato não alterado! ' gs_cont_h-DESCRIPTION INTO gs_log_ret-message RESPECTING BLANKS.
  else.
      IF lv_changed eq abap_true.
        gs_log_ret-ind     = S_VERDE.
        IF p_teste IS INITIAL.
          CONCATENATE 'Contrato alterado com sucesso: ' gs_cont_h-DESCRIPTION INTO gs_log_ret-message RESPECTING BLANKS.
        ELSE.
          gs_log_ret-message = 'Simulação da alteração  do Contrato executada com sucesso'.
        ENDIF.
      ELSE.
        gs_log_ret-ind     = S_VERDE.
        IF p_teste IS INITIAL.
          CONCATENATE 'Contrato alterado com ressalvas: ' gs_cont_h-DESCRIPTION INTO gs_log_ret-message RESPECTING BLANKS.
        ELSE.
          gs_log_ret-message = 'Simulação da alteração do Contrato executada com ressalvas'.
        ENDIF.
      ENDIF.
  endif.
  APPEND gs_log_ret TO gt_log_ret.


  LOOP AT gt_bapimsg INTO gs_bapimsg. "WHERE msgty = 'S' OR msgty = 'E' OR msgty = 'A'.
    CLEAR gs_log_ret.
    MOVE-CORRESPONDING gs_bapimsg TO gs_log_ret.
    IF gs_bapimsg-msgty = 'S'.
      gs_log_ret-ind = S_VERDE.
    ELSEIF gs_bapimsg-msgty = 'E'.
      gs_log_ret-ind = S_AMARELO.
    ELSEIF gs_bapimsg-msgty = 'W'.
      gs_log_ret-ind = S_AMARELO.
    ELSEIF gs_bapimsg-msgty = 'I'.
      gs_log_ret-ind = S_AMARELO.
    ELSEIF gs_bapimsg-msgty = 'A'.
      gs_log_ret-ind = S_VERMELHO.
    ENDIF.
    APPEND gs_log_ret TO gt_log_ret.
  ENDLOOP.

* Limpar os shared locks no ECC
  IF p_teste = abap_false.
    CALL FUNCTION 'ZSRM_AVC_REMOVE_SHARED_LOCKS'
      DESTINATION gs_cont_h-logsys_fi.
  ENDIF.

ENDFORM.


FORM append_msg  USING VALUE(p_msgid) TYPE symsgid
                       VALUE(p_msgno) TYPE symsgno
                       VALUE(p_msgty) TYPE symsgty
                       VALUE(p_msgv1)
                       VALUE(p_msgv2)
                       VALUE(p_msgv3)
                       VALUE(p_msgv4).
  DATA ls_msg LIKE LINE OF gt_msglog.
  ls_msg-msgid = p_msgid.
  ls_msg-msgno = p_msgno.
  ls_msg-msgty = p_msgty.
  ls_msg-msgv1 = p_msgv1.
  ls_msg-msgv2 = p_msgv2.
  ls_msg-msgv3 = p_msgv3.
  ls_msg-msgv4 = p_msgv4.
  APPEND ls_msg TO gt_msglog.
ENDFORM.

FORM limpa_tabelas_badi.
  FREE: gs_cont_h,
        gt_partner,
        gt_orgdata,
        gt_cont_i,
        gt_acc,
        gt_msglog,
        gs_key_badi,
        gt_ld_contas,
        gt_ld_contas_comp,
        gt_ld_itens,
        gt_ld_orgdata,
        gt_ld_partners,
        gt_ld_conditions,
        gv_erro.
ENDFORM.


FORM manter_parceiro  USING p_user TYPE bu_partner  p_fpar TYPE bbp_pds_partner-partner_fct.
   CLEAR gs_partner.
   data parceiro type BBP_PDS_PARTNER.

   " localizar o parceiro correspondente
   IF p_fpar = '00000075'.
     read table gt_ld_partners into parceiro with key PARTNER_FCT = p_fpar  P_GUID = gs_cont_i-guid.
   ELSE.
     read table gt_ld_partners into parceiro with key PARTNER_FCT = p_fpar  P_GUID = gs_cont_h-guid.
   ENDIF.

   "decidir entre manter, alterar ou incluir
   if p_user is initial.
        " nada a fazer / manter o que estava
        return.
   endif.

   if parceiro is initial.
       " incluir
       perform gera_guid changing gs_partner-partner_guid.
   else.
       " alterar
       gs_partner-partner_guid = parceiro-PARTNER_GUID.
   endif.

   UNPACK p_user TO p_user.

   DATA: lv_partner_guid TYPE but000-partner_guid.
   SELECT partner_guid
     INTO lv_partner_guid
     FROM but000
     WHERE partner = p_user.
   ENDSELECT.

   gs_partner-partner_fct = p_fpar.
   gs_partner-partner_id  = p_user.
   gs_partner-partner_no  = lv_partner_guid.

   APPEND gs_partner TO gt_partner.
ENDFORM.


FORM log.
  CALL FUNCTION 'ZSRMFM_019_02_12_POPUP_MESSAGE'
    TABLES
      it_alv = gt_log_ret.
ENDFORM.


FORM set_additional_acc  USING p_acc TYPE bbp_pds_acc p_dist_quan p_dist_value.
  DATA: lv_ano(4) TYPE n.
  CLEAR: p_acc-dist_quan, p_acc-dist_value.

  lv_ano = p_acc-ref_date(4).
  add 1 to lv_ano.
  p_acc-ref_date(4) = lv_ano.

  " verifica se existe class. contábil para o ano
  PERFORM atualiza_guid_conta_adicional using lv_ano changing p_acc-guid.

  if p_acc-guid is initial.
      if p_dist_quan is initial and ( p_dist_value is initial or p_dist_value eq '0,00' ).
          " nada a fazer
          return.
      endif.

      " incluir
      perform gera_guid changing p_acc-guid.
  else.
      " alterar ou manter
      read table gt_ld_contas_comp into p_acc with key GUID = p_acc-guid.
  endif.

* Quantidade em UM do Pedido
  if p_dist_quan is not initial.
      REPLACE ALL OCCURRENCES OF '.' IN p_dist_quan WITH ''.
      CONDENSE p_dist_quan NO-GAPS.
      PERFORM: valida_valor USING p_dist_quan 'DIST_QUAN' '13' '3' CHANGING p_acc-dist_quan.
  endif.

* Valor Líquido
  if p_dist_value is not initial and p_dist_value ne '0,00'.
      REPLACE ALL OCCURRENCES OF '.' IN p_dist_value WITH ''.
      CONDENSE p_dist_value NO-GAPS.
      PERFORM: valida_valor USING p_dist_value 'DIST_VALUE' '15' '2' CHANGING p_acc-dist_value.
  endif.

* Atribui numero sequencial
  if p_acc-acc_no is initial.
      p_acc-acc_no = lines( gt_acc ) + 1.
  endif.

  APPEND p_acc TO gt_acc.

ENDFORM.

FORM gera_guid  CHANGING p_guid_x16 TYPE sysuuid_x16.
  DATA lr_uuid_error TYPE REF TO cx_uuid_error.
  DATA lv_emsg       TYPE string.
  TRY.
      p_guid_x16 = cl_system_uuid=>create_uuid_x16_static( ).
    CATCH cx_uuid_error INTO lr_uuid_error.
      lv_emsg = lr_uuid_error->get_text( ).
      PERFORM write_msg USING 018 lv_emsg space space space.
  ENDTRY.
ENDFORM.

form nullSafeSet using input changing output.
   if input is not initial and input <> ''.
        output = input.
   endif.
endform.

form logErro using texto type string referencia.
  gs_log_ret-ind = S_VERMELHO.
  gs_log_ret-msgid = 'CONTRATO'.
  data str type string.
  str = referencia.
  concatenate texto ' ' str into gs_log_ret-message RESPECTING BLANKS.
  APPEND gs_log_ret TO gt_log_ret.
  gv_erro = abap_true.
endform.

form logInfo using texto type string referencia.
  gs_log_ret-ind = S_AMARELO.
  gs_log_ret-msgid = 'INFO'.
  data str type string.
  str = referencia.
  concatenate texto ' ' str into gs_log_ret-message RESPECTING BLANKS.
  APPEND gs_log_ret TO gt_log_ret.
endform.

form carrega_contrato.
  data pdlist type table of BBP_PDS_CTR_PDLIST.
  data msgs type table of BBP_PDS_MESSAGES.

  if gs_arq-object_id is initial.
      perform logErro using 'Nome do Contrato (descrição) não foi informado!' ''.
      return.
  endif.

  CALL FUNCTION 'BBP_PD_CTR_GETLIST'
   EXPORTING
      I_OBJECT_ID = gs_arq-object_id
      "I_DESCRIPTION = gs_arq-description
    TABLES
      E_PDLIST = pdlist
      E_MESSAGES = msgs.

  if pdlist is initial.
      perform logErro using 'Contrato não encontrado!' gs_arq-object_id.
      return.
  elseif lines( pdlist ) > 1.
      perform logErro using 'Há mais de um contrato com esse nome!' gs_arq-object_id.
      return.
  endif.

  " dados do cabeçalho
  data pdheader type BBP_PDS_CTR_PDLIST.
  read table pdlist index 1 into pdheader.

  data guidPrincipal type BBP_GUID_TAB.
  guidPrincipal-GUID = pdHeader-GUID.

  data lt_active_header_guids type table of BBP_GUID_TAB.
  append guidPrincipal to lt_active_header_guids.

  data lt_pdlist type table of BBP_PDS_VERSION_LIST_INTERNAL.
  CALL FUNCTION 'BBP_PROCDOC_GET_CHANGE_VERSION'
    EXPORTING
      iv_read_all_change_versions = ' ' "c_off
    TABLES
      it_header_guids             = lt_active_header_guids
      et_pdlist                   = lt_pdlist.

  if lt_pdlist is not initial.
      data alt_header type BBP_PDS_VERSION_LIST_INTERNAL.
      read table lt_pdlist index 1 into alt_header.
      gs_cont_h-guid = alt_header-GUID.
      perform logInfo using 'Há uma versão em alteração do contrato!' alt_header-VERSION_NO.
  else.
      gs_cont_h-guid = pdheader-GUID.
  endif.

  " detalhes do contrato
  data readflags type BBPS_CTR_DETAIL_REQUESTED.
  readflags-ITEM_TAB = 'X'.
  readflags-ACCOUNT_TAB = 'X'.
  readflags-ORGDATA_TAB = 'X'.
  readflags-PARTNER_TAB = 'X'.
  readflags-CONDITIONS_TAB = 'X'.
  readflags-STATUS_TAB = 'X'.

  data ls_header type BBP_PDS_CTR_HEADER_D.
  data gt_ld_status type table of BBP_PDS_STATUS.

  CALL FUNCTION 'BBP_PD_CTR_GETDETAIL'
   EXPORTING
     I_GUID = gs_cont_h-guid
     I_READ_FLAGS = readflags
   IMPORTING
     E_HEADER = ls_header
     ET_CONDITIONS = gt_ld_conditions
   TABLES
     E_ITEM = gt_ld_itens
     E_PARTNER = gt_ld_partners
     E_ORGDATA = gt_ld_orgdata
     E_STATUS = gt_ld_status.

  " Há uma limitação que não permite a alteração de contrato no status liberado
  " Aparentemente trata-se de um bug na função BBP_PROCDOC_UPDATE
  data c_s_ctr_released TYPE j_istat VALUE /sapsrm/if_pdo_status_c=>gc_pdo_ctr_released.
  READ TABLE gt_ld_status WITH KEY stat = c_s_ctr_released inact = abap_false transporting no fields.
  if sy-subrc eq 0.
      perform logErro using 'Contrato possui status Liberado - Altere manualmente!' gs_arq-object_id.
      return.
  endif.

  " preenche header
  MOVE-CORRESPONDING ls_header TO gs_cont_h.

  " dados da contabilidade
  data tbContas type table of BBP_PDS_ACC.
  data conta type BBP_PDS_ACC.
  data guidConta type ty_guids_contas.
  loop at gt_ld_itens into gs_eitem.
       CALL FUNCTION 'BBP_PD_CTR_GET_ACC'
         EXPORTING
           IV_GUID = gs_eitem-GUID
         TABLES
           ET_ACCOUNT = tbContas.

       loop at tbContas into conta.
           guidConta-COST_CTR = conta-COST_CTR.
           SHIFT guidConta-COST_CTR LEFT DELETING LEADING '0'.
           guidConta-ORDERED_PROD = gs_eitem-ORDERED_PROD.
           SHIFT guidConta-ORDERED_PROD LEFT DELETING LEADING '0'.
           guidConta-GUID_CONTA = conta-GUID.
           guidConta-GUID_ITEM = gs_eitem-GUID.
           guidConta-ANO = conta-REF_DATE(4).
           guidConta-ASSET_NO = conta-ASSET_NO.
           SHIFT guidConta-ASSET_NO LEFT DELETING LEADING '0'.
           guidConta-ORDER_NO = conta-ORDER_NO.
           SHIFT guidConta-ORDER_NO LEFT DELETING LEADING '0'.
           append guidConta to gt_ld_contas.
           append conta to gt_ld_contas_comp.
       endloop.
  endloop.

  CALL FUNCTION 'BBP_PD_CTR_GET_ACC'
    EXPORTING
      IV_GUID = gs_cont_h-guid
    TABLES
      ET_ACCOUNT = tbContas.
  loop at tbContas into conta.
      append conta to gt_ld_contas_comp.
  endloop.

endform.


form atualiza_guid_item.
  data: guidItem type BBP_PDS_CTR_ITEM_D, guidItemEncontrado type BBP_GUID.
  data count type i.
  if gs_arq-ordered_prod is initial.
      perform logErro using 'Cod. Produto não informado!' gs_arq-object_id.
      return.
  endif.
  clear: count, guidItemEncontrado, gs_cont_i-guid.
  loop at gt_ld_itens into guidItem.
      SHIFT guidItem-ORDERED_PROD LEFT DELETING LEADING '0'.
      if guidItem-ORDERED_PROD eq gs_arq-ordered_prod.
          guidItemEncontrado = guidItem-GUID.
          add 1 to count.
      endif.
  endloop.
  if count = 0.
      perform logErro using 'GUID do Item nao encontrado!' gs_arq-ORDERED_PROD.
      return.
  elseif count > 1.
      perform logErro using 'Contrato possui mais de um item do tipo ' gs_arq-ORDERED_PROD.
      return.
  endif.
  gs_cont_i-guid = guidItemEncontrado.
endform.

form atualiza_guid_conta using lv_ano changing p_acc_guid.
  IF gs_cont_h-ps_gm_level = 'HDR'.
      data idConta type BBP_PDS_ACC.
      read table gt_ld_contas_comp into idConta
         with key P_GUID = gs_cont_h-guid
                  REF_DATE(4) = lv_ano.
      if idConta is initial.
          perform logErro using 'GUID da Conta nao encontrada para o Cabecalho!' gs_arq-object_id.
          return.
      endif.
      p_acc_guid = idConta-GUID.
  else.
      data: guidConta type ty_guids_contas, guidContaEncontrado type BBP_GUID.
      data count type i.
      clear: count, guidContaEncontrado.
      loop at gt_ld_contas into guidConta.
          if guidConta-GUID_ITEM ne gs_cont_i-GUID or guidConta-ANO ne lv_ano.
              continue.
          endif.
          if ( guidConta-ASSET_NO is not initial and guidConta-ASSET_NO eq gs_arq-ASSET_NO )
              or ( guidConta-ORDER_NO is not initial and guidConta-ORDER_NO eq gs_arq-ORDER_NO )
              or ( guidConta-COST_CTR is not initial and guidConta-COST_CTR eq gs_arq-COST_CTR ).
                  guidContaEncontrado = guidConta-GUID_CONTA.
                  add 1 to count.
          endif.
      endloop.
      if count = 0.
          perform logErro using 'GUID da Conta nao encontrada para o item!' gs_arq-ORDERED_PROD.
          return.
      elseif count > 1.
          perform logErro using 'Contrato possui mais de uma classificação contábil ' gs_arq-ORDERED_PROD.
          return.
      endif.
      p_acc_guid = guidContaEncontrado.
  endif.
endform.

form atualiza_guid_conta_adicional using lv_ano changing p_acc_guid.
  clear p_acc_guid.
  IF gs_cont_h-ps_gm_level = 'HDR'.
      data idConta type BBP_PDS_ACC.
      read table gt_ld_contas_comp into idConta
         with key P_GUID = gs_cont_h-guid
                  REF_DATE(4) = lv_ano.
      if idConta is not initial.
         p_acc_guid = idConta-GUID.
      endif.
  else.
      data: guidConta type ty_guids_contas, guidContaEncontrado type BBP_GUID.
      data count type i.
      clear: count, guidContaEncontrado.
      loop at gt_ld_contas into guidConta.
          if guidConta-GUID_ITEM ne gs_cont_i-GUID or guidConta-ANO ne lv_ano.
              continue.
          endif.
          if ( guidConta-ASSET_NO is not initial and guidConta-ASSET_NO eq gs_arq-ASSET_NO )
              or ( guidConta-ORDER_NO is not initial and guidConta-ORDER_NO eq gs_arq-ORDER_NO )
              or ( guidConta-COST_CTR is not initial and guidConta-COST_CTR eq gs_arq-COST_CTR ).
                  guidContaEncontrado = guidConta-GUID_CONTA.
                  add 1 to count.
          endif.
      endloop.
      if count > 1.
          perform logErro using 'Contrato possui mais de uma classificação contábil ' gs_arq-ORDERED_PROD.
          return.
      endif.
      p_acc_guid = guidContaEncontrado.
  endif.
endform.

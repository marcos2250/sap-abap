*&--------------------------------------------------------------------------------------*
*& Report  ZPPOME_LDAP
*&--------------------------------------------------------------------------------------*
*& Imports LDAP/Active Directory user data and merges all organizational structure tree,
*& positions and SU01 users, over PPOME (HRP1000) automatically.
*& Caution - Work in progress.
*&--------------------------------------------------------------------------------------*
REPORT ZPPOME_LDAP.

CONSTANTS:
    AD_AETERNUM TYPE ENDDA VALUE '99991231',
    TP_USUARIO type HRP1000-OTYPE value 'US',
    TP_UNIDADE type HRP1000-OTYPE value 'O',
    TP_POSICAO type HRP1000-OTYPE value 'S',
    BARRA type C value '/', UNDERLINE type C value '_',
    OP_BUFFER type HRRHAP-VTASK value 'B',
    OP_DIRECT type HRRHAP-VTASK value 'D',
    OP_INCLUIR type C value 'I',
    OP_DELETAR type C value 'D',
    C_P_ADM type String value '@'.


TYPES:
    BEGIN OF TY_AJUSTES_ROTULOS,
      original type string,
      corrigido type string,
    END OF TY_AJUSTES_ROTULOS,

    BEGIN OF TY_OBJETO_ORGANIZACIONAL,
      objid type HRP1000-OBJID,
      tipo type HRP1000-OTYPE,
      nomeCurto type string,
      nomeLongo type string,
      departamento type string,
    END OF TY_OBJETO_ORGANIZACIONAL,

    BEGIN OF TY_DADOS_LDAP,
      login type string,
      funcao type string,
      departamento type string,
      gerente type string,
    END OF TY_DADOS_LDAP,

    BEGIN OF TY_RELATORIO_DIFF,
      tipoObjeto type c,
      tipoOperacao type c,
      nomeCurto type string,
      nomeLongo(30) type c,
      nomeDepartamento(30) type c,
      objid type HRP1000-OBJID,
      sobid type HRP1001-SOBID,
    END OF TY_RELATORIO_DIFF,

    BEGIN OF TY_BUFFER_RELACOES,
      tipoPai type HRP1001-OTYPE,
      objidPai type HRP1001-OBJID,
      tipoFilho type HRP1001-OTYPE,
      objidFilho type HRP1001-OBJID,
      realoFilho type OBJEC-REALO,
      isChefia type c,
    END OF TY_BUFFER_RELACOES.

DATA:
    StrUtil type ref to ZCLSTRINGUTILS,
    TB_AJUSTES_ROTULOS type hashed table of TY_AJUSTES_ROTULOS with unique key table_line,
    TB_DADOS_LDAP type hashed table of TY_DADOS_LDAP with unique key login,
    TB_RELATORIO_DIFF type table of TY_RELATORIO_DIFF,
    TB_USUARIOS_ECC type hashed table of USR01-BNAME with unique key table_line,
    TB_UNIDADES_RAIZES type hashed table of string with unique key table_line,
    TB_BUFFER_RELACOES type table of TY_BUFFER_RELACOES,
    TB_CHEFES type hashed table of string with unique key table_line.


INITIALIZATION.
  CREATE OBJECT StrUtil.

SELECTION-SCREEN BEGIN OF BLOCK 02 WITH FRAME TITLE TEXT-001.
PARAMETERS: piLdap RADIOBUTTON GROUP g1 USER-COMMAND g1 DEFAULT 'X',
            pUsr(30) type c LOWER CASE,
            pPwd(30) type c LOWER CASE.
SELECTION-SCREEN SKIP.
PARAMETERS: piArq  RADIOBUTTON GROUP g1,
            pfArq TYPE String LOWER CASE.
SELECTION-SCREEN END OF BLOCK 02.

SELECTION-SCREEN BEGIN OF BLOCK 03 WITH FRAME TITLE TEXT-002.
PARAMETERS: pePpoma RADIOBUTTON GROUP g2 USER-COMMAND g2 DEFAULT 'X',
            p_inclui AS CHECKBOX,
            p_deleta AS CHECKBOX.
SELECTION-SCREEN SKIP.
PARAMETERS: peText  RADIOBUTTON GROUP g2.
SELECTION-SCREEN END OF BLOCK 03.

AT SELECTION-SCREEN OUTPUT.
  loop at screen.
    if screen-name = 'PPWD'.
        screen-invisible = 1.
        modify screen.
    endif.
  endloop.

START-OF-SELECTION.

  " INPUT - Import user data from LDAP or CSV file
  if piLdap eq abap_true.
      perform cargaAjustesRotulosLDAP.
      perform carregarInformacoesLDAP.
  else.
      perform importaTabelaLdapArquivo.
  endif.

  " OUTPUT - Merges into PPOME organizational tree 
  " or just dumps LDAP CSV data on screen
  if pePpoma eq abap_true.
      perform buscarUnidadesRaizes.
      perform processaComparacaoECC.
  else.
      perform imprimeDumpTabelaLdap.
  endif.


*&---------------------------------------------------------------------*
*& Lista as unidades raízes no LDAP. A comparação partirá destas unidades.
*&---------------------------------------------------------------------*
form buscarUnidadesRaizes.
    data: unidade type TY_OBJETO_ORGANIZACIONAL, nomeUnidade type string.
    loop at TB_DADOS_LDAP assigning field-symbol(<dados>).
        nomeUnidade = StrUtil->findToken( value = <dados>-DEPARTAMENTO chr = BARRA index = 1 ).
        if nomeUnidade is not initial.
            insert nomeUnidade into table TB_UNIDADES_RAIZES.
        endif.
    endloop.
endform.

*&---------------------------------------------------------------------*
*& Some org. unit paths need some tweaks before processing. 
*&---------------------------------------------------------------------*
form cargaAjustesRotulosLDAP.
    perform incluiRenomearRotulo using '/MGMT/OFFICE/DEPT' '/2000/DEPT'.
endform.

form processaComparacaoECC.
    data: unidade type TY_OBJETO_ORGANIZACIONAL.
    data: rotuloRaiz type string, auxRotuloDepartamento type string.
    field-symbols: <compDiff> type TY_RELATORIO_DIFF.

    " pré carrega lista de usuarios ECC
    select BNAME from USR01 into table TB_USUARIOS_ECC.

    " flag para a janela da request não aparecer
    update T77S0 set GSVAL = 'X' where GRPID = 'TRSP' and SEMID = 'CORR'.

    " busca raizes e compara árvores a partir delas
    " faz inclusões se permitido
    loop at TB_UNIDADES_RAIZES into rotuloRaiz.
        clear unidade.
        perform buscarObjetoPorNome using rotuloRaiz changing unidade.
        if unidade is not initial.
            auxRotuloDepartamento = BARRA && rotuloRaiz.
            perform comparaSubcomponentes using unidade auxRotuloDepartamento.
        endif.
    endloop.

    " mostra diferenças
    if p_inclui ne abap_true and p_deleta ne abap_true.
        sort TB_RELATORIO_DIFF by tipoOperacao tipoObjeto sobid nomeDepartamento.
        loop at TB_RELATORIO_DIFF assigning <compDiff>.
            write :/ <compDiff>-tipoOperacao, <compDiff>-tipoObjeto, <compDiff>-objid(8),
                     <compDiff>-nomeCurto, <compDiff>-nomeLongo,
                     <compDiff>-sobid(8), <compDiff>-nomeDepartamento.
        endloop.
    endif.

    " aplica as alterações em buffer para a base
    if p_inclui eq abap_true.
        perform commitarAlteracoesBuffer. " commita as inclusões
        perform criarRelacoes.
    endif.

    " Deleta na HRP1000/1001
    if p_deleta eq abap_true.
      sort TB_RELATORIO_DIFF by objid descending.
      loop at TB_RELATORIO_DIFF assigning <compDiff> where tipoOperacao eq OP_DELETAR.
          perform deletarItemHR USING <compDiff>.
      endloop.
    endif.

    perform excluirRelacoesChefiaObsoletas.

endform.

*&---------------------------------------------------------------------*
*& Algoritmo - busca em profundidade - com recursão
*&---------------------------------------------------------------------*
form comparaSubcomponentes using  componente type TY_OBJETO_ORGANIZACIONAL caminho type string.

    data: listaSubunidadesEcc type table of TY_OBJETO_ORGANIZACIONAL.
    data: listaSubunidadesLdap type hashed table of TY_OBJETO_ORGANIZACIONAL with unique key NOMECURTO.
    data: encontrou type i, auxCompara type c.

    field-symbols: <subunidade> type TY_OBJETO_ORGANIZACIONAL, <compLdap> type TY_OBJETO_ORGANIZACIONAL.

    "subcomponentes no ECC
    perform buscarPpomeFilhosDe using componente changing listaSubunidadesEcc.

    "subcomponentes no LDAP
    perform buscarLdapFilhosDe using componente caminho changing listaSubunidadesLdap.

    if listaSubunidadesEcc is initial and listaSubunidadesLdap is initial.
        exit. "não há subcomponentes
    endif.

    " Compara ECC em LDAP
    loop at listaSubunidadesEcc assigning <subunidade>.
         encontrou = 0.
         loop at listaSubunidadesLdap assigning <compLdap>.
                perform comparaRotuloObjeto using <subunidade> <compLdap> changing auxCompara.
                if auxCompara eq abap_true.
                    encontrou = 1.
                    if <subunidade>-tipo = TP_UNIDADE.
                         perform comparaSubcomponentes using <subunidade> <compLdap>-DEPARTAMENTO.
                    else.
                         perform comparaSubcomponentes using <subunidade> <compLdap>-DEPARTAMENTO.
                    endif.
                    exit.
                endif.
         endloop.
         if encontrou eq 0.
                perform deletarObjeto using
                          <subunidade>-tipo <subunidade>-OBJID <subunidade>-nomeCurto <subunidade>-nomeLongo
                          componente-OBJID componente-NOMECURTO.
                perform complementaSubarvoreEcc using <subunidade>.
         endif.
    endloop.

    " Compara LDAP em ECC
    loop at listaSubunidadesLdap assigning <compLdap>.
          encontrou = 0.
          loop at listaSubunidadesEcc assigning <subunidade>.
                perform comparaRotuloObjeto using <subunidade> <compLdap> changing auxCompara.
                if auxCompara eq abap_true.
                    encontrou = 1.
                    exit.
                endif.
          endloop.
          if encontrou eq 0.
                data objetoCriado type TY_OBJETO_ORGANIZACIONAL.
                perform incluirObjeto using <compLdap> componente changing objetoCriado.
                perform complementaSubarvoreLdap using objetoCriado.
          endif.
    endloop.

endform.


*&---------------------------------------------------------------------*
*& Complementa relatório com a sub-árvore ECC
*&---------------------------------------------------------------------*
form complementaSubarvoreEcc using componente type TY_OBJETO_ORGANIZACIONAL.
    data: listaSubunidadesEcc type table of TY_OBJETO_ORGANIZACIONAL.
    field-symbols: <subunidade> type TY_OBJETO_ORGANIZACIONAL.

    "subcomponentes no ECC
    perform buscarPpomeFilhosDe using componente changing listaSubunidadesEcc.

    if listaSubunidadesEcc is initial.
        exit. "não há subcomponentes
    endif.

    " Atravessa a árvore do ECC complementando com os subitens
    loop at listaSubunidadesEcc assigning <subunidade>.
        perform deletarObjeto using
                  <subunidade>-tipo <subunidade>-OBJID <subunidade>-nomeCurto <subunidade>-nomeLongo
                  componente-OBJID componente-NOMECURTO.
        perform complementaSubarvoreEcc using <subunidade>.
    endloop.
endform.

*&---------------------------------------------------------------------*
*& Complementa relatório com a sub-árvore LDAP
*&---------------------------------------------------------------------*
form complementaSubarvoreLdap using objetoSuperior type TY_OBJETO_ORGANIZACIONAL.
    data: listaSubunidadesLdap type hashed table of TY_OBJETO_ORGANIZACIONAL with unique key NOMECURTO.
    field-symbols: <compLdap> type TY_OBJETO_ORGANIZACIONAL.

    "subcomponentes no LDAP
    perform buscarLdapFilhosDe using objetoSuperior objetoSuperior-DEPARTAMENTO changing listaSubunidadesLdap.

    if listaSubunidadesLdap is initial.
        exit. "não há subcomponentes
    endif.

    " Atravessa a árvore do LDAP complementando com os subitens
    loop at listaSubunidadesLdap assigning <compLdap>.
        data objetoCriado type TY_OBJETO_ORGANIZACIONAL.
        perform incluirObjeto using <compLdap> objetoSuperior changing objetoCriado.
        perform complementaSubarvoreLdap using objetoCriado.
    endloop.
endform.

form buscarLdapFilhosDe using componente type TY_OBJETO_ORGANIZACIONAL caminhoDepartamento type string
      changing sublista type hashed table.
  if componente-tipo eq TP_USUARIO.
      exit.
  endif.

  data: resultadoBusca type hashed table of TY_OBJETO_ORGANIZACIONAL with unique key NOMECURTO.
  field-symbols: <obj> type TY_DADOS_LDAP.
  data objeto type TY_OBJETO_ORGANIZACIONAL.

  data: token type string, nivel type i, isChefe type c.
  nivel = StrUtil->CHARCOUNT( VALUE = caminhoDepartamento CHR = BARRA ) + 1.

  " selecao dos objetos filhos
  loop at TB_DADOS_LDAP assigning <obj>.
      if not <obj>-DEPARTAMENTO cp caminhoDepartamento && '*'.
         continue. " componentes de outras unidades, não rela
      endif.

      read table TB_CHEFES WITH KEY table_line = <obj>-LOGIN transporting no fields.
      if sy-subrc eq 0.
          isChefe = abap_true.
      else.
          isChefe = abap_false.
      endif.

      if componente-tipo eq TP_UNIDADE.
          " inclui cargos deste nível
          if <obj>-DEPARTAMENTO cp '*' && componente-NOMECURTO.
                objeto-TIPO = TP_POSICAO.
                objeto-NOMELONGO = <obj>-FUNCAO.
                perform defineNomeCurtoPosicao using <obj>-FUNCAO componente-NOMECURTO isChefe changing objeto-nomeCurto.
                objeto-DEPARTAMENTO = <obj>-DEPARTAMENTO.
                insert objeto into table resultadoBusca.

          else.
                " inclui sub componente organizacional (ao nivel correspondente)
                token = StrUtil->findToken( value = <obj>-DEPARTAMENTO chr = BARRA index = nivel ).
                if token is not initial.
                      objeto-TIPO = TP_UNIDADE.
                      objeto-NOMECURTO = token.
                      objeto-NOMELONGO = token.
                      objeto-DEPARTAMENTO = StrUtil->stringLeft( chars = token value = <obj>-DEPARTAMENTO ).

                      read table TB_UNIDADES_RAIZES WITH KEY table_line = objeto-NOMECURTO transporting no fields.
                      if sy-subrc eq 0.
                           continue. " achou uma unidade raiz não rela (será varrida em outra busca)
                      endif.

                      " inclui somente se o departamento confere com o caminho percorrido
                      if <obj>-DEPARTAMENTO cp caminhoDepartamento && BARRA && token && '*'.
                          insert objeto into table resultadoBusca.
                      endif.
                endif.
          endif.

      elseif componente-tipo eq TP_POSICAO .
          " inclui empregados desta função

          if componente-nomeCurto(1) eq C_P_ADM.
              if isChefe ne abap_true.
                 continue.
              endif.
          else.
              if isChefe eq abap_true.
                 continue.
              endif.
          endif.

          if <obj>-DEPARTAMENTO eq caminhoDepartamento and <obj>-FUNCAO eq componente-nomeLongo.
              objeto-TIPO = TP_USUARIO.
              objeto-NOMECURTO = <obj>-LOGIN.
              translate objeto-NOMECURTO to upper case.
              objeto-NOMELONGO = objeto-NOMECURTO.
              objeto-DEPARTAMENTO = <obj>-DEPARTAMENTO.
              insert objeto into table resultadoBusca.
          endif.
      endif.
  endloop.
  sublista = resultadoBusca.
endform.


form incluirObjeto using objeto type TY_OBJETO_ORGANIZACIONAL objetoAcima type TY_OBJETO_ORGANIZACIONAL changing objetoSalvo.
    data novoObjId type HRP1000-OBJID.

    " evita incluir usuário que ainda não existe na SU01
    if objeto-TIPO eq TP_USUARIO.
        "find first occurrence of objeto-NOMECURTO in table TB_USUARIOS_ECC.
        read table TB_USUARIOS_ECC with table key table_line = objeto-NOMECURTO transporting no fields.
        if sy-SUBRC ne 0.
            exit.
        endif.
    endif.

    " Aplica na PPOME
    if p_inclui eq abap_true.
        perform cadastrarItemHR using objeto objetoAcima changing novoObjId.
    endif.

    data diff type TY_RELATORIO_DIFF.
    diff-TIPOOPERACAO = OP_INCLUIR.
    diff-TIPOOBJETO = objeto-tipo.
    diff-NOMECURTO = objeto-nomeCurto.
    diff-NOMELONGO = objeto-nomeLongo.
    diff-SOBID = objetoAcima-OBJID.
    diff-OBJID = novoObjId.
    diff-nomeDepartamento = objeto-DEPARTAMENTO.
    insert diff into table TB_RELATORIO_DIFF.

    data objSalvo type TY_OBJETO_ORGANIZACIONAL.
    objSalvo-TIPO = objeto-TIPO.
    objSalvo-DEPARTAMENTO = objeto-DEPARTAMENTO.
    objSalvo-OBJID = novoObjId.
    objSalvo-NOMECURTO = objeto-nomeCurto.
    objSalvo-NOMELONGO = objeto-nomeLongo.
    objetoSalvo = objSalvo.
endform.

form deletarObjeto using tipo objId nomeCurto nomeLongo sobId nomeDepartamento.
    data diff type TY_RELATORIO_DIFF.
    diff-TIPOOPERACAO = OP_DELETAR.
    diff-TIPOOBJETO = tipo.
    diff-OBJID = objId.
    diff-NOMECURTO = nomeCurto.
    diff-NOMELONGO = nomeLongo.
    diff-SOBID = sobId.
    diff-nomeDepartamento = nomeDepartamento.
    insert diff into table TB_RELATORIO_DIFF.
endform.

form defineNomeCurtoPosicao using nomeLongo nomeDepartamento isChefe changing nomeCurto.
    " rótulos de posições - ex: ABC_0123, ou DIR_GECON
    data: depto type string, sigla type string.
    depto = StrUtil->lastToken( chr = BARRA value = nomeDepartamento ).

    if nomeLongo cp 'Diretor*'.
        sigla = 'DIR'.
    else.
        sigla = nomeLongo.
        translate sigla to upper case.
        REPLACE ALL OCCURRENCES OF 'Á' IN sigla WITH 'A'.
        REPLACE ALL OCCURRENCES OF ' DA ' IN sigla WITH ' '.
        REPLACE ALL OCCURRENCES OF ' DE ' IN sigla WITH ' '.
        REPLACE ALL OCCURRENCES OF ' DO ' IN sigla WITH ' '.
        REPLACE ALL OCCURRENCES OF ' EM ' IN sigla WITH ' '.
        replace all occurrences of regex '[^A-Z0-9\s]' in sigla with ''.
        replace all occurrences of regex '(\b\w)\w+\s*' in sigla with '$1'.
        condense sigla no-gaps.
    endif.

    if strlen( depto ) > 2 and depto(2) eq 'AG'.
        nomeCurto = sigla && UNDERLINE && depto+2.
    else.
        nomeCurto = sigla && UNDERLINE && depto.
    endif.

    if isChefe eq abap_true.
        nomeCurto = C_P_ADM && nomeCurto.
    endif.
endform.

form comparaRotuloObjeto using pObjEcc type TY_OBJETO_ORGANIZACIONAL pObjLdap type TY_OBJETO_ORGANIZACIONAL changing resultado.
  resultado = abap_false.

  if pObjEcc-nomeCurto eq pObjLdap-nomeCurto or pObjEcc-nomeLongo eq pObjLdap-nomeCurto.
      resultado = abap_true.
      exit.
  endif.

  " aceitar rotulos das agencias
  if pObjEcc-nomeCurto cp '0*' and pObjLdap-nomeCurto cp 'AG*'.
      if pObjEcc-nomeCurto+1(3) eq pObjLdap-nomeCurto+2(3).
        resultado = abap_true.
      endif.
      exit.
  endif.

endform.


*&----------------------------------------------------------------------*
*& Funções para consultar da PPOME (infotipos 1000 e 1001).
*&----------------------------------------------------------------------*

form buscarObjeto using idObjeto changing objeto type TY_OBJETO_ORGANIZACIONAL.
    select single
               o~OBJID as OBJID,
               o~OTYPE as tipo,
               o~SHORT as nomeCurto,
               o~STEXT as nomeLongo
         into corresponding fields of @objeto
         from HRP1000 as o
         where o~OBJID eq @idObjeto
         and o~PLVAR eq '01'
         and o~ENDDA eq @AD_AETERNUM.
endform.

form buscarObjetoPorNome using nome changing objeto type TY_OBJETO_ORGANIZACIONAL.
    data selecao type table of TY_OBJETO_ORGANIZACIONAL.
    select o~OBJID as OBJID,
               o~OTYPE as tipo,
               o~SHORT as nomeCurto,
               o~STEXT as nomeLongo
         into corresponding fields of table @selecao
         from HRP1000 as o
         where ( o~STEXT eq @nome or o~SHORT eq @nome )
         and o~PLVAR eq '01'
         and o~ENDDA eq @AD_AETERNUM
         order by o~OBJID ascending.

    if lines( selecao ) = 0.
        write :/ 'Aviso! Unidade ', nome, ' não encontrada! Favor cadastrar na PPOME.'.
    elseif lines( selecao ) > 1.
        write :/ 'Aviso! Há mais de uma unidade com nome: ', nome, '!'.
    else.
        read table selecao into objeto index 1.
    endif.
endform.

form buscarPpomeFilhosDe using unidadeAcima type TY_OBJETO_ORGANIZACIONAL changing listaFilhos.
  data: componentes type table of TY_OBJETO_ORGANIZACIONAL, objeto type TY_OBJETO_ORGANIZACIONAL.

  if unidadeAcima-TIPO eq TP_UNIDADE or unidadeAcima-TIPO eq TP_POSICAO.
      data tbIdsFilhos type table of HRP1001.
      select r~OBJID into corresponding fields of table tbIdsFilhos
        from HRP1001 as r
        where r~SOBID eq unidadeAcima-OBJID
        and r~PLVAR eq '01' and r~RSIGN = 'A' and r~ENDDA gt SY-DATUM. "and r~RELAT = '002'.

      if tbIdsFilhos is not initial.
        select o~OBJID as OBJID,
               o~OTYPE as tipo,
               o~SHORT as nomeCurto,
               o~STEXT as nomeLongo
           into corresponding fields of @objeto
           from HRP1000 as o
           for all entries in @tbIdsFilhos
           where o~OBJID eq @tbIdsFilhos-OBJID and o~PLVAR eq '01' and o~ENDDA gt @SY-DATUM.

            read table TB_UNIDADES_RAIZES with key table_line = objeto-NOMECURTO transporting no fields.
            if sy-subrc ne 0.
                 append objeto to componentes.
            endif.

        endselect.
      endif.
  endif.

  if unidadeAcima-TIPO eq TP_POSICAO.
      select 'US' as tipo, r~SOBID as nomeCurto
        into corresponding fields of table @componentes
        from HRP1001 as r
        where r~OBJID eq @unidadeAcima-OBJID
        and r~PLVAR eq '01' and r~RSIGN = 'A' and r~ENDDA gt @SY-DATUM and r~RELAT = '008'.
  endif.

  listaFilhos = componentes.
endform.


*&----------------------------------------------------------------------*
*& Funções do RH/OM para incluir e alterar objetos da PPOME.
*&----------------------------------------------------------------------*

FORM cadastrarItemHR USING objeto type TY_OBJETO_ORGANIZACIONAL objetoPai type TY_OBJETO_ORGANIZACIONAL
      changing idObjetoCriado type HRP1000-OBJID.

  if objetoPai-OBJID is initial.
      exit.
  endif.

  data novoObjId like hrp1000-objid.

  data: nomeCurto type HRP1000-SHORT, nomeLongo type HRP1000-STEXT.
  nomeCurto = objeto-nomeCurto.
  nomeLongo = objeto-nomeLongo.

  if objeto-TIPO eq TP_UNIDADE or objeto-TIPO eq TP_POSICAO.
      CALL FUNCTION 'RH_OBJECT_CREATE' "Insere na HRP1000
        EXPORTING
         LANGU                      = SY-LANGU
          PLVAR                     = '01'
          OTYPE                     = objeto-TIPO
          SHORT                     = nomeCurto
          STEXT                     = nomeLongo
          BEGDA                     = SY-DATUM
          ENDDA                     = AD_AETERNUM
          OSTAT                     = '1'
          VTASK                     = OP_BUFFER
       IMPORTING
         OBJID                     = novoObjId
       EXCEPTIONS
         TEXT_REQUIRED             = 1
         INVALID_OTYPE             = 2
         INVALID_DATE              = 3
         ERROR_DURING_INSERT       = 4
         ERROR_EXT_NUMBER          = 5
         UNDEFINED                 = 6
         OTHERS                    = 7.

       idObjetoCriado = novoObjId.
  else.
       idObjetoCriado = objeto-nomeCurto.
  endif.

  data buffer type TY_BUFFER_RELACOES.
  clear buffer.
  buffer-TIPOFILHO = objeto-TIPO.
  if objeto-TIPO eq TP_USUARIO.
      buffer-REALOFILHO = objeto-nomeCurto.
  else.
      buffer-OBJIDFILHO = novoObjId.
  endif.
  buffer-TIPOPAI = OBJETOPAI-TIPO.
  buffer-OBJIDPAI = OBJETOPAI-OBJID.
  if objeto-TIPO eq TP_POSICAO and nomeCurto(1) eq C_P_ADM.
      buffer-isChefia = abap_true.
  endif.
  append buffer to TB_BUFFER_RELACOES.

  write :/ 'Incluido: ', objeto-TIPO, novoObjId, nomeCurto, nomeLongo.
endform.


form criarRelacoes.
  data: ld_orgeh type pa0001-orgeh.
  data: ld_pobject type OBJEC, it_cobject type OBJEC_T, ld_cobject like line of it_cobject, ld_flag TYPE FLAG.
  data: tipoRelacao type HRP1001-RELAT, sentidoRelacao type HRP1001-RSIGN.

  loop at TB_BUFFER_RELACOES assigning field-symbol(<buffer>).
         clear: ld_orgeh, ld_pobject, ld_cobject, it_cobject.

         "Fill Parent object details (OBJID details)
         ld_pobject-PLVAR = '01'.
         ld_pobject-OTYPE = <buffer>-TIPOPAI.
         ld_pobject-OBJID = <buffer>-OBJIDPAI.
         ld_pobject-BEGDA = SY-DATUM.
         ld_pobject-ENDDA = AD_AETERNUM.
         ld_pobject-ISTAT = '1'.

         "Fill child object details (SOBID details)
         ld_cobject-PLVAR = '01'.
         ld_cobject-OTYPE = <buffer>-TIPOFILHO.
         ld_cobject-REALO = <buffer>-REALOFILHO.
         ld_cobject-OBJID = <buffer>-OBJIDFILHO.
         ld_cobject-BEGDA = sy-datum.
         ld_cobject-ENDDA = AD_AETERNUM.
         ld_cobject-ISTAT = '1'.
         append ld_cobject to it_cobject.

         case <buffer>-TIPOFILHO.
             when TP_UNIDADE.
                 tipoRelacao = '002'.
                 sentidoRelacao = 'B'.
             when TP_POSICAO.
                 tipoRelacao = '003'.
                 sentidoRelacao = 'B'.
             when TP_USUARIO.
                 tipoRelacao = '008'.
                 sentidoRelacao = 'A'.
         endcase.

         CALL FUNCTION 'OM_CREATE_NEW_RELATIONS'
           EXPORTING
             PARENT_OBJECT          = ld_pobject
             CHILD_OBJECTS          = it_cobject
             VRSIGN                 = sentidoRelacao
             VRELAT                 = tipoRelacao
             VBEGDA                 = sy-datum
             VENDDA                 = AD_AETERNUM
          IMPORTING
            RELATION_CREATED       = ld_flag.

         if <buffer>-isChefia eq abap_true.
               tipoRelacao = '012'.
               sentidoRelacao = 'B'.
               CALL FUNCTION 'OM_CREATE_NEW_RELATIONS'
                 EXPORTING
                   PARENT_OBJECT          = ld_pobject
                   CHILD_OBJECTS          = it_cobject
                   VRSIGN                 = sentidoRelacao
                   VRELAT                 = tipoRelacao
                   VBEGDA                 = sy-datum
                   VENDDA                 = AD_AETERNUM
                IMPORTING
                  RELATION_CREATED       = ld_flag.
         endif.
  endloop.

  CALL FUNCTION 'RHOM_WRITE_BUFFER_TO_DB'
    EXCEPTIONS
      DATABASE_ERROR       = 1
      CORR_EXIT            = 2
      OTHERS               = 3.
   IF SY-SUBRC NE 0.
     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
   endif.

endform.


FORM deletarItemHR USING objetoDelecao type TY_RELATORIO_DIFF .
  data: tipoRelacao(3) type c, sentidoRelacao type c.

  " TODO: DON'T USE DELETE DIRECTLY!
  
  if objetoDelecao-TIPOOBJETO eq TP_USUARIO.
      translate objetoDelecao-NOMECURTO to upper case.
      delete from HRP1001 where OBJID eq objetoDelecao-SOBID and SOBID eq objetoDelecao-NOMECURTO.
  else.
      "usuario não tem objeto na HRP1000
      delete from HRP1001 where OBJID eq objetoDelecao-OBJID or SOBID eq objetoDelecao-OBJID.
      delete from HRP1000 where OBJID eq objetoDelecao-OBJID.
  endif.

  write :/ 'Excluido: ', objetoDelecao-TIPOOBJETO, objetoDelecao-OBJID, objetoDelecao-SOBID, objetoDelecao-NOMECURTO.
endform.

form excluirRelacoesChefiaObsoletas.
    data: relacoesACortar type table of HRP1001-OBJID, delObjid type HRP1001-OBJID.
    select distinct r~objid into table relacoesACortar
      from HRP1001 as r
      inner join HRP1000 as o on o~OBJID eq r~OBJID
      where r~RELAT eq '012' and o~SHORT not like '@%'.

    loop at relacoesACortar into delObjid.
        write :/ OP_DELETAR, TP_POSICAO, delObjid.
        if p_deleta eq abap_true.
            delete from HRP1001 where RELAT eq '012' and OBJID eq delObjid.
        endif.
    endloop.
endform.

form commitarAlteracoesBuffer.
   CALL FUNCTION 'RH_UPDATE_DATABASE'
    EXPORTING
      VTASK              = OP_DIRECT
   EXCEPTIONS
     CORR_EXIT          = 1
     OTHERS             = 2.
   IF SY-SUBRC NE 0.
     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
   endif.
endform.


*&----------------------------------------------------------------------*
*& LDAP data import functions
*&----------------------------------------------------------------------*

form carregarInformacoesLDAP.
    data componentes type hashed table of String with unique key table_line.
    data strFilter type LDAPDEFS-FILT.
    data strUser type LDAPDEFS-USR.
    data strPass type LDAPDEFS-PWD.

    IF pUsr is initial or pPwd is initial.
      MESSAGE 'Login/Senha não informados!' TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.

    strUser = 'CN=' && pUsr && ',OU=Employees,OU=Users,DC=myfirm,DC=com'.
    strPass = pPwd.

    CALL FUNCTION 'LDAP_SIMPLEBIND'
     EXPORTING
       SERVERID           = 'CON_LDAP'
       USR                = strUser
       PWD                = strPass
     EXCEPTIONS
       NO_AUTHORIZ        = 1
       CONFIG_ERROR       = 2
       NOMORE_CONNS       = 3
       LDAP_FAILURE       = 4
       NOT_ALIVE          = 5
       OTHER_ERROR        = 6
       OTHERS             = 7.
    IF SY-SUBRC NE 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    endif.

	strFilter = '(&(objectCategory=person)(objectClass=user)(CN=*))'.

	DATA: ENTRIES TYPE LDAPETAB.
	CALL FUNCTION 'LDAP_READ'
	 EXPORTING
	   BASE                = 'OU=Users,DC=myfirm,DC=com'
	   FILTER              = strFilter
	 IMPORTING
	   ENTRIES             = ENTRIES
	 EXCEPTIONS
	   OTHERS              = 6.
	IF SY-SUBRC NE 0.
	   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
	endif.

	field-symbols: <entriesInfo> type LDAPE, <atrib> type LDAPAL, <val> type VALSTRUCTC.

	data: empregado type TY_DADOS_LDAP.

	LOOP AT ENTRIES assigning <entriesInfo>.

		 clear: empregado.

		 loop at <entriesInfo>-ATTRIBUTES assigning <atrib>.
			  if <atrib>-NAME eq 'DEPARTMENT'.
				  loop at <atrib>-VALS assigning <val>.
					  if <val>-VAL(1) eq BARRA.
						  empregado-DEPARTAMENTO = <val>-VAL.
					  endif.
				  endloop.
			  elseif <atrib>-NAME eq 'NAME'.
				  loop at <atrib>-VALS assigning <val>.
					  empregado-LOGIN = <val>-VAL.
				  endloop.
			  elseif <atrib>-NAME eq 'TITLE'.
				  loop at <atrib>-VALS assigning <val>.
					  empregado-FUNCAO = <val>-VAL.
				  endloop.
			  elseif <atrib>-NAME eq 'MANAGER'.
				  loop at <atrib>-VALS assigning <val>.
					  if <val>-VAL+9(1) eq ','.
						  empregado-GERENTE = <val>-VAL+3(6).
					  else.
						  empregado-GERENTE = <val>-VAL+3(7).
					  endif.
				  endloop.
			  endif.
		 endloop.

		 if empregado-DEPARTAMENTO is not initial
		   and strlen( empregado-DEPARTAMENTO ) > 2
		   and empregado-FUNCAO is not initial
		   and strlen( empregado-FUNCAO ) > 2.
				perform ajustarRotuloDepartamento changing empregado-DEPARTAMENTO.
				insert empregado into table TB_DADOS_LDAP.
				insert empregado-GERENTE into table TB_CHEFES.
		 endif.
	ENDLOOP.

    CALL FUNCTION 'LDAP_UNBIND'
     EXCEPTIONS
       CONN_OUTDATE       = 1
       LDAP_FAILURE       = 2
       NOT_ALIVE          = 3
       OTHER_ERROR        = 4
       OTHERS             = 5.
endform.

form incluiRenomearRotulo using nomeOriginal nomeCorrigido.
    data unid type TY_AJUSTES_ROTULOS.
    unid-ORIGINAL = nomeOriginal.
    unid-CORRIGIDO = nomeCorrigido.
    insert unid into table TB_AJUSTES_ROTULOS.
endform.

form ajustarRotuloDepartamento changing nomeOriginalLdap.
    field-symbols: <ren> type TY_AJUSTES_ROTULOS.
    loop at TB_AJUSTES_ROTULOS assigning <ren>.
        replace all occurrences of <ren>-ORIGINAL in nomeOriginalLdap with <ren>-CORRIGIDO.
    endloop.
endform.

form imprimeDumpTabelaLdap.
   loop at TB_DADOS_LDAP assigning field-symbol(<dados>).
        write :/ <dados>-LOGIN && ';' && <dados>-FUNCAO && ';' && <dados>-DEPARTAMENTO && ';' && <dados>-GERENTE.
   endloop.
endform.

form importaTabelaLdapArquivo.
  data: linha type String, LW_ARQUIVO_ENTRADA TYPE TABLE OF STRING,
        tValues type table of string, novo type TY_DADOS_LDAP.
    IF pfArq is initial.
      MESSAGE 'Arquivo não informado!' TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
    CL_GUI_FRONTEND_SERVICES=>GUI_UPLOAD(
      EXPORTING
        FILENAME = pfArq
      CHANGING
        DATA_TAB = LW_ARQUIVO_ENTRADA
      EXCEPTIONS
        OTHERS = 17
    ).
    IF sy-subrc ne 0.
      MESSAGE 'Erro ao abrir o arquivo!' TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
    LOOP AT LW_ARQUIVO_ENTRADA INTO linha.
        SPLIT linha AT ';' INTO novo-LOGIN novo-FUNCAO novo-DEPARTAMENTO novo-GERENTE.
        insert novo into table TB_DADOS_LDAP.
        insert novo-GERENTE into table TB_CHEFES.
    ENDLOOP.
endform.

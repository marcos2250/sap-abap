class ZCLSTRINGUTILS definition
  public
  final
  create public .

public section.

  types:
    packDecimal TYPE p LENGTH 16 DECIMALS 2 .

  methods CHARCOUNT
    importing
      !VALUE type STRING
      !CHR type C
    returning
      value(V_OUTPUT) type I .
  methods CRIASTRING
    importing
      !LEN type I
    returning
      value(V_OUTPUT) type STRING .
  methods DESFORMATADATA
    importing
      !p_in        TYPE string
    returning
      VALUE(p_out) TYPE datum .
  methods DESFORMATANUMC11
    importing
      !INPUT type STRING
    returning
      value(V_OUTPUT) type STRING .
  methods DESFORMATANUMC14
    importing
      !INPUT type STRING
    returning
      value(V_OUTPUT) type STRING .
  methods DESFORMATANUMERAL
    importing
      !V_INPUT type STRING
    returning
      value(V_OUTPUT) type STRING .
  methods FINDTOKEN
    importing
      !VALUE type STRING
      !CHR type C
      !INDEX type I
    returning
      value(V_OUTPUT) type STRING .
  METHODS filtrar_numeros
    importing
      !p_in        TYPE string
      RETURNING
      VALUE(p_out) TYPE datum .
  METHODS formatacep
    importing
      !VALUE type STRING
    returning
      value(V_OUTPUT) type STRING .
  methods FORMATACNPJ
    importing
      !VALUE type STRING
    returning
      value(V_OUTPUT) type STRING .
  methods FORMATACPF
    importing
      !VALUE type STRING
    returning
      value(V_OUTPUT) type STRING .
  methods FORMATANUMERAL
    importing
      !VALUE type P
    returning
      value(V_OUTPUT) type STRING .
  methods FORMATANUMERALJUST
    importing
      !VALUE type P
      !LARG type I
    returning
      value(V_OUTPUT) type STRING .
  methods FORMATANUMERALTRUNC
    importing
      !V_INPUT type P
    returning
      value(V_OUTPUT) type STRING .
  methods INDEXOF
    importing
      !VALUE type STRING
      !CHR type C
    returning
      value(V_OUTPUT) type I .
  methods INVERTEDATA
    importing
      !V_INPUT type STRING
    returning
      value(V_OUTPUT) type STRING .
  methods JSUBSTRING
    importing
      !INPUT type STRING
      !ST type I
      !END type I
    returning
      value(V_OUTPUT) type STRING .
  methods JUSTIFY
    importing
      !VALUE type STRING
      !LARG type I
    returning
      value(V_OUTPUT) type STRING .      
  methods LASTINDEXOF
    importing
      !VALUE type STRING
      !CHR type C
    returning
      value(V_OUTPUT) type I .
  methods LASTTOKEN
    importing
      !VALUE type STRING
      !CHR type C
    returning
      value(V_OUTPUT) type STRING .
  methods PONTUANUMERAL
    importing
      !VALUE type STRING
      !SCALE type I
    returning
      value(V_OUTPUT) type STRING .
  methods REMOVEACENTOS
    importing
      !V_INPUT type STRING
    returning
      value(V_OUTPUT) type STRING .
  methods STRINGLEFT
    importing
      !VALUE type STRING
      !CHARS type STRING
    returning
      value(V_OUTPUT) type STRING .
  methods SUBSTRING
    importing
      !INPUT type STRING
      !ST type I
      !LEN type I
    returning
      value(V_OUTPUT) type STRING .
  methods TOPACKEDDECIMAL
    importing
      !V_INPUT type STRING
    returning
      value(V_OUTPUT) type PACKDECIMAL .
  methods TOSTRING
    importing
      !V_INPUT type ANY
    returning
      value(V_OUTPUT) type STRING .
  methods TRIM
    importing
      !V_INPUT type STRING
    returning
      value(V_OUTPUT) type STRING .
  methods CONV_CSV_PARA_TAB
    importing
      !V_INPUT type STRING
    returning
      value(V_OUTPUT) type ZTSTRINGS .
protected section.
private section.
ENDCLASS.



CLASS ZCLSTRINGUTILS IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCLSTRINGUTILS->CHARCOUNT
* +-------------------------------------------------------------------------------------------------+
* | [--->] VALUE                          TYPE        STRING
* | [--->] CHR                            TYPE        C
* | [<-()] V_OUTPUT                       TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD charCount.
  FIND ALL OCCURRENCES OF CHR IN VALUE MATCH COUNT v_output.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCLSTRINGUTILS->CONV_CSV_PARA_TAB
* +-------------------------------------------------------------------------------------------------+
* | [--->] V_INPUT                        TYPE        STRING
* | [<-()] V_OUTPUT                       TYPE        ZTSTRINGS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD conv_csv_para_tab.
    DATA:
      lin     TYPE string,
      buf     TYPE string,
      len     TYPE i, pos TYPE i,
      chr     TYPE c, quo TYPE c,
      sep     TYPE c, buf_tam TYPE i.

    lin = v_input.
    len = strlen( lin ).
    IF len > 102400.
      len = 102400.
    ENDIF.

** identificar qual o separador
    DO len TIMES.
      pos = sy-index - 1.
      chr = lin+pos(1).
      IF sep IS INITIAL
        AND ( chr = ';' OR chr = ',' OR chr = '|' OR chr = cl_abap_char_utilities=>horizontal_tab ).
        sep = chr.
        EXIT.
      ENDIF.
    ENDDO.

** tokenizar a string, ignorando trechos entre parenteses
    DO len TIMES.
      pos = sy-index - 1.
      chr = lin+pos(1).
      IF chr = '"'.
        IF quo = abap_true.
          quo = abap_false.
        ELSE.
          quo = abap_true.
        ENDIF.
        CONTINUE.
      ENDIF.
      IF quo IS INITIAL AND chr = sep OR sy-index = len.
        IF sy-index = len AND chr <> sep.
          CONCATENATE buf chr INTO buf RESPECTING BLANKS.
        ENDIF.
        APPEND buf TO v_output.
        CLEAR buf.
      ELSE.
        CONCATENATE buf chr INTO buf RESPECTING BLANKS.
      ENDIF.
    ENDDO.

    IF buf IS NOT INITIAL.
      APPEND buf TO v_output.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCLSTRINGUTILS->CRIASTRING
* +-------------------------------------------------------------------------------------------------+
* | [--->] LEN                            TYPE        I
* | [<-()] V_OUTPUT                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
 METHOD criaString.
   v_output = repeat( val = ` ` occ = len ).
 ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCLSTRINGUTILS->DESFORMATADATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] P_IN                           TYPE        STRING
* | [<-()] P_OUT                          TYPE        DATUM
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD desformatadata.
    DATA lv_txt TYPE c LENGTH 20.
    lv_txt = p_in.
    REPLACE ALL OCCURRENCES OF REGEX '[^\d]' IN lv_txt WITH space.
    CONDENSE lv_txt NO-GAPS.
    IF strlen( lv_txt ) = 8.
      p_out = lv_txt+4(4) && lv_txt+2(2) && lv_txt(2).
    ELSEIF strlen( lv_txt ) = 6.
      p_out = '20' && lv_txt+4(2) && lv_txt+2(2) && lv_txt(2).
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCLSTRINGUTILS->DESFORMATANUMC11
* +-------------------------------------------------------------------------------------------------+
* | [--->] INPUT                          TYPE        STRING
* | [<-()] V_OUTPUT                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
 METHOD desformataNumc11.
   data: x_input type string, campo type c length 11.
   x_input = input.
   REPLACE ALL OCCURRENCES OF REGEX '[^\d]' IN x_input WITH SPACE.
   condense x_input NO-GAPS.
   write x_input to campo RIGHT-JUSTIFIED.
   OVERLAY campo WITH '00000000000'.
   v_output = campo.
 ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCLSTRINGUTILS->DESFORMATANUMC14
* +-------------------------------------------------------------------------------------------------+
* | [--->] INPUT                          TYPE        STRING
* | [<-()] V_OUTPUT                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
 METHOD desformataNumc14.
   data: x_input type string, campo type c length 14.
   x_input = input.
   REPLACE ALL OCCURRENCES OF REGEX '[^\d]' IN x_input WITH SPACE.
   condense x_input NO-GAPS.
   write x_input to campo RIGHT-JUSTIFIED.
   OVERLAY campo WITH '00000000000000'.
   v_output = campo.
 ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCLSTRINGUTILS->DESFORMATANUMERAL
* +-------------------------------------------------------------------------------------------------+
* | [--->] V_INPUT                        TYPE        STRING
* | [<-()] V_OUTPUT                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD desformatanumeral.
    DATA:
      len          TYPE i,
      cont         TYPE i,
      ultimoponto  TYPE i,
      caractere    TYPE c,
      numerais     TYPE string,
      contnumerais TYPE i.

    len = strlen( v_input ).
    cont = 0.
    ultimoponto = -1.
    WHILE cont < len.
      caractere = v_input+cont(1).
      IF caractere EQ '.' OR caractere EQ ','.
        ultimoponto = contnumerais.
      ELSEIF caractere EQ ' '.
        " ignorar espacos
      ELSEIF caractere CO '1234567890-'.
        numerais = numerais && caractere.
        ADD 1 TO contnumerais.
      ENDIF.
      ADD 1 TO cont.
    ENDWHILE.
    IF numerais IS INITIAL OR numerais = '-'.
      EXIT.
    ENDIF.
    IF ultimoponto >= 0 AND ultimoponto < contnumerais.
      cont = contnumerais - ultimoponto.
      v_output = numerais+0(ultimoponto) && '.' && numerais+ultimoponto(cont).
    ELSE.
      v_output = numerais.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCLSTRINGUTILS->FINDTOKEN
* +-------------------------------------------------------------------------------------------------+
* | [--->] VALUE                          TYPE        STRING
* | [--->] CHR                            TYPE        C
* | [--->] INDEX                          TYPE        I
* | [<-()] V_OUTPUT                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD findToken.
  data: contaToken type i, pos type i, len type i, inicio type i, fim type i.
  len = strlen( value ).
  contaToken = 0.
  pos = 0.
  inicio = -1.
  fim = -1.
  WHILE pos < len.
    IF value+pos(1) eq chr.
       add 1 to contaToken.
       if contaToken eq index.
          inicio = pos + 1.
       elseif inicio >= 0.
          fim = pos.
          exit.
       endif.
    ENDIF.
    add 1 to pos.
  ENDWHILE.
  if inicio < 0.
    v_output = ''.
  elseif fim < 0.
    v_output = value+inicio.
  else.
    fim = fim - inicio.
    v_output = value+inicio(fim).
  endif.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCLSTRINGUTILS->FILTRAR_NUMEROS
* +-------------------------------------------------------------------------------------------------+
* | [--->] P_IN                           TYPE        STRING
* | [<-()] P_OUT                          TYPE        DATUM
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD filtrar_numeros.
    DATA lv_txt TYPE c LENGTH 20.
    lv_txt = p_in.
    REPLACE ALL OCCURRENCES OF REGEX '[^\d]' IN lv_txt WITH space.
    CONDENSE lv_txt NO-GAPS.
    p_out = lv_txt.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCLSTRINGUTILS->FORMATACEP
* +-------------------------------------------------------------------------------------------------+
* | [--->] VALUE                          TYPE        STRING
* | [<-()] V_OUTPUT                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD formatacep.
    DATA campo(8) TYPE c.
    WRITE value TO campo RIGHT-JUSTIFIED.
    OVERLAY campo WITH '00000000'.
    v_output = campo+0(2) && '.' && campo+2(3) && '-' && campo+5(3).
  ENDMETHOD.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCLSTRINGUTILS->FORMATACNPJ
* +-------------------------------------------------------------------------------------------------+
* | [--->] VALUE                          TYPE        STRING
* | [<-()] V_OUTPUT                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
 METHOD formataCnpj.
   data campo(14) type c.
   write value to campo RIGHT-JUSTIFIED.
   OVERLAY campo WITH '00000000000000'.
   v_output = campo+0(2) && '.' && campo+2(3) && '.' && campo+5(3) && '/' && campo+8(4) && '-' && campo+12(2).
 ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCLSTRINGUTILS->FORMATACPF
* +-------------------------------------------------------------------------------------------------+
* | [--->] VALUE                          TYPE        STRING
* | [<-()] V_OUTPUT                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
 METHOD formataCpf.
   data campo(14) type c.
   write value to campo RIGHT-JUSTIFIED.
   OVERLAY campo WITH '00000000000000'.
   v_output = campo+3(3) && '.' && campo+6(3) && '.' && campo+9(3) && '-' && campo+12(2).
 ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCLSTRINGUTILS->FORMATANUMERAL
* +-------------------------------------------------------------------------------------------------+
* | [--->] VALUE                          TYPE        P
* | [<-()] V_OUTPUT                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD formataNumeral.
  data saida(21) type c.
  write value to saida.
  condense saida.
  v_output = saida.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCLSTRINGUTILS->FORMATANUMERALJUST
* +-------------------------------------------------------------------------------------------------+
* | [--->] VALUE                          TYPE        P
* | [--->] LARG                           TYPE        I
* | [<-()] V_OUTPUT                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD formataNumeralJust.
  data: saida(25) type c, off type i.
  write value to saida.
  off = 24 - larg.
  if off < 0.
     off = 0.
  endif.
  v_output = saida+off(larg).
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCLSTRINGUTILS->FORMATANUMERALTRUNC
* +-------------------------------------------------------------------------------------------------+
* | [--->] V_INPUT                        TYPE        P
* | [<-()] V_OUTPUT                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD formatanumeraltrunc.
    IF v_input = 0.
      v_output = '0.00'.
      EXIT.
    ENDIF.

    DATA valor TYPE p LENGTH 16 DECIMALS 6.
    IF v_input < 0.
      valor = v_input * -1.
    ELSE.
      valor = v_input.
    ENDIF.

    DATA: lv_int_char    TYPE string, lv_dec_char(6) TYPE c.
    lv_int_char = valor DIV 1.
    lv_dec_char = valor MOD 1.
    CONDENSE lv_dec_char.
    OVERLAY lv_dec_char WITH '0.0000'.

    CONCATENATE lv_int_char '.' lv_dec_char+2(2) INTO v_output.
    CONDENSE v_output NO-GAPS.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCLSTRINGUTILS->INDEXOF
* +-------------------------------------------------------------------------------------------------+
* | [--->] VALUE                          TYPE        STRING
* | [--->] CHR                            TYPE        C
* | [<-()] V_OUTPUT                       TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD indexOf.
  if value CS chr.
      v_output = SY-FDPOS.
  else.
      v_output = -1.
  endif.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCLSTRINGUTILS->INVERTEDATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] V_INPUT                        TYPE        STRING
* | [<-()] V_OUTPUT                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
 METHOD inverteData.
   v_output = v_input+6(2) && v_input+4(2) && v_input+0(4).
 ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCLSTRINGUTILS->JUSTIFY
* +-------------------------------------------------------------------------------------------------+
* | [--->] VALUE                          TYPE        STRING
* | [--->] LARG                           TYPE        I
* | [<-()] V_OUTPUT                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD justify.
  data: gap type i.
  gap = larg - strlen( value ).
  if gap < 0.
     gap = 0.
  endif.
  data spaces type string.
  spaces = repeat( val = ` ` occ = gap ).
  concatenate spaces value into v_output respecting blanks.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCLSTRINGUTILS->JSUBSTRING
* +-------------------------------------------------------------------------------------------------+
* | [--->] INPUT                          TYPE        STRING
* | [--->] ST                             TYPE        I
* | [--->] END                            TYPE        I
* | [<-()] V_OUTPUT                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
 METHOD jsubstring.
   data len type i.
   len = end - st.
   v_output = input+st(len).
 ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCLSTRINGUTILS->LASTINDEXOF
* +-------------------------------------------------------------------------------------------------+
* | [--->] VALUE                          TYPE        STRING
* | [--->] CHR                            TYPE        C
* | [<-()] V_OUTPUT                       TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD lastIndexOf.
  data: pos type i, cont type i, len type i.
  len = strlen( value ).
  cont = 0.
  WHILE cont < len.
    IF value+cont(1) eq chr.
      pos = cont.
    ENDIF.
    add 1 to cont.
  ENDWHILE.
  v_output = pos.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCLSTRINGUTILS->LASTTOKEN
* +-------------------------------------------------------------------------------------------------+
* | [--->] VALUE                          TYPE        STRING
* | [--->] CHR                            TYPE        C
* | [<-()] V_OUTPUT                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD lastToken.
  data: pos type i, len type i, inicio type i, fim type i.
  len = strlen( value ).
  pos = 0.
  inicio = 0.
  WHILE pos < len.
    IF value+pos(1) eq chr.
       inicio = pos + 1.
    ENDIF.
    add 1 to pos.
  ENDWHILE.
  v_output = value+inicio.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCLSTRINGUTILS->PONTUANUMERAL
* +-------------------------------------------------------------------------------------------------+
* | [--->] VALUE                          TYPE        STRING
* | [--->] SCALE                          TYPE        I
* | [<-()] V_OUTPUT                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
 METHOD pontuaNumeral.
   data: x_input type string, ptPos type i, len type i.
   x_input = value.
   REPLACE ALL OCCURRENCES OF ',' IN x_input WITH ''.
   REPLACE ALL OCCURRENCES OF '.' IN x_input WITH ''.
   condense x_input no-gaps.
   len = strlen( x_input ).
   if len >= scale.
     ptPos = len - scale.
     v_output = x_input+0(ptPos) && '.' && x_input+ptPos(scale).
   ELSE.
     v_output = X_INPUT.
   endif.
 ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCLSTRINGUTILS->REMOVEACENTOS
* +-------------------------------------------------------------------------------------------------+
* | [--->] V_INPUT                        TYPE        STRING
* | [<-()] V_OUTPUT                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD removeacentos.
*Invoca funcao que ira remover os acentos
    CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
      EXPORTING
        intext            = v_input
*       INTEXT_LG         = 0
*       INTER_CP          = '0000'
*       INTER_BASE_CP     = '0000'
*       IN_CP             = '0000'
*       REPLACEMENT       = 46
      IMPORTING
        outtext           = v_output
*       OUTUSED           =
*       OUTOVERFLOW       =
      EXCEPTIONS
        invalid_codepage  = 1
        codepage_mismatch = 2
        internal_error    = 3
        cannot_convert    = 4
        fields_not_type_c = 5
        OTHERS            = 6.

    IF sy-subrc <> 0.
      v_output = v_input.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCLSTRINGUTILS->STRINGLEFT
* +-------------------------------------------------------------------------------------------------+
* | [--->] VALUE                          TYPE        STRING
* | [--->] CHARS                          TYPE        STRING
* | [<-()] V_OUTPUT                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
 METHOD stringLeft.
    DATA len TYPE i.
    SEARCH value FOR chars.
    IF sy-fdpos <> 0.
      len = sy-fdpos.
      v_output = VALUE(len).
    ELSE.
      v_output = value.
    ENDIF.
 ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCLSTRINGUTILS->SUBSTRING
* +-------------------------------------------------------------------------------------------------+
* | [--->] INPUT                          TYPE        STRING
* | [--->] ST                             TYPE        I
* | [--->] LEN                            TYPE        I
* | [<-()] V_OUTPUT                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
 METHOD substring.
   v_output = input+st(len).
 ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCLSTRINGUTILS->TOPACKEDDECIMAL
* +-------------------------------------------------------------------------------------------------+
* | [--->] V_INPUT                        TYPE        STRING
* | [<-()] V_OUTPUT                       TYPE        PACKDECIMAL
* +--------------------------------------------------------------------------------------</SIGNATURE>
 METHOD toPackedDecimal.
    v_output = me->desformatanumeral( v_input ).
 ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCLSTRINGUTILS->TOSTRING
* +-------------------------------------------------------------------------------------------------+
* | [--->] V_INPUT                        TYPE        ANY
* | [<-()] V_OUTPUT                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD toString.
  v_output = v_input && ''.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCLSTRINGUTILS->TRIM
* +-------------------------------------------------------------------------------------------------+
* | [--->] V_INPUT                        TYPE        STRING
* | [<-()] V_OUTPUT                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
 METHOD trim.
   data: x_input type string.
   x_input = v_input.
   condense x_input.
   v_output = x_input.
 ENDMETHOD.
ENDCLASS.

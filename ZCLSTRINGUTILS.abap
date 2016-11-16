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
  methods FINDTOKEN
    importing
      !VALUE type STRING
      !CHR type C
      !INDEX type I
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
  methods REMOVESPECIALCHARS
    importing
      !V_INPUT type STRING
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
* | Instance Public Method ZCLSTRINGUTILS->CRIASTRING
* +-------------------------------------------------------------------------------------------------+
* | [--->] LEN                            TYPE        I
* | [<-()] V_OUTPUT                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
 METHOD criaString.
   v_output = repeat( val = ` ` occ = len ).
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
   REPLACE ALL OCCURRENCES OF ',' IN x_input WITH ''.
   REPLACE ALL OCCURRENCES OF '.' IN x_input WITH ''.
   REPLACE ALL OCCURRENCES OF '-' IN x_input WITH ''.
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
   REPLACE ALL OCCURRENCES OF ',' IN x_input WITH ''.
   REPLACE ALL OCCURRENCES OF '.' IN x_input WITH ''.
   REPLACE ALL OCCURRENCES OF '-' IN x_input WITH ''.
   condense x_input NO-GAPS.
   write x_input to campo RIGHT-JUSTIFIED.
   OVERLAY campo WITH '00000000000000'.
   v_output = campo.
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
* | Instance Public Method ZCLSTRINGUTILS->REMOVESPECIALCHARS
* +-------------------------------------------------------------------------------------------------+
* | [--->] V_INPUT                        TYPE        STRING
* | [<-()] V_OUTPUT                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD REMOVESPECIALCHARS.
    DATA: X_INPUT  TYPE STRING,
          NO_SPLIT TYPE TABLE OF STRING.

    X_INPUT = V_INPUT.
    SPLIT X_INPUT AT '-' INTO TABLE NO_SPLIT.
    READ TABLE NO_SPLIT INDEX 1 INTO V_OUTPUT.
    IF STRLEN( V_OUTPUT ) < 10.
      V_OUTPUT = V_INPUT.
    ENDIF.
    REPLACE ALL OCCURRENCES OF REGEX '[^A-Za-z0-9\s]' IN V_OUTPUT WITH SPACE.
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
* | Instance Public Method ZCLSTRINGUTILS->STRINGLEFT
* +-------------------------------------------------------------------------------------------------+
* | [--->] VALUE                          TYPE        STRING
* | [--->] CHARS                          TYPE        STRING
* | [<-()] V_OUTPUT                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
 METHOD stringLeft.
    data len type i.
    SEARCH value FOR chars.
    len = sy-fdpos + strlen( chars ).
    v_output = value(len).
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
   data: len type i, cont type i, ultimoPonto type i, caractere type c,
         numerais type string, contNumerais type i.
   len = strlen( v_input ).
   cont = 0.
   ultimoPonto = -1.
   WHILE cont < len.
     caractere = v_input+cont(1).
     if caractere eq '.' or caractere eq ','.
       ultimoPonto = contNumerais.
     elseif caractere eq ' '.
       " ignorar espacos
     else.
       numerais = numerais && caractere.
       add 1 to contNumerais.
     endif.
     add 1 to cont.
   ENDWHILE.
   if ultimoPonto >= 0 and ultimoPonto < contNumerais.
      cont = contNumerais - ultimoPonto.
      v_output = numerais+0(ultimoPonto) && '.' && numerais+ultimoPonto(cont).
   else.
      v_output = numerais.
   endif.
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

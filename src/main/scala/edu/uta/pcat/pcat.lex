/********************************************************************************
*
* File: pcat.lex
* The PCAT scanner
*
********************************************************************************/

package edu.uta.pcat;

import java_cup.runtime.Symbol;

%%
%class PcatLex
%public
%line
%column
%cup
%state comment

DIGIT=[0-9]
ID=[a-zA-Z][a-zA-Z0-9_]*

%{

  private Symbol symbol ( int type ) {
    return new Symbol(type, yyline, yycolumn);
  }

  private Symbol symbol ( int type, Object value ) {
    return new Symbol(type, yyline, yycolumn, value);
  }

  public void lexical_error ( String message ) {
    throw new Error("*** Lexical Error: " + message + " (line: " + yyline
                    + ", position: " + yycolumn + ")");
  }
%}

%%

"BEGIN"         { return symbol(sym.BEGIN); }
{DIGIT}+    { return symbol(sym.INTEGER_LITERAL,new Integer(yytext())); }
{DIGIT}+"."{DIGIT}+ { return symbol(sym.REAL_LITERAL,new Float(yytext())); }
"IF" { return symbol(sym.IF); }
"AND" { return symbol(sym.AND); }
"FOR" { return symbol(sym.FOR);}
"IS" { return symbol(sym.IS);}
"LOOP" { return symbol(sym.LOOP);}
"ARRAY" { return symbol(sym.ARRAY);}
"BY" { return symbol(sym.BY);}
"DIV" { return symbol(sym.DIV);}
"DO" { return symbol(sym.DO);}
"ELSE" { return symbol(sym.ELSE); }
"ELSIF" { return symbol(sym.ELSIF); }
"END" { return symbol(sym.END);}
"OR" { return symbol(sym.OR); }
"NOT" { return symbol(sym.NOT); }
"MOD" { return symbol(sym.MOD);}
"OF" { return symbol(sym.OF); } 
"DIV" { return symbol(sym.DIV); }
"EXIT" { return symbol(sym.EXIT); }
"PROCEDURE" { return symbol(sym.PROCEDURE); }
"PROGRAM" { return symbol(sym.PROGRAM); }
"READ" { return symbol(sym.READ); }
"RECORD" { return symbol(sym.READ); }
"RETURN" { return symbol(sym.RETURN); }
"THEN" { return symbol(sym.THEN); }
"TO" { return symbol(sym.TO); }
"TYPE" { return symbol(sym.TYPE); }
"VAR" { return symbol(sym.VAR); }
"WHILE" { return symbol(sym.WHILE); }
"WRITE" { return symbol(sym.WRITE); }
"(" { return symbol(sym.LPAREN); }
")" { return symbol(sym.RPAREN); }
"[" { return symbol(sym.LSQBRA); }
"]" { return symbol(sym.RSQBRA); }
"{" { return symbol(sym.LCUBRA); }
"}" { return symbol(sym.RCUBRA); }
"<>" { return symbol(sym.NEQ); }
"," { return symbol(sym.COMMA); }
";" { return symbol(sym.SEMI); }
":=" { return symbol(sym.ASGN); }
":" { return symbol(sym.COLON);}
"." { return symbol(sym.DOT);}
"+" { return symbol(sym.PLUS); }
"*" { return symbol(sym.TIMES); }
"-" { return symbol(sym.MINUS); }
"/" { return symbol(sym.SLASH); }
"=" { return symbol(sym.EQ); }
"<" { return symbol(sym.LT); }
">" { return symbol(sym.GT); }
"<=" { return symbol(sym.LEQ); }
">=" { return symbol(sym.GEQ); }
{ID}      { return symbol(sym.ID,yytext()); }
\"[^\"]*\"    { return symbol(sym.STRING_LITERAL,yytext().substring(1,yytext().length()-1)); }
[ \t\r\n\f] { /* ignore white spaces. */ }
.               { lexical_error("Illegal character"); }
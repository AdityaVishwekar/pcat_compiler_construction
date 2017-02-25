import java_cup.runtime.Symbol;
%%
%class CalcLex
%public
%line
%char
%cup
DIGIT=[0-9]
ID=[a-zA-Z][a-zA-Z0-9_]*

%%

{DIGIT}+ { return new Symbol(sym.INT,new Integer(yytext())); }
{DIGIT}+"."{DIGIT}+ { return new Symbol(sym.REALN,new Float(yytext())); }
"(" { return new Symbol(sym.LP); }
")" { return new Symbol(sym.RP); }
"," { return new Symbol(sym.COMMA); }
";" { return new Symbol(sym.SEMI); }
":=" { return new Symbol(sym.ASSIGN); }
"define" { return new Symbol(sym.DEFINE); }
"quit" { return new Symbol(sym.QUIT); }
"if" { return new Symbol(sym.IF); }
"then" { return new Symbol(sym.THEN); }
"else" { return new Symbol(sym.ELSE); }
"and" { return new Symbol(sym.AND); }
"or" { return new Symbol(sym.OR); }
"not" { return new Symbol(sym.NOT); }
"false" { return new Symbol(sym.FALSE); }
"true" { return new Symbol(sym.TRUE); }
"+" { return new Symbol(sym.PLUS); }
"*" { return new Symbol(sym.TIMES); }
"-" { return new Symbol(sym.MINUS); }
"/" { return new Symbol(sym.DIV); }
"=" { return new Symbol(sym.EQ); }
"<" { return new Symbol(sym.LT); }
">" { return new Symbol(sym.GT); }
"<=" { return new Symbol(sym.LE); }
"!=" { return new Symbol(sym.NE); }
">=" { return new Symbol(sym.GE); }
{ID} { return new Symbol(sym.ID,yytext()); }
\"[^\"]*\" { return new Symbol(sym.STRINGT,
yytext().substring(1,yytext().length()-1)); }
[ \t\r\n\f] { /* ignore white spaces. */ }
. { System.err.println("Illegal character: "+yytext()); }
/********************************************************************************
*
* File: pcat.cup
* The PCAT parser
*
********************************************************************************/

package edu.uta.pcat;

import scala.collection.immutable.*;
import scala.Tuple2;
import java_cup.runtime.*;


parser code {:

    static int[] tokens = {
      sym.error, sym.ID, sym.STRING_LITERAL, sym.INTEGER_LITERAL, sym.REAL_LITERAL,
      sym.AND, sym.ARRAY, sym.BEGIN, sym.BY, sym.DIV, sym.DO, sym.ELSE, sym.ELSIF, sym.END,
      sym.EXIT, sym.FOR, sym.IF, sym.IS, sym.LOOP, sym.MOD, sym.NOT, sym.OF, sym.OR,
      sym.PROCEDURE, sym.PROGRAM, sym.READ, sym.RECORD, sym.RETURN, sym.THEN, sym.TO,
      sym.TYPE, sym.VAR, sym.WHILE, sym.WRITE, sym.ASGN, sym.PLUS, sym.MINUS, sym.TIMES, sym.SLASH,
      sym.LT, sym.LEQ, sym.GT, sym.GEQ, sym.EQ, sym.NEQ, sym.COLON, sym.SEMI, sym.COMMA, sym.DOT,
      sym.LPAREN, sym.RPAREN, sym.LSQBRA, sym.RSQBRA, sym.LCUBRA, sym.RCUBRA
    };

    static String[] token_names = {
      "error", "ID", "STRING_LITERAL", "INTEGER_LITERAL", "REAL_LITERAL",
      "AND", "ARRAY", "BEGIN", "BY", "DIV", "DO", "ELSE", "ELSIF", "END",
      "EXIT", "FOR", "IF", "IS", "LOOP", "MOD", "NOT", "OF", "OR",
      "PROCEDURE", "PROGRAM", "READ", "RECORD", "RETURN", "THEN", "TO",
      "TYPE", "VAR", "WHILE", "WRITE", "ASGN :=", "PLUS +", "MINUS -", "TIMES *", "SLASH /",
      "LT <", "LEQ <=", "GT >", "GEQ >=", "EQ =", "NEQ <>", "COLON :", "SEMI ;", "COMMA ,", "DOT .",
      "LPAREN (", "RPAREN )", "LSQBRA [", "RSQBRA ]", "LCUBRA "+'{', "RCUBRA "+'}'
    };

    public static String print ( Symbol s ) {
        for ( int i = 0; i < tokens.length; i++ ) {
            if (tokens[i] == s.sym) {
                String res = token_names[i] + " ";
                if (s.value == null)
                    return res;
                else if (s.value instanceof Integer)
                    return res + ((Integer) s.value).intValue();
                else if (s.value instanceof Float)
                    return res + ((Float) s.value).floatValue();
                else if (s.value instanceof String)
                    return res + "\"" + (String) s.value + "\"";
            }
        };
        return "?";
    }

    public void syntax_error ( Symbol token ) {
        System.err.println("*** Syntax Error: " + print(token) + " (line: " + token.left
                           + ", column: " + token.right + ")");
        System.exit(1);
    }

    /** nil is the empty list */
    final static List nil = Nil$.MODULE$;

    /** Append list2 at the end of list1 (ie, return a new list containing
        the elements from list1 followed by the elements from list2) */
    static<T> List<T> append ( List<T> list1, List<T> list2 ) {
        return list1.$colon$colon$colon(list2);
    }

    /** append the element elem at the end of the list
       (ie, return a new list containing the list followed by the elem) */
    static<T> List<T> append ( List<T> list, T elem ) {
        return nil.$colon$colon(elem).$colon$colon$colon(list);
    }

    /** Insert the element elem at the beginning of the list
        (ie, return a new list containing the elem followed by the list) */
    static<T> List<T> add ( T elem, List<T> list ) {
        return list.$colon$colon(elem);
    }

:};

/* Terminals (tokens returned by the scanner). */
terminal String  ID, STRING_LITERAL;
terminal Integer INTEGER_LITERAL;
terminal Float   REAL_LITERAL;
terminal         AND, ARRAY, BEGIN, BY, DIV, DO, ELSE, ELSIF, END, EXIT, FOR, IF, IS, LOOP, MOD,
                 NOT, OF, OR, PROCEDURE, PROGRAM, READ, RECORD, RETURN, THEN, TO, TYPE, VAR, WHILE,
                 WRITE, ASGN, PLUS, MINUS, TIMES, SLASH, LT, LEQ, GT, GEQ, EQ, NEQ, COLON, SEMI,
                 COMMA, DOT, LPAREN, RPAREN, LSQBRA, RSQBRA, LCUBRA, RCUBRA, UMINUS;

non terminal             program;
non terminal Body        body;
non terminal Expr        expression;
non terminal Stmt        statement;
non terminal List<Stmt>  statementRec;
non terminal             number;
non terminal             array_init;
non terminal List<Expr>  array_inits;
non terminal             array_initsRec;
non terminal List<Expr>  record_inits;
non terminal             record_initsRec;
non terminal List<Expr>  actual_params;
non terminal List<Expr>  actual_paramsRec;
non terminal Lvalue      lvalue;
non terminal List<Lvalue> lvalueRec;
non terminal Expr        write_expr;
non terminal List<Expr>  write_params;
non terminal List<Expr>  write_paramsRec;
non terminal List<Tuple2<String,String>> fp_section;
non terminal List<Tuple2<String,String>> fp_sectionRec;
non terminal String      formal_params;
non terminal List<String> formal_paramsRec;
non terminal List<Tuple2<String,String>> component;
non terminal Type        type;
non terminal List<Tuple2<String,String>> componentRec;
non terminal ProcDecl    procedure_decl;
non terminal List<ProcDecl> procedure_decls;
non terminal List<ProcDecl> procedure_declsRec;
non terminal TypeDecl    type_decl;
non terminal List<TypeDecl> type_decls;
non terminal List<TypeDecl> type_declsRec;
non terminal VarDecl     var_decl;
non terminal List<VarDecl> var_decls;
non terminal List<VarDecl> var_declsRec;
non terminal Declaration declaration;
non terminal String      typename;
non terminal List<Declaration> declaration_lst;
non terminal             elsif_lst;
non terminal            var_decl_type;
non terminal List<Tuple2<List<String>,String>> procedure_decl_type;
non terminal Stmt       statement_else_type;
non terminal            statement_expression_type;
non terminal            for_decl_type;
non terminal            array_inits_type;
non terminal            array_init_type;

precedence nonassoc ELSE;
precedence nonassoc ELSIF;
precedence right  OR;
precedence right  AND;
precedence nonassoc NOT;
precedence left   ASGN;
precedence left   EQ, LT, GT, LEQ, GEQ, NEQ;
precedence left   PLUS, MINUS;
precedence left   TIMES, SLASH, DIV, MOD;
precedence left   LPAREN, RPAREN;


start with program;

program         ::= PROGRAM IS body:b SEMI      {: PCAT.setAST(new ProcDecl("main","NoType",nil,b)); :}
                ;
body            ::= declaration_lst:dl BEGIN statementRec:sl END  {: RESULT = new Body(dl,sl); :}
                |   BEGIN statementRec:sl END       {: RESULT = new Body(nil,sl); :}
                ;
statementRec   ::= statement:s                       {: RESULT = add(s,nil); :}
                |   statementRec:sl statement:s      {: RESULT = append(sl,s); :}
                ;
declaration_lst ::= declaration:d                    {: RESULT = add(d,nil); :}
                |   declaration_lst:dl declaration:d {: RESULT = append(dl,d); :}  
                ;

/** Abstract syntax trees for declarations 
sealed abstract class Declaration
case class TypeDecls ( decls: List[TypeDecl] ) extends Declaration
case class VarDecls ( decls: List[VarDecl] ) extends Declaration
case class ProcDecls ( decls: List[ProcDecl] ) extends Declaration
*/
declaration     ::= VAR var_decls:vd             {: RESULT = new VarDecls(vd); :}
                | TYPE type_decls:td             {: RESULT = new TypeDecls(td); :}
                | PROCEDURE procedure_decls:pd   {: RESULT = new ProcDecls(pd); :}
                ;
var_decls       ::= var_decl:v                   {: RESULT = add(v,nil); :}            
                |   var_decl:v var_declsRec:vdr  {: RESULT = add(v,vdr); :} /* Insert the element at the beginning of the list*/
                ;
var_declsRec   ::= var_declsRec:vdr var_decl:v   {: RESULT = append(vdr,v); :} /* Insert the element'v' at the end of the list'vdr'*/
                |  var_decl:v                    {: RESULT = append(nil,v); :}
                ;
/** Abstract syntax trees for variable declarations */
// case class VarDecl ( names: List[String], typename: String, value: Expr )

var_decl        ::= ID ASGN expression:e SEMI                                    {: RESULT = new VarDecl(nil,null,e); :}
                |   ID fp_sectionRec:fl ASGN expression:e SEMI                   {: RESULT = new VarDecl(fl,null,e); :}
                |   ID fp_sectionRec:fl COLON typename:tn ASGN expression:e SEMI {: RESULT = new VarDecl(fl,tn,e); :}
                |   ID COLON typename:tn ASGN expression:e SEMI                  {: RESULT = new VarDecl(nil,tn,e); :}
                ;
type_decls      ::= type_decl:t type_declsRec:tl                {: RESULT = add(t,tl); :} 
                |   type_decl:t                                 {: RESULT = add(t,nil); :}
                ;
type_declsRec   ::= type_declsRec:tl AND type_decl:t            {: RESULT = append(tl,t); :}
                |   AND type_decl:t                             {: RESULT = append(nil,t); :}
                ;

/** Abstract syntax trees for type declarations */
// case class TypeDecl ( name: String, isType: Type )
type_decl       ::= typename:tn IS type:t SEMI               {: RESULT = new TypeDecl(tn,t); :}
                ;
procedure_decls ::= procedure_decl:p procedure_declsRec:pl   {: RESULT = add(p,pl); :}
                |   procedure_decl:p                         {: RESULT = add(p,nil); :}
                ;
procedure_declsRec::= procedure_declsRec:pl AND procedure_decl:p {: RESULT = append(pl,p); :}
                |   AND procedure_decl:p                         {: RESULT = append(nil,p); :}
                ;

/** Abstract syntax trees for procedure declarations */
// case class ProcDecl ( name: String, outtype: String, params: List[(List[String],String)], body: Body ) Error(No parameter for List)

procedure_decl  ::= ID:nm formal_params:fp procedure_decl_type:pdt IS body:b SEMI {: RESULT = new ProcDecl(nm,fp,pdt,b); :}
                |   ID:nm formal_params:fp IS body:b SEMI                         {: RESULT = new ProcDecl(nm,fp,nil,b); :}
                ;
procedure_decl_type ::= COLON typename:t            {: RESULT = append(nil,t); :}
                ;
typename        ::= ID:nm                         {: RESULT = new String(nm); :}      
                ;
/** Abstract syntax trees for types 
sealed abstract class Type
case class NamedType ( typename: String ) extends Type
case class ArrayType ( typename: String ) extends Type
case class RecordType ( components: List[(String,String)] ) extends Type
case class AnyType () extends Type
*/
type            ::= ARRAY OF typename:tn                          {: RESULT = new ArrayType(tn); :}
                |   RECORD component:c END                        {: RESULT = append(c,nil); :}
                |   RECORD component:c componentRec:cl END        {: RESULT = append(c,cl); :}
                ;
componentRec   ::= component:c                                    {: RESULT = append(c,nil); :}
                |  componentRec:cl component:c                    {: RESULT = append(cl,c); :}          
                ;
component       ::= ID:nm COLON typename:t SEMI                   {: RESULT = append(nm,t); :}
                ;
formal_params   ::= LPAREN fp_section:fs RPAREN                          {: RESULT = append(fs,nil); :}               
                |   LPAREN fp_section:fs formal_paramsRec:fpl RPAREN     {: RESULT = append(fs,fpl); :}
                |   LPAREN RPAREN                                        {: RESULT = append(nil,nil); :}
                ;
formal_paramsRec  ::= formal_paramsRec:fpl SEMI fp_section:fs  {: RESULT = append(fpl,fs); :}
                |   SEMI fp_section:fs                         {: RESULT = append(nil,fs); :}
                ;
fp_section      ::= ID:nm COLON typename:t                 {: RESULT = append(nm,t); :}
                |   ID fp_sectionRec:fl COLON typename:t   {: RESULT = append(fl,t); :}
                ;
fp_sectionRec     ::= fp_sectionRec:fl COMMA ID:nm         {: RESULT = append(fl,nm); :}   
                |   COMMA ID:nm                            {: RESULT = append(nil,nm); :}
                ;
/** Abstract syntax trees for statements
sealed abstract class Stmt
case class AssignSt ( destination: Lvalue, source: Expr ) extends Stmt
case class CallSt ( name: String, arguments: List[Expr] ) extends Stmt
case class ReadSt ( arguments: List[Lvalue] ) extends Stmt
case class WriteSt ( arguments: List[Expr] ) extends Stmt
case class IfSt ( condition: Expr, then_stmt: Stmt, else_stmt: Stmt ) extends Stmt
case class WhileSt ( condition: Expr, body: Stmt ) extends Stmt
case class LoopSt ( body: Stmt ) extends Stmt
case class ForSt ( variable: String, initial: Expr, step: Expr, increment: Expr, body: Stmt ) extends Stmt
case class ExitSt () extends Stmt
case class ReturnValueSt ( value: Expr ) extends Stmt
case class ReturnSt () extends Stmt
case class SeqSt ( stmts: List[Stmt] ) extends Stmt
*/
statement       ::=  lvalue:l ASGN expression:e SEMI              {: RESULT = new AssignSt(l,e); :}
                |    ID:nm actual_params:ap SEMI                  {: RESULT = new CallSt(nm,ap); :}
                |    READ LPAREN lvalue lvalueRec:ll RPAREN SEMI  {: RESULT = new ReadSt(ll); :}
                |    READ LPAREN lvalue RPAREN SEMI
                |    WRITE write_params:w SEMI                    {: RESULT = new WriteSt(w); :}
                |    IF expression:e THEN statementRec:st
                      END SEMI                                    {: RESULT = new IfSt(e,new SeqSt(st),null); :}
                |    IF expression THEN statementRec
                      elsif_lst
                      statement_else_type END SEMI                
                |    IF expression:e THEN statementRec:st
                      statement_else_type:es END SEMI               {: RESULT = new IfSt(e,new SeqSt(st),es); :}
                |    WHILE expression:e DO statementRec:st END SEMI {: RESULT = new WhileSt(e,new SeqSt(st)); :}
                |    WHILE expression:e DO END SEMI                 {: RESULT = new WhileSt(e,null); :}
                |    LOOP statementRec:st END SEMI                  {: RESULT = new LoopSt(new SeqSt(st)); :}
                |    LOOP END SEMI                                  {: RESULT = new LoopSt(null); :}
                |    FOR ID:nm ASGN expression:e1 TO expression:e2 BY expression:e3
                      DO END SEMI                                   {: RESULT = new ForSt(nm,e1,e2,e3,null); :}             
                |    FOR ID:nm ASGN expression:e1 TO expression:e2
                      DO statementRec:st END SEMI                   {: RESULT = new ForSt(nm,e1,e2,null,new SeqSt(st)); :}
                |    FOR ID:nm ASGN expression:e1 TO expression:e2 BY expression:e3
                      DO statementRec:st END SEMI                   {: RESULT = new ForSt(nm,e1,e2,e3,new SeqSt(st)); :}               
                |    EXIT SEMI                                      {: RESULT = new ExitSt(); :}
                |    RETURN expression:e SEMI {: RESULT = new ReturnValueSt(e); :}
                |    RETURN SEMI         {: RESULT = new ReturnSt(); :}             
                ;
lvalueRec       ::=  lvalueRec COMMA lvalue
                |    COMMA lvalue
                ;
statement_else_type ::= ELSE statementRec
                ;
/*
for_decl_type   ::= BY expression
                ;
*/
elsif_lst       ::=  elsif_lst ELSIF expression THEN statementRec
                |    ELSIF expression THEN statementRec
                ;
write_params    ::=  LPAREN write_expr:w RPAREN                     {: RESULT = add(w,nil); :} 
                |    LPAREN write_expr:w write_paramsRec:wl RPAREN  {: RESULT = add(w,wl); :} 
                |    LPAREN RPAREN
                ;
write_paramsRec   ::=  write_paramsRec:wl COMMA write_expr:w        {: RESULT = append(wl,w); :} 
                |    COMMA write_expr:w                             {: RESULT = append(nil,w); :} 
                ;
write_expr      ::=  STRING_LITERAL:s                          {: RESULT = new StringConst(s); :}
                |    expression:e                              
                ;
expression      ::=  INTEGER_LITERAL:n                          {: RESULT = new IntConst(n); :} 
                |    REAL_LITERAL:n                             {: RESULT = new RealConst(n); :}
                |    lvalue:l                                   {: RESULT = new LvalExp(l); :}
                |    LPAREN expression:e RPAREN                 {: RESULT = e; :}
                |    MINUS expression:e %prec UMINUS            
                |    NOT expression:e                           {: RESULT = new UnOpExp("not",e); :}
                |    expression:e1 PLUS expression:e2           {: RESULT = new BinOpExp("plus",e1,e2); :}
                |    expression:e1 MINUS expression:e2          {: RESULT = new BinOpExp("minus",e1,e2); :}
                |    expression:e1 TIMES expression:e2          {: RESULT = new BinOpExp("times",e1,e2); :}
                |    expression:e1 SLASH expression:e2          {: RESULT = new BinOpExp("slash",e1,e2); :}
                |    expression:e1 DIV expression:e2            {: RESULT = new BinOpExp("div",e1,e2); :}
                |    expression:e1 MOD expression:e2            {: RESULT = new BinOpExp("mod",e1,e2); :}
                |    expression:e1 OR expression:e2             {: RESULT = new BinOpExp("or",e1,e2); :}
                |    expression:e1 AND expression:e2            {: RESULT = new BinOpExp("and",e1,e2); :}
                |    expression:e1 GT expression:e2             {: RESULT = new BinOpExp("gt",e1,e2); :}
                |    expression:e1 LT expression:e2             {: RESULT = new BinOpExp("lt",e1,e2); :}
                |    expression:e1 EQ expression:e2             {: RESULT = new BinOpExp("eq",e1,e2); :}
                |    expression:e1 GEQ expression:e2            {: RESULT = new BinOpExp("geq",e1,e2); :}
                |    expression:e1 LEQ expression:e2            {: RESULT = new BinOpExp("leq",e1,e2); :}
                |    expression:e1 NEQ expression:e2            {: RESULT = new BinOpExp("neq",e1,e2); :}
                |    ID:nm actual_params:ap                     {: RESULT = new CallExp(nm,ap); :}
                |    ID:nm record_inits:ri                      {: RESULT = new CallExp(nm,ri); :}
                |    ID:nm array_inits:ai                       {: RESULT = new CallExp(nm,ai); :}
                ;
lvalue          ::=  ID:nm                                     {: RESULT = new Var(nm); :}
                |    lvalue:l LSQBRA expression:e RSQBRA       {: RESULT = new ArrayDeref(l,e); :}
                |    lvalue:l DOT ID:nm                        {: RESULT = new RecordDeref(l,nm); :}                     
                ;
actual_params   ::=  LPAREN expression:e actual_paramsRec:apl RPAREN {: RESULT = add(e,apl); :}
                |    LPAREN expression:e RPAREN                      {: RESULT = add(e,nil); :}
                |    LPAREN RPAREN                                   
                ;
actual_paramsRec::=  actual_paramsRec:apl COMMA expression:e    {: RESULT = append(apl,e); :}
                |    COMMA expression:e                         {: RESULT = append(nil,e); :}
                ;
record_inits    ::=  LCUBRA ID:nm ASGN expression:e record_initsRec:ri RCUBRA
                ;
record_initsRec ::=  record_initsRec SEMI ID ASGN expression    
                |    SEMI ID ASGN expression
                ;
array_inits     ::=  LCUBRA array_inits_type RCUBRA
                |    LCUBRA RCUBRA
                ;
array_inits_type::= array_init array_initsRec
                |   array_init
                ;
array_initsRec  ::=  array_initsRec COMMA array_init   
                |    COMMA array_init
                ;
array_init      ::= expression OF expression
                |   expression
                ;
/*
array_init_type ::= expression OF
                ;
number          ::= INTEGER_LITERAL:n       {: RESULT = new IntConst(n); :} 
                | REAL_LITERAL:n            {: RESULT = new RealConst(n); :}
                ;
*/
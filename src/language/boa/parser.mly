%{
  open Batteries;;

  open Expressions;;
%}

%token <int> INTEGER
%token <string> IDENTIFIER
%token INCREMENT DECREMENT LPAREN RPAREN LET IN EQUAL EOF
%token PLUS MINUS TIMES IF THEN ELSE

%right IN ELSE
%left PLUS MINUS
%left TIMES

%type <Expressions.expr> program

%start program

%%

program:
  | expr EOF { $1 }

expr:
  | constant { $1 }
  | IDENTIFIER { EVar($1) }
  | INCREMENT LPAREN expr RPAREN { EUnaryOp(OpInc,$3) }
  | DECREMENT LPAREN expr RPAREN { EUnaryOp(OpDec,$3) }
  | expr PLUS expr { EBinaryOp(OpPlus,$1,$3) }
  | expr MINUS expr { EBinaryOp(OpMinus,$1,$3) }
  | expr TIMES expr { EBinaryOp(OpTimes,$1,$3) }
  | LET identifier EQUAL expr IN expr { ELet($2,$4,$6) }
  | IF expr THEN expr ELSE expr { EIf($2,$4,$6) }
  | LPAREN expr RPAREN { $2 }

constant:
  | INTEGER { EInt($1) }

identifier:
  | IDENTIFIER { $1 }

%%

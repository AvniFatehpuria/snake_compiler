%{
  open Batteries;;

  open Expressions;;
%}

%token <int> INTEGER
%token <string> IDENTIFIER
%token INCREMENT DECREMENT LPAREN RPAREN LET IN EQUAL EOF

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
  | LET identifier EQUAL expr IN expr { ELet($2,$4,$6) }
  | LPAREN expr RPAREN { $2 }

constant:
  | INTEGER { EInt($1) }

identifier:
  | IDENTIFIER { $1 }

%%

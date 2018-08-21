%{
  open Batteries;;

  open Expressions;;
%}

%token <string> INTEGER
%token <string> IDENTIFIER
%token INCREMENT DECREMENT LPAREN RPAREN LET IN EQUAL EOF
%token PLUS MINUS TIMES IF THEN ELSE
%token PRINT ISINT ISBOOL LESS GREATER BOOL_AND BOOL_OR TRUE FALSE

%right IN ELSE
%left BOOL_OR
%left BOOL_AND
%nonassoc LESS GREATER EQUAL
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
  | PRINT LPAREN expr RPAREN { EUnaryOp(OpPrint,$3) }
  | ISINT LPAREN expr RPAREN { EUnaryOp(OpIsInt,$3) }
  | ISBOOL LPAREN expr RPAREN { EUnaryOp(OpIsBool,$3) }
  | expr PLUS expr { EBinaryOp(OpPlus,$1,$3) }
  | expr MINUS expr { EBinaryOp(OpMinus,$1,$3) }
  | expr TIMES expr { EBinaryOp(OpTimes,$1,$3) }
  | expr LESS expr { EBinaryOp(OpLessThan,$1,$3) }
  | expr GREATER expr { EBinaryOp(OpGreaterThan,$1,$3) }
  | expr EQUAL expr { EBinaryOp(OpEqualTo,$1,$3) }
  | expr BOOL_AND expr { EBinaryOp(OpAnd,$1,$3) }
  | expr BOOL_OR expr { EBinaryOp(OpOr,$1,$3) }
  | LET identifier EQUAL expr IN expr { ELet($2,$4,$6) }
  | IF expr THEN expr ELSE expr { EIf($2,$4,$6) }
  | LPAREN expr RPAREN { $2 }

constant:
  | INTEGER { EInt(int_of_string $1) }
  | MINUS INTEGER { EInt(int_of_string ("-" ^ $2)) }
  | TRUE { EBool(true) }
  | FALSE { EBool(false) }

identifier:
  | IDENTIFIER { $1 }

%%

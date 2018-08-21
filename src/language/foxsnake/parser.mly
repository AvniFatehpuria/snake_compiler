%{
  open Expressions;;
%}

%token <string> INTEGER
%token <string> IDENTIFIER
%token INCREMENT DECREMENT LPAREN RPAREN LET IN EQUAL EOF
%token PLUS MINUS TIMES IF THEN ELSE
%token PRINT ISINT ISBOOL LESS GREATER BOOL_AND BOOL_OR TRUE FALSE
%token DEF END COMMA
%token LBRACK RBRACK ISTUPLE
%token FUN ARROW

%nonassoc fake_FUNCTION_BODY
%right IN ELSE
%left BOOL_OR
%left BOOL_AND
%nonassoc LESS GREATER EQUAL
%left PLUS MINUS
%left TIMES
%nonassoc LBRACK

%type <Expressions.program> program

%start program

%%

program:
  | declList expr EOF { EProgram($1,$2) }
  | expr EOF { EProgram([],$1) }

declList:
  | decl declList { $1::$2 }
  | decl { $1::[] }

decl:
  | DEF identifier parameterList EQUAL expr END { EFunction($2,$3,$5) }

parameterList:
  | identifier parameterList { $1::$2 }
  | identifier { [$1] }

expr:
  | FUN IDENTIFIER ARROW expr { ELambda($2,$4) } %prec fake_FUNCTION_BODY
  | INCREMENT LPAREN expr RPAREN { EUnaryOp(OpInc,$3) }
  | DECREMENT LPAREN expr RPAREN { EUnaryOp(OpDec,$3) }
  | PRINT LPAREN expr RPAREN { EUnaryOp(OpPrint,$3) }
  | ISINT LPAREN expr RPAREN { EUnaryOp(OpIsInt,$3) }
  | ISBOOL LPAREN expr RPAREN { EUnaryOp(OpIsBool,$3) }
  | ISTUPLE LPAREN expr RPAREN { EUnaryOp(OpIsTuple,$3) }
  | expr PLUS expr { EBinaryOp(OpPlus,$1,$3) }
  | expr MINUS expr { EBinaryOp(OpMinus,$1,$3) }
  | expr TIMES expr { EBinaryOp(OpTimes,$1,$3) }
  | expr LESS expr { EBinaryOp(OpLessThan,$1,$3) }
  | expr GREATER expr { EBinaryOp(OpGreaterThan,$1,$3) }
  | expr EQUAL expr { EBinaryOp(OpEqualTo,$1,$3) }
  | expr BOOL_AND expr { EBinaryOp(OpAnd,$1,$3) }
  | expr BOOL_OR expr { EBinaryOp(OpOr,$1,$3) }
  | expr LBRACK expr RBRACK { EBinaryOp(OpTupleIndex,$1,$3) }
  | LET identifier EQUAL expr IN expr { ELet($2,$4,$6) }
  | IF expr THEN expr ELSE expr { EIf($2,$4,$6) }
  | LPAREN tupleArgumentList RPAREN { ETuple($2) }
  | MINUS INTEGER { EInt(int_of_string ("-" ^ $2)) }
  | applExpr { $1 }

applExpr:
  | applExpr primitiveExpr { EAppl($1,$2) }
  | primitiveExpr { $1 }

primitiveExpr:
  | INTEGER { EInt(int_of_string $1) }
  | TRUE { EBool(true) }
  | FALSE { EBool(false) }
  | IDENTIFIER { EVar($1) }
  | LPAREN expr RPAREN { $2 }

argumentList:
  | exprList { $1 }

tupleArgumentList:
  | expr COMMA exprList { $1::$3 }

exprList:
  | expr COMMA argumentList { $1::$3 }
  | expr { [$1] }

identifier:
  | IDENTIFIER { $1 }

%%

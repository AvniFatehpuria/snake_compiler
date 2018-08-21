{
  open Batteries;;

  open Lexing;;
  open Parser;;
  open Printf;;

  exception LexerError of string;;
}

let digit = ['0'-'9']
let integer = '-'? digit+

let ident_start = ['a'-'z' 'A'-'Z' '_']
let ident_cont = ident_start | ['0'-'9']
let ident = ident_start ident_cont*

let whitespace = [' ' '\t']+

rule token = parse
  | whitespace { token lexbuf }
  | '\n' { new_line lexbuf; token lexbuf }
  | integer as x { INTEGER(int_of_string x) }
  | "inc" { INCREMENT }
  | "dec" { DECREMENT }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "let" { LET }
  | "in" { IN }
  | "=" { EQUAL }
  | ident as x { IDENTIFIER x }
  | eof { EOF }
  | _ as c { raise (LexerError (sprintf "unrecognized character: %c" c)) }

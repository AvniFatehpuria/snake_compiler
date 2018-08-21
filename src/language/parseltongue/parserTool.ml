open Expressions;;
open Lexer;;
open Parser;;
exception ParseFailure of string;;

let parse (filename:string) (program:string) : program =
  let tokens = lex program in
  let (e, rest) = parse tokens in
  match rest with
  | [] -> print_endline (show_expr e); EProgram([], e)
  | _ -> print_endline (show_expr e); failwith "could not successfully parse entire file"

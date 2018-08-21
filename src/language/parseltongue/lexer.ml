open Batteries;;

type token =
  | TokIdentifier of string
  | TokInteger of int
  | TokLet
  | TokIn
  | TokEqual
  | TokInc
  | TokDec
  | TokOpenParen
  | TokClosedParen
  | TokPlus
  | TokTimes
  | TokMinus
  | TokTrue
  | TokFalse
  | TokLess
  | TokGreater
  | TokAnd
  | TokOr
  | TokPrint
  | TokIsBool
  | TokIsInt
  | TokIf
  | TokThen
  | TokElse
[@@deriving show]
;;

let is_ident_char (c : char) : bool =
  Char.is_letter c || Char.is_digit c || c = '_'
;;

let starts_with_keyword (s : string) (kw : string) : bool =
  let has_kw = String.starts_with s kw in
  let next_not_alpha =
    (String.length s <= String.length kw) ||
    let char_after_kw = (String.get s (String.length kw)) in
    not (is_ident_char char_after_kw)
  in
  has_kw && next_not_alpha
;;

let get_prefix_matching (f : char -> bool) (s : string) : string =
  let rec loop i =
    if String.length s > i && f (String.get s i) then
      loop (i+1)
    else
      i
  in
  let pfx_len = loop 0 in
  String.sub s 0 pfx_len
;;

let rec lex (s : string) : token list =
  if String.starts_with s "(" then
    let s' = String.lchop s in
    let tok = TokOpenParen in
    tok :: lex s'
  else if String.starts_with s ")" then
    let s' = String.lchop s in
    let tok = TokClosedParen in
    tok :: lex s'
  else if String.starts_with s "=" then
    let s' = String.lchop s in
    let tok = TokEqual in
    tok :: lex s'
  else if String.starts_with s "+" then
    let s' = String.lchop s in
    let tok = TokPlus in
    tok :: lex s'
  else if String.starts_with s "*" then
    let s' = String.lchop s in
    let tok = TokTimes in
    tok :: lex s'
  else if String.starts_with s "-" then
    let s' = String.lchop s in
    let tok = TokMinus in
    tok :: lex s'
  else if String.starts_with s "<" then
    let s' = String.lchop s in
    let tok = TokLess in
    tok :: lex s'
  else if String.starts_with s ">" then
    let s' = String.lchop s in
    let tok = TokGreater in
    tok :: lex s'
  else if String.starts_with s "&&" then
    let s' = String.tail s 2 in
    let tok = TokAnd in
    tok :: lex s'
  else if String.starts_with s "||" then
    let s' = String.tail s 2 in
    let tok = TokOr in
    tok :: lex s'
  else if starts_with_keyword s "let" then
    let s' = String.tail s 3 in
    let tok = TokLet in
    tok :: lex s'
  else if starts_with_keyword s "in" then
    let s' = String.tail s 2 in
    let tok = TokIn in
    tok :: lex s'
  else if starts_with_keyword s "inc" then
    let s' = String.tail s 3 in
    let tok = TokInc in
    tok :: lex s'
  else if starts_with_keyword s "dec" then
    let s' = String.tail s 3 in
    let tok = TokDec in
    tok :: lex s'
  else if starts_with_keyword s "isbool" then
    let s' = String.tail s 6 in
    let tok = TokIsBool in
    tok :: lex s'
  else if starts_with_keyword s "isint" then
    let s' = String.tail s 5 in
    let tok = TokIsInt in
    tok :: lex s'
  else if starts_with_keyword s "if" then
    let s' = String.tail s 2 in
    let tok = TokIf in
    tok :: lex s'
  else if starts_with_keyword s "then" then
    let s' = String.tail s 4 in
    let tok = TokThen in
    tok :: lex s'
  else if starts_with_keyword s "else" then
    let s' = String.tail s 4 in
    let tok = TokElse in
    tok :: lex s'
  else if starts_with_keyword s "print" then
    let s' = String.tail s 5 in
    let tok = TokPrint in
    tok :: lex s'
  else if starts_with_keyword s "true" then
    let s' = String.tail s 4 in
    let tok = TokTrue in
    tok :: lex s'
  else if starts_with_keyword s "false" then
    let s' = String.tail s 5 in
    let tok = TokFalse in
    tok :: lex s'


  else
    let pfx_digits = get_prefix_matching Char.is_digit s in
    if String.length pfx_digits > 0 then
      let s' = String.tail s (String.length pfx_digits) in
      let tok = TokInteger (int_of_string pfx_digits) in
      tok :: lex s'
    else
      let pfx_ident_chars = get_prefix_matching is_ident_char s in
      if String.length pfx_ident_chars > 0 then
        let s' = String.tail s (String.length pfx_ident_chars) in
        let tok = TokIdentifier pfx_ident_chars in
        tok :: lex s'
      else
      if String.length s = 0 then
        []
      else if Char.is_whitespace (String.get s 0) then
        lex (String.lchop s)
      else
        failwith ("Unrecognized character: " ^ s)
;;

let tokens = lex "inc(4)" in
let tokens_str = Printf.sprintf "[%s]" (String.join ", " (List.map show_token tokens)) in
print_endline tokens_str
;;

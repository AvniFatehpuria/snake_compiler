open Batteries;;
open Expressions;;
open Lexer;;

let rec parse (tokens : token list) : expr * token list =
  match tokens with
  | TokLet :: TokIdentifier x :: TokEqual :: rest ->
    let (e1,rest1) = parse rest in
    begin
      match rest1 with
      | TokIn :: rest1' ->
        let (e2,rest2) = parse rest1' in
        (ELet(x,e1,e2), rest2)
      | _ -> failwith "expected 'in' token"
    end
  | TokIf :: rest ->
    let (e1, rest1) = parse rest in
    begin
      match rest1 with
      | TokThen :: rest1' ->
        let (e2,rest2) = parse rest1' in
        begin
          match rest2 with
          | TokElse:: rest2' ->
            let (e3, rest3) = parse rest2' in
            (EIf(e1, e2, e3), rest3)
          | _ -> failwith "expected 'else' token"
        end
      | _ -> failwith "expected 'then' token"
    end
  | _ -> orParse tokens

and orParse (tokens : token list) : expr * token list =
 (* let (e, rest) = andParse tokens in
  match rest with
  | TokOr :: rest' ->
    let (e2, rest2) = orParse rest' in
    (EBinaryOp(OpOr, e, e2), rest2)
  | _ ->
    (e, rest)
*)
  let (e1::elist, toklist, rest) = helper andParse [TokOr] tokens in
  (List.fold_left (fun (x:expr) (y:expr) -> EBinaryOp(OpOr, x, y)) e1 elist, rest)

and helper (f: token list -> expr * token list) (nextlist: token list) (tokens: token list) : expr list * token list * token list  =
  let (e, rest) = f tokens in
  match rest with
 | [] -> ([e], [], [])
 | next::rest' ->
  if List.mem next nextlist then
  begin  
    match rest' with
  |  [] -> failwith "ends with operator"
  | _ ->
    let (e_rest,tok_rest, rest_rest) = helper f nextlist rest' in
    (e::e_rest, next::tok_rest, rest_rest)
    end
  else
    ([e], [], rest)
and andParse (tokens : token list) : expr * token list =
  (*let (e, rest) = comparisonParse tokens in
  match rest with
  | TokAnd :: rest' ->
    let (e2, rest2) = andParse rest' in
    (EBinaryOp(OpAnd, e, e2), rest2)
  | _ ->
    (e, rest)
*)
    let (e1::elist, toklist, rest) = helper comparisonParse [TokAnd] tokens in
      (List.fold_left (fun (x:expr) (y:expr) -> EBinaryOp(OpAnd, x, y)) e1 elist, rest)
        
      
and comparisonParse (tokens : token list) : expr * token list =
 (* let (e, rest) = additiveParse tokens in
  match rest with
  | TokLess :: rest' ->
    let (e2, rest2) = comparisonParse rest' in
    (EBinaryOp(OpLessThan, e, e2), rest2)
  | TokGreater :: rest' ->
    let (e2, rest2) = comparisonParse rest' in
    (EBinaryOp(OpGreaterThan, e, e2), rest2)
  | TokEqual :: rest' ->
    let (e2, rest2) = comparisonParse rest' in
    (EBinaryOp(OpEqualTo, e, e2), rest2)
  | _ ->
    (e, rest)*)
let (e1::elist, toklist, rest) = helper additiveParse [TokLess; TokGreater; TokEqual] tokens in
  (List.fold_left2 (fun (x:expr) (tok: token) (y:expr) ->
    match tok with
    | TokLess ->
      EBinaryOp(OpLessThan, x, y)
    | TokGreater ->
      EBinaryOp(OpGreaterThan, x, y)
    | TokEqual -> 
      EBinaryOp(OpEqualTo, x, y)) e1 toklist elist, rest)
          

and additiveParse (tokens : token list) : expr * token list =
  (*let (e, rest) = multiplicativeParse tokens in
  match rest with
  | TokPlus :: rest' ->
    st.rev (List.tl (List.rev lst))let (e2, rest2) = additiveParse rest' in
    (EBinaryOp(OpPlus, e, e2), rest2)
  | TokMinus :: rest' ->
    let (e2, rest2) = additiveParse rest' in
    (EBinaryOp(OpMinus, e, e2), rest2)
  | _ ->
    (e, rest)*)
let (e1::elist, toklist, rest) = helper multiplicativeParse [TokPlus; TokMinus] tokens in
  (List.fold_left2 (fun (x:expr) (tok: token) (y:expr) ->
   match tok with
   | TokPlus ->
       EBinaryOp(OpPlus, x, y)                    
   | TokMinus ->
       EBinaryOp(OpMinus, x, y)) e1 toklist elist, rest)




and multiplicativeParse (tokens : token list) : expr * token list =
  (*let (e, rest) = primaryParse tokens in
  match rest with
  | TokTimes :: rest' ->
    let (e2, rest2) = multiplicativeParse rest' in
    (EBinaryOp(OpTimes, e, e2), rest2)
  | _ ->
    (e, rest)
*)
let (e1::elist, toklist, rest) = helper primaryParse [TokTimes] tokens in
(List.fold_left (fun (x:expr)  (y:expr) -> EBinaryOp(OpTimes, x, y)) e1 elist, rest)
       
       
and primaryParse (tokens : token list) : expr * token list =
  match tokens with
  | TokTrue :: rest -> (EBool(true), rest)
  | TokFalse :: rest -> (EBool(false), rest)
  | TokInteger x :: rest -> (EInt(x), rest)
  | TokMinus :: TokInteger x :: rest -> (EInt(-1*x), rest)
  | TokIdentifier x :: rest -> (EVar(x), rest)
  | TokInc :: rest ->
    let (e1, rest1) = parse rest in
    (EUnaryOp(OpInc, e1), rest1)
  | TokDec :: rest ->
    let (e1, rest1) = parse rest in
    (EUnaryOp(OpDec, e1), rest1)
  | TokPrint :: rest ->
    let (e1, rest1) = parse rest in
    (EUnaryOp(OpPrint, e1), rest1)
  | TokIsInt :: rest ->
    let (e1, rest1) = parse rest in
    (EUnaryOp(OpIsInt, e1), rest1)
  | TokIsBool :: rest ->
    let (e1, rest1) = parse rest in
    (EUnaryOp(OpIsBool, e1), rest1)
  | TokOpenParen :: rest ->
    let (e1, rest1) = parse rest in
    begin
      match rest1 with
      | TokClosedParen::rest1' ->let tokens_str = Printf.sprintf "[%s]" (String.join ", " (List.map show_token rest1)) in
      print_endline tokens_str;(e1, rest1')
      | _ -> let tokens_str = Printf.sprintf "[%s]" (String.join ", " (List.map show_token rest1)) in
      print_endline tokens_str; failwith "expected closed parenthesis"
    end
  | _ -> failwith "how did you even get here? what are you even doing with your life?"
;;

(** This file contains tools to translate a surface AST into an A-normalized
    form. *)

open Batteries;;

open Expressions;;
open Freshening;;


let rec tco (ae: a_expr) : a_expr =
  match ae with
  | ALet(var, ae1, ae2) -> ALet(var, ae1, tco ae2)
  | ACExpr(ce) ->
    begin
      match ce with
      | CIf(x, ae1, ae2) -> ACExpr(CIf(x, tco ae1, tco ae2))
      | CAppl(ie1, ie2, boolean) -> ACExpr(CAppl(ie1, ie2, true))
      | _ -> ae
    end

(** This function transforms an expression into a list of bindings and a final
    result, both in A-normal form. *)
let rec a_normalize_expression (e : expr) : a_expr =
  match e with
  | EInt n -> ACExpr(CIExpr(IInt n))
  | EBool b -> ACExpr(CIExpr(IBool b))
  | EVar x -> ACExpr(CIExpr(IVar x))
  | EUnaryOp(op, e) ->
    let x = fresh_name "$" in
    ALet(x, a_normalize_expression e, ACExpr(CUnaryOp(op, IVar x)))
  | EBinaryOp(op, e1, e2) ->
    let x1 = fresh_name "$" in
    let x2 = fresh_name "$" in
    ALet(x1, a_normalize_expression e1,
         ALet(x2, a_normalize_expression e2,
              ACExpr(CBinaryOp(op, IVar x1, IVar x2))))
  | EIf(e1,e2,e3) ->
    let x = fresh_name "$" in
    ALet(x, a_normalize_expression e1,
         ACExpr(
           CIf(IVar x,
               a_normalize_expression e2,
               a_normalize_expression e3)))
  | ELet(x,e1,e2) ->
    ALet(x, a_normalize_expression e1, a_normalize_expression e2)
  | EAppl(e1, e2) -> (*
    let alist =  List.map a_normalize_expression elist in
    let varlist = List.map (fun a_expr -> fresh_name "$") alist in
    let pairs = List.combine varlist alist in
    let acc_update (name, ae) acc = (*to put all our pairs of vars and exprs in let bindings*)
      ALet(name, ae, acc)
    in
    let ivarlist = List.map (fun var -> IVar(var)) varlist in
    List.fold_right acc_update pairs (ACExpr(CAppl(x, ivarlist))) *)
    let x1 = fresh_name "$" in
    let x2 = fresh_name "$" in
    ALet(x1, a_normalize_expression e1,
         ALet(x2, a_normalize_expression e2,
              ACExpr(CAppl(IVar x1, IVar x2, false))))

  | ETuple(elist) ->
    let alist = List.map a_normalize_expression elist in
    let varlist = List.map (fun a_expr -> fresh_name "$") alist in
    let pairs = List.combine varlist alist in
    let acc_update (name, ae) acc = (*to put all our pairs of vars and exprs in let bindings*)
      ALet(name, ae, acc)
    in
    let ivarlist = List.map (fun var -> IVar(var)) varlist in
    let tuple = (ACExpr(CTuple(ivarlist))) in
    List.fold_right acc_update pairs tuple
  | ESet(e1, e2, e3) ->
    let x1 = fresh_name "$" in
    let x2 = fresh_name "$" in
    let x3 = fresh_name "$" in
    ALet(x1, a_normalize_expression e1,
         ALet(x2, a_normalize_expression e2,
              ALet(x3, a_normalize_expression e3,
                   ACExpr(CSet(IVar x1, IVar x2, IVar x3)))))
  | ELambda _-> failwith "How did you even get here?"

;;

let a_normalize_declaration (decl:declaration): a_declaration =
  let EFunction(name, params, body) = decl in
  AFunction(name, params, (tco (a_normalize_expression body)))
(**This function transforms a program into a list of declarations and an expression*)

let a_normalize_program (p:program) : a_program =
  match p with
  | EProgram(decList, expr) -> AProgram((List.map a_normalize_declaration decList),((*tco *)(a_normalize_expression expr)))
;;



(*
(** This function transforms a list of bindings and a result (in A-normal
    compound expressions) into an A-normalized AST. *)
let rec bindings_and_result_to_a_expr
    (bindings : (string * a_expr) list) (result : c_expr) : a_expr =
  (* NOTE: this could be done with List.fold_right, but it is written out in its
     entirety here to make clear what is happening. *)
  match bindings with
  | [] -> ACExpr result
  | first :: rest ->
    let (x,body) = first in
    ALet(x,body,bindings_and_result_to_a_expr rest result)
;;

(** This function transforms an expression into a list of bindings and a final
    result, both in A-normal form. *)
let rec a_normalize_expression_inner (e : expr)
  : (string * a_expr) list * c_expr =
  match e with
  | EInt value -> [],CIExpr(IInt(value))
  | EVar var -> [],CIExpr(IVar(var))
  | EUnaryOp (unary_op, expr) ->
    (
      match unary_op with
      | OpInc ->
        let (bindings, answer) = a_normalize_expression_inner expr in
        let varname = fresh_name("variable$") in
        let new_binding = (varname, ACExpr(answer)) in
        (bindings @ [new_binding], CUnaryOp (OpInc, (IVar varname)))
      | OpDec ->
        let (bindings, answer) = a_normalize_expression_inner expr in
        let varname = fresh_name("variable$") in
        let new_binding = (varname, ACExpr(answer)) in
        (bindings @ [new_binding], CUnaryOp (OpDec, (IVar varname)))
    )
  | EBinaryOp (binary_operator, expr1, expr2) ->
    ( let (bindings1, answer1) = a_normalize_expression_inner expr1 in
      let varname1 = fresh_name("variable$") in
      let new_binding1 = (varname1, ACExpr(answer1)) in
      let (bindings2, answer2) = a_normalize_expression_inner expr2 in
      let varname2 = fresh_name("variable$") in
      let new_binding2 = (varname2, ACExpr(answer2)) in
      match binary_operator with
      | OpPlus -> (bindings1 @ [new_binding1] @ bindings2 @ [new_binding2], CBinaryOp(OpPlus, (IVar(varname1)), IVar(varname2)))
      | OpMinus -> (bindings1 @ [new_binding1] @ bindings2 @ [new_binding2], CBinaryOp(OpMinus, (IVar(varname1)), IVar(varname2)))
      | OpTimes -> (bindings1 @ [new_binding1] @ bindings2 @ [new_binding2], CBinaryOp(OpTimes, (IVar(varname1)), IVar(varname2)))
    )
  | ELet (varname, expr1, expr2) ->

    let (bindings, answer) = a_normalize_expression_inner expr1 in
    let aexpr1 = bindings_and_result_to_a_expr bindings answer in
    (*let new_binding = (varname, ACExpr(answer)) in*)
    let (bindings2, answer2) = a_normalize_expression_inner expr2 in
    ([(varname, aexpr1)] @ bindings2, answer2)
  | EIf (expr1, expr2, expr3) ->
    (*Recurse on the i_expr *)
    let (bindings1, answer1) = a_normalize_expression_inner expr1 in
    let varname1 = fresh_name("variable$") in
    let var = IVar(varname1) in
    let new_binding = ACExpr(answer1) in
    (*Recurse on the first a_expr
      We will need to call bindings_and_result_to_a_expr on it to put it into a form that we can
      put into the CIf *)
    let (bindings2, answer2) = a_normalize_expression_inner expr2 in
    let aexpr1 = bindings_and_result_to_a_expr bindings2 answer2 in
    let (bindings3, answer3) = a_normalize_expression_inner expr3 in
    let aexpr2 = bindings_and_result_to_a_expr bindings3 answer3 in
    (bindings1@[(varname1, new_binding)], CIf(var, aexpr1, aexpr2))






;;

(** This function A-normalizes an expression into an ANF AST. *)
let a_normalize_expression (e : expr) : a_expr =
  let bindings, result = a_normalize_expression_inner e in
  bindings_and_result_to_a_expr bindings result
;;
*)

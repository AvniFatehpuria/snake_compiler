(** This file contains tools to translate a surface AST into an A-normalized
    form. *)

open Batteries;;

open Expressions;;
open Freshening;;

(** This function transforms an expression into a list of bindings and a final
    result, both in A-normal form. *)
let rec a_normalize_expression (e : expr) : a_expr =
  match e with
  | EInt n -> ACExpr(CIExpr(IInt n))
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
;;

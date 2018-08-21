(* This module contains code which allows an expression to be transformed to
   eliminate lambdas by converting them into top-level functions. *)

open Batteries;;

open Expressions;;
open Freshening;;

module StringSet = Set.Make(
  struct
    type t = string
    let compare = Pervasives.compare
  end);;
(*let rec find_local_vars (e: expr) (st: StringSet.t): StringSet.t =
  match e with
  | EInt _ -> StringSet.empty
  | EBool _ -> StringSet.empty
  | EVar x -> StringSet.empty
  | EUnaryOp(_,e') -> find_local_vars e' st
  | EBinaryOp(_,e1,e2) -> StringSet.union (find_local_vars e1 st) (find_local_vars e2 st)
  | EIf(e1,e2,e3) ->
    StringSet.union (StringSet.union (find_local_vars e1 st) (find_local_vars e2 st) )(find_local_vars e3 st)
  | EAppl(e1,e2) ->
    StringSet.union (find_local_vars e1 st) (find_local_vars e2 st)
  | ETuple(es) ->
    List.fold_left (fun (l:StringSet.t) (e: expr)  -> StringSet.union (find_local_vars e st) l) StringSet.empty es
  | ELet(x,e1,e2) ->
    let newst = StringSet.add x st in
    (StringSet.union newst (StringSet.union (find_local_vars e1 newst) (find_local_vars e2 newst)))
  | ELambda(x,e') ->
    (find_local_vars e' st)*)
let rec free_vars (e : expr) : StringSet.t =
  match e with
  | EInt _ -> StringSet.empty
  | EBool _ -> StringSet.empty
  | EVar x -> StringSet.singleton x
  | ESet(e1,e2,e3) ->
    StringSet.union (StringSet.union (free_vars e1) (free_vars e2) )(free_vars e3)
  | EUnaryOp(_,e') -> free_vars e'
  | EBinaryOp(_,e1,e2) -> StringSet.union (free_vars e1) (free_vars e2)
  | EIf(e1,e2,e3) ->
    StringSet.union (StringSet.union (free_vars e1) (free_vars e2) )(free_vars e3)
  | EAppl(e1,e2) ->
    StringSet.union (free_vars e1) (free_vars e2)
  | ETuple(es) ->
    List.fold_left (fun (l:StringSet.t) (e: expr)  -> StringSet.union (free_vars e) l) StringSet.empty es
  | ELet(x,e1,e2) ->
    StringSet.union (free_vars e1) (StringSet.remove x (free_vars e2))
  | ELambda(x,e') ->
    StringSet.remove x (free_vars e')
;;

let rec closure_convert_expression (e : expr) : expr * declaration list =
  match e with
  | EInt _ -> (e, [])
  | EBool _ -> (e, [])
  | EVar _ -> (e, [])
  | ESet (e1, e2, e3) ->
  let (e1', decls1) = closure_convert_expression e1 in
  let (e2', decls2) = closure_convert_expression e2 in
  let (e3', decls3) = closure_convert_expression e3 in
  (ESet(e1', e2', e3'), decls1@decls2@decls3)
  | EUnaryOp(op,e1) ->
    let (e1', decls1) = closure_convert_expression e1 in
    (EUnaryOp(op,e1'), decls1)
  | EBinaryOp(op,e1,e2) ->
    let (e1', decls1) = closure_convert_expression e1 in
    let (e2', decls2) = closure_convert_expression e2 in
    (EBinaryOp(op,e1',e2'), decls1 @ decls2)
  | EIf(e1,e2,e3) ->
    let (e1', decls1) = closure_convert_expression e1 in
    let (e2', decls2) = closure_convert_expression e2 in
    let (e3', decls3) = closure_convert_expression e3 in
    (EIf(e1', e2', e3'), decls1@decls2@decls3)
  | EAppl(e1,e2) ->
    let (e1', decls1) = closure_convert_expression e1 in
    let (e2', decls2) = closure_convert_expression e2 in
    (EAppl(e1', e2'), decls1@decls2)
  | ETuple(es) ->
    (*let declist = [] in
    let (es', decls) = List.fold_left (fun declist e' -> declist @ closure_convert_expression e') declist es in
      (ETuple(es1), decls) *)
    let converted_list = List.map (fun e -> closure_convert_expression e) es in
    let (elist, declslist) = List.split converted_list in
    let decls = List.fold_left (fun d l -> d @ l) [] declslist in
    ETuple(elist), decls

  | ELet(x,e1,e2) ->
    let (e1', decls1) = closure_convert_expression e1 in
    let (e2', decls2) = closure_convert_expression e2 in
    ELet(x, e1', e2'), decls1@decls2
  | ELambda(x,body) ->
    let (e', decls) = closure_convert_expression body in
    let vars = free_vars (ELambda(x, e')) in
    let name = fresh_name "$lambda" in
    let funky_name = EVar(name) in
    let params = StringSet.elements vars in
    let paramlist = List.map (fun x' -> EVar(x')) params in
    let call = List.fold_left (fun e param -> EAppl(e, param)) funky_name paramlist in
    (*let call =  (*List.fold_left (fun param e -> EAppl(e, param)) funky_name paramlist*) in*)
    let funk = EFunction(name, params@[x], e') in
    call, funk::decls


    (* NOTE: This case is not like the others! *)
;;

let closure_convert_declaration (d : declaration)
  : declaration * declaration list =
  let EFunction(name,params,body) = d in
  let (body', decls) = closure_convert_expression body in
  (EFunction(name,params,body'), decls)
;;

let closure_convert_program (p : program) : program =
  let EProgram(decls,main) = p in
  let conversion = List.map closure_convert_declaration decls in
  let (decls', lifted_decls) = List.split conversion in
  let (main', lifted_decls_main) = closure_convert_expression main in
  let all_decls = decls' @ lifted_decls_main @ List.concat lifted_decls in
  EProgram(all_decls, main')
;;

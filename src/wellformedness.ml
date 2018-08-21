open Batteries;;
open Utils;;
open Printf;;
open Expressions;;


exception IllFormed of string list;;

let rec check_for_duplicate_parameter_names (paramList : string list) (map: int StringMap.t) (funkyName:string): (string list* int StringMap.t) =
  match paramList with
  | [] -> [], map
  | first::rest ->
    let (errors, newMap) = check_for_duplicate_parameter_names rest map funkyName in
    (*check if the current parameter is already in the map and thus is a duplicate name *)
    try
      let _ = StringMap.find first newMap in

      (([("Duplicate parameter "^first^" in function "^funkyName^"\n")]@errors), newMap)
    with

      Not_found -> (errors, (StringMap.add first 9 newMap))
;;

let rec check_expression_for_errors (e:expr) (env:environment) (funkyMap:string list StringMap.t): string list =
  match e with
  | EInt _ -> []
  | EBool _ -> []
  | ESet (tupl, indx, value) -> (check_expression_for_errors tupl env funkyMap) @ (check_expression_for_errors indx env funkyMap) @ (check_expression_for_errors value env funkyMap)
  | ETuple exprList -> (List.fold_left (fun (l) (argument) -> l@check_expression_for_errors argument env funkyMap) [] exprList)
  | EVar x ->
    begin
    try let _ = environment_get_var_offset env x in []
    with Not_found -> ["Unbound variable " ^ x ^"\n"]
    end
  | EUnaryOp (op, ex) -> check_expression_for_errors ex env funkyMap
  | EBinaryOp (op, ex1, ex2) -> (check_expression_for_errors ex1 env funkyMap ) @ (check_expression_for_errors ex2 env funkyMap )
  | ELet (var, ex1, ex2) ->
    (check_expression_for_errors ex1 env funkyMap ) @
    (check_expression_for_errors ex2 (environment_add_var env var) funkyMap )
  | EIf (ex1, ex2, ex3) ->
    (check_expression_for_errors ex1 env funkyMap ) @ (check_expression_for_errors ex2 env funkyMap ) @ (check_expression_for_errors ex3 env funkyMap )
  | EAppl (funkyName, argument) -> check_expression_for_errors funkyName env funkyMap @
                                   check_expression_for_errors argument env funkyMap
  | ELambda(x, expr) -> check_expression_for_errors expr (environment_add_var env x) funkyMap
                                      (* JUST RECURSE ON THE TWO EXPRESSIONS HERE. IT'S NOT THAT DEEP *)
    (*  begin
      let argumentChecks = (List.fold_left (fun (l) (argument) -> l@check_expression_for_errors argument env funkyMap) [] arguments) in
      (*try
        let params = (StringMap.find funkyName funkyMap) in
        let numParams = List.length params in
        let numArguments = List.length arguments in
        if numParams == numArguments then
          argumentChecks@[]
        else
          argumentChecks@["Function " ^ funkyName ^ " is called with " ^ string_of_int numArguments ^" number of arguments, expected "^ string_of_int numParams ^ "\n"]
      with
        Not_found -> argumentChecks@["Function " ^ funkyName ^ " not defined\n"]*)
      argumentChecks
      end*)



let check_declaration_for_errors (ed: declaration) (edList: declaration list) funkyMap : string list=
  let EFunction(name, params, body) = ed in
  let (paramNameErrors, _) = (check_for_duplicate_parameter_names params StringMap.empty name) in
  (check_expression_for_errors body (create_function_environment params edList) funkyMap) @ paramNameErrors
;;
(* This function produces a list of compile-time errors found in a program. *)

let rec check_for_duplicate_function_names (edList : declaration list) (map: string list StringMap.t): (string list* string list StringMap.t) =
  match edList with
  | [] -> [], map
  | EFunction(name, params, body)::rest ->
    let (errors, newMap) = check_for_duplicate_function_names rest map in
    (*check if the current function is already in the map and thus is a duplicate name *)
    try
      let _ = StringMap.find name newMap in

      (([("Duplicate definition of function "^name^"\n")]@errors), newMap)
    with
      (*Left in the deprecated 9 *)
      Not_found -> (errors, (StringMap.add name (*9*)params newMap))
;;


let check_program_for_errors (p : program) : string list =
  let EProgram(edList, e_expr) = p in

  (*let decl_errors = List.fold_left (fun l ad -> l@check_declaration_for_errors ad) [] adlist in*)
  let (errors, funkyMap) = check_for_duplicate_function_names edList (StringMap.empty) in

  let decl_errors = (List.fold_left (fun l ed -> l@check_declaration_for_errors ed edList funkyMap) [] edList) in
  let e_errors = check_expression_for_errors e_expr (create_program_environment edList)  funkyMap in
  errors @ decl_errors @ e_errors

;;

(* This function will check a program for compile-time errors.  If any errors
   are found, an IllFormed exception is thrown.  Otherwise, unit is returned. *)
let check_well_formed (p : program) : unit =
  let errors = check_program_for_errors p in
  if List.is_empty errors then () else  raise (IllFormed(errors));
;;

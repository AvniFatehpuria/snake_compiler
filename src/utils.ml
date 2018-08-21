(* This file contains the data structure and functions we will use to manage
   the environment as we compile our code. *)

open Batteries;;
open AssemblyLanguage;;
open Expressions;;
(*
  This code creates a "module" which defines a dictionary data type with keys
  of type string.  The syntax below is probably pretty alien and you don't need
  to understand it to use the data structure.  It's enough to know that the
  members on the module are documented here:

    http://ocaml-batteries-team.github.io/batteries-included/hdoc2/BatMap.S.html

  You can refer to these members by prefixing them with this module's name:
  StringMap.  For instance, you may write

    let m = StringMap.add "x" 5 (StringMap.empty) in
    StringMap.find "x" m

  to create a single-mapping dictionary called m and then look up the key "x" in
  that dictionary.  See the documentation above for more StringMap functions.
*)
module StringMap = Map.Make(
  struct
    type t = string
    let compare = Pervasives.compare
  end);;

(* This data type represents the kind of environment we will use in our
   compiler. *)
(*Original version of environment*)
type environment = argument StringMap.t * int * int;;
let empty_environment = (StringMap.empty, -4, 8);;

(*type environment = int StringMap.t;;
  let empty_environment = StringMap.empty;;
*)(* The value representing an empty environment. *)
let environment_add_var (env : environment) (var : string): environment =
  (*StringMap.add var ((4)*((StringMap.cardinal env) + 1)) env*)

  let (map, var_offset, param_offset) = env in

  (StringMap.add var (XMemory(XAddressByRegisterOffset(EBP, var_offset))) map),(var_offset-4),(param_offset)
;;

let environment_add_param (env: environment) (param : string) : environment =
  let (map, var_offset, param_offset) = env in

  (StringMap.add param (XMemory(XAddressByRegisterOffset(EBP, param_offset))) map),(var_offset),(param_offset+4)
;;

let environment_add_funk (env: environment) (funk: string) : environment =
  let (map, var_offset, param_offset) = env in
  (StringMap.add funk (XLabelLocationOffset("closure_of"^funk, 1)) map), (var_offset), (param_offset)

;;

exception UnboundVariable of string * environment;;

let environment_get_var_offset (env: environment) (var : string) : argument =
  let (map, var_offset, param_offset) = env in

  StringMap.find var map
  (*   with
       Not_found -> raise(UnboundVariable(var, env))
  *)
;;

let create_program_environment (declarations: declaration list): environment =
  let env = empty_environment in
  List.fold_left (fun env ad -> let EFunction(funk, _, _) = ad in environment_add_funk env funk) env declarations

;;
(*This one is for when we have a function, ergo params*)
let create_function_environment (params: string list) (declarations: declaration list): environment =
  let env = empty_environment in
  let env_with_params = List.fold_left environment_add_param env params in
  List.fold_left (fun env ad -> let EFunction(funk, _, _) = ad in environment_add_funk env funk) env_with_params declarations

;;

let string_of_environment (env : environment) =
  "?" (* Fill this in if you want. *)
;;

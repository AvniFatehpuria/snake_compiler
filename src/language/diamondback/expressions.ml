(** Contains the AST definitions for the Adder language. *)

open Batteries;;

(** The unary operators in the language. *)
type unary_operator =
  | OpInc
  | OpDec
  | OpPrint
  | OpIsInt
  | OpIsBool
[@@deriving eq, ord, show]
;;

(** The binary oeprators in the language. *)
type binary_operator =
  | OpPlus
  | OpMinus
  | OpTimes
  | OpLessThan
  | OpGreaterThan
  | OpEqualTo
  | OpAnd
  | OpOr
[@@deriving eq, ord, show]
;;

(** All forms of expression in the surface language. *)
type expr =
  | EInt of int
  | EBool of bool
  | EVar of string
  | EUnaryOp of unary_operator * expr
  | EBinaryOp of binary_operator * expr * expr
  | ELet of string * expr * expr
  | EIf of expr * expr * expr
  | EAppl of string * expr list
[@@deriving eq, ord, show]
;;

(** Forms of declaration in the surface language. *)
type declaration =
  | EFunction of string * string list * expr
[@@deriving eq, ord, show]
;;

(** The form of programs in the surface language. *)
type program =
  | EProgram of declaration list * expr
[@@deriving eq, ord, show]
;;

(** The type of simple ANF expressions. *)
type i_expr =
  | IInt of int
  | IBool of bool
  | IVar of string
[@@deriving eq, ord, show]
;;

(** The type of compound ANF expressions. *)
type c_expr =
  | CUnaryOp of unary_operator * i_expr
  | CBinaryOp of binary_operator * i_expr * i_expr
  | CIf of i_expr * a_expr * a_expr
  | CAppl of string * i_expr list
  | CIExpr of i_expr
[@@deriving eq, ord, show]

(** The type of top-level ANF expressions. *)
and a_expr =
  | ALet of string * a_expr * a_expr
  | ACExpr of c_expr
[@@deriving eq, ord, show]
;;

(** The type of declarations in ANF. *)
type a_declaration =
  | AFunction of string * string list * a_expr
[@@deriving eq, ord, show]
;;

(** The type of programs in ANF. *)
type a_program =
  | AProgram of a_declaration list * a_expr
[@@deriving eq, ord, show]
;;

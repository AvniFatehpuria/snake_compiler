(** Contains the AST definitions for the Adder language. *)

open Batteries;;

(** The unary operators in the language. *)
type unary_operator =
  | OpInc
  | OpDec
[@@deriving eq, ord, show]
;;

(** The binary oeprators in the language. *)
type binary_operator =
  | OpPlus
  | OpMinus
  | OpTimes
[@@deriving eq, ord, show]
;;

(** All forms of expression in the surface language. *)
type expr =
  | EInt of int
  | EVar of string
  | EUnaryOp of unary_operator * expr
  | EBinaryOp of binary_operator * expr * expr
  | ELet of string * expr * expr
  | EIf of expr * expr * expr
[@@deriving eq, ord, show]
;;

(** The type of simple ANF expressions. *)
type i_expr =
  | IInt of int
  | IVar of string
[@@deriving eq, ord, show]
;;

(** The type of compound ANF expressions. *)
type c_expr =
  | CUnaryOp of unary_operator * i_expr
  | CBinaryOp of binary_operator * i_expr * i_expr
  | CIf of i_expr * a_expr * a_expr
  | CIExpr of i_expr
[@@deriving eq, ord, show]

(** The type of top-level ANF expressions. *)
and a_expr =
  | ALet of string * a_expr * a_expr
  | ACExpr of c_expr
[@@deriving eq, ord, show]
;;

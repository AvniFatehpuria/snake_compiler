(** Contains the AST definitions for the Adder language. *)

open Batteries;;

type unary_operator =
  | OpInc
  | OpDec
;;

type expr =
  | EInt of int
  | EVar of string
  | EUnaryOp of unary_operator * expr
  | ELet of string * expr * expr
;;

(** This file contains type declarations and functions related to the compiler's
    representation of x86 assembly language. *)

open Batteries;;

(** Describes the registers of x86 that our code will use. *)
type register =
  | EAX
  | ECX
  | EDX
  | EBP
  | ESP
  | ESI
  | EDI
;;

(** Describes a memory address expression in x86 assembly. *)
type address =
  | XAddressByRegister of register
  | XAddressByRegisterOffset of register * int
  | XAddressByLabel of string
  | XAddressByRegisterProductOffset of register * register * int

;;

(** Describes the type of arguments in our x86 assembly representation.  We use
    this type somewhat loosely: not every argument is valid everywhere an
    argument type is written below, but capturing the precise syntax limitations
    of x86 would make our assembly language types a lot more complicated. *)
type argument =
  | XConstant of int
  | XRegister of register
  | XMemory of address
  | XSized of string * argument
  | XLabelLocationOffset of string * int
;;

(** The type that represents single x86 instructions. *)
type instruction =
  | XMov of argument * argument
  | XAdd of argument * argument
  | XSub of argument * argument
  | XMul of argument * argument
  | XLabel of string
  | XCmp of argument * argument
  | XJmp of string
  | XJe of string
  | XJg of string
  | XJge of string
  | XJle of string
  | XPush of argument
  | XShl of argument * argument
  | XPop of argument
  | XSal of argument * argument
  | XSar of argument * argument
  | XAnd of argument * argument
  | XOr of argument * argument
  | XXor of argument * argument
  | XJo of string
  | XJl of string
  | XJno of string
  | XJne of string
  | XCall of string
  | XSection of string
  | XAlign of int
  | XDd of string list
  | XRet
  | XRepMovsd
  | XRepStosd
  | XComment of string
;;

(** A function which transforms an x86 register into a string suitable for
    writing into an assembly language file. *)
let code_of_register (register : register) : string =
  match register with
  | EAX -> "eax"
  | ECX -> "ecx"
  | EDX -> "edx"
  | EBP -> "ebp"
  | ESP -> "esp"
  | ESI -> "esi"
  | EDI -> "edi"
;;

(** A function which transforms an x86 address expression into a string suitable
    for writing into an assembly language file. *)
let code_of_address (address : address) : string =
  match address with
  | XAddressByRegister register -> "[" ^ code_of_register register ^ "]"
  | XAddressByRegisterOffset (register, offset) -> "[" ^ code_of_register register ^ "+" ^ string_of_int offset ^ "]"
  | XAddressByLabel label -> "[" ^ label ^ "]"
  | XAddressByRegisterProductOffset (reg1, reg2, offset) -> "[" ^ code_of_register reg1 ^ " + "
                                                            ^ code_of_register reg2 ^ " * " ^ string_of_int offset ^ "]"

;;

(** A function which transforms an x86 argument into a string suitable for
    writing into an assembly language file. *)
let rec code_of_argument (argument : argument) : string =
  match argument with
  | XConstant constant -> string_of_int constant
  | XRegister register -> code_of_register register
  | XMemory address -> code_of_address address
  | XSized (strng, arg) -> strng ^ code_of_argument arg
  | XLabelLocationOffset (label, offset) -> label^" + "^string_of_int offset
;;

(** A function which transforms an x86 instruction into a string suitable for
    writing into an assembly language file. *)
let code_of_instruction (instruction : instruction) : string =
  match instruction with
  | XMov (argument1, argument2) -> "  mov " ^ code_of_argument argument1 ^ ", " ^ code_of_argument argument2
  | XAdd (argument1, argument2) -> "  add " ^ code_of_argument argument1 ^ ", " ^ code_of_argument argument2
  | XSub (argument1, argument2) -> "  sub " ^ code_of_argument argument1 ^ ", " ^ code_of_argument argument2
  | XMul (argument1, argument2) -> "  imul " ^ code_of_argument argument1 ^ ", " ^ code_of_argument argument2
  | XCmp (argument1, argument2) -> "  cmp " ^ code_of_argument argument1 ^ ", " ^ code_of_argument argument2
  | XLabel (label) -> (label ^ ":")
  | XJmp (label)-> ("  jmp " ^ label)
  | XJe (label) -> ("  je " ^ label)
  | XJle (label) -> ("  jle " ^ label)
  | XJge (label) -> ("  jge " ^ label)
  | XJg (label) -> ("  jg " ^ label)
  | XJl (label) -> ("   jl " ^ label)
  | XJne (label) -> ("  jne " ^ label)
  | XJo label -> "  jo " ^ label
  | XJno label -> "  jno " ^ label
  | XPush arg -> "  push " ^ code_of_argument arg
  | XPop arg -> "  pop " ^ code_of_argument arg
  | XCall strng -> "  call " ^ strng
  | XSal (arg1, arg2) -> "  sal " ^ code_of_argument arg1 ^ ", " ^ code_of_argument arg2
  | XSar (arg1, arg2) -> "  sar " ^ code_of_argument arg1 ^ ", " ^ code_of_argument arg2
  | XShl (arg1, arg2) -> "  shl " ^ code_of_argument arg1 ^ ", " ^ code_of_argument arg2
  | XAnd (arg1, arg2) -> "  and " ^ code_of_argument arg1 ^ ", " ^ code_of_argument arg2
  | XOr (arg1, arg2) -> "  or " ^ code_of_argument arg1 ^ ", " ^ code_of_argument arg2
  | XXor (arg1, arg2) -> "  xor " ^ code_of_argument arg1 ^ ", " ^ code_of_argument arg2
  | XSection section_name -> "section ."^section_name
  | XAlign constant -> "align "^string_of_int constant
  | XDd strng_list -> " dd "^ String.concat ", " strng_list
  | XRet -> "ret"
  | XRepMovsd -> "rep movsd"
  | XRepStosd -> "rep stosd"
  | XComment strng -> "; " ^ strng
;;

(** A function which transforms a list of x86 instructions into a string
    suitable for writing into an assembly language file. *)
let code_of_instruction_list (instruction_list : instruction list) : string =
  let mapped_instruction_list = List.map code_of_instruction instruction_list in
  List.fold_left (fun h t -> h ^ t ^ "\n") "" mapped_instruction_list
;;

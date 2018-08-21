open Freshening;;
open AssemblyLanguage;;
open Environment;;
open Expressions;;
open Printf;;
open ANormalization;;

(* An exception type which can be raised when an unbound variable is found. *)

(*
let rec compile_expression_with_environment (env : environment) (e : expr)
  : instruction list =
  match e with
  | EInt value -> [(XMov ((XRegister EAX), (XConstant value)))]
  | EUnaryOp (operator, expr) ->
    (
      match operator with
      | OpInc ->
        let instr = compile_expression_with_environment env expr in
        instr@[ XAdd((XRegister EAX), (XConstant 1))]
      | OpDec ->
        let instr = compile_expression_with_environment env expr in
        instr@[ XAdd((XRegister EAX), (XConstant (-1)))]
    )
  | ELet (var, expr1, expr2) ->
    let instr1 = compile_expression_with_environment env expr1 in
    let new_env = environment_add_var env var in
    let offset = environment_get_var_offset new_env var in
    let store_instr = [(XMov( XMemory(XAddressByRegisterOffset( EBP, offset)), (XRegister EAX)))] in
    let instr2 = compile_expression_with_environment new_env expr2 in
    instr1 @ store_instr @ instr2
  | EVar var ->
    let offset = environment_get_var_offset env var in
    let location = (XAddressByRegisterOffset( EBP, offset)) in
    [XMov((XRegister EAX), (XMemory location ))]
*)
(* We need to know how many variables we will have in a given function so we can allocate enough space for them
   we count them all before we start compiling *)

let rec count_vars_in_a_expression (ae: a_expr) : int =
  match ae with
  | ALet(varname, expr1, expr2) -> max (count_vars_in_a_expression expr1) (1+count_vars_in_a_expression expr2)
  | ACExpr(cexpr) ->
    (match cexpr with
     | CIf(iexpr, aexpr1, aexpr2) -> max(count_vars_in_a_expression aexpr1) (count_vars_in_a_expression aexpr2)
     | _ -> 0 (*THIS MIGHT BREAK WHEN WE HAVE NEW CEXPRs*)
    )
;;

let rec compile_i_expression (env : environment) (ie : i_expr) : argument =
  match ie with
  | IInt(value)-> XConstant(value*2)
  | IBool (boolean) ->
    (match boolean with
     | true -> XConstant(0xFFFFFFFF)
     | false -> XConstant(0x7FFFFFFF)
    )
  | IVar(var) -> environment_get_var_offset env var

and compile_c_expression (env : environment) (ce : c_expr) : instruction list =
  match ce with
  | CUnaryOp(op, ie) ->
    let arg = compile_i_expression env ie in
    (
    match op with
    | OpInc ->
      let move = XMov((XRegister(EAX)), arg) in
      let xand = XAnd((XRegister EAX), (XConstant 0x00000001)) in
      let cmp = XCmp((XRegister EAX), (XConstant 0)) in
      let intgr = (fresh_name "intgr") in
      let lbl = XLabel(intgr) in
      let je = XJe(intgr) in
      let push = XPush(XConstant 1) in
      let call = XCall("stopWithError") in
      let inc = XAdd((XRegister EAX), (XConstant 2)) in
      let rest = (fresh_name "rest") in
      let label = XLabel(rest) in
      let jno = XJno(rest) in
      let push3 = XPush(XConstant 3) in
      let call3 = XCall("stopWithError") in
      [move; xand; cmp; je; push; call; lbl; move; inc; jno; push3; call3; label]
    | OpDec ->
      let move = XMov((XRegister(EAX)),arg) in
      let xand = XAnd((XRegister EAX), (XConstant 0x00000001)) in
      let cmp = XCmp((XRegister EAX), (XConstant 0)) in
      let intgr = (fresh_name "intgr") in
      let lbl = XLabel(intgr) in
      let je = XJe(intgr) in
      let push = XPush(XConstant 1) in
      let call = XCall("stopWithError") in
      let move2 = XMov((XRegister(EAX)),arg) in
      let dec = XSub((XRegister EAX), (XConstant 2)) in
      let rest = (fresh_name "rest") in
      let label = XLabel(rest) in
      let jno = XJno(rest) in
      let push3 = XPush(XConstant 3) in
      let call3 = XCall("stopWithError") in
      [move; xand; cmp; je; push; call; lbl; move2; dec; jno; push3; call3; label]
    | OpIsBool ->
      let move = XMov((XRegister(EAX)), arg) in
      let xand = XAnd((XRegister EAX), (XConstant 0x00000003)) in
      let cmp = XCmp((XRegister EAX), (XConstant 3)) in
      let boolean = (fresh_name "boolean") in
      let lbl = XLabel(boolean) in
      let je = XJe(boolean) in
      (*if not bool*)
      let move_false = XMov((XRegister(EAX)), XConstant 0x7FFFFFFF) in
      let continue = (fresh_name "continue") in
      let continue_lbl = XLabel(continue) in
      let jump_continue = XJmp(continue) in
      let move_true = XMov((XRegister(EAX)), XConstant 0xFFFFFFFF) in
      [move; xand; cmp; je; move_false; jump_continue; lbl; move_true; continue_lbl]
    | OpIsInt ->
      let move = XMov((XRegister(EAX)), arg) in
      let xxor = XXor((XRegister(EAX)), (XConstant 0x00000001)) in
      let shift = XShl((XRegister(EAX)), (XConstant 31)) in
      let xor = XOr((XRegister(EAX)), (XConstant 0x7FFFFFFF)) in
      [move; xxor; shift; xor]
    | OpIsTuple ->
      let move = XMov((XRegister(EAX)), arg) in
      let xand = XAnd((XRegister(EAX)), XConstant(0x00000003)) in
      let xcmp = XCmp((XRegister(EAX)), XConstant(0x00000001)) in
      let is_pointer = (fresh_name "is_pointer") in
      let pointer_label = XLabel(is_pointer) in
      let jump_is_pointer = XJe(is_pointer) in
      let get_pointer = XSub((XRegister(EAX)), XConstant(0x00000001)) in
      let get_first_word = XMov(XRegister(ECX), XMemory(XAddressByRegister(EAX))) in
      let get_high_bit = XAnd(XRegister(ECX), XConstant(0x80000000)) in
      let cmp_is_tuple = XCmp(XRegister(ECX), XConstant(0x80000000)) in
      let tuple = (fresh_name "tuple") in
      let tuple_label = XLabel(tuple) in
      let xjne = XJne(tuple) in
      let set_false = XMov((XRegister(EAX)), XConstant(0x7FFFFFFF)) in
      let rest = (fresh_name "rest") in
      let rest_label = XLabel(rest) in
      let jmp = XJmp(rest) in
      let set_true = XMov((XRegister(EAX)), XConstant(0xFFFFFFFF)) in
      [move; xand; xcmp; jump_is_pointer; set_false; jmp; pointer_label; move; get_pointer; get_first_word; get_high_bit; cmp_is_tuple; xjne; set_false; jmp; tuple_label; set_true; rest_label]
    | OpPrint ->
      [XMov((XRegister EAX), arg); XPush(XRegister EAX); XCall("printValue"); XPop(XRegister EAX)]
    )
  | CBinaryOp(op, ie1, ie2) ->
    let arg1 = compile_i_expression env ie1 in
    let arg2 = compile_i_expression env ie2 in
    let mov = XMov((XRegister EAX),arg1) in
    let movArg2 = XMov((XRegister EAX), arg2) in
    (
    match op with
    |OpPlus ->
      let xand = XAnd((XRegister EAX), (XConstant 0x00000001)) in
      let cmp = XCmp((XRegister EAX), (XConstant 0)) in
      let intgr = (fresh_name "intgr") in
      let lbl = XLabel(intgr) in
      let je = XJe(intgr) in
      let push = XPush(XConstant 1) in
      let call = XCall("stopWithError") in
      let move2 = XMov((XRegister(EAX)),arg2) in
      let cmp2 = XCmp((XRegister EAX), (XConstant 0)) in
      let intgr2 = (fresh_name "intgr") in
      let lbl2 = XLabel(intgr2) in
      let je2 = XJe(intgr2) in
      let push2 = XPush(XConstant 1) in
      let call2 = XCall("stopWithError") in
      let move3 = XMov((XRegister(EAX)),arg2) in
      let rest = (fresh_name "rest") in
      let label = XLabel(rest) in
      let jno = XJno(rest) in
      let push3 = XPush(XConstant 3) in
      let call3 = XCall("stopWithError") in
      [mov; xand; cmp; je; push; call; lbl; move2; xand; cmp2; je2; push2; call2;
       lbl2; move3; XAdd((XRegister EAX), arg1); jno; push3; call3; label]
    |OpMinus ->
      let xand = XAnd((XRegister EAX), (XConstant 0x00000001)) in
      let cmp = XCmp((XRegister EAX), (XConstant 0)) in
      let intgr = (fresh_name "intgr") in
      let lbl = XLabel(intgr) in
      let je = XJe(intgr) in
      let push = XPush(XConstant 1) in
      let call = XCall("stopWithError") in
      let move2 = XMov((XRegister(EAX)),arg2) in
      let cmp2 = XCmp((XRegister EAX), (XConstant 0)) in
      let intgr2 = (fresh_name "intgr") in
      let lbl2 = XLabel(intgr2) in
      let je2 = XJe(intgr2) in
      let push2 = XPush(XConstant 1) in
      let call2 = XCall("stopWithError") in
      let rest = (fresh_name "rest") in
      let label = XLabel(rest) in
      let jno = XJno(rest) in
      let push3 = XPush(XConstant 3) in
      let call3 = XCall("stopWithError") in
      [mov; xand; cmp; je; push; call; lbl; move2; xand; cmp2; je2; push2; call2;
       lbl2; mov; XSub((XRegister EAX), arg2); jno; push3; call3; label]
    |OpTimes ->
      let xand = XAnd((XRegister EAX), (XConstant 0x00000001)) in
      let cmp = XCmp((XRegister EAX), (XConstant 0)) in
      let intgr = (fresh_name "intgr") in
      let lbl = XLabel(intgr) in
      let je = XJe(intgr) in
      let push = XPush(XConstant 1) in
      let call = XCall("stopWithError") in
      let move2 = XMov((XRegister(EAX)),arg2) in
      let cmp2 = XCmp((XRegister EAX), (XConstant 0)) in
      let intgr2 = (fresh_name "intgr") in
      let lbl2 = XLabel(intgr2) in
      let je2 = XJe(intgr2) in
      let push2 = XPush(XConstant 1) in
      let call2 = XCall("stopWithError") in
      let rest = (fresh_name "rest") in
      let label = XLabel(rest) in
      let jno = XJno(rest) in
      let push3 = XPush(XConstant 3) in
      let call3 = XCall("stopWithError") in
      [mov; xand; cmp; je; push; call; lbl; move2; xand; cmp2; je2; push2; call2;
      lbl2; mov; XSar((XRegister EAX), (XConstant 1)); XMul((XRegister EAX), arg2); jno; push3; call3; label]
    (* For OpLessThan, we subtract the second arg from the first. If we get a negative,
   then arg 1 must be less than arg2. then we or it with false. If its a negative
   then the sign bit will be 1, so we wwill end up with all 1s which is true. otherwise
   we will end up with a 0 and then all 1s which is false. For OpGreater than we do the
   same thing but we subtract arg1 from arg2 instead *)
    |OpLessThan ->
      let xand = XAnd((XRegister EAX), (XConstant 0x00000001)) in
      let cmp = XCmp((XRegister EAX), (XConstant 0)) in
      let intgr = (fresh_name "intgr") in
      let lbl = XLabel(intgr) in
      let je = XJe(intgr) in
      let push = XPush(XConstant 1) in
      let call = XCall("stopWithError") in
      let move2 = XMov((XRegister(EAX)),arg2) in
      let cmp2 = XCmp((XRegister EAX), (XConstant 0)) in
      let intgr2 = (fresh_name "intgr") in
      let lbl2 = XLabel(intgr2) in
      let je2 = XJe(intgr2) in
      let push2 = XPush(XConstant 1) in
      let call2 = XCall("stopWithError") in
      [mov; xand; cmp; je; push; call; lbl; move2; xand; cmp2; je2; push2; call2;
      lbl2; mov; XSub((XRegister EAX), arg2); XOr((XRegister EAX), (XConstant 0x7FFFFFFF))]
    |OpGreaterThan ->
      let xand = XAnd((XRegister EAX), (XConstant 0x00000001)) in
      let cmp = XCmp((XRegister EAX), (XConstant 0)) in
      let intgr = (fresh_name "intgr") in
      let lbl = XLabel(intgr) in
      let je = XJe(intgr) in
      let push = XPush(XConstant 1) in
      let call = XCall("stopWithError") in
      let move2 = XMov((XRegister(EAX)),arg2) in
      let cmp2 = XCmp((XRegister EAX), (XConstant 0)) in
      let intgr2 = (fresh_name "intgr") in
      let lbl2 = XLabel(intgr2) in
      let je2 = XJe(intgr2) in
      let push2 = XPush(XConstant 1) in
      let call2 = XCall("stopWithError") in
      [mov; xand; cmp; je; push; call; lbl; move2; xand; cmp2; je2; push2; call2;
      lbl2; movArg2; XSub((XRegister EAX), arg1); XOr((XRegister EAX), (XConstant 0x7FFFFFFF))]
    (* For OpEqualTo, we compare onearg to the other and use a jump equal. we hope to replace this with omething that doesn't require jumps *)
    |OpEqualTo ->
      let equalCase = (fresh_name "label") in
      let equalLabel = XLabel(equalCase) in (*Label to jump to if the two arguments are equal*)
      let continue = (fresh_name "label") in
      let continueLabel = XLabel(continue) in (*Label to jump to after the else case finishes *)
      let equal = XMov((XRegister EAX), (XConstant 0xFFFFFFFF)) in
      let notEqual = XMov((XRegister EAX), (XConstant 0x7FFFFFFF)) in
      let xand = XAnd((XRegister EAX), (XConstant 0x00000001)) in
      let cmp = XCmp((XRegister EAX), (XConstant 0)) in
      let intgr = (fresh_name "intgr") in
      let lbl = XLabel(intgr) in
      let je = XJe(intgr) in
      let push = XPush(XConstant 1) in
      let call = XCall("stopWithError") in
      let move2 = XMov((XRegister(EAX)),arg2) in
      let cmp2 = XCmp((XRegister EAX), (XConstant 0)) in
      let intgr2 = (fresh_name "intgr") in
      let lbl2 = XLabel(intgr2) in
      let je2 = XJe(intgr2) in
      let push2 = XPush(XConstant 1) in
      let call2 = XCall("stopWithError") in
      [mov; xand; cmp; je; push; call; lbl; move2; xand; cmp2; je2; push2; call2;
      lbl2; mov; XCmp((XRegister EAX), arg2); XJe(equalCase); notEqual; XJmp(continue);equalLabel;equal;continueLabel]
    |OpAnd ->
      let xand = XAnd((XRegister EAX), (XConstant 0x00000003)) in
      let cmp = XCmp((XRegister EAX), (XConstant 3)) in
      let boolean = (fresh_name "boolean") in
      let lbl = XLabel(boolean) in
      let je = XJe(boolean) in
      let push = XPush(XConstant 2) in
      let call = XCall("stopWithError") in
      let move2 = XMov((XRegister(EAX)),arg2) in
      let boolean2 = (fresh_name "boolean") in
      let lbl2 = XLabel(boolean2) in
      let je2 = XJe(boolean2) in
      let push2 = XPush(XConstant 2) in
      let call2 = XCall("stopWithError") in
      [mov; xand; cmp; je; push; call; lbl; move2; xand; cmp; je2; push2; call2;
      lbl2; mov; XAnd((XRegister EAX), arg2)]
    |OpOr ->
      let xand = XAnd((XRegister EAX), (XConstant 0x00000003)) in
      let cmp = XCmp((XRegister EAX), (XConstant 3)) in
      let boolean = (fresh_name "boolean") in
      let lbl = XLabel(boolean) in
      let je = XJe(boolean) in
      let push = XPush(XConstant 2) in
      let call = XCall("stopWithError") in
      let move2 = XMov((XRegister(EAX)),arg2) in
      let boolean2 = (fresh_name "boolean") in
      let lbl2 = XLabel(boolean2) in
      let je2 = XJe(boolean2) in
      let push2 = XPush(XConstant 2) in
      let call2 = XCall("stopWithError") in
      [mov; xand; cmp; je; push; call; lbl; move2; xand; cmp; je2; push2; call2;
       lbl2; mov; XOr((XRegister EAX), arg2)]
    |OpTupleIndex ->
      let xand = XAnd((XRegister(EAX)), XConstant(0x00000003)) in
      let xcmp = XCmp((XRegister(EAX)), XConstant(0x00000001)) in
      let is_pointer = (fresh_name "is_pointer") in
      let pointer_label = XLabel(is_pointer) in
      let jump_is_pointer = XJe(is_pointer) in
      let get_pointer = XSub((XRegister(EAX)), XConstant(0x00000001)) in
      let get_first_word = XMov(XRegister(ECX), XMemory(XAddressByRegister(EAX))) in
      let get_high_bit = XAnd(XRegister(ECX), XConstant(0x80000000)) in
      let cmp_is_tuple = XCmp(XRegister(ECX), XConstant(0x80000000)) in
      let tuple = (fresh_name "tuple") in
      let tuple_label = XLabel(tuple) in
      let xjne = XJne(tuple) in
      let push_not_tuple = XPush(XConstant 4) in
      let error = XCall("stopWithError") in
      let xand_int = XAnd((XRegister EAX), (XConstant 0x00000001)) in
      let cmp_int = XCmp((XRegister EAX), (XConstant 0)) in
      let intgr = (fresh_name "intgr") in
      let lbl = XLabel(intgr) in
      let je = XJe(intgr) in
      let push = XPush(XConstant 1) in
      let tuple_pointer = XSub((XRegister EAX), XConstant(1)) in
      let mov_tuple_size = XMov(XRegister ECX, XMemory(XAddressByRegister(EAX))) in
      let push_tuple_ptr = XPush(XRegister EAX) in
      let divide_index_by_2 = XSar(XRegister EAX, XConstant 1) in
      let check_index = XCmp(XRegister ECX, XRegister EAX) in
      let valid_index = fresh_name "valid" in
      let valid_label = XLabel(valid_index) in
      let jump_greater_than = XJg(valid_index) in
      let push_bad_index = XPush(XConstant 5) in
      let cmp_to_0 = XCmp(XRegister EAX, XConstant 0) in
      let positive_index = fresh_name "positive" in
      let positive_label = XLabel(positive_index) in
      let jge0 = XJge(positive_index) in
      let add_1_to_index = XAdd(XRegister EAX, XConstant 2) in
      let pop_tuple_ptr = XPop(XRegister ECX) in
      let get_item_at_index = XMov(XRegister EAX, XMemory(XAddressByRegisterProductOffset(ECX, EAX, 4))) in
      [mov; xand; xcmp; jump_is_pointer; push_not_tuple; error; pointer_label; mov; get_pointer; get_first_word; get_high_bit;
       cmp_is_tuple; xjne; push_not_tuple; error; tuple_label; movArg2; xand_int;
       cmp_int; je; push; error; lbl; movArg2; cmp_to_0; jge0; push_bad_index; error; positive_label;
       mov; tuple_pointer; mov_tuple_size; push_tuple_ptr; movArg2; divide_index_by_2; check_index;
       jump_greater_than; push_bad_index; error; valid_label; add_1_to_index; pop_tuple_ptr; get_item_at_index]
     )
  | CIExpr(ie) ->
    [XMov((XRegister EAX), (compile_i_expression env ie))]
  | CIf(ie, ae1, ae2) ->
    (* compile the condition *)
    let condition = compile_i_expression env ie in
    let store_condition = XMov((XRegister EAX), condition) in
    let case1 = compile_a_expression env ae1 in
    let case2 = compile_a_expression env ae2 in
    let compare = XCmp(XRegister(EAX), XConstant(0x7FFFFFFF)) in
    let compare2 = XCmp(XRegister(EAX), XConstant(0xFFFFFFFF)) in
    let push = XPush(XConstant 2) in
    let call = XCall("stopWithError") in
    let ifCase = (fresh_name "label") in
    let jif = XJe(ifCase) in
    let ifLabel = XLabel(ifCase) in
    let elseCase = (fresh_name "label") in
    let elseLabel = XLabel(elseCase) in (*Label to jump to if condition is false *)
    let je = XJe(elseCase) in (*if condition is equal to zero, jumpt o elseLabel *)
    let continue = (fresh_name "label") in
    let continueLabel = XLabel(continue) in (*Label to jump to to get to rest of program *)
    let jump_after_if_then = XJmp(continue) in (*If condition is true, we evaluate case1 and then jump to continueLabel to continue the program *)
    [store_condition; compare; je; compare2; jif; push; call; ifLabel] @ case1 @ [jump_after_if_then;elseLabel] @ case2 @ [continueLabel]
  | CAppl(closure, param, tco) ->

    (*let x = sprintf "I think the value for num_caller_params is %s" num_caller_params in*)
    let closur = compile_i_expression env closure in
    let para = compile_i_expression env param in
    let move = XMov((XRegister(EAX)), closur) in
    let xand = XAnd((XRegister(EAX)), XConstant(0x00000003)) in
    let xcmp = XCmp((XRegister(EAX)), XConstant(0x00000001)) in
    let is_pointer = (fresh_name "is_pointer") in
    let pointer_label = XLabel(is_pointer) in
    let push_not_closure = XPush(XConstant 6) in(*TODO: find out what the correct error code for not closure is*)
    let call = XCall("stopWithError") in
    let jump_is_pointer = XJe(is_pointer) in
    let getPointer = XSub((XRegister(EAX)), XConstant(0x00000001)) in (*after moving the closure into eax*)
    let getNumArgs = XMov(XRegister(ECX), XMemory(XAddressByRegister(EAX))) in
    let get_high_bit = XAnd(XRegister(ECX), XConstant(0x80000000)) in
    let cmp_is_closure = XCmp(XRegister(ECX), XConstant(0x80000000)) in
    let is_closure = (fresh_name "is_closure") in
    let closure_label = XLabel(is_closure) in
    let jump_is_closure = XJe(is_closure) in
    let set_high_bit_to_zero = XSub(XRegister(ECX), XConstant(0x80000000)) in
    let add_one_to_num_args = XAdd((XRegister(ECX)), XConstant(1)) in
    let getNumParams = XMov((XRegister(EDX)), XMemory(XAddressByRegisterOffset(EAX, 8))) in
    let compareArgsToParams = XCmp(XRegister(ECX), XRegister(EDX)) in
      let not_ready_to_call = (fresh_name "not_ready_to_call") in
      let not_ready_to_call_label = XLabel(not_ready_to_call) in
      let jump_less_than = XJl(not_ready_to_call) in
      let ready_to_call = (fresh_name "ready_to_call") in
      let ready_to_call_label = XLabel(ready_to_call) in
      let jump_to_call = XJmp(ready_to_call) in
      let get_heap_cursor = XMov(XRegister EDX, XMemory(XAddressByLabel "heap_cursor")) in
      let update_ecx = XSal(XRegister ECX, XConstant(2)) in
      let calculating_heap_cursor = XAdd(XRegister EDX, XRegister ECX) in
      let next_heap_cursor = XAdd(XRegister EDX, XConstant 20) in
      let get_end_of_heap = XMov(XRegister EDI, XMemory(XAddressByLabel "end_of_heap")) in
      let cmp = XCmp(XRegister EDX, XRegister EDI) in
      let have_space = fresh_name "have_space" in
      let space_needed = XSub(XRegister EDX, XMemory(XAddressByLabel "heap_cursor")) in
      let set_end_of_stack = XMov(XMemory(XAddressByLabel "end_of_stack"), XRegister ESP) in
      let reset_ecx = XSar(XRegister ECX, XConstant 2) in
      let set_high_bit_to_one = XAdd(XRegister (ECX), XConstant(0x80000000) ) in
      let set_reg_to_zero = XMov(XRegister EDI, XConstant(0)) in
      let copy_num_args = XMov(XMemory(XAddressByRegister(EDX)), XRegister(ECX)) in
      let copy_gc_word = XMov(XMemory(XAddressByRegisterOffset(EDX, 4)), XRegister(EDI)) in
      let calc_dest = XAdd(XRegister(EDX), XConstant(8)) in
      let set_dest = XMov(XRegister(EDI),XRegister(EDX)) in
      let set_count = XAdd(XRegister(ECX), XConstant(1)) in
      let increment_closure_pointer = XAdd(XRegister(EAX), XConstant(8)) in
      let set_source = XMov(XRegister(ESI), XRegister(EAX)) in
      let copy_size_of_closure = XMov(XRegister EAX, XRegister ECX) in
      let get_offset = XMul(XRegister EAX, XConstant(4)) in
      let update_heap_cursor = XAdd(XRegister(EDX), XRegister(EAX)) in
      let movsd = XRepMovsd in
      let get_new_argument = XMov(XRegister(ECX), para) in
      let copy_new_argument = XMov(XMemory(XAddressByRegister(EDX)), XRegister(ECX)) in
      let increment_heap_cursor = XAdd(XRegister(EDX), XConstant(4)) in
      let get_new_closure_pointer = XMov(XRegister(EAX), XMemory(XAddressByLabel "heap_cursor")) in
      let snakify_closure_pointer = XAdd(XRegister EAX, XConstant(1)) in
      let save_heap_cursor = XMov(XMemory(XAddressByLabel "heap_cursor"), XRegister EDX) in
      let continue = (fresh_name "continue") in
      let continue_label = XLabel(continue) in
      let jump_continue = XJmp(continue) in
    let subtract_one = XSub(XRegister(EDX), XConstant(1)) in
    let get_esp_offset = XMul(XRegister(EDX), XConstant(4)) in
    let set_destination_for_call = XMov(XRegister(EDI), XRegister(ESP)) in
    let allocate_stack_space = XSub(XRegister(ESP), XRegister(EDX)) in
    let get_source_for_call = XAdd(XRegister(EAX), XConstant(16)) in
    let set_source_for_call = XMov(XRegister(ESI), XRegister(EAX)) in
    let set_count_for_call = XSub(XRegister(ECX), XConstant(1)) in
    let push_parameter = XPush(XSized("DWORD ", (para))) in
    let get_function = XMov(XRegister(EAX), XMemory(XAddressByRegisterOffset(EAX, -4))) in
    let call_func = XCall("eax") in
    let retrieve_ESP_offset_after_call =
      [XMov((XRegister(ECX)), closur); (*retrieve closure*)
       XSub((XRegister(ECX)), XConstant(1)); (*make it not a snake pointer*)
       XMov((XRegister(EDX)), XMemory(XAddressByRegisterOffset(ECX, 8))); (*get the number of params*)
       XMul(XRegister(EDX), XConstant(4))] in (*multiply by four to make it so we're working in bytes*)
    let restoreESP = XAdd(XRegister(ESP), XRegister(EDX)) in
    (*At this point is the code for when we are ready to call the function
      i.e. when the number of args + 1 = the number of params*)
    let num_caller_params = environment_get_num_params env in (*the number of params in the caller function, needed for knowing if we can tco*)
    let tail_call = fresh_name "tail_call" in
    let not_tail_call = fresh_name "not_tail_call" in
    let continue = fresh_name "continue" in
    let ready_to_tail_call =
      [(*XAdd(XRegister(EDX), XRegister(EBP)); (*Get the start of the section of code that has our caller params*)*)
        XLabel(tail_call);
        XComment("WE ARE READY TO TAIL CALL");
        XMov(XRegister(EDX), XConstant(num_caller_params));
        XSub(XRegister(EDX), XConstant(8));
        XSar(XRegister(EDX), XConstant(2));
        XCmp(XRegister(EDX), XRegister(ECX));
        XJl(not_tail_call);
        XAdd(XRegister(ECX), XConstant(1));
        XMov(XRegister(EDI), XRegister(EBP));
        XAdd(XRegister(EDI), XConstant 8);
        XPush (XRegister ECX);
        XPush (XRegister EAX);
        XMov (XRegister EAX, XConstant 0);
        XMov(XRegister(ECX), XRegister(EDX));
        XRepStosd;
        XPop(XRegister EAX);
        XPop(XRegister ECX);
        XMov(XRegister(EDI), para); (*Move the new parameter into EDI*)
        XMov(XMemory(XAddressByRegisterProductOffset(EBP, ECX, 4)), XRegister(EDI)); (*Push the new parameter into the caller param position*)
        XMov(XRegister(EDI), XRegister(EBP)); (*Put EBP into EDI*)
        XAdd(XRegister(EDI), XConstant(8)); (*Add the number of caller params (times 4) (plus 8) to EDI, which contains EBP, so now EDI has the start of the caller param section*)
        (*XSub(XRegister(EDX), XConstant(4)); subtract 4 because we have already moved the new param*)
        XAdd(XRegister(EAX), XConstant(16)); (*Add 16 to EAX, because we want to copy the arguments of the closure from EAX, so we want to bypass all the other stuff in the closure*)
        XMov(XRegister(ESI), XRegister(EAX)); (*Set the source for the call*)
        XSub(XRegister(ECX), XConstant(2)); (*Set the count for the movsd*)
        XRepMovsd; (*Copy the callee params*)
        XMov(XRegister(ESP), XRegister(EBP)); (*restore esp to what it was before the caller function*)
        XPop(XRegister(EBP));(*pop ebp*)
        (*XSub(XRegister(EAX), XConstant(4)); (*we need the function that we're going to jump to now*)*)
        XMov(XRegister(EAX), XMemory(XAddressByRegisterOffset(EAX, -4)));

        XJmp("eax"); (*Jump to the callee function*)
      ] in
    let ready_to_not_tail_call = [XMov(XRegister(EDX), (XRegister(ECX))); push_parameter; subtract_one;
                                  get_esp_offset; allocate_stack_space; set_destination_for_call;  get_source_for_call; set_count_for_call; set_source_for_call;
                                  movsd;  get_function; call_func] in
    let ready_to_call_code = if (tco) then XJmp(tail_call) else XJmp(not_tail_call) in

    [move; xand; xcmp; jump_is_pointer; push_not_closure; call; pointer_label; move;
     getPointer; getNumArgs; get_high_bit; cmp_is_closure; jump_is_closure;
     push_not_closure; call; closure_label; getNumArgs; set_high_bit_to_zero; add_one_to_num_args;
     getNumParams; compareArgsToParams; jump_less_than; jump_to_call; not_ready_to_call_label;
     get_heap_cursor; update_ecx; calculating_heap_cursor; next_heap_cursor; get_end_of_heap; cmp; XJl(have_space); space_needed;
     set_end_of_stack; XPush(XRegister ECX); XPush(XRegister EDX); XCall("gc"); XPop(XRegister EDX);
     XPop(XRegister ECX); move; XSub(XRegister EAX, XConstant 1);
     XLabel(have_space);reset_ecx; get_heap_cursor;
     set_high_bit_to_one; set_reg_to_zero; copy_num_args; copy_gc_word; calc_dest; increment_closure_pointer;
     set_source; set_high_bit_to_zero; set_count; set_dest; copy_size_of_closure; get_offset; update_heap_cursor;
     movsd; get_new_argument; copy_new_argument; increment_heap_cursor; get_new_closure_pointer;
     snakify_closure_pointer; save_heap_cursor; jump_continue; ready_to_call_label; ready_to_call_code;
     XLabel(not_tail_call)] @ ready_to_not_tail_call @ [XJmp(continue)] @ ready_to_tail_call @ [XLabel(continue)] @
     retrieve_ESP_offset_after_call @ [restoreESP; continue_label]


    (*let pushes = List.rev_map (fun x -> XPush(XSized("DWORD ", (compile_i_expression env x)))) params in
    let call = XCall("_"^name) in
    let add = XAdd((XRegister ESP), (XConstant (List.length pushes))) in
      pushes @ [call; add]*)
  | CTuple(ilist) ->
    let elements = List.map (fun element -> compile_i_expression env element) ilist in
    let tuple_size_and_elements = [XConstant(List.length(ilist)); XConstant(0)]@elements in
    let get_heap_cursor = XMov(XRegister EAX, XMemory(XAddressByLabel "heap_cursor")) in
    let next_heap_cursor = XAdd(XRegister EAX, XConstant(4*List.length(tuple_size_and_elements))) in
    let get_end_of_heap = XMov(XRegister ECX, XMemory(XAddressByLabel "end_of_heap")) in
    let cmp = XCmp(XRegister EAX, XRegister ECX) in
    let space_needed = XMov(XRegister EAX, XConstant(4*List.length(tuple_size_and_elements))) in
    let set_end_of_stack = XMov(XMemory(XAddressByLabel "end_of_stack"), XRegister ESP) in
    let continue = fresh_name "continue" in

    (*let memory_pointer = XMov(XRegister ECX, XRegister EAX) in*)
    (*let set_size = XMov(XMemory(XAddressByRegister EAX), XConstant(List.length(ilist))) in*)
    (*Insert the size of the tuple and then each element of the tuple into the heap*)
    let heap_insertions = List.mapi (fun index element -> [XMov( XRegister ECX, element);XMov(XMemory(XAddressByRegisterOffset(EAX, (index*4))),XRegister ECX )]) tuple_size_and_elements in
    let concatenated_heap_insertions = List.concat heap_insertions in
    let store_heap_cursor = XMov(XRegister ECX, XRegister EAX) in
    let update_heap_cursor = XAdd(XRegister ECX, XConstant(4*(List.length(tuple_size_and_elements)))) in
    let move_heap_cursor = XMov(XMemory(XAddressByLabel "heap_cursor"), XRegister ECX) in
    let set_tag_bit_of_ptr = XAdd(XRegister EAX, XConstant 1) in
    [get_heap_cursor; next_heap_cursor; get_end_of_heap; cmp; XJl(continue); space_needed; set_end_of_stack; XPush(XRegister EAX); XCall("gc"); XAdd(XRegister ESP, XConstant 4); XLabel(continue); get_heap_cursor]
    @ concatenated_heap_insertions @ [store_heap_cursor;update_heap_cursor;move_heap_cursor;set_tag_bit_of_ptr]
  | CSet(ie1, ie2, ie3) ->
    let tuple = compile_i_expression env ie1 in
    let index = compile_i_expression env ie2 in
    let new_value = compile_i_expression env ie3 in
    let is_pointer = (fresh_name "is_pointer") in
    let tupl = (fresh_name "tuple") in
    let mov_tuple = XMov(XRegister(EAX), tuple) in
    let mov_index = XMov(XRegister(EAX), index) in
    let mov_new_value = XMov(XRegister(EAX), new_value) in
    let error = XCall("stopWithError") in
    let get_started_and_check_is_tuple =
    [XMov((XRegister(EAX)), tuple); (*Move the tuple into EAX*)
     XAnd((XRegister(EAX)), XConstant(0x00000003));
     XCmp((XRegister(EAX)), XConstant(0x00000001)); (*Check that it is a valid pointer*)
     XJe(is_pointer); (*If it is a pointer, jump to that label*)
     XPush(XConstant 4);
     XCall("stopWithError"); (*If it is not a pointer, it's not a tuple, so error*)
     XLabel(is_pointer); (*Now it is pointer time! but is it a tuple?*)
     XMov((XRegister(EAX)), tuple);
     XSub((XRegister(EAX)), XConstant(0x00000001)); (*desnakify*)
     XMov(XRegister(ECX), XMemory(XAddressByRegister(EAX))); (*Dereference to get what should be num elements*)
     XAnd(XRegister(ECX), XConstant(0x80000000)); (*check high bit to see if it's a tuple or a closure*)
     XCmp(XRegister(ECX), XConstant(0x80000000));
     XJne(tupl);
     XPush(XConstant 4);
     XCall("stopWithError"); (*It's not a tuple*)
     XLabel(tupl)] in (*TUPLE TIME!*)

    let xand_int = XAnd((XRegister EAX), (XConstant 0x00000001)) in
    let cmp_int = XCmp((XRegister EAX), (XConstant 0)) in
    let intgr = (fresh_name "intgr") in
    let lbl = XLabel(intgr) in
    let je = XJe(intgr) in
    let push = XPush(XConstant 1) in
    let tuple_pointer = XSub((XRegister EAX), XConstant(1)) in
    let mov_tuple_size = XMov(XRegister ECX, XMemory(XAddressByRegister(EAX))) in
    let push_tuple_ptr = XPush(XRegister EAX) in
    let move_index_into_edx = XMov(XRegister(EDX), index) in
    let divide_index_by_2 = XSar(XRegister EDX, XConstant 1) in
    let check_index = XCmp(XRegister ECX, XRegister EDX) in
    let valid_index = fresh_name "valid" in
    let valid_label = XLabel(valid_index) in
    let jump_greater_than = XJg(valid_index) in
    let push_bad_index = XPush(XConstant 5) in
    let cmp_to_0 = XCmp(XRegister EAX, XConstant 0) in
    let positive_index = fresh_name "positive" in
    let positive_label = XLabel(positive_index) in
    let jge0 = XJge(positive_index) in
    let add_1_to_index = XAdd(XRegister EDX, XConstant 2) in
    let pop_tuple_ptr = XPop(XRegister ECX) in
    let insert_item_into_index = XMov(XMemory(XAddressByRegisterProductOffset(ECX, EDX, 4)), XRegister(EAX)) in
    get_started_and_check_is_tuple @ [mov_index; xand_int; cmp_int; je; push; error; lbl; mov_index; cmp_to_0;
                                      jge0; push_bad_index; error; positive_label; mov_tuple; tuple_pointer;
                                      mov_tuple_size; push_tuple_ptr; move_index_into_edx; divide_index_by_2;
                                      check_index; jump_greater_than; push_bad_index; error; valid_label;
                                      add_1_to_index; pop_tuple_ptr; mov_new_value; insert_item_into_index]

and compile_a_expression (env : environment) (ae : a_expr) : instruction list =
  match ae with
  | ALet(varname, aexpr1, aexpr2) ->
    let instr1 = compile_a_expression env aexpr1 in
    let new_env = environment_add_var env varname in
    let param = environment_get_var_offset new_env varname in
    let store_instr = [(XMov(param, (XRegister EAX)))] in
    let instr2 = compile_a_expression new_env aexpr2 in
    let zero_out_param = [XMov(XRegister ECX, (XConstant 0)); XMov(param, (XRegister ECX))] in
    instr1 @ store_instr @ instr2 @ zero_out_param
  | ACExpr(cexpr) -> compile_c_expression env cexpr

let compile_a_declaration (ad: a_declaration) (decList: a_declaration list): instruction list =
  let AFunction (name, params, body) = ad in
  let label = XLabel("_"^name) in
  let numvars = count_vars_in_a_expression body in
  let push = XPush (XRegister EBP) in
  let setEBP = XMov ((XRegister EBP), (XRegister ESP)) in
  let move = XSub ((XRegister ESP), (XConstant (numvars*4))) in
  let set_edi_to_esp = XMov(XRegister EDI, XRegister ESP) in
  let set_count = XMov(XRegister ECX, XConstant(numvars)) in
  let set_zero = XMov(XRegister EAX, XConstant(0)) in
  let restoreESP = XMov ((XRegister ESP), (XRegister EBP)) in
  let pop = XPop (XRegister EBP) in
  let env = create_function_environment params decList in

  let instructions = compile_a_expression env body in
  [label; push; setEBP; move; set_edi_to_esp; set_count; set_zero; XRepStosd]@instructions@[restoreESP; pop; XRet]

let compile_dec_data (ad: a_declaration): instruction list =
  let AFunction (name, params, body) = ad in
  let numparams = List.length(params) in
  let label = XLabel ("closure_of_"^name) in
  let data_name = ("_"^name) in
  let align_4_byte = XAlign 4 in
  let ddlist = XDd ["0x80000000"; "0x0"; string_of_int numparams; data_name] in
  [align_4_byte; label; ddlist]

let compile_program (p : program) : instruction list =
  let ap = a_normalize_program p in
  let AProgram(decList, ae) = ap in
  (*Use List.fold_left to apply compile_a_declaration to everything in decList and create funkyList *)
  let funkyList = List.fold_left (fun (l) (ad: a_declaration) -> l@compile_a_declaration ad decList) [] decList in
  let funkyData = List.fold_left (fun (l) (ad: a_declaration) -> l@compile_dec_data ad) [] decList in
  (*let (emptyDecList, ae) = ap in*)
  (*let ae = a_normalize_expression e in*)
  let numvars = count_vars_in_a_expression ae in
  let push = XPush (XRegister EBP) in
  let setEBP = XMov ((XRegister EBP), (XRegister ESP)) in
  let move = XSub ((XRegister ESP), (XConstant (numvars*4))) in
  let load_heap_cursor = XMov (XRegister EAX, XMemory(XAddressByRegisterOffset (EBP, 8))) in
  let store_heap_cursor = XMov (XMemory(XAddressByLabel "heap_cursor"), (XRegister EAX)) in
  let store_start_of_heap = XMov(XMemory(XAddressByLabel "start_of_heap"), (XRegister EAX)) in
  let load_heap_end = XMov (XRegister EAX, XMemory(XAddressByRegisterOffset (EBP, 12))) in
  let store_heap_end = XMov (XMemory(XAddressByLabel "end_of_heap"), (XRegister EAX)) in
  let store_start_of_stack = XMov (XMemory(XAddressByLabel "start_of_stack"), (XRegister EBP)) in
  let set_edi_to_esp = XMov(XRegister EDI, XRegister ESP) in
  let set_count = XMov(XRegister ECX, XConstant(numvars)) in
  let set_zero = XMov(XRegister EAX, XConstant(0)) in
  let restoreESP = XMov ((XRegister ESP), (XRegister EBP)) in
  let pop = XPop (XRegister EBP) in
  let instructions = compile_a_expression (create_program_environment decList) ae in
  let data_section = XSection "data" in
  let align_4_byte = XAlign 4 in
  let heap_cursor = XLabel "heap_cursor" in
  let space_for_heap_cursor_ptr = XDd ["0"] in
  let start_of_heap = XLabel "start_of_heap" in
  let space_for_start_of_heap = XDd ["0"] in
  let end_of_heap = XLabel "end_of_heap" in
  let space_for_end_of_heap = XDd ["0"] in
  let start_of_stack = XLabel "start_of_stack" in
  let space_for_start_of_stack = XDd ["0"] in
  let end_of_stack = XLabel "end_of_stack" in
  let space_for_end_of_stack = XDd ["0"] in
  [push; setEBP; move; load_heap_cursor; store_heap_cursor; store_start_of_heap; load_heap_end; store_heap_end; store_start_of_stack; set_edi_to_esp; set_count; set_zero; XRepStosd] @ instructions @ [restoreESP; pop; XRet]  @ funkyList @
  [data_section; align_4_byte; heap_cursor; space_for_heap_cursor_ptr; start_of_heap; space_for_start_of_heap; start_of_stack; space_for_start_of_stack; end_of_heap; space_for_end_of_heap; end_of_stack; space_for_end_of_stack] @ funkyData
;;

let compile_to_assembly_code (p : program) : string =
  let instructions = compile_program p in
  let instruction_code = code_of_instruction_list instructions in

  "section .text\n" ^
  "extern stopWithError\n" ^
  "extern printValue\n" ^
  "extern gc\n" ^
  "global snake_main\n" ^
  "global heap_cursor\n" ^
  "global start_of_heap\n" ^
  "global end_of_heap\n" ^
  "global start_of_stack\n" ^
  "global end_of_stack\n" ^
  "snake_main:\n" ^

  instruction_code ^
  "\n"
;;

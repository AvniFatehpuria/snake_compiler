(** This file should be used to write your tests of your other code. *)

open Batteries;;

open OUnit2;;

open TestUtils;;

(* TODO: put your unit tests here! *)



let all_tests =
  [
    test_success "test_code/4.snake" "4";
    test_success "test_code/dec_inc_4.snake" "4";
    test_success "test_code/double_let.snake" "4";
    test_success "test_code/inc_4.snake" "5";
    test_success "test_code/let_inc.snake" "7";
    test_success "test_code/let_x_10.snake" "10";
    test_success "test_code/nested_let.snake" "4";
    test_success "test_code/sharing.snake" "4";
    test_success "test_code/add2things.snake" "16";
    test_success "test_code/addnsub.snake" "6";
    test_success "test_code/anon_param_order.snake" "1";
    test_success "test_code/param_order.snake" "6";
    test_success "test_code/let-multiplication.snake" "40";
    test_success  "test_code/if.snake" "1";
    test_success  "test_code/if_within_let.snake" "10";
    test_success  "test_code/if_with_false_condition.snake" "0";
    test_success "test_code/true.snake" "true";
    test_success "test_code/is_bool_3.snake" "false";
    test_success "test_code/isBool_False.snake" "true";
    test_success "test_code/isint_5.snake" "true";
    test_success "test_code/isint_false.snake" "false";
    test_success "test_code/simple_<_false.snake" "false";
    test_success "test_code/simple_>_true.snake" "true";
    test_success "test_code/many_binary_ops_true.snake" "true";
    test_success "test_code/anding_bools.snake" "false";
    test_success "test_code/or_bools.snake" "true";
    test_success "test_code/1func_1param.snake" "5";
    test_success "test_code/2funcs_1param.snake" "18";
    test_success "test_code/recursive_func.snake" "15";
    test_success "test_code/mutualrecursion.snake" "1";
    test_success "test_code/tuple.snake" "(5, 3, 1)";
    test_success "test_code/is_tuple_a_tuple.snake" "true";
    test_success "test_code/is_tuple_a_bool.snake" "false";
    test_success "test_code/is_bool_a_tuple.snake" "false";
    test_success "test_code/is_int_a_tuple.snake" "false";
    test_success "test_code/is_tuple_an_int.snake" "false";
    test_success "test_code/valid_indexing.snake" "3";
    test_success "test_code/printing_a_tuple.snake" "(1, 2, 3)\n(1, 2, 3)";
    test_success "test_code/tuple_containing_prints.snake" "1\n2\n1";
    test_success "test_code/tuple_containing_bools.snake" "(true, false)";
    test_success "test_code/sorcerer_is_funk.snake" "4";
    test_success "test_code/tuple_containing_tuple.snake" "((1, 2, 3), true, 3, (5, 27, false))\nfalse";
    test_success "test_code/higher_order_funkytimes.snake" "28";
    test_success "test_code/printing_tons_of_args.snake" "1\n2\ntrue\nfalse\n6\n1\ntrue";
    test_success "test_code/multi-arg_funk.snake" "5";
  (*  test_success "test_code/building_and_growing_a_closure.snake" "<closure@0804869d>[3/6](1, 2, 3, ?, ?, ?)";
    test_success "test_code/testing_closures.snake" "<closure@0804853b>[1/2](1, ?)";*)
    test_success "test_code/growing_and_calling_closure_6_args_2_steps.snake" "21";
    test_success "test_code/is_closure_a_tuple.snake" "false";
    test_success "test_code/anon_funk_multiple_args.snake" "0";
    test_success "test_code/anon_funk_within_anon_funk.snake" "14";
    test_success "test_code/indexing_last_element_of_tuple.snake" "3";
    test_success "test_code/testing_tco_three_params.snake" "(10000003, true)";
    (*test_runtime_failure "test_code/if.snake" "0";*)
    test_runtime_failure "test_code/overflow_multiplication.snake" 3;
    test_runtime_failure "test_code/or_bool_int.snake" 2;
    test_runtime_failure "test_code/anding_ints.snake" 2;
    test_runtime_failure "test_code/inc_bool.snake" 1;
    test_runtime_failure "test_code/adding_bools.snake" 1;
    test_runtime_failure "test_code/multiplying_bools.snake" 1;
    test_runtime_failure "test_code/subtracting_bools.snake" 1;
    test_runtime_failure "test_code/indexing_into_a_bool.snake" 4;
    test_runtime_failure "test_code/indexing_into_a_closure.snake" 4;
    test_runtime_failure "test_code/indexing_with_a_bool.snake" 1;
    test_runtime_failure "test_code/indexing_tuple_negative.snake" 5;
    test_runtime_failure "test_code/indexing_tuple_out_of_range.snake" 5;
    test_runtime_failure "test_code/anding_tuple_and_false.snake" 2;
    test_runtime_failure "test_code/anding_tuples.snake" 2;
    test_runtime_failure "test_code/or_true_and_tuple.snake" 2;
    test_runtime_failure "test_code/or_two_tuples.snake" 2;
    test_compile_failure "test_code/nested_lets_unbound.snake"
      "Unbound variable y\n";
    test_compile_failure "test_code/unbound_var_in_tuple.snake" "Unbound variable x\n";
    test_compile_failure "test_code/1func_declared_twice.snake"
      "Duplicate definition of function f\n";
    test_compile_failure "test_code/1_undefined_function.snake"
      "Unbound variable g\n";
    test_compile_failure "test_code/1function_declared_3_times.snake"
      "Duplicate definition of function f\nDuplicate definition of function f\n";
    test_compile_failure "test_code/four_errors.snake"
      "Unbound variable g\nUnbound variable y\nUnbound variable z\n";
    test_compile_failure "test_code/func_decl_with_1_duplicate_param.snake"
      "Duplicate parameter x in function f\n";
    test_compile_failure "test_code/unbound_var_in_function_body.snake"
      "Unbound variable y\n";
    test_compile_failure "test_code/unbound.snake"
      "Unbound variable y\n";
    test_compile_failure "test_code/unbound_let.snake"
      "Unbound variable y\n";
    test_compile_failure "test_code/unbound_within_let.snake"
      "Unbound variable x\n"
  ];;

let suite = "compiler test suite" >::: all_tests;;

run_test_tt_main suite;;

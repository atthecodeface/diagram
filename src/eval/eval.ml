(*a To add

dupn
swapn
popn

 *)

(*a Modules *)
exception Syntax_error = Assignment.Syntax_error
type t_eval = Rpn_eval.t
type 'a t_resolver = 'a Rpn_eval.t_resolver

let make = Rpn_eval.make
let make_resolver = Rpn_eval.make_resolver
let resolve_all = Rpn_eval.resolve_all
let value_of = Rpn_eval.value_of
let eval_error = Assignment.tokenize_error

let value_make_vector  = Value.make_vector

let value_of_float     = Value.of_float
let value_of_floats    = Value.of_floats
let value_of_floats2   = Value.of_floats2
let value_of_floats_floats    = Value.of_floats_floats

let value_as_float     = Value.as_float
let value_as_floats    = Value.as_floats
let value_as_int       = Value.as_int

let no_value   = Value.no_value
let no_float   = Value.no_float
let no_vector  = Value.no_vector
let no_array   = Value.no_array

(*
let test _ =
  Test_stack.test_stack ();
  Test_assignment.test_expr ();
  Test_assignment.test_tokens ();
  Test_eval.test_assignment ();
  ()

let _ =
  test ()
 *)

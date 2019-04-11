(*a To add

dupn
swapn
popn

 *)

(*a Modules *)
module Value=Value
exception Syntax_error = Assignment.Syntax_error
type t_eval = Rpn_eval.t
type 'a t_resolver = 'a Rpn_eval.t_resolver

let make = Rpn_eval.make
let make_resolver = Rpn_eval.make_resolver
let resolve_all = Rpn_eval.resolve_all
let value_of = Rpn_eval.value_of

(*a Test stuff *)
(*f test_stack *)
let test_stack _ = Stack.(
  let s = init () in
  push_floats s [0.; 1.; 2.];
  let v = as_value s in
  let s = init () in
  Stack.push_value s v;
  let v = Value.apply_unary_float_fn (fun x-> (x *. 2.) -. 3. ) v in
  push_value s v;
  Printf.printf "Value v has %d floats\n" (Value.num_floats v);
  let v = Value.apply_unary_float_fn (fun x-> -. x) v in
  push_value s v;
  let v0 = Stack.get_abs s 0 in
  let v1 = Stack.get_rel s 2 in
  Printf.printf "Value v0 has %d floats\n" (Value.num_floats v0);
  Printf.printf "Value v1 has %d floats\n" (Value.num_floats v1);
  let v = Value.apply_binary_float_fn (fun x y -> x +. y) v0 v1 in
  Printf.printf "Value v has %d floats\n" (Value.num_floats v);
  push_value s v;
  let v = Value.split 5 v in
  push_value s v;
  let v = as_value s in
  let fa = Value.flatten v in
  Array.iteri (Printf.printf "%d %f\n") fa;
             )



  type test_t = { r : Rpn_eval.t;
             id : string;
             children : test_t list;
           }

  exception Unknown_child of string
let test_assignment _ =
  let find_child r id =
    let rec find_it = function
      | [] -> raise (Unknown_child id)
      | hd::_ when (String.equal id hd.id) -> hd
      | _::tl -> find_it tl
    in
    find_it r.children
  in
  let r1 = Rpn_eval.make "c=2;d=.c ^.e +;" in
  let t1 = {r=r1; id="r1"; children=[]} in
  let r0 = Rpn_eval.make "a=.r1.c;b=.a .r1.d +;e=5;" in
  let t0 = {r=r0; id="top"; children=[t1;]} in
  let al = t0.r in
  List.iter (Printf.printf "Assigns '%s'\n") (Rpn_eval.list_assigns al);
  let tres = make_resolver find_child (fun t->t.r) (fun _ s -> None) (fun t->t.id) in
  let v = Rpn_eval.resolve_value tres [t0] 0 ["b"] in
  Printf.printf "Value type %s\n" (Value.str_type v);
  let fa = Value.flatten v in
  Array.iteri (Printf.printf "%d %f\n") fa;
  ()

(*
let _ =  test_stack ()
let _ = test_expr ()
let _ = test_tokens ()
let _ = test_assignment ()
 *)



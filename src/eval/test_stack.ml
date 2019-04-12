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
  Printf.printf "stack : %s\n" (Value.str v)
)




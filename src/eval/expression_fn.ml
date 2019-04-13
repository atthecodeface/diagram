(*a Functions to manipulate stack and values *)
(*f unary_stack_value_fn (Stack.t -> Value.t -> Value.t) -> Stack.t -> unit *)
let unary_stack_value_fn f s = 
  let top = Stack.get_rel s 1 in
  Stack.pop s 1;
  let r = f s top in
  Stack.push_value s r
                   
(*f binary_stack_value_fn (Stack.t -> Value.t -> Value.t -> Value.t) -> Stack.t -> unit *)
let binary_stack_value_fn f s = 
  let t0 = Stack.get_rel s 2 in
  let t1 = Stack.get_rel s 1 in
  Stack.pop s 2;
  let r = f s t0 t1 in
  Stack.push_value s r
                   
(*f unary_value_fn (Value.t -> Value.t) -> Stack.t -> unit *)
let unary_value_fn f = unary_stack_value_fn (fun _ v -> f v)
                                            
(*f binary_value_fn (Value.t -> Value.t -> Value.t) -> Stack.t -> unit *)
let binary_value_fn f = binary_stack_value_fn (fun _ v0 v1 -> f v0 v1)
                                              
(*f unary_float_fn (float -> float) -> Stack.t -> unit *)
let unary_float_fn f = unary_value_fn (Value.apply_unary_float_fn f)
                                      
(*f binary_float_fn (float -> float -> float) -> Stack.t -> unit *)
let binary_float_fn f = binary_value_fn (Value.apply_binary_float_fn f)
                                        
(*f int_value_fn (int -> Value.t -> Value.t) -> Stack.t -> unit *)
let int_value_fn f s = 
  let v = Stack.get_rel s 2 in
  let i = Value.as_int (Stack.get_rel s 1) in
  Stack.pop s 2;
  let r = f i v in
  Stack.push_value s r
                   
(*f int_int_value_fn (int -> int -> Value.t -> Value.t) -> Stack.t -> unit *)
let int_int_value_fn f s = 
  let v = Stack.get_rel s 3 in
  let i0 = Value.as_int (Stack.get_rel s 2) in
  let i1 = Value.as_int (Stack.get_rel s 1) in
  Stack.pop s 3;
  let r = f i0 i1 v in
  Stack.push_value s r
                   
(*a Actual eq_fn functions list and their operation *)
(*t eq_fn *)
type eq_fn = {
    name : string;
    fn : Stack.t -> unit;
  }

(*v functions - add in functions *)
let functions =  [
    {name = "+"; fn = binary_float_fn (fun x y -> x +. y)} ;
    {name = "-"; fn = binary_float_fn (fun x y -> x -. y)} ;
    {name = "*"; fn = binary_float_fn (fun x y -> x *. y)} ;
    {name = "/"; fn = binary_float_fn (fun x y -> x /. y)} ;
    {name = "sqrt"; fn = unary_float_fn sqrt} ;
    {name = "sqr";  fn = unary_float_fn (fun x -> x ** 2.) };
    {name = "neg";  fn = unary_float_fn (fun x -> -. x) };
    {name = "sub";  fn = int_value_fn   Value.subscript };
    {name = "slice";  fn = int_int_value_fn   Value.slice };
    {name = "split";  fn = int_value_fn   Value.split };
    {name = "close"; fn = unary_value_fn Value.close} ;
    {name = "mod";  fn = unary_value_fn Value.modulus };
    {name = "len";  fn = unary_value_fn (fun s -> Value.(of_int (size s))) };
    {name = "dup";  fn = unary_stack_value_fn (fun s v -> Stack.push_value s v; v) };
    {name = "swap";  fn = binary_stack_value_fn (fun s v0 v1 -> Stack.push_value s v1; v0) };
    {name = "debug"; fn = (fun s -> Printf.printf "stack : %s\n" (Value.str (Stack.as_value s))) };
  ]

(*f find_fn string -> eq_fn option *)
let find_fn s =
  let rec find_it = function
    | []    -> None
    | hd::_ when (String.equal s hd.name) -> Some hd
    | _::tl -> find_it tl
  in
  find_it functions


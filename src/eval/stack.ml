(*a Exceptions *)
exception Out_of_bounds of string

(*a Stack functions and type t *)
(*t t *)
type t = {
    mutable size : int;
    mutable data : Value.t array;
    mutable ptr : int;
  }

(*v default_slack *)
let default_slack = 8

(*v null_slack *)
let null_stack = Array.make 0 Value.no_value

(*f init : unit -> t *)
let init _ = {
    size = 0;
    data = null_stack;
    ptr = 0;
  }

(*f data : t -> value array *)
let data s = s.data

(*f as_value : t -> value *)
let as_value s =
  if (s.ptr==0) then
    raise (Out_of_bounds "as_value with empty stack")
  else if (s.ptr==1) then
    s.data.(0)
  else
    Value.of_array s.data 0 s.ptr

(*f get_rel : t -> int -> value *)
let get_rel s n =
  if (n<=0) || (n>s.ptr) then
    raise (Out_of_bounds (Printf.sprintf "get relative of %d with ptr %d" n s.ptr))
  else
    s.data.(s.ptr-n)

(*f get_abs : t -> int -> value *)
let get_abs s n =
  if (n>=s.ptr) then
    raise (Out_of_bounds (Printf.sprintf "get absolute of %d with ptr %d" n s.ptr))
  else
    s.data.(n)

(*f pop : t -> int -> unit *)
let pop s n =
  if (n>s.ptr) then
    raise (Out_of_bounds (Printf.sprintf "pop of %d with ptr %d" n s.ptr))
  else
    s.ptr <- s.ptr-n

(*f push : t -> int -> ((int->value->unit)->unit) -> unit *)
let push s n f =
  if s.ptr + n > s.size then (
    let size = s.ptr + n + default_slack in
    let v i = (if i<s.ptr then s.data.(i) else Value.no_value) in
    let new_floats = Array.init size v in
    s.size <- size;
    s.data <- new_floats
  );
  let p = s.ptr in
  f (fun i v -> s.data.(p+i) <- v);
  s.ptr <- (p + n)

(*f push_value : t -> value -> unit *)
let push_value s v =
  push s 1 (fun set -> set 0 v)

(*f push_floats : t -> float list -> unit *)
let push_floats s fl =
  let n = List.length fl in
  push s n (fun set -> List.iteri (fun i f -> set i (Value.of_float f)) fl)

(*a Stack operation and type op *)
(*t op *)
type op = | Pop  of int
          | Push of (float list)

(*f op t -> op ->  *)
let op s o = 
  match o with 
  | Pop n -> pop s n
  | Push fl -> push_floats s fl


(*a Base layout element types and modules *)
open Types
exception Bad_type of string

let str_type = function
  | Ev_float _ -> "float"
  | Ev_floats (n,_) -> Printf.sprintf "floats[%d]" n
  | Ev_rect r -> "rect"
  | Ev_vector _ -> "vector"
  | Ev_vectors (n,_,_) -> Printf.sprintf "vectors[%d]" n
  | Ev_string _ -> "string"

let eval_value_of = function
  | Ev_float f -> Eval.value_of_float f
  | Ev_floats (n,arr) -> Eval.value_of_floats arr 0 n
  | Ev_rect r ->
               let (x0,y0,x1,y1)=r in
               Eval.value_of_floats2 [|x0;x1;x1;x0|] [|y0;y0;y1;y1|] 0 4
  | Ev_vector (x,y) -> Eval.value_make_vector x y
  | Ev_vectors (n,xs,ys) -> Eval.value_of_floats2 xs ys 0 n
  | _ -> Eval.no_value

let as_float = function
  | Ev_float f -> f
  | x -> raise (Bad_type (Printf.sprintf "wanted a float but had a %s" (str_type x)))

let as_floats = function
  | Ev_float f        -> [|f;|]
  | Ev_floats (n,arr) -> arr
  | Ev_vector (f0,f1) -> [|f0;f1;|]
  | Ev_rect r         -> Primitives.Rectangle.as_floats r
  | x -> raise (Bad_type (Printf.sprintf "wanted floats but had a %s" (str_type x)))

let as_string = function
  | Ev_string s -> s
  | x -> raise (Bad_type (Printf.sprintf "wanted a string but had a %s" (str_type x)))


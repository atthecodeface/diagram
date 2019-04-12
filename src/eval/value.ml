(*a Exceptions *)
exception Type_mismatch of string
exception Invalid_subscript of string
exception Bad_type of string

(*a Basic type and values *)
(*t t *)
type t = | Float of float
         | Vector of (float * float)
         | Subarray of (int * int * (t array))

(*v no_value *)
let no_value = Float 0.

(*v no_float *)
let no_float = Float 0.

(*v no_vector *)
let no_vector = Vector (0., 0.)

(*v no_array *)
let no_array = Subarray (0,0,(Array.make 0 (Float 0.)))

(*a Base functions *)
(*f str_type : t -> string *)
let str_type = function
  | Float f -> "float"
  | Vector v -> "vector"
  | Subarray (s,n,a) -> Printf.sprintf "array[%d]" n

(*f str : t -> string *)
let rec str = function
  | Float f -> Printf.sprintf "float %f" f
  | Vector (f0,f1) -> Printf.sprintf "vector (%f,%f)" f0 f1
  | Subarray (s,n,a) -> Printf.sprintf "array[%d;%d] {%s}" s n (String.concat "," (List.map str (Array.to_list a)))

(*f float_range : f0:float -> f1:float -> n -> float array of size n with values f0 to f1 linearly
 *)
let float_range f0 f1 n =
  let d = (f1 -. f0) /. (float (n-1)) in
  Array.init n (fun i -> f0 +. (d *. (float i)))

(*f num_floats : t -> int number of floats required to flatten the value*)
let rec num_floats = function
  | Float _ -> 1
  | Vector _ -> 2
  | Subarray (s,n,a) -> 
     let acc_num_floats (acc,i) a =
       if (i<s) then
         (acc,i+1)
       else if (i>=s+n) then
         (acc,i+1)
       else
         (acc + (num_floats a),i+1)
     in
     let (acc,_) = Array.fold_left acc_num_floats (0,0) a in
     acc

(*f fill_float_array (float array) -> int offset -> int index -> t -> int next_index ; fills the float array *)
let rec fill_float_array a i o t =
  match t with
  | Float f -> (a.(i) <- f; i+1)
  | Vector (f0,f1) -> ( a.(i) <- f0; a.(i+1) <- f1; i+2)
  | Subarray (s,n,arr) ->
     if (o<s) then
       fill_float_array a i s t
     else if (o>=s+n) then
       i
     else (
       let ni = fill_float_array a i 0 arr.(o) in
       fill_float_array a ni (o+1) t
     )

(*f subarray_foldi *)
let rec subarray_foldi f acc arr s n  =
  if (n<=0) then
    acc
  else 
    let acc = f acc arr s in
    subarray_foldi f acc arr (s+1) (n-1) 

(*f append_value *)
let append_value acc value =
  match acc with 
  | Subarray (s,n,arr) ->
     let fill i = if (i<n) then arr.(s+i) else value in
     let na = Array.init (n+1) fill in
     Subarray (0,n+1,na)
  | _ -> raise (Bad_type "For append_value")

(*a Value extraction *)
(*f as_int t *)
let as_int = function
  | Float f -> int_of_float f
  | t -> raise (Bad_type (Printf.sprintf "cannot represent '%s' as int" (str_type t)))

(*f as_float t *)
let as_float = function
  | Float f -> f
  | t -> raise (Bad_type (Printf.sprintf "cannot represent '%s' as float" (str_type t)))

(*f as_floats t -> (float array) *)
let as_floats v =
  let n = num_floats v in
  let a = Array.make n 0. in
  ignore (fill_float_array a 0 0 v);
  a

(*a Value creation *)
(*f of_int int -> t *)
let of_int n =  Float (float n)

(*f of_float float -> t *)
let of_float f =  Float f

(*f make_vector float -> float -> t *)
let make_vector f0 f1 = Vector (f0, f1)

(*f of_array t -> int -> int -> t *)
let of_array arr s n = Subarray (s,n,arr)

(*f of_floats (float array) -> int -> int -> t as Subarray of Float *)
let of_floats arr s n = 
  let fill i =  Float arr.(s+i) in
  let na = Array.init n fill in
  Subarray (0,n,na)

(*f of_floats2 (float array) -> (float array) -> int -> int -> t as Subarray of Vector
    Build Array of vectors with xs and ys from two separate arrays
 *)
let of_floats2 arr0 arr1 s n = 
  let fill i =  Vector (arr0.(s+i), arr1.(s+i)) in
  let na = Array.init n fill in
  Subarray (0,n,na)

(*a Functions to apply to values and arrays etc *)
(*f apply_unary_float_fn (float -> float) -> t -> t *)
let rec apply_unary_float_fn fn = function
  | Float f -> Float (fn f)
  | Vector (f0, f1) -> Vector ((fn f0),(fn f1))
  | Subarray (s,n,arr) ->
     let na = Array.init n (fun i -> apply_unary_float_fn fn arr.(s+i)) in
     Subarray (0,n,na)

(*f apply_binary_float_fn (float -> float -> float) -> t -> t *)
let rec apply_binary_float_fn fn t0 t1 =
  match (t0, t1) with 
  | Float f0, Float f1 -> Float (fn f0 f1)
  | Vector (f00, f01), Vector (f10, f11) -> Vector ((fn f00 f10),(fn f01 f11))
  | Subarray (s0,n0,arr0), Subarray (s1,n1,arr1) when (n0==n1)->
     let na = Array.init n0 (fun i -> apply_binary_float_fn fn arr0.(s0+i) arr1.(s1+i)) in
     Subarray (0,n0,na)
  | _ -> raise (Type_mismatch (Printf.sprintf "types %s and %s" (str_type t0) (str_type t1)))

(*f subscript int -> t -> t *)
let subscript i = function
  | Vector (f0, _)  when (i==0) -> (Float f0)
  | Vector (_,  f1) when (i==1) -> (Float f1)
  | Subarray (s,n,arr) when ((i>=0) && (i<n)) ->
     arr.(s+i)
  | Subarray (_,n,_) -> raise (Invalid_subscript (Printf.sprintf "index %d max %d" i n))
  | _ -> raise (Bad_type "For subscript")

(*f slice int -> int -> t -> t *)
let slice i0 i1 = function
  | Subarray (s,n,arr) when ((i0>=0) && (i1>i0) && (i1<=n)) ->
     Subarray (s+i0,i1-i0,arr)
  | _ -> raise (Bad_type "For slice")

(*f size t -> int *)
let size = function
  | Vector _ -> 2
  | Subarray (_,n,_) -> n
  | _ -> raise (Bad_type "For size")

(*f split int -> t -> t
    split of a float is not legal
    split by M of a vector (a0,a1) is an array of M values from a0 to a1
    split by M of an array of N floats (a0...a(N-1)) is an array of N arrays of M floats from a(n)...a(n+1)
    split by M of an array of N vectors (v0...v(N-1)) is an array of N arrays of M vectors from v(n)...v(n+1)
 *)
let split m = function
  | Vector (f0, f1) -> 
     of_floats (float_range f0 f1 m) 0 m
  | Subarray (s,n,arr) ->
     let acc_split acc arr i = 
       match arr.(s+i), arr.(s+i+1) with
       | Float f0, Float f1 ->
          append_value acc (of_floats (float_range f0 f1 m) 0 m)
       | Vector (f00, f01), Vector (f10, f11) ->
          append_value acc (of_floats2 (float_range f00 f10 m) (float_range f01 f11 m) 0 m)
       | _ -> raise (Bad_type "For split of subarray")
     in
     subarray_foldi acc_split no_array arr s (n-1)
  | _ -> raise (Bad_type "For split")

(*f modulus t -> t
    mod of a float f is abs(f)
    mod of a vector v is |v|
    mod of an array is an array of the mod of its internals
 *)
let rec modulus = function
  | Float f -> Float (abs_float f)
  | Vector (f0, f1) -> 
     Float (sqrt (f0*.f0 +. f1*.f1))
  | Subarray (s,n,arr) ->
     let na = Array.init n (fun i -> modulus arr.(s+i)) in
     Subarray (0,n,na)

(*f close t -> t
    close of an array is an array with the first element replicated at the end
 *)
let close = function
  | Subarray (s,n,arr) ->
     let na = Array.init (n+1) (fun i -> if i<n then arr.(s+i) else arr.(s)) in
     Subarray (0,n+1,na)
  | _ -> raise (Bad_type "For close")

(*f All done *)

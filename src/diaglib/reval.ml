(*a To add

dup
swap
dupn
swapn
popn

 *)

(*a Value module *)
module Value = struct
  exception Type_mismatch of string
  exception Invalid_subscript of string
  exception Bad_type of string

  (*t t *)
  type t = | Float of float
           | Vector of (float * float)
           | Subarray of (int * int * (t array))

  (*v no_value *)
  let no_value = Float 0.

  (*v no_array *)
  let no_array = Subarray (0,0,(Array.make 0 (Float 0.)))

  (*f as_int t *)
  let as_int = function
    | Float f -> int_of_float f
    | _ -> raise (Bad_type "as_int")

  (*f of_int int -> t *)
  let of_int n =  Float (float n)

  (*f of_float float -> t *)
  let of_float f =  Float f

  (*f of_array t -> int -> int -> t *)
  let of_array arr s n = Subarray (s,n,arr)

  (*f of_floats (float array) -> int -> int -> t as Subarray of Float *)
  let of_floats arr s n = 
    let fill i =  Float arr.(s+i) in
    let na = Array.init n fill in
    Subarray (0,n,na)

  (*f of_floats2 (float array) -> (float array) -> int -> int -> t as Subarray of Vector *)
  let of_floats2 arr0 arr1 s n = 
    let fill i =  Vector (arr0.(s+i), arr1.(s+i)) in
    let na = Array.init n fill in
    Subarray (0,n,na)

  (*f float_range
   *)
  let float_range f0 f1 n =
    let d = (f1 -. f0) /. (float (n-1)) in
    Array.init n (fun i -> f0 +. (d *. (float i)))

  (*f num_floats t -> int *)
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

  (*f fill_float_array (float array) -> int offset -> int index -> t -> int next_index *)
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

  (*f flatten t -> (float array) *)
  let flatten v =
    let n = num_floats v in
    let a = Array.make n 0. in
    ignore (fill_float_array a 0 0 v);
    a

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

  (*f apply_unary_float_fn (float -> float) -> t -> t *)
  let rec apply_unary_float_fn fn = function
    | Float f -> Float (fn f)
    | Vector (f0, f1) -> Vector ((fn f0),(fn f1))
    | Subarray (s,n,arr) ->
       let na = Array.init n (fun i -> apply_unary_float_fn fn arr.(s+i)) in
       Subarray (0,n,na)

  (*f subscript int -> t -> t *)
  let subscript i = function
    | Vector (f0, _)  when (i==0) -> (Float f0)
    | Vector (_,  f1) when (i==1) -> (Float f1)
    | Subarray (s,n,arr) when ((i>=0) && (i<=n)) ->
       arr.(s+i)
    | _ -> raise (Invalid_subscript (Printf.sprintf "index %d" i))

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

  (*f apply_binary_float_fn (float -> float -> float) -> t -> t *)
  let str_type = function
    | Float f -> "float"
    | Vector v -> "vector"
    | Subarray (s,n,a) -> "array"
  let rec apply_binary_float_fn fn t0 t1 =
    match (t0, t1) with 
    | Float f0, Float f1 -> Float (fn f0 f1)
    | Vector (f00, f01), Vector (f10, f11) -> Vector ((fn f00 f10),(fn f01 f11))
    | Subarray (s0,n0,arr0), Subarray (s1,n1,arr1) when (n0==n1)->
       let na = Array.init n0 (fun i -> apply_binary_float_fn fn arr0.(s0+i) arr1.(s1+i)) in
       Subarray (0,n0,na)
    | _ -> raise (Type_mismatch (Printf.sprintf "types %s and %s" (str_type t0) (str_type t1)))

  (*f All done *)

end

(*a Stack module *)
module Stack = struct
  exception Out_of_bounds of string

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

  (*t op *)
  type op = | Pop  of int
            | Push of (float list)

  (*f op t -> op ->  *)
  let op s o = 
    match o with 
    | Pop n -> pop s n
    | Push fl -> push_floats s fl

end

(*a ExpressionFunc functor module *)
(*m ExpressionRef type *)
module type ExpressionRef = sig
  type t_ref
end

(*m ExpressionFn *)
module ExpressionFn = struct
  (*t eq_fn *)
  type eq_fn = {
      name : string;
      fn : Stack.t -> unit;
    }

  (*f unary_value_fn (Value.t -> Value.t) -> Stack.t -> unit *)
  let unary_value_fn f s = 
    let top = Stack.get_rel s 1 in
    Stack.pop s 1;
    let r = f top in
    Stack.push_value s r
                     
  (*f unary_float_fn (float -> float) -> Stack.t -> unit *)
  let unary_float_fn f s = 
    let top = Stack.get_rel s 1 in
    Stack.pop s 1;
    let r = Value.apply_unary_float_fn f top in
    Stack.push_value s r
                     
  (*f binary_float_fn (float -> float -> float) -> Stack.t -> unit *)
  let binary_float_fn f s = 
    let t0 = Stack.get_rel s 2 in
    let t1 = Stack.get_rel s 1 in
    Stack.pop s 2;
    let r = Value.apply_binary_float_fn f t0 t1 in
    Stack.push_value s r
                     
  (*f int_value_fn (int -> Value.t -> Value.t) -> Stack.t -> unit *)
  let int_value_fn f s = 
    let v = Stack.get_rel s 2 in
    let i = Value.as_int (Stack.get_rel s 1) in
    Stack.pop s 2;
    let r = f i v in
    Stack.push_value s r
                     
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
    {name = "split";  fn = int_value_fn   Value.split };
    {name = "mod";  fn = unary_value_fn Value.modulus };
    {name = "len";  fn = unary_value_fn (fun s -> Value.(of_int (size s))) };
    ]

  (*f find_fn string -> eq_fn option *)
  let find_fn s =
    let rec find_it = function
      | []    -> None
      | hd::_ when (String.equal s hd.name) -> Some hd
      | _::tl -> find_it tl
    in
    find_it functions

end

(*m ExpressionFunc (ExpressionRef) *)
module ExpressionFunc (ERef : ExpressionRef) = struct
  (*e Exceptions *)
  exception Function_unknown of string

  (*t elt *)
  type elt = | Float of float
             | Function of ExpressionFn.eq_fn
             | Ref of ERef.t_ref

  (*f prepend_float elt list -> float -> elt_list *)
  let prepend_float f e = (Float f) :: e

  (*f prepend_function elt list -> s -> elt_list *)
  let prepend_function s e =
    match ExpressionFn.find_fn s with
    | None -> raise (Function_unknown s)
    | Some f ->  (Function f) :: e

  (*f prepend_ref elt list -> s -> elt_list *)
  let prepend_ref s e = (Ref s) :: e

  (*f evaluate_element resolve_fn -> Stack.t -> elt -> unit *)
  let evaluate_element rfn s elt =
    match elt with
    | Float f -> Stack.push_value s (Value.of_float f)
    | Function efn -> efn.fn s
    | Ref r -> Stack.push_value s (rfn r)

  (*f evaluate elt list -> Value.t *)
  let evaluate rfn e = 
    let stack = Stack.init () in
    let rec eval_iter stack el =
      match el with
        [] -> ()
      | elt::tl -> (
        evaluate_element rfn stack elt;
        eval_iter stack tl
      )
    in
    eval_iter stack e;
    Stack.as_value stack

  (*f All done *)
end

(*a TokenFunc module *)
(*m TokenType type *)
module type TokenType = sig
  type t
  type tokenize_fn = string -> int -> int -> (t * int) option
  val tokenizers : tokenize_fn list
  val str : t -> string
  val strl : t list -> string
end

(*m TokenFunc module *)
module TokenFunc (T : TokenType) = struct
  (*t t *)
  type t = T.t

  (*f is_whitespace int -> bool *)
  let is_whitespace ch = ((ch==9) || (ch==32))

  (*f next_token string -> len -> ofs -> (t * int) option *)
  let rec next_token s n i =
    if i>=n then
      None
    else
      let ch = Char.code (String.get s i) in
      if is_whitespace ch then
        next_token s n (i+1)
      else (
        let rec try_token_fn = function
          | [] -> None
          | hd::tl ->
             match hd s n i with
             | Some x -> Some x
             | _ -> try_token_fn tl
        in
        try_token_fn T.tokenizers
      )

  (*f tokenize_string string -> (t list * int) *)
  let tokenize_string s =
    let n = String.length s in
    let rec acc_tokens i rev_tokens = 
      match next_token s n i with
      | None -> (rev_tokens, i)
      | Some (t,ni) -> 
         acc_tokens ni (t::rev_tokens)
    in
    let (rev_tokens, ni) = acc_tokens 0 [] in
    (List.rev rev_tokens, ni)

  (*f str, strl *)
  let str  = T.str
  let strl = T.strl

  (*f All done *)
end

(*a TokenString module *)
module TokenString = struct
  (*t t_ref *)
  type t_ref = {
    parents : int;
    names : string list;
    }

  (*f fold_left_string *)
  let fold_left_string f acc s =
    let n = String.length s in
    let rec fold acc i =
      if i>=n then acc else fold (f acc (String.get s i)) (i+1)
    in
    fold acc 0

  (*f ref_of_string *)
  let ref_of_string s =
    let n = String.length s in
    let (_,parents) = fold_left_string (fun (state,n) ch -> if (state && (ch=='^')) then (state,n+1) else (false,n)) (true,0) s in
    let refs = String.sub s (parents+1) (n-parents-1) in
    let names = String.split_on_char '.' refs in
    {parents; names;}

  (*t t *)
  type t = | Float of float
           | Ref of   t_ref
           | Op of string
           | Assign
           | Semi
           | Lvalue of string

  (*t tokenize_fn *)
  type tokenize_fn = string -> int -> int -> (t * int) option

  (*f string_as_op_rex Re.t *)
  let string_as_op_rex =
    let op_name  = Re.(rep1 alpha) in
    let op_punct = Re.set "+-*/" in
    let op    = Re.(group (alt [op_name; op_punct] )) in
    let rest  = Re.(group (rep any)) in
    Re.(compile (seq [start; op ; rest]))

  (*f string_as_lvalue_rex Re.t *)
  let string_as_lvalue_rex =
    let lvalue = Re.(group (seq [alpha; (rep wordc)])) in
    let rest   = Re.(group (rep any)) in
    Re.(compile (seq [start; lvalue ; rest]))

  (*f string_as_float_rex Re.t *)
  let string_as_float_rex =
    let point = Re.char '.' in
    let minus = Re.char '-' in
    let float = Re.(group (seq [opt minus; rep1 digit ; opt point ; rep digit] )) in
    let rest  = Re.(group (rep any)) in
    Re.(compile (seq [start; float ; rest]))

  (*f string_as_ref_rex Re.t *)
  let string_as_ref_rex =
    let refstart = Re.(rep (char '^')) in
    let refel   = Re.(seq [char '.'; alpha ; (rep wordc)]) in
    let ref   = Re.(group (seq [refstart ; rep1 refel])) in
    let rest  = Re.(group (rep any)) in
    Re.(compile (seq [start; ref ; rest]))

  (*f tokenize_assign string -> len -> ofs -> (t * int) option *)
  let tokenize_assign s n i =
    if (String.get s i) == '=' then (Some (Assign, i+1)) else None

  (*f tokenize_semi string -> len -> ofs -> (t * int) option *)
  let tokenize_semi s n i =
    if (String.get s i) == ';' then (Some (Semi, i+1)) else None

  (*f tokenize_float string -> len -> ofs -> (t * int) option *)
  let tokenize_float s n i =
    match (Re.exec_opt ~pos:i string_as_float_rex s) with
    | None -> None
    | Some g -> (
      let ns = Re.Group.get g 1 in
      let ni = i + (String.length ns) in
      let f = float_of_string ns in
      Some (Float f, ni)
    )

  (*f tokenize_ref string -> len -> ofs -> (t * int) option *)
  let tokenize_ref s n i =
    match (Re.exec_opt ~pos:i string_as_ref_rex s) with
    | None -> None
    | Some g -> (
      let ns = Re.Group.get g 1 in
      let ni = i + (String.length ns) in
      Some (Ref (ref_of_string ns), ni)
    )

  (*f tokenize_op string -> len -> ofs -> (t * int) option *)
  let tokenize_op s n i =
    match (Re.exec_opt ~pos:i string_as_op_rex s) with
    | None -> None
    | Some g ->
       let ns = Re.Group.get g 1 in
       match ExpressionFn.find_fn ns with
       | None   -> None
       | Some _ -> (
         let ni = i + (String.length ns) in
         Some (Op ns, ni)
       )

  (*f tokenize_lvalue string -> len -> ofs -> (t * int) option *)
  let tokenize_lvalue s n i =
    match (Re.exec_opt ~pos:i string_as_lvalue_rex s) with
    | None -> None
    | Some g ->
       let ns = Re.Group.get g 1 in
       let ni = i + (String.length ns) in
       Some (Lvalue ns, ni)

  (*v tokenizers *)
  let tokenizers = [
    tokenize_float;
    tokenize_assign;
    tokenize_semi;
    tokenize_ref;
    tokenize_op;
    tokenize_lvalue;
    ]

  (*f str *)
  let str = function
    | Float f  -> Printf.sprintf "Float %f" f
    | Ref t    -> Printf.sprintf "Ref %d:[%s]" t.parents (String.concat " " t.names)
    | Op s     -> Printf.sprintf "Op %s" s
    | Assign   -> "="
    | Semi     -> ";"
    | Lvalue s -> Printf.sprintf "Lvalue %s" s

  (*f strl *)
  let strl tl =
    String.concat "\n" (List.map str tl)

  (*f All done *)
end

(*a Expression and token *)
module Expression = ExpressionFunc(TokenString)
module Token      = struct
  exception Unexpected_end_of_string
  exception Syntax_error of string
  include TokenFunc(TokenString)

  let get_lvalue = function
    | [] -> raise Unexpected_end_of_string
    | (TokenString.Lvalue s)::tl -> (s, tl)
    | _ -> raise (Syntax_error "expected lvalue")

  let build_expression tl =
    let rec add_expression_elt acc = function 
      | (TokenString.Float f)::tl -> add_expression_elt (Expression.prepend_float f acc) tl
      | (TokenString.Op s)::tl    -> add_expression_elt (Expression.prepend_function s acc) tl
      | (TokenString.Ref s)::tl   -> add_expression_elt (Expression.prepend_ref s acc) tl
      | tl -> (acc, tl)
    in
    let (reve,tl) = add_expression_elt [] tl in
    (List.rev reve, tl)

  let skip t = function
    | [] -> raise Unexpected_end_of_string
    | hd::tl when (hd==t) -> tl
    | _ -> raise (Syntax_error (Printf.sprintf "expected token '%s'" (str t)))

end

(*a Assignment module *)
module Assignment = struct
  (*t t *)
  type t = {
      lvalue : string;
      expression : Expression.elt list;
      mutable in_eval : bool;
      mutable value : Value.t option;
    }

  (*e Recursive_evaluation *)
  exception Recursive_evaluation of t
  exception Failed_to_evaluate of t * exn

  (*f build_from_tokens *)
  let build_from_tokens tl =
    let (lvalue, tl) = Token.get_lvalue tl in
    let tl = Token.(skip Assign tl) in
    let (expression,tl) = Token.build_expression tl in
    let tl = Token.(skip Semi tl) in
    let t = {lvalue; expression; in_eval=false; value=None;} in
    (t, tl)

  (*f name t -> string *)
  let name t = t.lvalue

  (*f name_matches t -> string -> bool *)
  let name_matches t = String.equal t.lvalue

  (*f value_of rfn -> t -> Value.t; raise exception if loop in resolution *)
  let value_of rfn a =
    if a.in_eval then raise (Recursive_evaluation a) else
      match a.value with
      | Some f -> f
      | None -> (
        a.in_eval <- true;
        let value = 
          try Expression.evaluate rfn a.expression with
          | e -> raise (Failed_to_evaluate (a, e))
        in
        a.value <- Some value;
        a.in_eval <- false;
        value
      )

  (*f is_resolved t -> bool *)
  let is_resolved a =
    match a.value with
    | Some f -> true
    | None -> false

  (*f being_resolved t -> bool *)
  let being_resolved a = a.in_eval

  (*f All done *)
end

(*a Reval module *)
module Reval = struct
  (*e Unexpected_character *)
  exception Unexpected_character of string
  exception Unknown_lvalue of string
  exception Failed_to_evaluate = Assignment.Failed_to_evaluate

  (*t t *)
  type t = {
      s : string;
      assigns : Assignment.t list;
    }

  (*f make string -> t *)
  let make s =
    let (tl, n) = Token.tokenize_string s in
    if n<(String.length s) then raise (Unexpected_character (Printf.sprintf "'%c' at offset %d" (String.get s n) n));
    let rec add_assignment acc = function
      | [] -> acc
      | tl -> (
        let (a, tl) = Assignment.build_from_tokens tl in
        add_assignment (a::acc) tl
      )
    in
    let assigns = add_assignment [] tl in
    { s; assigns }

  (*f find_assignment *)
  let find_assignment t s =
    let rec find_it = function
      | []    -> None
      | hd::_ when (Assignment.name_matches hd s) -> Some hd
      | _::tl -> find_it tl
    in
    find_it t.assigns

  (*f value_of *)
  let value_of t name rfn =
    match find_assignment t name with
    | None -> None
    | Some a -> Some (Assignment.value_of rfn a)

  (*f list_assigns *)
  let list_assigns t =
    List.map (fun a->Assignment.name a)t.assigns

  (*t t_resolver *)
  type 'a t_resolver = {
    find_child : 'a -> string -> 'a;
    get_id     : 'a -> string;
    get_ref    : 'a -> t;
    get_value  : 'a -> string -> Value.t option; (* Get the value of 'string' thing within 'a us if possible *)
    }

  (*f make_resolver *)
  let make_resolver find_child get_ref get_value get_id =
    {find_child; get_ref; get_value; get_id}

  (*f resolve_value *)
  let hier tres rev_stack = List.fold_left (fun acc t -> Printf.sprintf "%s.%s" (tres.get_id t) acc) "" rev_stack

  let rec evaluate tres rev_stack lvalue =
    let t = List.hd rev_stack in
    try (
      match value_of (tres.get_ref t) lvalue (fun tr -> resolve_value tres rev_stack tr.parents tr.names) with
      | Some v -> v
      | None -> (
        match tres.get_value t lvalue with
        | Some v -> v
        | None ->       let hier = hier tres rev_stack in
                        raise (Unknown_lvalue (hier ^ lvalue))
      )
    ) with
    | Failed_to_evaluate (a,e) -> (
      let hier = hier tres rev_stack in
      Printf.printf "Evaluating %s\n" (hier ^ (Assignment.name a));
      raise e
    )
  and resolve_value tres rev_stack parents names =
    if (parents>0) then 
      resolve_value tres (List.tl rev_stack) (parents-1) names
    else
      match names with 
      | lvalue::[] -> evaluate tres rev_stack lvalue
      | id::tl ->
        let t = List.hd rev_stack in
        let t = tres.find_child t id in
        resolve_value tres (t::rev_stack) 0 tl
      | _ -> raise Not_found (* Should not be possible *)

  (*f resolve_all *)
  let resolve_all (tres : 'a t_resolver) (t:'a) (rev_stack: 'a list)  =
    (*List.map (fun a -> let lvalue=Assignment.name a in (lvalue, resolve_value tres rev_stack 0 [lvalue])) t.assigns*)
    List.map (fun a ->
        let lvalue=Assignment.name a in
        (lvalue, evaluate tres (t::rev_stack) lvalue)
      ) (tres.get_ref t).assigns

  (*f All done *)    
end

(*a Promote things to module level *)
exception Syntax_error = Token.Syntax_error
let make = Reval.make
let make_resolver = Reval.make_resolver
let resolve_all = Reval.resolve_all
type t_reval = Reval.t
type 'a t_resolver = 'a Reval.t_resolver

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
let test_expr _ =
  let e = Expression.( [] |>
    prepend_float 1.0 |>
    prepend_float 2.0 |>
    prepend_function "+"
    ) in
  let e = List.rev e in
  let v = Expression.evaluate (fun _ -> Value.no_value) e in
  let fa = Value.flatten v in
  Array.iteri (Printf.printf "%d %f\n") fa

let test_tokens _ =
  let str = "a=2;b=a 3 +;" in (* joe = ^^^.fred 34 sqrt +;  *)
  let (tl,ni) = Token.tokenize_string str in
  Printf.printf "Tokenized %s length %d\n" str (String.length str);
  Printf.printf "next index %d\n" ni;
  Printf.printf "Tokens: %s\n" (Token.strl tl);
  let (a,tl) = Assignment.build_from_tokens tl in
  Printf.printf "Now tokens: %s\n" (Token.strl tl);
  let v = Assignment.value_of (fun _ -> Value.no_value) a in
  let fa = Value.flatten v in
  Array.iteri (Printf.printf "%d %f\n") fa;
  ()


  type test_t = { r : Reval.t;
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
  let r1 = Reval.make "c=2;d=.c ^.e +;" in
  let t1 = {r=r1; id="r1"; children=[]} in
  let r0 = Reval.make "a=.r1.c;b=.a .r1.d +;e=5;" in
  let t0 = {r=r0; id="top"; children=[t1;]} in
  let al = t0.r in
  List.iter (Printf.printf "Assigns '%s'\n") (Reval.list_assigns al);
  let tres = make_resolver find_child (fun t->t.r) (fun _ s -> None) (fun t->t.id) in
  let v = Reval.resolve_value tres [t0] 0 ["b"] in
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



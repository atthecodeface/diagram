(*a Useful functions *)
(*f fold_left_string *)
let fold_left_string f acc s =
  let n = String.length s in
  let rec fold acc i =
    if i>=n then acc else fold (f acc (String.get s i)) (i+1)
  in
  fold acc 0

(*a Type t_ref - a reference that is up N hierarchires then a list of strings to go down *)
(*t t_ref *)
type t_ref = {
  parents : int;
  names : string list;
}

(*f ref_of_string *)
let ref_of_string s =
  let n = String.length s in
  let (_,parents) = fold_left_string (fun (state,n) ch -> if (state && (ch=='^')) then (state,n+1) else (false,n)) (true,0) s in
  let refs = String.sub s (parents+1) (n-parents-1) in
  let names = String.split_on_char '.' refs in
  {parents; names;}

(*a Regular expressions *)
(*v string_as_op_rex Re.t *)
let string_as_op_rex =
  let op_name  = Re.(rep1 alpha) in
  let op_punct = Re.set "+-*/" in
  let op    = Re.(group (alt [op_name; op_punct] )) in
  let rest  = Re.(group (rep any)) in
  Re.(compile (seq [start; op ; rest]))

(*v string_as_lvalue_rex Re.t *)
let string_as_lvalue_rex =
  let lvalue = Re.(group (seq [alpha; (rep wordc)])) in
  let rest   = Re.(group (rep any)) in
  Re.(compile (seq [start; lvalue ; rest]))

(*v string_as_float_rex Re.t *)
let string_as_float_rex =
  let point = Re.char '.' in
  let minus = Re.char '-' in
  let float = Re.(group (seq [opt minus; rep1 digit ; opt point ; rep digit] )) in
  let rest  = Re.(group (rep any)) in
  Re.(compile (seq [start; float ; rest]))

(*v string_as_ref_rex Re.t *)
let string_as_ref_rex =
  let refstart = Re.(rep (char '^')) in
  let refel   = Re.(seq [char '.'; alpha ; (rep wordc)]) in
  let ref   = Re.(group (seq [refstart ; rep1 refel])) in
  let rest  = Re.(group (rep any)) in
  Re.(compile (seq [start; ref ; rest]))

(*a Type t - a token that may be float, ref, op, assign, lvalue or semicolon *)
(*t t *)
type t = | Float of float
         | Ref of   t_ref
         | Op of string
         | Assign
         | Semi
         | Lvalue of string

(*f str *)
let str = function
  | Float f  -> Printf.sprintf "Float %f" f
  | Ref t    -> Printf.sprintf "Ref %d:[%s]" t.parents (String.concat " " t.names)
  | Op s     -> Printf.sprintf "Op %s" s
  | Assign   -> "="
  | Semi     -> ";"
  | Lvalue s -> Printf.sprintf "Lvalue %s" s

(*a tokenize_fn type and tokenizers - required to be a tokenizer *)
(*t tokenize_fn  *)
type tokenize_fn = string -> int -> int -> (t * int) option

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
     match Expression_fn.find_fn ns with
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

(*v tokenizers - should work in any order as the tokens are distinct *)
let tokenizers = [
    tokenize_float;
    tokenize_assign;
    tokenize_semi;
    tokenize_ref;
    tokenize_op;
    tokenize_lvalue;
  ]


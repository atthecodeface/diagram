(*a Expression and Token modules - using the Token_string.t_ref and Token_string tokenizers *)
module Expression = Expression_eval.ExpressionFunc(Token_string)
module Token      = struct
  exception Unexpected_end_of_string of string
  exception Syntax_error of int * string
  include Token_func.TokenFunc(Token_string)

  (*f get_lvalue : tl -> string * tl *)
  let get_lvalue = function
    | [] -> raise (Unexpected_end_of_string "lvalue")
    | (pos, Token_string.Lvalue s)::tl -> (s, tl)
    | (pos, tr)::_ -> raise (Syntax_error (pos, (Printf.sprintf "expected lvalue but got a '%s'" (str tr))))

  let build_expression tl =
    let rec add_expression_elt acc = function 
      | (pos, Token_string.Float f)::tl -> add_expression_elt (Expression.prepend_float f acc) tl
      | (pos, Token_string.Op s)::tl    -> add_expression_elt (Expression.prepend_function s acc) tl
      | (pos, Token_string.Ref s)::tl   -> add_expression_elt (Expression.prepend_ref s acc) tl
      | tl -> (acc, tl)
    in
    let (reve,tl) = add_expression_elt [] tl in
    (List.rev reve, tl)

  (*f skip : t -> tl -> tl ; skips token if got token t at head of tl *)
  let skip (t:t) = function
    | [] -> raise (Unexpected_end_of_string (str t))
    | (pos,hd)::tl when (hd==t) -> tl
    | (pos,tr)::_ -> raise (Syntax_error (pos, (Printf.sprintf "expected token '%s' but got a '%s'" (str t) (str tr))))

  (*f tokenize_error : string -> Token.t list * int *)
  let tokenize_error s e = 
    match e with 
    | Syntax_error (pos,se) -> Some (Printf.sprintf "Syntax error, %s at char %d in %s" se pos s)
    | Unexpected_end_of_string ts -> Some (Printf.sprintf "Unexpected end of string, was expecting a '%s' token" ts)
    | _ -> None

end

(*a Type t and interrogation functions *)
(*t t *)
type t = {
    lvalue : string;
    expression : Expression.elt list;
    mutable in_eval : bool;
    mutable value : Value.t option;
  }

(*f name t -> string *)
let name t = t.lvalue

(*f name_matches t -> string -> bool *)
let name_matches t = String.equal t.lvalue

(*f is_resolved t -> bool *)
let is_resolved a =
  match a.value with
  | Some f -> true
  | None -> false

(*f being_resolved t -> bool *)
let being_resolved a = a.in_eval

(*a Exceptions *)
(*e Recursive_evaluation *)
exception Recursive_evaluation of t
exception Failed_to_evaluate of t * exn
exception Syntax_error = Token.Syntax_error

(*a Building and evaluation *)
(*f build_from_tokens : Token *)
let build_from_tokens (tl:Token.t_with_pos list) =
  let (lvalue, tl) = Token.get_lvalue tl in
  let tl = Token.(skip Token_string.Assign tl) in
  let (expression,tl) = Token.build_expression tl in
  let tl = Token.(skip Semi tl) in
  let t = {lvalue; expression; in_eval=false; value=None;} in
  (t, tl)

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

(*f tokenize_string : string -> Token.t list * int *)
let tokenize_string = Token.tokenize_string

let tokenize_error = Token.tokenize_error


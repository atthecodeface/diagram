(*a Expression and token *)
module Expression = Expression_eval.ExpressionFunc(Token_string)
module Token      = struct
  exception Unexpected_end_of_string
  exception Syntax_error of string
  include Token_func.TokenFunc(Token_string)

  let get_lvalue = function
    | [] -> raise Unexpected_end_of_string
    | (Token_string.Lvalue s)::tl -> (s, tl)
    | _ -> raise (Syntax_error "expected lvalue")

  let build_expression tl =
    let rec add_expression_elt acc = function 
      | (Token_string.Float f)::tl -> add_expression_elt (Expression.prepend_float f acc) tl
      | (Token_string.Op s)::tl    -> add_expression_elt (Expression.prepend_function s acc) tl
      | (Token_string.Ref s)::tl   -> add_expression_elt (Expression.prepend_ref s acc) tl
      | tl -> (acc, tl)
    in
    let (reve,tl) = add_expression_elt [] tl in
    (List.rev reve, tl)

  let skip t = function
    | [] -> raise Unexpected_end_of_string
    | hd::tl when (hd==t) -> tl
    | _ -> raise (Syntax_error (Printf.sprintf "expected token '%s'" (str t)))

end

(*a Toplevel Assignment module *)

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


let tokenize_string = Token.tokenize_string
exception Syntax_error = Token.Syntax_error

(*a Test stuff *)
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
  let (a,tl) = build_from_tokens tl in
  Printf.printf "Now tokens: %s\n" (Token.strl tl);
  let v = value_of (fun _ -> Value.no_value) a in
  let fa = Value.flatten v in
  Array.iteri (Printf.printf "%d %f\n") fa;
  ()


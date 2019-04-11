(*a ExpressionFunc functor module *)
(*m ExpressionRef type *)
module type ExpressionRef = sig
  type t_ref
end

(*m ExpressionFunc (ExpressionRef) *)
module ExpressionFunc (ERef : ExpressionRef) = struct
  (*e Exceptions *)
  exception Function_unknown of string

  (*t elt *)
  type elt = | Float of float
             | Function of Expression_fn.eq_fn
             | Ref of ERef.t_ref

  (*f prepend_float elt list -> float -> elt_list *)
  let prepend_float f e = (Float f) :: e

  (*f prepend_function elt list -> s -> elt_list *)
  let prepend_function s e =
    match Expression_fn.find_fn s with
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


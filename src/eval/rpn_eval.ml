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
    let (tl, n) = Assignment.tokenize_string s in
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


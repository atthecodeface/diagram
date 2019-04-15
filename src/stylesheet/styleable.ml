(*a Libraries *)
open Types
module Value = Value
module Style_ids     = Style_id.Style_ids
module Style_id      = Style_id.Style_id
open Base

(*a Code *)
(*f set_parent*)
let set_parent p t = t.parent <- Some p
                                      
(*f get_id *)
let get_id t = t.id_name

(*f get_type *)
let get_type t = t.type_name

(*f get_nth_sid *)
let get_nth_sid n t = 
  if n<t.num_base_styles then
    Styleable_desc_built.get_nth_sid n t.desc_built
  else
    t.extra_sids.(n-t.num_base_styles)

(*f find_sid_index *)
let find_sid_index sid t =
  match (Styleable_desc_built.find_sid_index sid t.desc_built) with
    Some sid -> Some sid
  | None -> (
    let find_sid acc i =
      let (n, opt_res) = acc in
      if (i=sid) then (n+1, Some n)
      else (n+1, opt_res)
    in
    let (_,opt_index) = Array.fold_left find_sid (0,None) t.extra_sids in
    opt_index
  )

(*f find_sid_index_exn *)
let find_sid_index_exn sid t =
  match find_sid_index sid t with 
    None -> raise (Styleable_desc_built.Style_id_not_found_in_binding (Style_id.str sid))
  | Some index -> index

(*f get_value_ref *)
let get_value_ref t sheet (s:string) =
  let sid = style_id_of_name_exn s sheet in
  let sindex = find_sid_index_exn sid t in
  t.values.(sindex)

(*f get_value *)
let get_value t sheet (s:string) =
  Value_ref.get_value (get_value_ref t sheet s)

(*f re_match_fn *)
let re_match_fn re =
  let re = Re.compile re in
  fun x -> match Re.exec_opt re x with
           | None -> false
           | _ -> true

(*f match_of_string *)
let match_of_string s =
    let n = String.length s in
    if (n==0) then `Equals (String.equal "")
    else (
      let starts_with_star = (String.get s 0)     = '*' in
      let ends_with_star   = (String.get s (n-1)) = '*' in
      match (starts_with_star, ends_with_star) with
      | (false, false) -> `Equals (String.equal s)
      | (true,  false) -> `Matches (re_match_fn Re.(seq [str (String.sub s 1 (n-1)); eos]))
      | (true,  true)  -> `Matches (re_match_fn Re.(seq [str (String.sub s 1 (n-2))]))
      | (false, true)  -> `Matches (re_match_fn Re.(seq [bos; str (String.sub s 0 (n-1))]))
    )

(*f match_element_id *)
let match_element_id f t = f t.id_name

(*f match_element_type *)
let match_element_type f t = f t.type_name 

(*f match_element_class *)
let match_element_class f t = List.exists f t.classes

(*f is_element_state *)
let is_element_state state value t =
  if (state>=(Array.length t.state)) then false
  else (
    (*Printf.printf "Is_Element_State %d %d : %d\n" state value t.state.(state);*)
    t.state.(state) = value
  )

(*f set_element_state *)
let set_element_state state value t =
  if (state>=(Array.length t.state)) then ()
  else (t.state.(state) <- value)

(*f create *)
let create desc sheet type_name name_values style_change_callback children =
  let desc_built = build_desc desc sheet in
  let id_name = 
    if (List.mem_assoc "id" name_values) then (List.assoc "id" name_values) else "no_id"
  in
  let classes = 
    let class_str = if (List.mem_assoc "class" name_values) then (List.assoc "class" name_values) else "" in
    let class_list = String.split_on_char ' ' class_str in
    List.filter (fun x->(x<>"")) class_list
  in
  let count_extra_styles acc nv =
    let (name,_) = nv in
    match style_id_of_name name sheet with
      None -> acc
    | Some sid -> (
      match Styleable_desc_built.find_sid_index sid desc_built with
        None -> (acc+1)
      | Some sid_index -> acc
    )
  in
  let num_extra_styles = List.fold_left count_extra_styles 0 name_values in
  let num_base_styles = (Array.length desc_built.sids) in
  let num_styles = (num_base_styles+num_extra_styles) in
  let t = {
      desc_built;
      num_base_styles;
      num_styles;
      children;
      style_change_callback;
      id_name;
      parent = None;
      type_name;
      classes;
      extra_sids = Array.make num_extra_styles Style_id.dummy;
      state      = Array.make (List.length desc.state_descriptor) 0;
      values     = Array.init num_styles (fun i -> Value_ref.create ());
    }
  in
  add_styleable t sheet;
  let add_extra_style acc nv =
    let (name,_) = nv in
    match style_id_of_name name sheet with
      None -> acc
    | Some sid -> (
      match Styleable_desc_built.find_sid_index sid t.desc_built with
        Some sid_index -> acc
      | None -> (t.extra_sids.(acc) <- sid; acc+1)
    )
  in
  ignore (List.fold_left add_extra_style 0 name_values);
  let set_default_value nv =
    let (name,value) = nv in
    match style_id_of_name name sheet with
      None -> ()
    | Some sid -> (
      match find_sid_index sid t with
        None -> ()
      | Some sid_index -> (
        let stype = Style_id.get_type sid in
        (*Printf.printf "Set default value of %s.%s.%s to be %s\n" t.id_name t.type_name name value;*)
        Value_ref.set_default_from_string t.values.(sid_index) stype value
      )
    )
  in
  List.iter set_default_value name_values;
  let set_inheritance n vr =
    let sid = get_nth_sid n t in
    let di = (is_default_inherit sid sheet) in
    Value_ref.set_default_inherit vr di
  in
  Array.iteri set_inheritance t.values;
  t

(*f reset_next_values *)
let rec reset_next_values t =
  Array.iter Value_ref.reset t.values;
  List.iter reset_next_values t.children

(*f apply_styles *)
let apply_styles l styles t = 
  let apply_style sid_sv =
    let (sid,sv) = sid_sv in
    match find_sid_index sid t with
      None -> ()
    | Some sindex ->
       Value_ref.apply l sv t.values.(sindex) 
  in
  List.iter apply_style styles

(*f resolve_next_value *)
let rec resolve_next_value sid t sheet = 
  let value_ref =
    match find_sid_index sid t with
      None -> (
      if (is_default_inherit sid sheet) then
        Value_ref.Inherit
      else
        Value_ref.Default
    )
    | Some sindex ->
       (
         Value_ref.next_value_ref t.values.(sindex)
       )
  in
  match value_ref with
    Value_ref.Ref v -> v
  | Value_ref.Default -> get_default_value sid sheet
  | Value_ref.Inherit ->
     (
       match t.parent with
         Some p -> resolve_next_value sid p sheet
       | None -> get_default_value sid sheet
     )

(*f update_current_values_from_next *)
let rec update_current_values_from_next sheet t =
  let update_nth_value n acc =
    let sid = get_nth_sid n t in
    let value = resolve_next_value sid t sheet in
    if (value == Value_ref.get_value t.values.(n)) then acc
    else (
      (*Printf.printf "update_nth_value %s : %s : %s \n%!" t.id_name (Style_id.str sid) (str_of_svalue value);*)
      Value_ref.set_value t.values.(n) value;
      (sid,value)::acc
    )
  in
  let rec update_values l n acc = 
    if (n>=l) then
      acc
    else
      let next_acc = update_nth_value n acc in
      update_values l (n+1) next_acc
  in
  let changed_sids = update_values t.num_styles 0 [] in
  if changed_sids <> [] then
    (
      (*Printf.printf "Sids changed for this %s.%s\n" t.id_name t.type_name;*)
      t.style_change_callback changed_sids
    );
  List.iter (update_current_values_from_next sheet) t.children

(*f element_callback_matching_children *)
let rec element_callback_matching_children selector e t callback = 
  List.iter (fun e -> if (selector e) then callback e) e.children

(*f element_callback_all_children *)
let element_callback_all_children _ e t callback = 
  element_callback_matching_children (fun e -> true) e t callback

(*f element_callback_matching_subelements *)
let rec element_callback_matching_subelements (subtree,selector) e t callback = 
  element_callback_matching_children selector e t callback;
  if subtree then (
    element_callback_all_children () e t (fun e -> element_callback_matching_subelements (true,selector) e t callback)
  );
  ()

(*f element_callback_all_subelements *)
let element_callback_all_subelements _ e t callback = 
  element_callback_matching_children (fun e -> true) e t callback

(*f element_callback_matching_tree : style_selector -> t -> 'a -> (styleable->unit) -> unit *)
let element_callback_matching_tree (subtree,element_style_selector) e t callback = 
  if (element_style_selector e) then (callback e);
  element_callback_matching_subelements (true,element_style_selector) e t callback

(*f All done *)

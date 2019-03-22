(*a Libraries *)
open Types
module Value = Value
module Style_ids     = Style_id.Style_ids
module Style_id      = Style_id.Style_id

(*m Styleable_desc - immutable *)
module Styleable_desc = struct
  (*t Structure *)

  (*f create *)
  let create state_descriptor styles =
    {
      state_descriptor;
      styles;
    }

(*f All done *)
end

(*m Styleable_desc_built -  immutable *)
module Styleable_desc_built = struct

  (*f get_nth_sid *)
  let get_nth_sid n t = t.sids.(n)

  (*f create *)
  exception Style_id_not_found_in_binding of string
  exception Style_type_mismatch of string
  let create desc ids =
    let fn acc n_t =
      let (name,stype,sv,i) = n_t in
      let opt_id = (Style_ids.find_opt_id (Style_id.hash_of_string name) ids) in
      match opt_id with
        None -> raise (Style_id_not_found_in_binding name)
      | Some sid -> 
         if (sid.stype != stype) then raise (Style_type_mismatch name)
         else
           sid :: acc
    in
    let t = {
        desc;
        sids = Array.of_list (List.fold_left fn [] desc.styles);
      }
    in
    (*
        Printf.printf "Bind to ids\n";
        Array.iter (fun sid -> Printf.printf "%s\n" (Style_id.str sid)) t.sids;
     *)
    t

  (*f find_sid_index  *)
  let find_sid_index sid t =
    let find_sid acc i =
      let (n, opt_res) = acc in
      if (i=sid) then (n+1, Some n)
      else (n+1, opt_res)
    in
    let (_,opt_index) = Array.fold_left find_sid (0,None) t.sids in
    opt_index

  (*f find_sid_index_exn  *)
  let find_sid_index_exn sid t =
    match find_sid_index sid t with
      None -> raise (Style_id_not_found_in_binding (Style_id.str sid))
    | Some index -> index

(*f All done *)
end

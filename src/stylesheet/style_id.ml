(** Copyright (C) 2017-2018,  Gavin J Stark.  All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * @file    style.ml
 * @brief   A library for having cascading stylesheets
 *
 *)

open Types

(*a Style_id_hash, Style_id, Style_ids, Style, Styleable_desc - immutable after construction, but could be fully immutable *)
(*m Style_id - immutable *)
module Style_id = struct
  let hash_of_string s = s
  let dummy : t_style_id = {name=""; hash=(hash_of_string ""); stype=St_int; }
  let create name stype : t_style_id = { name; hash=(hash_of_string name); stype; }
  let get_type t = t.stype
  let str t =
    Printf.sprintf "sid %s" t.name
end

(*m Style_ids - immutable, but contains a hash table of Style_id_hash.t -> Style_id.t *)
module Style_ids = struct
  exception Unknown_id of string
  exception Duplicate_id of string
  let create _ =
    {
      set = Hashtbl.create 1024;
    }
  let find_opt_id hash t =
    if (Hashtbl.mem t.set hash) then
      Some (Hashtbl.find t.set hash)
    else
      None

  let find_id_exn s t =
    let hash = Style_id.hash_of_string s in
    match find_opt_id hash t with
      None -> raise (Unknown_id (Printf.sprintf "could not find style name '%s'" s))
    | Some sid -> sid

  let add_id hash sid t =
    if (Hashtbl.mem t.set hash) then
      false
    else
      (Hashtbl.replace t.set hash sid; true)

  let add_id_exn s sid t =
    let hash = Style_id.hash_of_string s in
    match add_id hash sid t with
    | false -> raise (Duplicate_id (Printf.sprintf "style '%s' already registered" s))
    | _ -> ()

  let build_id_value_list nvs t = 
    let rec add_id_value acc (name,x) = 
      let hash = Style_id.hash_of_string name in
      let opt_sid = find_opt_id hash t in
      match opt_sid with
        None -> raise (Unknown_id name)
      | Some sid -> (sid,x)::acc
    in
    List.fold_left add_id_value [] nvs
end

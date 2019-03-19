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
  exception Duplicate_id
  let create _ =
    {
      set = Hashtbl.create 1024;
    }
  let find_opt_id hash t =
    if (Hashtbl.mem t.set hash) then
      Some (Hashtbl.find t.set hash)
    else
      None
  let find_id_exn hash t =
    match find_opt_id hash t with
      None -> raise (Unknown_id "hashed thing")
    | Some sid -> sid
  let add_id hash sid t =
    if (Hashtbl.mem t.set hash) then
      raise Duplicate_id
    else
      Hashtbl.replace t.set hash sid
  let build_id_value_list nvs t = 
    let rec add_id_value acc n_x = 
      let (name,x)=n_x in
      let hash = Style_id.hash_of_string name in
      let opt_sid = find_opt_id hash t in
      match opt_sid with
        None -> raise (Unknown_id name)
      | Some sid -> (sid,x)::acc
    in
    List.fold_left add_id_value [] nvs
end

(*m Style - mutable during construction of default - should probably be immutable *)
module Style = struct
  let create styles = { styles; }
  let add_styling sid value opt (t:t_style) =
    t.styles <- (sid,(value,opt))::t.styles
  let str (t:t_style) =
    let str_svo acc svo =
      let (sid,(svalue,opt)) = svo in
      Printf.sprintf "%s%s:%s:%b\n" acc (Style_id.str sid) (Value.str_of_svalue svalue) opt
    in
    List.fold_left str_svo "style:\n" t.styles
  let get_value sid (t:t_style) =
    fst (List.assoc sid t.styles)
  let get_opt sid (t:t_style) =
    snd (List.assoc sid t.styles)
end


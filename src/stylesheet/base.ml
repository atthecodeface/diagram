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
 * @file    base.ml
 * @brief   Stylesheet operations that styleables and so on depend on
 *
 *)

(*a Libraries *)
module Value = Value
module Style_ids     = Style_id.Style_ids
module Style_id      = Style_id.Style_id
module Styleable_desc_built  = Styleable_desc.Styleable_desc_built
open Types

(*f get_default_value *)
let get_default_value sid t =
  Style.get_value sid t.default_style

(*f is_default_inherit *)
let is_default_inherit sid t =
  Style.get_opt sid t.default_style

(*f style_id_of_name *)
let style_id_of_name name t =
  let hash = Style_id.hash_of_string name in
  Style_ids.find_opt_id hash t.ids

(*f style_id_of_name_exn *)
let style_id_of_name_exn name t =
  let hash = Style_id.hash_of_string name in
  Style_ids.find_id_exn hash t.ids

(*f add_styleable *)
let add_styleable s t =
  t.entity_list <- s :: t.entity_list

(*f build_desc *)
let build_desc desc t =
  if (not (List.mem_assoc desc t.built_descs)) then
    (t.built_descs <- (desc,Styleable_desc_built.create desc t.ids)::t.built_descs);
  List.assoc desc t.built_descs


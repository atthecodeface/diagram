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
 * @file    stylesheet.ml
 * @brief   A library for having cascading stylesheets
 *
 *)

(*a Notes
CSS notes

font
-family - comma separated list of font names
-size - size or percentage of inheritance
-weight
-stretch
[-thickness?]
line-height

vertical-align

max-width

margin 0 auto !important

padding (top, bottom, left, right)

border (top, bottom, left, right)
border-color
border-style
border-width

box-sizing

display


background

color


Module hierarchy

value

value_ref

types

style_id (needs types but no other code)
style_rule (needs types but no other code)

style    (needs style_id)
styleable_desc (needs Style_id/Style_ids)

base (needs Styleable_desc_built/Style_id/Style_ids)

styleable (needs Styleable_desc*, values, base stylesheet interrogation)

stylesheet (needs full styleable, style_rule)

*)

(*a Libraries *)
module Value = Value
module Style_ids     = Style_id.Style_ids
module Style_id      = Style_id.Style_id
module Styleable_desc_built  = Styleable_desc.Styleable_desc_built
module Styleable_desc        = Styleable_desc.Styleable_desc
open Types
open Base

(*a Code *)
(*f create *)
let create () = {
    entity_list = [];
    roots = [];
    ids = Style_ids.create ();
    default_style = Style.create [];
    rules = [];
    built_descs = [];
  }

(*f build *)
let build t roots =
  t.roots <- roots;
  t

(*f add_style_defaults *)
let add_style_defaults t ntvis =
  let add_id_and_style n_t_v_i = 
    let (name,stype,svalue,def_inherit)=n_t_v_i in
    let sid = Style_id.create name stype in
    if (Style_ids.add_id (Style_id.hash_of_string name) sid t.ids) then (
      Style.add_styling sid svalue def_inherit t.default_style
    ) else ()
  in
  List.iter add_id_and_style ntvis;
  (*Printf.printf "Default %s\n" (Style.str t.default_style);*)
  ()

(*f add_style_rule *)
let add_style_rule t selectors style_nvs =
  let id_vs = Style_ids.build_id_value_list style_nvs t.ids in
  let rule = Style_rule.create selectors id_vs in
  t.rules <- rule :: t.rules

(*f element_callback_matching_tree *)
let element_callback_matching_tree t selector callback =
  List.iter (fun e -> Styleable.element_callback_matching_tree selector e t callback) t.roots

(*f apply *)
let apply t =
  (* clear new values, setting all to inherit, and clear longest matching rule
   *)
  List.iter (fun e -> Styleable.reset_next_values e) t.roots;

  (* Apply rules *)
  List.iter (fun r -> Style_rule.apply r t element_callback_matching_tree) t.rules;

  (* resolve next values to current values, determining which ones have changed,
    and callback if any have for each styleable
   *)
  List.iter (fun e -> Styleable.update_current_values_from_next t e) t.roots;
  ()

(*f All done *)

type t_styleable_desc = Types.t_styleable_desc
type t_styleable      = Types.t_styleable
type t_stylesheet     = Types.t_stylesheet
let create_desc         = Styleable_desc.create

let se_create              = Styleable.create
let se_get_value_ref       = Styleable.get_value_ref
let se_get_value           = Styleable.get_value
let se_set_element_state   = Styleable.set_element_state
let se_set_parent          = Styleable.set_parent
let se_is_element_id       = Styleable.is_element_id
let se_is_element_state    = Styleable.is_element_state
let se_is_element_type     = Styleable.is_element_type
let se_has_element_class   = Styleable.has_element_class

let styleable_value_is_none stylesheet styleable name = Value_ref.value_is_none (Styleable.get_value_ref styleable stylesheet name)
let styleable_value_as_color_string ?default stylesheet styleable name = Value_ref.get_value_as_color_string ?default:default (Styleable.get_value_ref styleable stylesheet name)
let styleable_value_as_floats ?default stylesheet styleable name = Value_ref.get_value_as_floats ?default:default (Styleable.get_value_ref styleable stylesheet name)
let styleable_value_as_float ?default stylesheet styleable name = Value_ref.get_value_as_float ?default:default (Styleable.get_value_ref styleable stylesheet name)
let styleable_value_as_ints ?default stylesheet styleable name = Value_ref.get_value_as_ints ?default:default (Styleable.get_value_ref styleable stylesheet name)



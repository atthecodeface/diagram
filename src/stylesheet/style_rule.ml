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

(*a Libraries *)
module Value = Value
open Types

(*a Code *)
(*f create *)
let create selectors styles =
  {
    selectors;
    styles;
  }

(*f apply : t -> 'a -> ('a->style_selector->(styleable->unit)->'b)->'b *)
let apply t stylesheet element_callback_matching_tree =
  let apply_style e =
    Styleable.apply_styles (List.length t.selectors) t.styles e
  in
  let rec sel_cbk_for_remaining_rules rules =
    match rules with
    | hd::nxt::tail -> 
       let (sel,cbk) = sel_cbk_for_remaining_rules (nxt::tail) in
       (hd,  fun e -> Styleable.element_callback_matching_subelements sel e stylesheet cbk)
    | hd::tail -> (hd,apply_style)
    | [] -> ((true,fun e -> true),apply_style)
  in
  let (sel,cbk) = sel_cbk_for_remaining_rules t.selectors in
  element_callback_matching_tree stylesheet sel cbk

(*f All done *)

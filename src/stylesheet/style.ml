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

module Style_id      = Style_id.Style_id
let create styles = { styles; }
let add_styling sid value opt (t:t_style) =
  t.styles <- (sid,(value,opt))::t.styles
let str (t:t_style) =
  let str_svo acc svo =
    let (sid,(svalue,opt)) = svo in
    Printf.sprintf "%s%s:%s:%b\n" acc (Style_id.str sid) (Value.str svalue) opt
  in
  List.fold_left str_svo "style:\n" t.styles
let get_value sid (t:t_style) =
  fst (List.assoc sid t.styles)
let get_opt sid (t:t_style) =
  snd (List.assoc sid t.styles)


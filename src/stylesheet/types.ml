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
 * @file    types.ml
 * @brief   Types used throughout the stylesheet
 *
 *)

(*a Inherit some from Value *)
type t_styleable_value     = Value.t_styleable_value
type t_styleable_type      = Value.t_styleable_type
type t_styleable_name      = Value.t_styleable_name

(*a Style id types *)
type t_style_id_hash = string

type t_style_id = {
    hash : t_style_id_hash;
    name : string;
    stype : t_styleable_type;
  }
type t_style_ids = {
    set : ( t_style_id_hash , t_style_id) Hashtbl.t;
  }

(*a Style and style change callback *)
type t_style_change_callback = (t_style_id * t_styleable_value) list -> unit

type t_style = {
    mutable styles : (t_style_id * (t_styleable_value * bool)) list
}

(*a Styleable descriptor types *)
type t_styleable_desc = {
    state_descriptor : ( string * ((string * int) list)) list; (* each entry is a state_class -> (list of state_name of state_class->int) mappings *)
    styles : (string * t_styleable_type * t_styleable_value * bool) list; (* List of all stylenames the styleable cares about; usually a static list *)
  }

type t_styleable_desc_built = {
    desc : t_styleable_desc;
    sids : t_style_id array; (* Array of all style_ids the styleable cares about; created after the stylesheet is set up (effectively binds the desc to the stylesheet) *)
  }

type t_styleable= {
    desc_built            : t_styleable_desc_built;
    num_base_styles       : int;
    num_styles            : int;
    extra_sids            : t_style_id array;
    children              : t_styleable list;
    mutable parent        : t_styleable option;
    state                 : int array;
    style_change_callback : t_style_change_callback;
    id_name               : string;
    type_name             : string;
    classes               : string list;
    values                : Value_ref.t array; (* next_values.(i) corresponds to desc.sids.(i) *)
  }

(*a Selectors and rules *)
type t_style_selector = bool * (t_styleable -> bool)

type t_style_match = [ | `Equals of (string->bool) | `Matches of (string->bool) ]

type t_style_change_fn = t_styleable -> unit (* internal apply style rule *)

type t_style_rule = {
    selectors             : t_style_selector list;
    styles                : (t_style_id * t_styleable_value) list;
  }

(*a Stylesheet *)
type t_stylesheet = {
    mutable entity_list   : t_styleable list;
    mutable roots         : t_styleable list;
    ids                   : t_style_ids;
    default_style         : t_style;
    mutable rules         : t_style_rule list;
    mutable built_descs   : (t_styleable_desc * t_styleable_desc_built) list;
  }

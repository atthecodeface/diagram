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
 * @file    value_ref.ml
 * @brief   Styleable value references
 *
 *)

(*a Top level exports *)
(*m Styleable_value_ref *)
  type t_styleable_value_ref = 
    Ref of Value.t_styleable_value
  | Default
  | Inherit

  (*t t - structure for a reference to a t_styleable_value *)
  type t = { 
      mutable longest_rule   : int;
      mutable next_value_ref : t_styleable_value_ref;
      mutable value          : Value.t_styleable_value;
      mutable default_value  : Value.t_styleable_value;
      mutable default_inherit : bool;
    }

  (*f create - create a null value reference *)
  let create _ = {
      longest_rule    = 0;
      next_value_ref  = Default;
      value           = Value.sv_zero;
      default_value   = Value.sv_none_floats;
      default_inherit = false;
    }

  (*f get_value t - get the value *)
  let get_value t = t.value

  (*f next_value_ref *)
  let next_value_ref t = 
    t.next_value_ref 

  (*f value_is_none t - determine if value is none *)
  let value_is_none t =  Value.is_none t.value

  (*f value_is_some t - determine if value is not none *)
  let value_is_some t =  Value.is_some t.value

  (*f get_value_as_color_string t - get the value as an array of floats *)
  let get_value_as_color_string ?default t =  Value.as_color_string ?default:default t.value

  (*f get_value_as_floats t - get the value as an array of floats *)
  let get_value_as_floats ?default t =  Value.as_floats ?default:default t.value

  (*f get_value_as_float t - get the value as a float *)
  let get_value_as_float ?default t  =  Value.as_float ?default:default t.value

  (*f get_value_as_ints t - get the value as an array of ints *)
  let get_value_as_ints ?default t   =  Value.as_ints ?default:default t.value

  (*f get_value_as_string t - get the value as a string *)
  let get_value_as_string ?default t   =  Value.as_string ?default:default t.value

  (*f set_default_inherit - set the default inheritance *)
  let set_default_inherit t di = t.default_inherit <- di

  (*f set_value *)
  let set_value t v = t.value <- v

  (*f set_default_from_string *)
  let set_default_from_string t stype value =
    t.default_value <- Value.from_string stype value

  (*f reset *)
  let reset t =     
    t.longest_rule <- 0;
    if (Value.is_none t.default_value) then
      (
        if (t.default_inherit) then 
          t.next_value_ref <- Inherit
        else
          t.next_value_ref <- Default
      )
    else
      (
        t.longest_rule <- 100;
        t.next_value_ref <- Ref t.default_value
      )

  (*f apply - apply a rule *)
  let apply l sv t  =
    if (t.longest_rule < l) then (
      t.next_value_ref <- Ref sv;
      t.longest_rule <- l
    )

(*v svr_zero - default null value reference *)
let svr_zero = create ()
                                          


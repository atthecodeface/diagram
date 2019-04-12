(** Copyright (C) 2018,  Gavin J Stark.  All rights reserved.
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
 * @file     properites.ml
 * @brief    Properties for SVG elements
 *
 *)
open Primitives
open Types

exception Invalid_property of string
type t_ = | Padding_
          | Border_
          | BorderColor_
          | BorderStroke_
          | FillColor_
          | Margin_
          | Place_
          | Anchor_
          | Grid_

type t = | Padding      of t_rect
         | Border       of t_rect
         | FillColor    of Color.t
         | BorderColor  of Color.t
         | BorderStroke of Stroke.t
         | Margin       of t_rect
         | Place        of t_vector option
         | Anchor       of t_vector
         | Grid         of t_int4 option

let property_num t = 
  match t with 
  | Padding _      -> Padding_
  | Border _       -> Border_
  | BorderColor _  -> BorderColor_
  | FillColor _    -> FillColor_
  | BorderStroke _ -> BorderStroke_
  | Margin _       -> Margin_
  | Place _        -> Place_
  | Anchor _       -> Anchor_
  | Grid _         -> Grid_

let is_property t pn = (property_num t)==pn

let replace_if_is_property t pn default : t =
  if (is_property t pn) then t else default

let replace_if_is_property_vector t pn default =
  match t with 
  | Place v        -> if (pn==Place_)       then v else default
  | _ -> default

let replace_if_is_property_rect t pn default =
  match t with 
  | Padding v      -> if (pn==Padding_)      then v else default
  | Border v       -> if (pn==Border_)       then v else default
  | Margin v       -> if (pn==Margin_)       then v else default
  | _ -> default

let replace_if_is_property_int4 t pn default =
  match t with 
  | Grid v         -> if (pn==Grid_)       then v else default
  | _ -> default

let replace_if_is_property_color t pn default =
  match t with 
  | FillColor  v   -> if (pn==FillColor_)   then v else default
  | BorderColor v  -> if (pn==BorderColor_) then v else default
  | _ -> default

let get_property tl pn default : t = 
  List.fold_left (fun acc t -> replace_if_is_property t pn acc) default tl

let get_property_vector stylesheet styleable name =
    let f = Stylesheet.styleable_value_as_floats stylesheet styleable name in
    (f.(0), f.(1))

let get_property_vector_option stylesheet styleable name =
  if Stylesheet.styleable_value_is_none stylesheet styleable name then None else (
    let f = Stylesheet.styleable_value_as_floats stylesheet styleable name in
    Some (f.(0), f.(1))
  )

let get_property_rect stylesheet styleable name =
    let f = Stylesheet.styleable_value_as_floats stylesheet styleable name in
    (f.(0), f.(1), f.(2), f.(3))

let get_property_int4 stylesheet styleable name =
    let f = Stylesheet.styleable_value_as_ints stylesheet styleable name in
    (f.(0), f.(1), f.(2), f.(3))

let get_property_int4_option stylesheet styleable name =
  if Stylesheet.styleable_value_is_none stylesheet styleable name then None else (
    let f = Stylesheet.styleable_value_as_ints stylesheet styleable name in
    Some (f.(0), f.(1), f.(2), f.(3))
  )

let get_property_color stylesheet styleable name =
  if Stylesheet.styleable_value_is_none stylesheet styleable name then Color.None else (
    let f = Stylesheet.styleable_value_as_floats stylesheet styleable name in
    Color.Rgb (f.(0), f.(1), f.(2))
  )


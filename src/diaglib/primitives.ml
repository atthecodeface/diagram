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
 * @file     diaglib.ml
 * @brief    Diagrams for SVG initially
 *
 *)
module Color = struct
  type t = | None
           | Rgb of (float * float * float)
           | Color of string
  let black = Rgb (0.,0.,0.)
  let white = Rgb (1.,1.,1.)
  let svg_attr t tag_name =
    match t with 
    | None    -> Svg.attribute_string tag_name "none"
    | Color s -> Svg.attribute_string tag_name s
    | Rgb (r,g,b) -> Svg.attribute_string tag_name "some_rgb"

end

module Stroke = struct
  type t = {
    color : Color.t;
      width: float;
(* dash-array string number list *)
(* line-cap = butt/round/square *)
    }
  let none = { color=Color.None; width=0.}
  let black width = { color=Color.black; width}

end

exception Invalid_value of string
type t_id = int
type t_hdr = {
    int_id : t_id;
    mutable parent : t_id;
    id : string;
  }
type t_int4 = int * int * int * int
type t_rect = float * float * float * float
type t_vector = float * float
type t_value = | Scalar of float
               | Rect of t_rect
               | Int4 of t_int4
               | Vector of t_vector
               | String of string

type t_expr_resolver = t_hdr -> t_value

let th_svg_attr th al = (Svg.attribute_string "id" th.id) :: al
let th_make id =
  {int_id=0; parent=0; id;}
let value_rect = function | Rect r -> r | _ -> raise (Invalid_value "Expected rect")
let value_int4 = function | Int4 r -> r | _ -> raise (Invalid_value "Expected int4")

module Rectangle = struct
  type t = Uniform of float 
               | Fixed of t_rect
               | Func of (t_expr_resolver -> t_value)
    let zeros = (0., 0., 0., 0.)
    let add_values a b =
      let (a0,a1,a2,a3) = a in
      let (b0,b1,b2,b3) = b in
      (a0+.b0, a1+.b1, a2+.b2, a3+.b3)
    let expand a b =
      let (a0,a1,a2,a3) = a in
      let (b0,b1,b2,b3) = b in
      (a0-.b0, a1-.b1, a2+.b2, a3+.b3)
    let mk_fixed r = Fixed r
    let value x n =
      match x with
      | Uniform f -> (f,f,f,f)
      | Fixed r   -> r
      | Func f -> value_rect (f n)
    let get_wh bbox_r =
      let (d0,d1,d2,d3) = bbox_r in
      (d2-.d0, d3-.d1)
    let get_width bbox_r =
      let (d0,d1,d2,d3) = bbox_r in
      d2-.d0
    let get_height bbox_r =
      let (d0,d1,d2,d3) = bbox_r in
      d3-.d1
    
end


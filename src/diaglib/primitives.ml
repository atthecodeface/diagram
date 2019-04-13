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
  let red   = Rgb (1.,0.,0.)
  let green = Rgb (0.,1.,0.)
  let blue  = Rgb (0.,0.,1.)
  let white = Rgb (1.,1.,1.)
  let is_none =  function | None -> true |_ -> false
  let svg_attr tag_name t =
    match t with 
    | None    -> Svg.attribute_string tag_name "none"
    | Color s -> Svg.attribute_string tag_name s
    | Rgb (r,g,b) -> Svg.attribute_string tag_name (Printf.sprintf "rgb(%.0f,%.0f,%.0f)" (r*.255.) (g*.255.) (b*.255.))

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

open Types

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
    let as_floats (a,b,c,d) = [|a;b;c;d;|]
    let add_values a b =
      let (a0,a1,a2,a3) = a in
      let (b0,b1,b2,b3) = b in
      (a0+.b0, a1+.b1, a2+.b2, a3+.b3)
    let expand ?scale:(scale=1.0) a b =
      let (a0,a1,a2,a3) = a in
      let (b0,b1,b2,b3) = b in
      (a0-.scale*.b0, a1-.scale*.b1, a2+.scale*.b2, a3+.scale*.b3)
    let shrink ?scale:(scale=1.0) a b = expand ~scale:(-. scale) a b
    let union a b =
      let (a0,a1,(a2:float),a3) = a in
      let (b0,b1,(b2:float),b3) = b in
      ((min a0 b0), (min a1 b1), (max a2 b2), (max a3 b3))
    let intersect a b =
      let (a0,a1,a2,a3) = a in
      let (b0,b1,b2,b3) = b in
      let (r0,r1,r2,r3) = (max a0 b0, max a1 b1, min a2 b2, min a3 b3) in
      if ((r2<=r0) || (r3<=r1)) then zeros else (r0,r1,r2,r3)
    let mk_fixed r = Fixed r
    let value x n =
      match x with
      | Uniform f -> (f,f,f,f)
      | Fixed r   -> r
      | Func f -> value_rect (f n)

    let get_wh (d0,d1,d2,d3) =
      (d2-.d0, d3-.d1)

    let get_width (d0,_,d2,_) =
      d2-.d0

    let get_height (_,d1,_,d3) =
      d3-.d1

    let get_cwh (d0,d1,d2,d3) =
      ((d0+.d2)/.2., (d1+.d3)/.2., d2-.d0, d3-.d1)

    let of_cwh (x,y,w,h) =
      (x-.w/.2., y-.h/.2., x+.w/.2., y+.h/.2.)
   
    let str (a0,a1,a2,a3) = Printf.sprintf "(%f,%f,%f,%f)" a0 a1 a2 a3
end


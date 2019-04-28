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

(*m Rectangle module *)
module Rectangle = struct
  (*v zeros *)
  let zeros = [|0.; 0.; 0.; 0.|]

  (*f make a b c d *)
  let make a b c d  = [|a; b; c; d; |]
            
  (*f is_zero *)
  let is_zero r = r==zeros

  (*f as_floats *)
  let as_floats x = x

  (*f as_vectors *)
  let as_vectors ?close:(close=false) r =
    if close then 
      [| [|r.(0); r.(1);|];
         [|r.(2); r.(1);|];
         [|r.(2); r.(3);|];
         [|r.(0); r.(3);|];
         [|r.(0); r.(1);|];
      |]
    else
      [| [|r.(0); r.(1);|];
         [|r.(2); r.(1);|];
         [|r.(2); r.(3);|];
         [|r.(0); r.(3);|];
      |]

  (*f add_values *)
  let add_values a b = Array.map2 ( +. ) a b

  (*f expand *)
  let expand ?scale:(scale=1.0) a b =
    [| a.(0)-.scale*.b.(0);
       a.(1)-.scale*.b.(1);
       a.(2)+.scale*.b.(2);
       a.(3)+.scale*.b.(3) |]

  (*f shrink *)
  let shrink ?scale:(scale=1.0) a b = expand ~scale:(-. scale) a b

  (*f union *)
  let union a b =
    if (is_zero a) then b else if (is_zero b) then a else (
      [| (min a.(0) b.(0));
         (min a.(1) b.(1));
         (max a.(2) b.(2));
         (max a.(3) b.(3)) |]
    )
  (*f intersect *)
  let intersect a b =
    if (is_zero a) then a else if (is_zero b) then b else (
      let r0 = max a.(0) b.(0) in
      let r1 = max a.(1) b.(1) in
      let r2 = min a.(2) b.(2) in
      let r3 = min a.(3) b.(3) in
      if ((r2<=r0) || (r3<=r1)) then zeros else [|r0;r1;r2;r3|]
    )
  (*f translate *)
  let translate ?scale:(scale=1.) r v =
    [| r.(0) +. v.(0)*.scale;
       r.(1) +. v.(1)*.scale;
       r.(2) +. v.(0)*.scale;
       r.(3) +. v.(1)*.scale |]

  (*f get_wh *)
  let get_wh r = (r.(2)-.r.(0), r.(3)-.r.(1))

  (*f get_c *)
  let get_c r =
    ( (r.(0)+.r.(2))/.2.,
      (r.(1)+.r.(3))/.2. )

  (*f get_dim *)
  let get_dim r = function
    | 0 -> [|r.(0); r.(2)|]
    | _ -> [|r.(1); r.(3)|]

  (*f get_width *)
  let get_width r = r.(2) -. r.(0)

  (*f get_height *)
  let get_height r = r.(3) -. r.(1)

  (*f get_cwh *)
  let get_cwh r =
    ( (r.(0)+.r.(2))/.2.,
      (r.(1)+.r.(3))/.2.,
      r.(2)-.r.(0),
      r.(3)-.r.(1))

  (*f of_cwh *)
  let of_cwh (x,y,w,h) =
    [| x -. w/.2.;
       y -. h/.2.;
       x +. w/.2.;
       y +. h/.2. |]

  (*f str *)
  let str r = Printf.sprintf "(%g,%g,%g,%g)" r.(0) r.(1) r.(2) r.(3)

  (*f All done *)
end

(*m Vector module *)
module Vector = struct
  (*v zeros *)
  let zeros = [|0.; 0.|]

  (*f is_zero *)
  let is_zero v = v==zeros

  (*f make a b  *)
  let make a b  = [|a; b; |]
            
  (*f as_floats *)
  let as_floats x = x

  (*f union *)
  let union a b =
    if (is_zero a) then b else if (is_zero b) then a else (
      [| (min a.(0) b.(0));
         (max a.(1) b.(1)) |]
    )
  (*f intersect *)
  let intersect a b =
    if (is_zero a) then a else if (is_zero b) then b else (
      let r0 = max a.(0) b.(0) in
      let r1 = min a.(1) b.(1) in
      if (r1<=r0) then zeros else [|r0;r1|]
    )

  (*f add *)
  let add ?scale:(scale=1.) a b =
    [| a.(0) +. b.(0)*.scale;
       a.(1) +. b.(1)*.scale |]

  (*f len *)
  let len ?scale:(scale=1.) a =
    (a.(1) -. a.(0)) *. scale

  (*f str *)
  let str r = Printf.sprintf "(%g,%g)" r.(0) r.(1)

  (*f All done *)
end


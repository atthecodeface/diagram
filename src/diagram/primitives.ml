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

let deg_to_rad a = a *. (3.1415926538/.180.0)

(*m Rectangle module *)
module Rectangle = struct
  (*v zeros *)
  let zeros = [|0.; 0.; 0.; 0.|]

  (*f make a b c d *)
  let make a b c d  =
    let (a,c) = if (a<c) then (a,c) else (c,a) in
    let (b,d) = if (b<d) then (b,d) else (d,b) in
    [|a; b; c; d; |]
            
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
    make (a.(0)-.scale*.b.(0))
      (a.(1)-.scale*.b.(1))
      (a.(2)+.scale*.b.(2))
      (a.(3)+.scale*.b.(3))

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
    make (r.(0) +. v.(0)*.scale) 
      (r.(1) +. v.(1)*.scale)
      (r.(2) +. v.(0)*.scale)
      (r.(3) +. v.(1)*.scale)

  (*f rotate_around *)
  let rotate_around r v a =
    let sinr = sin (deg_to_rad a) in
    let cosr = cos (deg_to_rad a) in
    let rotate x y =
      ( ((x-.v.(0)) *. cosr -. (y-.v.(1)) *. sinr),
        ((x-.v.(0)) *. sinr +. (y-.v.(1)) *. cosr) )
    in      
    let x0,y0 = rotate r.(0) r.(1) in
    let x1,y1 = rotate r.(0) r.(3) in
    let x2,y2 = rotate r.(2) r.(1) in
    let x3,y3 = rotate r.(2) r.(3) in
    let lx = v.(0) +. (min (min x0 x1) (min x2 x3)) in
    let rx = v.(0) +. (max (max x0 x1) (max x2 x3)) in
    let ly = v.(1) +. (min (min y0 y1) (min y2 y3)) in
    let ry = v.(1) +. (max (max y0 y1) (max y2 y3)) in
    make lx ly rx ry

  (*f wh_of_largest_area_within w h a
    Find the width and height of the largest rectangle
    that fits within w h at angle a
    If a<0 then we can mirror vertically and use -a, hence abs(a)
    Then modulo 180

    If a>=90 then we can consider a-90 and swap w and h.
    Then, if w>h we can consider 90-a and swap w and h.

    Hence only consider a<90 (hence tan(a)>=0) and w<=h

    Note that the area of a RH triangle with angle a and
    adjacent length l is 1/2.l.l.tan(a) = l^2.t/2
    Assume the largest rectangle leaves rectangular spaces
    above and below; with coordinates of (xw,0), (w,(y'-y)h),
    (w-xw,y'h), (0,yh)
    This assumes largest rectangle is limited by width w, and y'<1.
    We know that tan(a) = xw/yh; i.e.
     yh=xw/t.
    And tan(a)=(y'-y)h/w(1-x); i.e.
     wt(1-x) = y'h-yh
     y'h     = wt(1-x) + xw/t = wt(1+x/(t^2)-x)

    Then the 'wasted space' is then two triangles of size xw : yh and
    two triangles of size w(1-x) : (y'-y)h, and the rectangle of size w : (1-y')h.

    The total is the sum of:
      xw.yh = x^2.w^2/t
      w(1-x).(y'-y)h = w^2.(1-x)^2.t = w^2.t.(1+x^2-2x) = w^2.t + x^2.w^2.t -2x.w^2.t
      wh-wy'h = wh - w^2.t(1+x/(t^2)-x) = wh -w^2.t -x.w^2/t + x.w^2.t

    Sum = x^2.w^2/t + w^2.t + x^2.w^2.t -2x.w^2.t + wh -w^2.t -x.w^2/t + x.w^2.t
        = x^2.w^2/t + x^2.w^2.t -x.w^2.t -x.w^2/t + wh

    This has a minimum (wasted) area when its derivative is 0 (since it has +ve x^2)

    dA/dx = 2x.w^2/t + 2x.w^2.t -w^2.t -w^2/t
          = (2x-1).w^2.(1/t+t)

    i.e. x=0.5; i.e. the correct x is independent of w, h, a.

    But, y' must be <=1. So, we want an x closest to 0.5 where y'<=1
    Now y' when x=0.5 is:
     y' = wt(1+0.5/(t^2)-0.5)/h
        = w/2h * (t + 1/t)
        <=1 if
    t+1/t <= 2h/w
    (t^2+1)/t <= 2h/w
    But (tan^1+1)/tan = sec^2/tan = 1/(sin.cos) = 2/(sin 2a), hence
    2/sin(2a) <= 2h/w
    sin(2a)   >= w/h
    So we only have to worry if sin(2a) < w/h

    Now y'=1 occurs when w/h.t(1+x/(t^2)-x) = 1
    i.e. 1+x/(t^2)-x  = h/(wt)
    i.e. x(1/(t^2)-1) = h/(wt) - 1
    i.e. x(1-(t^2))/t^2 = h/(wt) - 1
    i.e. x              = (h/(wt) - 1) * t^2 / (1-(t^2))
    i.e. x              = (ht/w - t^2) / (1-(t^2))
    Now when the a=45 we have t=1 hence 1-t^2 is 0;
      if w==h then we have a diamond solution (i.e. x-0.5 works)
      if w<=h then sin(2*a) = 1 >= w/h then x=0.5 works
    If w>h then, as at the top, we should have used 90-a and swapped w/h
    
   *)
  let wh_of_largest_area_within w h a =
    let a = abs_float a in
    let a = mod_float a 180. in
    let (w,h,a,flip) = if a>=90. then (h,w,a-.90.,true) else (w,h,a,false) in
    let (w,h,a,flip) = if w>h then (h,w,90.-.a,not flip) else (w,h,a,flip) in
    let sin2a = sin (2. *. a) in
    let t     = tan a in
    let y  x   = x *. w /. h /. t in
    let y' x y = w /. h *. t *. (1. -. x) +. y in
    let x =
      if (t > 1E10) then 0.5 (* cover a=45 *)
      else if (sin2a < w /. h) then (
        (h *. t /. w -. t *. t) /. (1. -. t *. t)
      ) else (
        0.5
      )
    in
    let y  = y x in
    let y' = y' x y in
    let yh = h *. y in
    let y'myh = h *. (y' -. y) in
    let wx = w *. x in
    let wmwx = w *. (1. -. x) in
    let (width,height) =
      if (t>1E-10) then
        ( sqrt (wx*.wx +. yh*.yh) , sqrt (wmwx*.wmwx +. y'myh*.y'myh) )
      else
        (w,h)
    in
    if flip then (height,width) else (width,height)        
    
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
    make  (x -. w/.2.) (y -. h/.2.) (x +. w/.2.) (y +. h/.2.)

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

  (*f rotate *)
  let rotate v r =
    let sinr = sin (deg_to_rad r) in
    let cosr = cos (deg_to_rad r) in
    make (v.(0) *. cosr -. v.(1) *. sinr) (v.(1) *. cosr +. v.(0) *. sinr)
    
  (*f abs *)
  let abs v =
    if (v.(0)>=0.) && (v.(1)>=0.) then v
    else make (abs_float v.(0)) (abs_float v.(1))
    
  (*f str *)
  let str r = Printf.sprintf "(%g,%g)" r.(0) r.(1)

  (*f All done *)
end


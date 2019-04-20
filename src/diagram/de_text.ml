(** Copyright (C) 2019,  Gavin J Stark.  All rights reserved.
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
 * @file     de_text.ml
 * @brief    Text components for diagrams
 *
 *)

(*a Libraries *)
open Types

(*a Submodule of de_base *)
include De_base

(*a Types *)
(*t et - basic element type *)
type et = {
    text : string list; (* One string per line *)
    num_lines : int; (* Length of text list *)
  }

(*t rt - resolved style additional record *)
type rt = {
    font : Font.t;
    size  : float;
    align  : float;
    line_spacing : float;
    text_height : float;
    text_widths : (float * string) list;
  (* anchor / alignment *)
  }

(*t lt - layout width and height, using resolved parameters *)
type lt = t_rect (* Rectangle to place text within *)

(*t gt - finalized geometry for rendering *)
type gt = {
    size  : float;
    color : string;
    text_anchor : string; (* middle unless align is <=-1 (start) or >=1 (end) *)
    xys: (float * float) list;
  }

(*a Variables - styles, particularly *)
(*v styles for the stylesheet/stylable, concatenated with the de_base styles *)
let styles = Stylesheet.Value.[
               (Attr_names.font_name,  St_string,  sv_none_string, true);
               (Attr_names.align,      St_float,   Sv_float (Some 0.), true);
               (Attr_names.font_size,  St_float,   Sv_float (Some 12.), true);
               (Attr_names.color, St_rgb,          Sv_rgb [|0.;0.;0.;|], true);
             ] @ styles

(*a Functions for basic element *)
(*f make : string list -> et *)
let make text =
  let num_lines = List.length text in
  {text; num_lines}

(*a Functions for resolved styling *)
(*f resolve_styles : et -> t_style_resolver -> rt *)
let resolve_styles et (resolver:t_style_resolver) =
  let font_name = resolver.value_as_string ~default:"" Attr_names.font_name in
  let font   = Fonts.find_font font_name in
  let align  = resolver.value_as_float         Attr_names.align in
  let size   = resolver.value_as_float         Attr_names.font_size in
  let text_widths = List.map (fun text -> (Font.width_of_text font size text), text) et.text in
  let text_height = Font.height_of_text font size in
  let line_spacing = Font.text_spacing font size in
  let rt = {font; size; align; line_spacing; text_height; text_widths} in
  (rt, [])

(*f get_min_bbox_ref : et -> rt -> vector reference point and bbox
  BBox is 0,0,w,h
  If 'aligh to top' then ref point has y of 0
  If 'aligh to bottom,' then ref point has y of h
  If 'aligh to middle,' then ref point has y of h/2

  If the ref point is explicitly placed somewhere then
 *)
let get_min_bbox et rt =
  let max_width = List.fold_left (fun acc (w,_) -> if (w>acc) then w else acc) 0. rt.text_widths in
  let height = ((float et.num_lines) *. rt.text_height) +. ((float (et.num_lines - 1)) *. rt.line_spacing) in
  (0.,0.,max_width,height)

(*a Functions for layout *)
(*f make_layout_within_bbox : et -> rt -> ref pt * bbox `-> lt * properties
  ref ptr and bbox are in the parent coordinate space.
  bbox shows where the entirety of the layout should fit within, and ref_ptr is where the layout ref_ptr should be placed
  The layout should not scale up to fit the bbox if there is room; it should just use more space if it requires it, otherwise
  leave space blank.
 *)
let make_layout_within_bbox et rt bbox : lt * t_element_properties = 
  (* bbox is in the coordinate space for us to do a layout in  can now do centering, and could prepare for justification. *)
  (* Printf.printf "\nText layout bbox %s\n\n" (Primitives.Rectangle.str bbox); *)
  (bbox, [])

(*a Finsalize geometry and rendering *)
(*f finalize_geometry et -> rt -> lt -> t_style_resolver -> gt
  Using alignment and ref point translation determine x,y for each line
 *)
let finalize_geometry et (rt:rt) lt (resolver:t_style_resolver) = 
  let color = resolver.value_as_color_string  ~default:"black" Attr_names.color in
  let (x0,y0,x1,y1) = lt in
  let (cx,w) = ( (x0+.x1)/.2., (x1-.x0)/.2. ) in
  let text_xy i (w,text) =
    let y = y0 +. (float i) *. (rt.line_spacing +. rt.text_height) +. rt.text_height in
    if (rt.align<(-1.)) then (x0, y)
    else if (rt.align>(1.)) then (x1, y)
    else (cx+.w*.rt.align,y)
  in
  let xys = List.mapi text_xy rt.text_widths in
  let text_anchor = if (rt.align<(-1.)) then "start" else if (rt.align>(1.)) then "end" else "middle" in
  {xys; size=rt.size; text_anchor; color;}

(*f render_line_svg : et -> rt -> lt -> gt -> string -> ref point -> svg
  Render a single line of text
 *)
let render_line_svg et rt lt gt text (x,y) = 
  Svg.(tag "text" [(* font-family, stroke *)
           FloatAttr ("x", x);
           FloatAttr ("y", y);
           FloatAttr ("font-size", gt.size);
           StringAttr ("text-anchor", gt.text_anchor);
           StringAttr ("fill", gt.color);
         ] [] [text])

(*f render_svg : et -> rt -> lt -> gt -> float zindex -> svg list
  Render into a list of SVG tags
 *)
let render_svg et rt lt gt z_index = 
  List.map2 (render_line_svg et rt lt gt) et.text gt.xys

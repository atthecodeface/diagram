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
module Properties = Properties
module Color     = Primitives.Color
module Rectangle = Primitives.Rectangle
module Layout = Layout

module Font = struct
    type t = {
    family : string;
    ascent : float;
    descent : float;
    avg_width : float;
      }
    let get_width t s = t.avg_width *. (float (String.length s))
    let get_bbox t s =
      let w = get_width t s in
      (0., w, 0., 10.)
    (*
    let defn_svg t = Svg.tag "font-face" [("font-family", t.family), 
    let use_svg  t = Svg.tag "font-face" [("font-family", t.family), 
     *)
    let make family ascent descent avg_width = 
    { family; ascent; descent; avg_width; }
end

module type LayoutElementType = sig
    type t    (* base element type *)
    type lt   (* layout of element type - does not include element *)

    val get_min_bbox : t -> Primitives.t_rect
    val make_layout_within_bbox : t -> Primitives.t_rect -> lt
end

module PathInt : LayoutElementType = struct
    type t = int
    let r = Rectangle.mk_fixed (0.,0.,100.,20.)
    let get_min_bbox t = (0.,0.,100.,20.)
    type lt = Primitives.t_rect
    let make_layout_within_bbox t bbox = bbox
end

module TextInt : sig
    include LayoutElementType
    val make : Font.t -> float -> Color.t -> Color.t -> string list -> t
end = struct 
    (* List of (?) text, font, style, base line, min font size, desired font size *)
    (* padding below lowest baseline, above upper baseline, to left of left-most pixel, to right of right-most pixel *)
    type t = {
        font : Font.t;
        size : float;
        text : string list; (* One string per line *)
        fill : Color.t;
        stroke : Color.t;
    (* anchor / alignment *)
      }
    let r = Rectangle.mk_fixed (0.,0.,100.,20.)
    let get_min_bbox t = (0.,0.,100.,20.)
    let svg_use t bbox = Svg.(tag "text" [(*StringAttr ("class", "TextShape");*)
                                     FloatAttr ("x", 0.);
                                     FloatAttr ("y", 0.);
        (Color.svg_attr t.fill "fill");
        (Color.svg_attr t.stroke "stroke");
                    ] )
    let make font size fill stroke text = {font; size; fill; stroke; text}
    type lt = Primitives.t_rect
    let make_layout_within_bbox t bbox = bbox
end

module BoxInt : sig
    include LayoutElementType
    val make : unit -> t
end = struct 
    type t = int

    let get_min_bbox t = Rectangle.zeros

    let make _ = 0
    type lt = Primitives.t_rect
    let make_layout_within_bbox t bbox = bbox

end

module LayoutElementFunc (E:LayoutElementType) = struct
    type t  = E.t
    type lt = E.lt
    let get_min_bbox = E.get_min_bbox
    let make_layout_within_bbox = E.make_layout_within_bbox
end

module Path = LayoutElementFunc(PathInt)
module Text = LayoutElementFunc(TextInt)
module Box  = LayoutElementFunc(BoxInt)

module type LayoutElementAggrType = sig
    type et
    type lt
    val get_min_bbox : et -> Primitives.t_rect
    val make_layout_within_bbox : et -> Primitives.t_rect -> lt
end

module LayoutElement  = struct
  type et = | EBox  of Box.t
            | EText of Text.t
            | EPath of Path.t

  type lt = | LBox of  Box.lt
            | LText of Text.lt
            | LPath of Path.lt

    let get_min_bbox et = 
      match et with
      | EText e      -> Text.get_min_bbox e
      | EPath e      -> Path.get_min_bbox e
      | EBox  e      -> Box.get_min_bbox e

    let make_layout_within_bbox et (bbox : Primitives.t_rect) : lt = 
      match et with
      | EText e  -> LText  (Text.make_layout_within_bbox e bbox)
      | EPath e  -> LPath  (Path.make_layout_within_bbox e bbox)
      | EBox  e  -> LBox   (Box.make_layout_within_bbox e bbox)

end

(*
    Long term the process should be:

    1. Construct the hierarchy
    2. Apply stylesheet (hierarchy -> stylesheet -> hierachy)
    3. Calculate min bbox (hierarchy -> hierarchy with min bbox )
    4. Layout within bbox (hierarchy with min bbox -> bbox -> hierarchy with layout )
    5. Finalize geometry (hierarchy with layout -> finalized hierarchy )
    6. Render

 *)
module ElementFunc (LE : LayoutElementAggrType) = struct
    type th = {
        hdr                : Primitives.t_hdr;
        layout_properties  : Layout.t_layout_properties;
      }
    type et = {
        th : th;
        et : LE.et;
        content : et list;
      }
    type lt = {
        th : th;
        et : LE.et;
        lt : LE.lt;
        content_layout : (et * lt) list;
      }

    (*f make_et - construct the hierarchy *)
    let make_et properties id et content : et =
        let layout_properties = Layout.make_layout_hdr properties in
        let hdr = Primitives.th_make id in
        let th = { hdr; layout_properties} in
        { th; et; content}

    (*f apply_stylesheet - map to new hierarchy with updated properties *)

    (*f calculate_min_bbox *)
    let rec get_min_bbox (et : et) =
      let content_bbox             = LE.get_min_bbox et.et in
      let content_layout_bbox_list = List.map (fun (x:et) -> (x.th.layout_properties, get_min_bbox x)) et.content in
      let xd_yd_list = Layout.gather_grid content_layout_bbox_list in
      let px_py_ax_ay_bbox_list = Layout.gather_place content_layout_bbox_list in
      Layout.expand_bbox et.th.layout_properties content_bbox

    let make_layout_within_bbox (et : et) bbox = 
      let layout = LE.make_layout_within_bbox et.et bbox in
      { th=et.th; et=et.et; lt=layout; content_layout = []}

    (* finalize geometry *)
    (* get geometry field (float along line?) *)
    (* generate svg *)
    (*let svg =
    let child_svg = 
     *)
end

module Element = struct
    include ElementFunc(LayoutElement)
    let make_text properties id text     = make_et properties id (LayoutElement.EText text) []
    let make_box  properties id elements = make_et properties id (LayoutElement.EBox (BoxInt.make ())) elements

end

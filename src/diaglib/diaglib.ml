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
module Svg = Svg

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
    val render_svg : t -> lt -> int -> Svg.t list
end

module PathInt : LayoutElementType = struct
    type t = int
    let r = Rectangle.mk_fixed (0.,0.,100.,20.)
    let get_min_bbox t = (0.,0.,100.,20.)
    type lt = Primitives.t_rect
    let make_layout_within_bbox t bbox = bbox
    let render_svg t lt i = 
      []
end

module TextInt : sig
    include LayoutElementType
    val make : Font.t -> float -> Color.t -> string list -> t
end = struct 
    (* List of (?) text, font, style, base line, min font size, desired font size *)
    (* padding below lowest baseline, above upper baseline, to left of left-most pixel, to right of right-most pixel *)
    type t = {
        font : Font.t;
        size : float;
        text : string list; (* One string per line *)
        color : Color.t;
    (* anchor / alignment *)
      }
    type lt = Primitives.t_rect
    let r = Rectangle.mk_fixed (0.,0.,100.,20.)
    let get_min_bbox t = (0.,0.,100.,20.)
    let svg_use t lt = 
      let (_,_,_,y) = lt in
      Svg.(tag "text" [(*StringAttr ("class", "TextShape");*)
                                     FloatAttr ("x", 0.);
                                     FloatAttr ("y", y);
        (Color.svg_attr "fill" t.color);
                    ] [] t.text)
    let make font size color text = {font; size; color; text}
    let make_layout_within_bbox t bbox = bbox
    let render_svg t lt i = 
      let svg = svg_use t lt in
      [svg]
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
    let render_svg t lt i = []

end

module LayoutElementFunc (E:LayoutElementType) = struct
    type t  = E.t
    type lt = E.lt
    let get_min_bbox = E.get_min_bbox
    let make_layout_within_bbox = E.make_layout_within_bbox
    let render_svg = E.render_svg
end

module Path = LayoutElementFunc(PathInt)
module Text = LayoutElementFunc(TextInt)
module Box  = LayoutElementFunc(BoxInt)

module type LayoutElementAggrType = sig
    type et
    type lt
    val get_min_bbox : et -> Primitives.t_rect
    val make_layout_within_bbox : et -> Primitives.t_rect -> lt
    val render_svg : et -> lt -> int -> Svg.t list
end

module LayoutElement  = struct
  exception Mismatch of string
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

    let render_svg et lt zindex = 
      match et with
      | EText e -> (match lt with | LText l -> Text.render_svg e l zindex | _ -> raise (Mismatch "Not both texts"))
      | EPath e -> (match lt with | LPath l -> Path.render_svg e l zindex | _ -> raise (Mismatch "Not both paths"))
      | EBox  e -> []

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
    type etb = {
        th : th;
        et : LE.et;
        content_bbox : Primitives.t_rect;
        element_bbox : Primitives.t_rect;
        min_bbox     : Primitives.t_rect;
        layout       : Layout.t;
        content_etb  : etb list;
      }
    type lt = {
        th : th;
        et : LE.et;
        lt : LE.lt;
        layout : Layout.t;
        ltr    : Layout.t_transform;
        content_layout : lt list;
        bbox : Primitives.t_rect;
      }

    (*f make_et - construct the hierarchy within a stylesheet *)
    let make_et properties id et content : et =
        let layout_properties = Layout.make_layout_hdr properties in
        let hdr = Primitives.th_make id in
        let th = { hdr; layout_properties} in
        { th; et; content}

    (*f apply_stylesheet - map to new hierarchy with updated properties *)

    (*f layout_content_create - create layout using any necessary et properties and etb content *)
    let layout_content_create (et : et) etb_list =
      let content_layout_properties_bbox = List.map (fun (x:etb)->(x.th.layout_properties,x.min_bbox)) etb_list in
      Layout.create et.th.layout_properties content_layout_properties_bbox 

    (*f make_min_bbox - make etb from et, i.e. create a structure with the min_bbox of the element given its properties *)
    let rec make_min_bbox (et : et) : etb =
      let content_etb   = List.map make_min_bbox et.content in
      let element_bbox  = LE.get_min_bbox et.et in
      let layout        = layout_content_create et content_etb in
      let content_bbox  = Layout.get_min_bbox layout in
      let merged_bbox   = Rectangle.union element_bbox content_bbox in
      let min_bbox      = Layout.expand_bbox et.th.layout_properties merged_bbox in
      { th=et.th; et=et.et; content_bbox; element_bbox; min_bbox; layout; content_etb }

    (*f make_layout_within_bbox - make ltb from etb *)
    let rec make_layout_within_bbox (etb : etb) bbox = 
      let (ltr, bbox) = Layout.layout_within_bbox etb.layout bbox in
      let lt = LE.make_layout_within_bbox etb.et bbox in
      let layout_content_element (x : etb) =
        let bbox = Layout.get_bbox_element etb.layout ltr x.th.layout_properties x.min_bbox in
        make_layout_within_bbox x bbox
      in
      let content_layout = List.map layout_content_element etb.content_etb in
      { th=etb.th; et=etb.et; lt; layout=etb.layout; ltr; content_layout; bbox;}

    (* finalize geometry *)
    (* get geometry field (float along line?) *)
    let rec show_layout lt indent =
      Printf.printf "%sid '%s' : bbox '%s'\n" indent lt.th.hdr.id (Rectangle.str lt.bbox);
      let indent = String.concat "" ["  "; indent] in
      List.iter (fun x -> show_layout x indent) lt.content_layout

    (*f render_svg lt index - return a list of SVG tags that make up the element *)
    let rec render_svg lt zindex =
      let content_svg   = List.fold_left (fun a x -> a @ (render_svg x zindex)) [] lt.content_layout in
      let element_svg   = LE.render_svg lt.et lt.lt zindex in
      Layout.render_svg lt.layout lt.ltr (content_svg @ element_svg) 

end

module Element = struct
    include ElementFunc(LayoutElement)
    (* These should include a stylesheet and style descriptor *)
    let make_text properties id text     = make_et properties id (LayoutElement.EText text) []
    let make_box  properties id elements = make_et properties id (LayoutElement.EBox (BoxInt.make ())) elements

end
(*a Toplevel *)
let layout_styles = [ ("padding", Stylesheet.Styleable_value.St_float_4);
                      ("margin",  Stylesheet.Styleable_value.St_float_4);
                      ("border",  Stylesheet.Styleable_value.St_float_4);
                      ("anchor",  Stylesheet.Styleable_value.St_float_2);
                      ("expand",  Stylesheet.Styleable_value.St_float_2);
                      ("place",   Stylesheet.Styleable_value.St_float_2);
                      ("grid",    Stylesheet.Styleable_value.St_int_4);
                      ("border_color", Stylesheet.Styleable_value.St_rgb );
                      ("face_color",   Stylesheet.Styleable_value.St_rgb );
                      ("rotation",   Stylesheet.Styleable_value.St_float);
                      ("scale",      Stylesheet.Styleable_value.St_float_2);
                    ]

let element_text_styles = [
                      ("font_size",  Stylesheet.Styleable_value.St_float );
                      ("color",      Stylesheet.Styleable_value.St_rgb );
                      ("rotation",   Stylesheet.Styleable_value.St_float);
  ] @ layout_styles

let element_text_style_desc  = Stylesheet.create_desc [] element_text_styles

(*a Stylesheet things *)
let create_stylesheet _ = 
  let stylesheet = Stylesheet.create () in
  Stylesheet.add_style_defaults stylesheet [("border",  Stylesheet.Styleable_value.Sv_floats (4,[|0.;0.;0.;0.;|]), false);
                                            ("padding", Stylesheet.Styleable_value.Sv_floats (4,[|0.;0.;0.;0.;|]), false);
                                            ("margin",  Stylesheet.Styleable_value.Sv_floats (4,[|0.;0.;0.;0.;|]), false);
                                            ("dims",    Stylesheet.Styleable_value.Sv_floats (2,[|0.;0.;|]), false);
                                            ("offset",  Stylesheet.Styleable_value.Sv_floats (2,[|0.;0.;|]), false);
                                            ("align",   Stylesheet.Styleable_value.Sv_floats (2,[|0.;0.;|]), false);
                                            ("faces",   Stylesheet.Styleable_value.Sv_ints (4,[|0;0;0;0;|]), false);
                                            ("fill",    Stylesheet.Styleable_value.Sv_ints (2,[|0;0;|]), false);
                                            ("width",   Stylesheet.Styleable_value.Sv_float 0., false);
                                            ("height",   Stylesheet.Styleable_value.Sv_float 0., false);
                                            ("face_color",   Stylesheet.Styleable_value.Sv_rgb [|0.;0.;0.;|], true); (* inherit *)
                                            ("border_color", Stylesheet.Styleable_value.Sv_rgb [|0.;0.;0.;|], true); (* inherit *)
                                            ("bg_color",     Stylesheet.Styleable_value.Sv_rgb [|0.;0.;0.;|], true); (* inherit *)
                                            ("font_size",    Stylesheet.Styleable_value.Sv_float 1., true); (* inherit *)
                                            ("font_height",    Stylesheet.Styleable_value.Sv_float 0., true); (* inherit *)
                                            ("font_thickness", Stylesheet.Styleable_value.Sv_float 0., true); (* inherit *)
                                            ("font_color",    Stylesheet.Styleable_value.Sv_rgb [|1.;1.;1.;|], true); (* inherit *)
                                           ];
    stylesheet
let stylesheet = create_stylesheet ()
let sel_true            =  (fun e -> true)
let sel_cbox            =  Stylesheet.se_is_element_id "control"
let sel_type_button     =  Stylesheet.se_is_element_type "text_button"
let sel_cls_rotate      =  Stylesheet.se_has_element_class "rotate"
let sel_state_pressed   =  Stylesheet.se_is_element_state 0 3
let sel_state_hover     =  Stylesheet.se_is_element_state 0 2
let sel_state_enable    =  Stylesheet.se_is_element_state 0 1

let sel_button_rotate = fun e -> (sel_type_button e) && (sel_cls_rotate e)
let sel_hover_button  = fun e -> (sel_type_button e) && (sel_state_hover e)

let _ = 
    Stylesheet.add_style_rule stylesheet [sel_cbox; sel_hover_button]
             [("border_color", Sv_rgb [|1.;1.;1.;|]);
             ];
    Stylesheet.add_style_rule stylesheet [sel_cbox; sel_type_button]
             [("border", Sv_floats (6,[|1.;1.;1.;1.;1.;1.;|]));
             ];
    Stylesheet.add_style_rule stylesheet [sel_true]
             [("margin", Sv_floats (6,[|0.;0.;0.;0.;0.;0.;|]));
             ];
    ()



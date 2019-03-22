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
    type et   (* base element type *)
    type rt   (* resolved styled of element type - does not include t *)
    type lt   (* layout of element type - does not include rt or t *)

    val styles       : (string * Stylesheet.Value.t_styleable_type * Stylesheet.Value.t_styleable_value * bool) list
    val resolve_styles : et -> Stylesheet.t_stylesheet -> Stylesheet.t_styleable -> rt
    val get_min_bbox : et -> rt -> Primitives.t_rect
    val make_layout_within_bbox : et -> rt -> Primitives.t_rect -> lt
    val render_svg   : et -> rt -> lt -> int -> Svg.t list
end
module LayoutElementBase = struct
  let styles = Stylesheet.Value.[ ("padding",      (St_floats 4),  Sv_floats (4,[|0.;0.;0.;0.;|]), false);
                                  ("margin",       (St_floats 4),  Sv_floats (4,[|0.;0.;0.;0.;|]), false);
                                  ("border",       (St_floats 4),  Sv_floats (4,[|0.;0.;0.;0.;|]), false);
                                  ("anchor",       (St_floats 2),  Sv_floats (2,[|0.5;0.5;|]), false);
                                  ("expand",       (St_floats 2),  Sv_floats (2,[|0.;0.;|]), false);
                                  ("place",        (St_floats 2),  sv_none_floats, false);
                                  ("grid",         (St_ints 4),    sv_none_ints, false);
                                  ("rotation",     St_float,       sv_none_float, false);
                                  ("scale",        (St_floats 2),  sv_none_floats, false);
                                  ("border_color", St_rgb,         sv_none_rgb, true); (* inherit *)
                                  ("fill_color",   St_rgb,         sv_none_rgb, true); (* inherit *)
    ]
end

module PathInt : LayoutElementType = struct
    include LayoutElementBase
    type et = int
    type rt = int
    type lt = Primitives.t_rect

    let resolve_styles et stylesheet styleable : rt = 0

    let r = Rectangle.mk_fixed (0.,0.,100.,20.)
    let get_min_bbox et rt = (0.,0.,100.,20.)
    let make_layout_within_bbox et rt bbox = bbox
    let render_svg et rt lt i = 
      []
end

module TextInt : sig
    include LayoutElementType
    val make : Font.t -> string list -> et
end = struct 
    include LayoutElementBase
    (* List of (?) text, font, style, base line, min font size, desired font size *)
    (* padding below lowest baseline, above upper baseline, to left of left-most pixel, to right of right-most pixel *)
    type et = {
        font : Font.t;
        text : string list; (* One string per line *)
      }
    type rt = {
        size : float;
        color : float array;
    (* anchor / alignment *)
      }
    type lt = Primitives.t_rect
    let styles = Stylesheet.Value.[
                   ("font_size",  St_float,  Sv_float (Some 12.), true);
                   ("font_color", St_rgb,    Sv_rgb [|0.;0.;0.;|], true);
                 ] @ styles

    let resolve_styles et stylesheet styleable : rt =
      let size  = Stylesheet.styleable_value_as_float stylesheet styleable "font_size" in
      let color = Stylesheet.styleable_value_as_floats stylesheet styleable "font_color" in
      {size; color}

    let r = Rectangle.mk_fixed (0.,0.,100.,20.)
    let get_min_bbox et rt = (0.,0.,100.,20.)
    let svg_use et rt lt = 
      let (_,_,_,y) = lt in
      Svg.(tag "text" [(*StringAttr ("class", "TextShape");*)
                                     FloatAttr ("x", 0.);
                                     FloatAttr ("y", y);
 (*       (Color.svg_attr "fill" et.color);*)
                    ] [] et.text)
    let make font text = {font; text}
    let make_layout_within_bbox et rt bbox = bbox
    let render_svg et rt lt i = 
      let svg = svg_use et rt lt in
      [svg]
end

module BoxInt : sig
    include LayoutElementType
    val make : unit -> et
end = struct 
    include LayoutElementBase
    type et = int
    type rt = int
    type lt = Primitives.t_rect

    let get_min_bbox et rt = Rectangle.zeros

    let make _ = 0
    let resolve_styles et stylesheet styleable : rt = 0
    let make_layout_within_bbox et rt bbox = bbox
    let render_svg et rt lt i = []

end

module LayoutElementFunc (E:LayoutElementType) = struct
    type et = E.et
    type rt = E.rt
    type lt = E.lt
    let styles = E.styles
    let resolve_styles = E.resolve_styles
    let get_min_bbox = E.get_min_bbox
    let make_layout_within_bbox = E.make_layout_within_bbox
    let render_svg = E.render_svg
end

module Path = LayoutElementFunc(PathInt)
module Text = LayoutElementFunc(TextInt)
module Box  = LayoutElementFunc(BoxInt)

module type LayoutElementAggrType = sig
    type et
    type rt
    type lt
    val styles       : (string * Stylesheet.Value.t_styleable_type * Stylesheet.Value.t_styleable_value * bool) list
    val style_desc   : Stylesheet.t_styleable_desc
    val type_name    : et -> string
    val resolve_styles : et -> Stylesheet.t_stylesheet -> Stylesheet.t_styleable -> rt
    val get_min_bbox : rt -> Primitives.t_rect
    val make_layout_within_bbox : rt -> Primitives.t_rect -> lt
    val render_svg   : lt -> int -> Svg.t list
end

module LayoutElement = struct
  exception Mismatch of string

  (*t et - Basic element type *)
  type et = | EBox  of Box.et
            | EText of Text.et
            | EPath of Path.et

  (*t rt - Additional resolved style structure for the element *)
  type rt = | RBox  of Box.et  * Box.rt
            | RText of Text.et * Text.rt
            | RPath of Path.et * Path.rt

  (*t lt - Additional resolved style structure for the element *)
  type lt = | LBox of  Box.et * Box.rt * Box.lt
            | LText of Text.et * Text.rt * Text.lt
            | LPath of Path.et * Path.rt * Path.lt

  let styles  = 
    Text.styles @ Path.styles @ Box.styles

  let style_desc = Stylesheet.create_desc [] styles

  let et_is_text = function | EText _ -> true | _ -> false
  let et_is_path = function | EPath _ -> true | _ -> false
  let et_is_box  = function | EBox  _ -> true | _ -> false

  let type_name et = 
    match et with
    | EText e      -> "text"
    | EPath e      -> "path"
    | EBox  e      -> "box"

  let resolve_styles et stylesheet styleable =
    match et with
    | EText e      -> RText (e,Text.resolve_styles e stylesheet styleable)
    | EPath e      -> RPath (e,Path.resolve_styles e stylesheet styleable)
    | EBox  e      -> RBox  (e,Box.resolve_styles e stylesheet styleable)

  let get_min_bbox rt = 
    match rt with
    | RText (e,r)      -> Text.get_min_bbox e r
    | RPath (e,r)      -> Path.get_min_bbox e r
    | RBox  (e,r)      -> Box.get_min_bbox  e r

  let make_layout_within_bbox rt (bbox : Primitives.t_rect) : lt = 
    match rt with
    | RText (e,r)  -> LText  (e,r,(Text.make_layout_within_bbox e r bbox))
    | RPath (e,r)  -> LPath  (e,r,(Path.make_layout_within_bbox e r bbox))
    | RBox  (e,r)  -> LBox   (e,r,(Box.make_layout_within_bbox e r bbox))

  let render_svg lt zindex = 
    match lt with
    | LText (e,r,l) -> Text.render_svg e r l zindex
    | LPath (e,r,l) -> Path.render_svg e r l zindex
    | LBox  (e,r,l) -> []

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
    (*t th - Basic header *)
    type th = Primitives.t_hdr

    (*t et - Element type, with its header and content *)
    type et = {
        th : th;
        properties: (string * string) list;
        et : LE.et;
        content_et : et list;
      }

    (*t st - Styleable element type; something to which stylesheet may be applied *)
    type st = {
        th : th;
        et : LE.et;
        styleable  : Stylesheet.t_styleable;
        content_st : st list;
      }

    (*t rt - Resolved styled element type; all properties of this have full values *)
    type rt = {
        th : th;
        rt : LE.rt;
        layout_properties  : Layout.t_layout_properties;
        content_rt : rt list;
      }

    (*t etb - Bounded-box element type; resolved and desired geometry calculated *)
    type etb = {
        th : th;
        rt : LE.rt;
        layout_properties  : Layout.t_layout_properties;
        content_bbox : Primitives.t_rect;
        element_bbox : Primitives.t_rect;
        min_bbox     : Primitives.t_rect;
        layout       : Layout.t;
        content_etb  : etb list;
      }

    (*t lt - Post-layout element type; actual bounding box and content post-layout *)
    type lt = {
        th : th;
        lt : LE.lt;
        layout : Layout.t;
        ltr    : Layout.t_transform;
        content_layout : lt list;
        bbox : Primitives.t_rect;
      }

    (*t gt - Fully resolved geometry type *)
    type gt = lt

    (*v Stylesheet - one for the whole document, not every element *)
    let create_stylesheet _ = 
      let stylesheet = Stylesheet.create () in
      Stylesheet.add_style_defaults stylesheet LayoutElement.styles;
      stylesheet

    (*f make_et ... : et - construct the hierarchy within the stylesheet *)
    let make_et properties id et content_et : et =
        let th = Primitives.th_make id in
        { th; properties; et; content_et}

    (*f make_styleable <stylesheet> <element> : st - make contents' styleables, and get a styleable for this element *)
    let rec make_styleable stylesheet (et:et) : st=
      let content_st = List.map (make_styleable stylesheet) et.content_et in
      let st_list = List.map (fun st -> st.styleable) content_st in
      let styleable = Stylesheet.se_create LE.style_desc stylesheet (LE.type_name et.et) et.properties (fun _ -> ()) st_list in
      List.iter (Stylesheet.se_set_parent styleable) st_list;
      { th=et.th; et=et.et; styleable; content_st; }

    (*f resolve_styles <stylesheet> <element> : rt - resolve all the properties from the stylesheet ready for layout *)
    let rec resolve_styles stylesheet (st:st) : rt =
      let content_rt = List.map (resolve_styles stylesheet) st.content_st in
      let rt = LE.resolve_styles st.et stylesheet st.styleable in
      let layout_properties = Layout.make_layout_hdr stylesheet st.styleable in
      { th=st.th; rt; layout_properties; content_rt}

    (*f layout_content_create - create layout using any necessary et properties and etb content *)
    let layout_content_create (rt:rt) etb_list =
      let content_layout_properties_bbox = List.map (fun (x:etb)->(x.layout_properties,x.min_bbox)) etb_list in
      Layout.create rt.layout_properties content_layout_properties_bbox 

    (*f make_min_bbox rt : etb - create a structure with the min_bbox of the element given its properties *)
    let rec make_min_bbox (rt:rt) : etb =
      let content_etb   = List.map make_min_bbox rt.content_rt in
      let element_bbox  = LE.get_min_bbox rt.rt in
      let layout        = layout_content_create rt content_etb in
      let content_bbox  = Layout.get_min_bbox layout in
      let merged_bbox   = Rectangle.union element_bbox content_bbox in
      let min_bbox      = Layout.expand_bbox rt.layout_properties merged_bbox in
      { th=rt.th; rt=rt.rt; layout_properties=rt.layout_properties; content_bbox; element_bbox; min_bbox; layout; content_etb }

    (*f make_layout_within_bbox - make lt from etb *)
    let rec make_layout_within_bbox (etb:etb) bbox = 
      let (ltr, bbox) = Layout.layout_within_bbox etb.layout bbox in
      let lt = LE.make_layout_within_bbox etb.rt bbox in
      let layout_content_element (x : etb) =
        let bbox = Layout.get_bbox_element etb.layout ltr x.layout_properties x.min_bbox in
        make_layout_within_bbox x bbox
      in
      let content_layout = List.map layout_content_element etb.content_etb in
      { th=etb.th; lt; layout=etb.layout; ltr; content_layout; bbox;}

    (* finalize geometry *)
    (* get geometry field (float along line?) *)
    let rec show_layout lt indent =
      Printf.printf "%sid '%s' : bbox '%s'\n" indent lt.th.id (Rectangle.str lt.bbox);
      let indent = String.concat "" ["  "; indent] in
      List.iter (fun x -> show_layout x indent) lt.content_layout

    (*f render_svg lt index - return a list of SVG tags that make up the element *)
    let rec render_svg lt zindex =
      let content_svg   = List.fold_left (fun a x -> a @ (render_svg x zindex)) [] lt.content_layout in
      let element_svg   = LE.render_svg lt.lt zindex in
      Layout.render_svg lt.layout lt.ltr (content_svg @ element_svg) 

    (*f layout_elements *)
    let layout_elements stylesheet page_bbox et =
        let st  = make_styleable stylesheet et in
        ignore (Stylesheet.build stylesheet [st.styleable]);
        Stylesheet.apply stylesheet;
        let rt  = resolve_styles stylesheet st in
        let etb = make_min_bbox rt in
        let lt  = make_layout_within_bbox etb page_bbox in
        lt

end

module Element = struct
    include ElementFunc(LayoutElement)

    let make_text properties id text     = make_et properties id (LayoutElement.EText text) []
    let make_box  properties id elements = make_et properties id (LayoutElement.EBox (BoxInt.make ())) elements
end

(*a Toplevel *)

(*a Stylesheet things *)
(*
let stylesheet = Element.create_stylesheet ()
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


 *)

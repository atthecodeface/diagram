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
(*a Import modules *)
module Properties = Properties
module Color     = Primitives.Color
module Rectangle = Primitives.Rectangle
module Layout = Layout
module Svg = Svg

(*a Modules *)
(*m LayoutElementAggrType *)
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

(*m ElementFunc *)
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
      Stylesheet.add_style_defaults stylesheet LE.styles;
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


(*m LayoutElementType *)
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

(*m LayoutElementFunc *)
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

(*m LayoutElementBase *)
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


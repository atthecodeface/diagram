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

(*a Useful functions *)
(*f list_collapse *)
let rec list_collapse f = function
  | [] -> None
  | hd::tl -> match f hd with
              | Some x -> Some x
              | None -> list_collapse f tl

(*f list_find *)
let list_find f = list_collapse (fun x->if (f x) then Some x else None)

(*a Base layout element types and modules *)
(*t t_element_value *)
type t_element_value =
  | Ev_rect       of Primitives.t_rect
  | Ev_floats     of int * (float array)
  | Ev_float      of float
  | Ev_vector     of float * float
  | Ev_string     of string

type t_element_properties = (string * t_element_value) list

exception Bad_type of string

let str_type_of_element_value = function
  | Ev_float _ -> "float"
  | Ev_floats (n,_) -> Printf.sprintf "floats[%d]" n
  | Ev_rect r -> "rect"
  | Ev_vector _ -> "vector"
  | Ev_string _ -> "string"

let reval_of_element_value = function
  | Ev_float f -> Reval.Value.of_float f
  | Ev_floats (n,arr) -> Reval.Value.of_floats arr 0 n
  | Ev_rect r ->
               let (x0,y0,x1,y1)=r in
               Reval.Value.of_floats2 [|x0;x1;x1;x0|] [|y0;y0;y1;y1|] 0 4
  | Ev_vector (x,y) -> Reval.Value.make_vector x y
  | _ -> Reval.Value.no_value

let element_value_as_float = function
  | Ev_float f -> f
  | x -> raise (Bad_type (Printf.sprintf "wanted a float but had a %s" (str_type_of_element_value x)))

let element_value_as_floats = function
  | Ev_float f        -> [|f;|]
  | Ev_floats (n,arr) -> arr
  | Ev_vector (f0,f1) -> [|f0;f1;|]
  | Ev_rect r         -> Primitives.Rectangle.as_floats r
  | x -> raise (Bad_type (Printf.sprintf "wanted floats but had a %s" (str_type_of_element_value x)))

let element_value_as_string = function
  | Ev_string s -> s
  | x -> raise (Bad_type (Printf.sprintf "wanted a string but had a %s" (str_type_of_element_value x)))

(*t t_style_resolver *)
type t_style_resolver = {
   value_as_float        : ?default:float -> string -> float;
   value_as_floats       : ?default:float array -> string -> float array;
   value_as_string       : ?default:string -> string -> string;
   value_as_color_string : ?default:float -> string -> string;
  }

(*m LayoutElementType *)
module type LayoutElementType = sig
    type et   (* base element type *)
    type rt   (* resolved styled of element type - does not include t *)
    type lt   (* layout of element type - does not include rt or t *)
    type gt   (* finalized geometry of element type - does not include lt, rt or t *)

    val styles       : (string * Stylesheet.Value.t_styleable_type * Stylesheet.Value.t_styleable_value * bool) list
    val resolve_styles : et -> t_style_resolver -> (rt * t_element_properties)
    val get_min_bbox : et -> rt -> Primitives.t_rect
    val make_layout_within_bbox : et -> rt -> Primitives.t_rect -> (lt * t_element_properties)
    val finalize_geometry : et -> rt -> lt -> t_style_resolver -> gt
    val render_svg   : et -> rt -> lt -> gt -> int -> Svg.t list
end

(*m LayoutElementBase - included in all LayoutElementType *)
module LayoutElementBase = struct
  let styles = Stylesheet.Value.[ ("class",        St_token_list,  sv_none_token_list, false);
                                  ("reval",        St_string,      sv_none_string, false);
                                  ("padding",      (St_floats 4),  Sv_floats (4,[|0.;0.;0.;0.;|]), false);
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

(*m LayoutElementFunc - create LayoutElement for aggregate from a LayoutElementType *)
module LayoutElementFunc (E:LayoutElementType) = struct
  type et = E.et
  type rt = E.rt
  type lt = E.lt
  type gt = E.gt
  let styles = E.styles
  let resolve_styles et res = (
      let layout_pl = ["padding",Ev_floats (4, res.value_as_floats "padding");
                       "margin",Ev_floats (4, res.value_as_floats "padding");
                       "border",Ev_floats (4, res.value_as_floats "padding");
                       "bbox",Ev_rect Primitives.Rectangle.zeros;
                      ] in
      let (rt,pl) = E.resolve_styles et res in (rt,pl@layout_pl)
    )
  let get_min_bbox = E.get_min_bbox
  let make_layout_within_bbox = E.make_layout_within_bbox
  let finalize_geometry = E.finalize_geometry
  let render_svg = E.render_svg
end

(*a Aggregate layout element modules *)
(*m LayoutElementAggrType *)
module type LayoutElementAggrType = sig
    type et
    type rt
    type lt
    type gt
    val styles       : (string * Stylesheet.Value.t_styleable_type * Stylesheet.Value.t_styleable_value * bool) list
    val style_desc   : Stylesheet.t_styleable_desc
    val type_name_et  : et -> string
    val type_name_rt  : rt -> string
    val type_name_lt  : lt -> string
    val type_name_gt  : gt -> string
    val resolve_styles : et -> t_style_resolver -> (rt * t_element_properties)
    val get_min_bbox : rt -> Primitives.t_rect
    val make_layout_within_bbox : rt -> Primitives.t_rect -> (lt * t_element_properties)
    val finalize_geometry : lt -> t_style_resolver -> gt
    val render_svg   : gt -> int -> Svg.t list
end

(*m ElementFunc - create the actual Element from a LayoutElementAggrType *)
module ElementFunc (LE : LayoutElementAggrType) = struct
    exception Eval_error of string

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
        properties : (string * t_element_value) list;
        layout_properties  : Layout.t_layout_properties;
        reval : Reval.t_reval;
        content_rt : rt list;
      }

    (*t etb - Bounded-box element type; resolved and desired geometry calculated *)
    type etb = {
        th : th;
        rt : LE.rt;
        properties : (string * t_element_value) list;
        layout_properties  : Layout.t_layout_properties;
        reval : Reval.t_reval;
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
        properties : (string * t_element_value) list;
        layout : Layout.t;
        ltr    : Layout.t_transform;
        reval : Reval.t_reval;
        content_lt : lt list;
        bbox : Primitives.t_rect;
      }

    (*t gt - Fully resolved geometry type with evaluations completed *)
    type gt = {
        th : th;
        gt : LE.gt;
        layout : Layout.t;
        ltr    : Layout.t_transform;
        content_gt : gt list;
        bbox : Primitives.t_rect;
      }

    (*f pp_attr *)
    let pp_attr ppf name value =
      Format.pp_print_string ppf (Printf.sprintf "%s=%s " name value);
      ()

    (*f pp_open *)
    let pp_open ppf name (th:th) =
      Format.pp_open_tag ppf name ;
      let id = th.id in
      pp_attr ppf "id" id;
      ()

    (*f pp_close *)
    let pp_close ppf _ =
      Format.pp_close_tag ppf ();
      ()

    (*f pp_element *)
    let rec pp_element ppf (et:et) =
      pp_open ppf (LE.type_name_et et.et) et.th;
      List.iter (fun (n,v) -> pp_attr ppf n v) et.properties;
      List.iter (pp_element ppf) et.content_et;
      pp_close ppf ()

    (*f pp_etb *)
    let rec pp_etb ppf (etb:etb) =
      pp_open ppf (LE.type_name_rt etb.rt) etb.th;
      Format.fprintf ppf "ele_bbox:%s" (Rectangle.str etb.element_bbox);
      Format.fprintf ppf "cont_bbox:%s" (Rectangle.str etb.content_bbox);
      Format.fprintf ppf "min_bbox:%s" (Rectangle.str etb.min_bbox);
      List.iter (pp_etb ppf) etb.content_etb;
      pp_close ppf ()

    (*f pp_layout *)
    let rec pp_layout ppf (lt:lt) =
      pp_open ppf (LE.type_name_lt lt.lt) lt.th;
      Format.fprintf ppf "bbox:%s" (Rectangle.str lt.bbox);
      List.iter (pp_layout ppf) lt.content_lt;
      pp_close ppf ()

    (*f pp_geometry *)
    let rec pp_geometry ppf (gt:gt) =
      pp_open ppf (LE.type_name_gt gt.gt) gt.th;
      Format.fprintf ppf "bbox:%s" (Rectangle.str gt.bbox);
      List.iter (pp_geometry ppf) gt.content_gt;
      pp_close ppf ()

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
      let styleable = Stylesheet.se_create LE.style_desc stylesheet (LE.type_name_et et.et) et.properties (fun _ -> ()) st_list in
      List.iter (Stylesheet.se_set_parent styleable) st_list;
      { th=et.th; et=et.et; styleable; content_st; }

    (*f resolve_styles <stylesheet> <element> : rt - resolve all the properties from the stylesheet ready for layout *)
    let rec resolve_styles stylesheet (st:st) : rt =
      let content_rt = List.map (resolve_styles stylesheet) st.content_st in
      let resolver = {
          value_as_float        = (fun ?default s -> Stylesheet.styleable_value_as_float ?default:default stylesheet st.styleable s);
          value_as_floats       = (fun ?default s -> Stylesheet.styleable_value_as_floats ?default:default stylesheet st.styleable s);
          value_as_string       = (fun ?default s -> Stylesheet.styleable_value_as_string ?default:default stylesheet st.styleable s);
          value_as_color_string = (fun ?default s -> Stylesheet.styleable_value_as_color_string stylesheet st.styleable s);
        } in
      let (rt, properties) = LE.resolve_styles st.et resolver in
      let layout_properties = Layout.make_layout_hdr stylesheet st.styleable in
      let reval_string = Stylesheet.styleable_value_as_string ~default:"" stylesheet st.styleable "reval" in
      let reval = 
        try Reval.make reval_string
        with Reval.Syntax_error s -> raise (Eval_error (Printf.sprintf "Syntax error '%s' when parsing '%s' for '%s'" s reval_string st.th.id))
      in
      { th=st.th; rt; properties; layout_properties; reval; content_rt}

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
      let properties = rt.properties in
      { th=rt.th; rt=rt.rt; properties; layout_properties=rt.layout_properties; reval=rt.reval; content_bbox; element_bbox; min_bbox; layout; content_etb }

    (*f make_layout_within_bbox - make lt from etb *)
    let rec make_layout_within_bbox (etb:etb) bbox = 
      let (ltr, bbox) = Layout.layout_within_bbox etb.layout bbox in
      let (lt, properties) = LE.make_layout_within_bbox etb.rt bbox in
      let layout_content_element (x : etb) =
        let bbox = Layout.get_bbox_element etb.layout ltr x.layout_properties x.min_bbox in
        make_layout_within_bbox x bbox
      in
      let content_lt = List.map layout_content_element etb.content_etb in
      let properties = properties @ etb.properties in
      { th=etb.th; lt; properties; reval=etb.reval; layout=etb.layout; ltr; content_lt; bbox;}

    (*f get_reval_value_from_properties pl -> s -> Reval.t option *)
    let get_reval_value_from_properties pl s =
      match list_find (fun (ps,pv)->String.equal s ps) pl with
      | Some (_,v) -> Some (reval_of_element_value v)
      | None -> None

    (*f finalize geometry - needs rev_stack *)
    let rec finalize_geometry rev_stack lt : gt =
      let content_gt = List.map (finalize_geometry (lt::rev_stack)) lt.content_lt in
      let find_child (lt:lt) id =
        match list_find (fun (x:lt)-> String.equal x.th.id id) lt.content_lt with
        | None -> raise Not_found
        | Some x -> x
      in
      let get_ref    (lt:lt) = lt.reval in
      let get_value  (lt:lt) s = get_reval_value_from_properties lt.properties s in
      let get_id     (lt:lt) = lt.th.id in
      let tres = Reval.make_resolver find_child get_ref get_value get_id in
      ignore (Reval.resolve_all tres lt rev_stack);
      let resolver = {
          value_as_float        = (fun ?default _ -> 0.);
          value_as_floats       = (fun ?default _ -> [|0.|]);
          value_as_string       = (fun ?default _ -> "");
          value_as_color_string = (fun ?default _ -> "black");
        } in
      let gt = LE.finalize_geometry lt.lt resolver in
      { th=lt.th; gt=gt; layout=lt.layout; ltr=lt.ltr; content_gt; bbox=lt.bbox;}

    (*f show_layout *)
    let rec show_layout gt indent =
      Printf.printf "%sid '%s' : bbox '%s'\n" indent gt.th.id (Rectangle.str gt.bbox);
      let indent = String.concat "" ["  "; indent] in
      List.iter (fun x -> show_layout x indent) gt.content_gt

    (*f layout_elements *)
    let layout_elements stylesheet page_bbox et =
        let st  = make_styleable stylesheet et in
        ignore (Stylesheet.build stylesheet [st.styleable]);
        Stylesheet.apply stylesheet;
        let rt  = resolve_styles stylesheet st in
        let etb = make_min_bbox rt in

    pp_etb Format.std_formatter etb ;
    Format.print_flush ();
    Printf.printf "\n";

        let lt  = make_layout_within_bbox etb page_bbox in
        let gt  = finalize_geometry [] lt in
        gt

    (*f render_svg gt index - return a list of SVG tags that make up the element *)
    let rec render_svg gt zindex =
      let content_svg   = List.fold_left (fun a x -> a @ (render_svg x zindex)) [] gt.content_gt in
      let element_svg   = LE.render_svg gt.gt zindex in
      Layout.render_svg gt.layout gt.ltr (content_svg @ element_svg) 

    (*f All done *)

end



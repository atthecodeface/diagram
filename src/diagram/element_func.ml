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
module Color     = Primitives.Color
module Rectangle = Primitives.Rectangle
module FloatElt = struct type t=float let compare  = compare end
module FloatSet = Set.Make(FloatElt)
open Types

exception Eval_error of string

(*a Utility functions*)
(*f pp_attr *)
let pp_attr ppf name value =
  Format.pp_print_string ppf (Printf.sprintf "%s=%s " name value);
  ()

(*f pp_open *)
let pp_open ppf name (th:t_hdr) =
  Format.pp_open_tag ppf name ;
  let id = th.id in
  pp_attr ppf "id" id;
  ()

(*f pp_close *)
let pp_close ppf _ =
  Format.pp_close_tag ppf ();
  ()

(*f properties_value 'a evfn -> pl -> s -> 'a option *)
let properties_value evfn pl s =
  match Utils.list_find (fun (ps,pv)->String.equal s ps) pl with
  | Some (_,v) -> Some (evfn v)
  | None   -> None

(*f properties_value_as_float pl -> s -> float option *)
let properties_value_as_float = properties_value Element_value.as_float

(*f properties_value_as_floats pl -> s -> float array option *)
let properties_value_as_floats = properties_value Element_value.as_floats

(*f get_eval_value_from_properties pl -> s -> Eval.t option *)
let get_eval_value_from_properties = properties_value Element_value.eval_value_of

(*a Types - through a functor so it can be used in the submodules  *)
module TypeFunc(LE : LayoutElementAggrType) = struct
  (*t t_base - Element type, with its header and content *)
  type t_base = {
      th : t_hdr;
      properties: (string * string) list;
      et : LE.et;
      content : t_base list;
    }

  (*t t_styleable - Styleable element type; something to which stylesheet may be applied *)
  type t_styleable = {
      th : t_hdr;
      et : LE.et;
      styleable  : Stylesheet.t_styleable;
      content_st : t_styleable list;
    }

  (*t t_resolved_style - Resolved styled element type; all properties of this have full values *)
  type t_resolved_style = {
      th : t_hdr;
      rt : LE.rt;
      properties : (string * t_element_value) list;
      layout_properties  : Layout.t_layout_properties;
      eval : Eval.t_eval;
      content_rt : t_resolved_style list;
    }

  (*t t_desired_geometry - Bounded-box element type; resolved and desired geometry calculated *)
  type t_desired_geometry = {
      th : t_hdr;
      rt : LE.rt;
      properties : (string * t_element_value) list;
      layout_properties  : Layout.t_layout_properties;
      eval : Eval.t_eval;
      element_geom : t_ref_bbox; (* Desired geometry of element *)
      des_geom     : t_ref_bbox; (* Minimum bbox relateive to element ref point *)
      layout       : Layout.t;
      content_etb  : t_desired_geometry list;
    }

  (*t t_layout - Post-layout element type; actual bounding box and content post-layout *)
  type t_layout = {
      th : t_hdr;
      lt : LE.lt;
      properties : (string * t_element_value) list;
      layout : Layout.t;
      ltr    : Layout.t_transform;
      eval : Eval.t_eval;
      content_lt : t_layout list;
      geom : t_ref_bbox; (* for the thing *)
      content_geom : t_ref_bbox; (* For all the content_lt if there are any *)
    }

  (*t t_finalized_geometry - Fully resolved geometry type with evaluations completed *)
  type t_finalized_geometry = {
      th : t_hdr;
      gt : LE.gt;
      layout : Layout.t;
      ltr    : Layout.t_transform;
      content_gt : t_finalized_geometry list;
      geom : t_ref_bbox;
    }

end

(*a Submodule Functors - one for each stage of the build *)
(*m BaseFunc - constructed element with its contents *)
module BaseFunc (LE : LayoutElementAggrType) = struct
  module Types=TypeFunc(LE)
  type t = Types.t_base
  (*f pp *)
  let rec pp ppf (et:t) =
    pp_open ppf (LE.type_name_et et.et) et.th;
    List.iter (fun (n,v) -> pp_attr ppf n v) et.properties;
    List.iter (pp ppf) et.content;
    pp_close ppf ()

  (*f make ... : et - construct the hierarchy within the stylesheet *)
  let make properties et content : t =
    let id:string = if List.mem_assoc "id" properties then (List.assoc "id" properties) else "no_id" in
    let th = Primitives.th_make id in
    { th; properties; et; content}

end

(*m StyledFunc - styled element with styled contents *)
module StyledFunc (LE : LayoutElementAggrType) = struct
  module Types=TypeFunc(LE)
  type t = Types.t_styleable

  (*v create_stylesheet : unit -> t_stylesheet - performed once for the whole document, not every element *)
  let create_stylesheet _ = 
    let stylesheet = Stylesheet.create () in
    Stylesheet.add_style_defaults stylesheet LE.styles;
    stylesheet

  (*f make_styleable <stylesheet> <element> : st - make contents' styleables, and get a styleable for this element *)
  let rec make_styleable stylesheet (et:Types.t_base) : t =
    let content_st = List.map (make_styleable stylesheet) et.content in
    let st_list = List.map (fun (st:t) -> st.styleable) content_st in
    let styleable = Stylesheet.se_create LE.style_desc stylesheet (LE.type_name_et et.et) et.properties (fun _ -> ()) st_list in
    List.iter (Stylesheet.se_set_parent styleable) st_list;
    { th=et.th; et=et.et; styleable; content_st; }

end

(*m ResolvedStyledFunc *)
module ResolvedStyledFunc (LE : LayoutElementAggrType) = struct
  module Types=TypeFunc(LE)
  type t = Types.t_resolved_style

  (*f resolve_styles <stylesheet> <element> : rt - resolve all the properties from the stylesheet ready for layout *)
  let rec resolve_styles stylesheet (st:Types.t_styleable) : t =
    let content_rt = List.map (resolve_styles stylesheet) st.content_st in
    let resolver = {
        value_as_float        = (fun ?default s -> Stylesheet.styleable_value_as_float ?default:default stylesheet st.styleable s);
        value_as_floats       = (fun ?default s -> Stylesheet.styleable_value_as_floats ?default:default stylesheet st.styleable s);
        value_as_string       = (fun ?default s -> Stylesheet.styleable_value_as_string ?default:default stylesheet st.styleable s);
        value_as_color_string = (fun ?default s -> Stylesheet.styleable_value_as_color_string ?default:default stylesheet st.styleable s);
      } in
    let (rt, properties) = LE.resolve_styles st.et resolver in
    let layout_properties = Layout.make_layout_hdr stylesheet st.styleable in
    let eval_string = Stylesheet.styleable_value_as_string ~default:"" stylesheet st.styleable Attr_names.eval in
    let eval = 
      try Eval.make eval_string
      with e -> (
        match (Eval.eval_error eval_string e) with
        | Some s -> raise (Eval_error (Printf.sprintf "In parsing eval for %s: %s" st.th.id s))
        | None -> raise e
      )
    in
    { th=st.th; rt; properties; layout_properties; eval; content_rt}

end

(*m DesiredGeometryFunc *)
module DesiredGeometryFunc (LE : LayoutElementAggrType) = struct
  module Types=TypeFunc(LE)
  type t = Types.t_desired_geometry

  (*f layout_content_create - create layout using any necessary et properties and etb content *)
  let layout_content_create (rt:Types.t_resolved_style) element_geom etb_list =
    let content_layout_properties_bbox = List.map (fun (x:t)->(x.layout_properties,x.des_geom)) etb_list in
    Layout.create rt.layout_properties element_geom content_layout_properties_bbox 

  (*f make rt : etb - create a structure with the min_bbox of the element given its properties

    The layout (for the children) has a width/height and a reference point
    The element has a width/height and a reference point (possibly not much of one)

    They can be merged by creating a desired geometry with reference (0.,0.) and a union of the bbox sizes

   *)
  let rec make (rt:Types.t_resolved_style) : t =
    let content_etb   = List.map make rt.content_rt in
    let element_geom  = LE.get_desired_geometry rt.rt in
    let layout        = layout_content_create rt element_geom content_etb in
    let des_geom      = Layout.get_desired_geometry layout in
    let properties = rt.properties in
    { th=rt.th; rt=rt.rt; properties; layout_properties=rt.layout_properties; eval=rt.eval; element_geom; des_geom; layout; content_etb }

  (*f get_des_geom *)
  let get_des_geom (etb:t) = etb.des_geom

  (*f pp *)
  let rec pp ppf (etb:t) =
    Format.fprintf ppf "@[<v 3>";
    pp_open ppf (LE.type_name_rt etb.rt) etb.th;
    Format.fprintf ppf "element geom: %s " (Desired_geometry.str etb.element_geom);
    Format.fprintf ppf "combined geom (inc border etc): %s " (Desired_geometry.str etb.des_geom);
    Format.fprintf ppf "@,";
    List.iter (pp ppf) etb.content_etb;
    pp_close ppf ();
    Format.fprintf ppf "@]@,";
    ()

end

(*m LayoutFunc *)
module LayoutFunc (LE : LayoutElementAggrType) = struct
  module Types=TypeFunc(LE)
  type t = Types.t_layout

  (*f make_layout_with_geometry - make lt from etb *)
  let rec make_layout_with_geometry (etb:Types.t_desired_geometry) geom : t= 
    let (ltr, content_geom, layout_properties) = Layout.layout_with_geometry etb.layout geom in
    let (lt, properties) = LE.make_layout_with_geometry etb.rt content_geom in
    let layout_content_element (x : Types.t_desired_geometry) =
      let geom = Layout.get_geom_element etb.layout ltr x.layout_properties x.des_geom in
      make_layout_with_geometry x geom
    in
    let content_lt = List.map layout_content_element etb.content_etb in
    let properties = properties @ layout_properties @ etb.properties in
    { th=etb.th; lt; properties; eval=etb.eval; layout=etb.layout; ltr; content_lt; geom; content_geom}

  (*f pp *)
  let rec pp ppf (lt:t) =
    Format.fprintf ppf "@[<v 3>";
    pp_open ppf (LE.type_name_lt lt.lt) lt.th;
    Format.fprintf ppf "geom:%s " (Desired_geometry.str lt.geom);
    Format.fprintf ppf "content geom:%s " (Desired_geometry.str lt.content_geom);
    Format.fprintf ppf "@,";
    List.iter (pp ppf) lt.content_lt;
    Format.fprintf ppf "@]@,";
    pp_close ppf ();
    ()

end

(*m FinalizedGeometryFunc *)
module FinalizedGeometryFunc (LE : LayoutElementAggrType) = struct
  module Types=TypeFunc(LE)
  type t = Types.t_finalized_geometry
  type lt = Types.t_layout

  (*f pp *)
  let rec pp ppf (gt:t) =
    Format.fprintf ppf "@[<v 3>";
    pp_open ppf (LE.type_name_gt gt.gt) gt.th;
    Format.fprintf ppf "finalized geometry:%s " (Desired_geometry.str gt.geom);
    Format.fprintf ppf "@,";
    List.iter (pp ppf) gt.content_gt;
    Format.fprintf ppf "@]@,";
    pp_close ppf ();
    ()

  (*f finalize_value ?default -> 'a rvfn -> 'a evfn -> lt -> string -> 'a *)
  let finalize_value ?default rvfn evfn (lt:Types.t_layout) s =
    match Eval.value_of lt.eval s (fun _ -> Eval.no_value) with
    | Some x -> rvfn x
    | None -> (
      match properties_value evfn lt.properties s with
      | Some x -> x
      | None-> ( match default with |Some x-> x | None -> raise (Eval_error (Printf.sprintf "No default to finalize_value '%s'" s)))
    )

  (*f finalize_value_as_float *)
  let finalize_value_as_float ?default ~lt:lt s =
    finalize_value ?default:default Eval.value_as_float Element_value.as_float lt s

  (*f finalize_value_as_string *)
  let finalize_value_as_string ?default ~lt:lt s =
    finalize_value ?default:default (fun _ -> "") Element_value.as_string lt s

  (*f finalize_value_as_color_string *)
  let finalize_value_as_color_string ?default ~lt:lt s =
    finalize_value ?default:default (fun _ -> "") Element_value.as_string lt s

  (*f finalize_value_as_floats *)
  let finalize_value_as_floats ?default ~lt:lt s =
    finalize_value ?default:default Eval.value_as_floats Element_value.as_floats lt s

  (*f finalize_resolver lt -> resolver *)
  let finalize_resolver lt = {
      value_as_float        = finalize_value_as_float ~lt:lt;
      value_as_floats       = finalize_value_as_floats ~lt:lt;
      value_as_string       = finalize_value_as_string ~lt:lt;
      value_as_color_string = finalize_value_as_color_string ~lt:lt;
    }

  (*f finalize geometry - needs rev_stack *)
  let rec finalize_geometry rev_stack (lt:Types.t_layout) : t =
    let content_gt = List.map (finalize_geometry (lt::rev_stack)) lt.content_lt in
    let find_child (lt:lt) id =
      match Utils.list_find (fun (x:lt)-> String.equal x.th.id id) lt.content_lt with
      | None -> raise Not_found
      | Some x -> x
    in
    let get_ref    (lt:lt) = lt.eval in
    let get_value  (lt:lt) s = get_eval_value_from_properties lt.properties s in
    let get_id     (lt:lt) = lt.th.id in
    let tres = Eval.make_resolver find_child get_ref get_value get_id in
    ignore (Eval.resolve_all tres lt rev_stack);
    let resolver = finalize_resolver lt in
    let gt = LE.finalize_geometry lt.lt resolver in
    { th=lt.th; gt=gt; layout=lt.layout; ltr=lt.ltr; content_gt; geom=lt.geom;}

  (*f find_zindices - add z_indices to the set *)
  let rec find_zindices z_indices (gt:t)  =
    let z_index = Layout.get_z_index gt.layout in
    let z_indices = FloatSet.add z_index z_indices in
    List.fold_left find_zindices z_indices gt.content_gt

  (*f render_svg gt index - return a list of SVG tags that make up the element *)
  let rec render_svg (gt:t) z_index =
    let content_svg   = List.fold_left (fun a x -> a @ (render_svg x z_index)) [] gt.content_gt in
    let make_element_svg = LE.render_svg gt.gt in
    Layout.render_svg gt.layout gt.ltr gt.th.id z_index make_element_svg content_svg

end

(*a Module Functor - combines the above submodules as steps *)
(*m ElementFunc - create the actual Element from a LayoutElementAggrType *)
module ElementFunc (LE : LayoutElementAggrType) = struct

    module Base = BaseFunc(LE)
    type et = Base.t
    let make_et = Base.make

    module Styled = StyledFunc(LE)
    module ResolvedStyled = ResolvedStyledFunc(LE)
    module DesiredGeometry = DesiredGeometryFunc(LE)
    module Layout = LayoutFunc(LE)
    module FinalizedGeometry = FinalizedGeometryFunc(LE)

    let create_stylesheet = Styled.create_stylesheet
    let pp_element  = Base.pp
    let pp_layout   = Layout.pp
    let pp_desired  = DesiredGeometry.pp
    let pp_geometry = FinalizedGeometry.pp
    let make_styleable = Styled.make_styleable
    let resolve_styles = ResolvedStyled.resolve_styles
    let make_desired_geometry = DesiredGeometry.make
    let get_desired_geometry = DesiredGeometry.get_des_geom
    let make_layout_with_geometry = Layout.make_layout_with_geometry
    let finalize_geometry = FinalizedGeometry.finalize_geometry
    let render_svg gt =
      let z_indices = FloatSet.empty in
      let z_indices = FinalizedGeometry.find_zindices z_indices gt in
      let z_indices = FloatSet.elements z_indices in
      let svg_list = List.map (FinalizedGeometry.render_svg gt) z_indices in
      List.concat svg_list

    (*f show_layout *)
    type gt = FinalizedGeometry.t
    let rec show_layout (gt:gt) indent =
      Printf.printf "%sid '%s' : bbox '%s'\n" indent gt.th.id (Desired_geometry.str gt.geom);
      let indent = String.concat "" ["  "; indent] in
      List.iter (fun x -> show_layout x indent) gt.content_gt

    (*f prepare_elements *)
    let prepare_elements stylesheet (et:et) =
      let st  = make_styleable stylesheet et in
      ignore (Stylesheet.build stylesheet [st.styleable]);
      Stylesheet.apply stylesheet;
      let rt  = resolve_styles stylesheet st in
      let etb = make_desired_geometry rt  in
      etb

    (*f get_prepared_size *)
    let get_prepared_size etb =
      let des_geom = get_desired_geometry etb in
      let (_,_,w,h) = Desired_geometry.get_cwh des_geom in
      (w, h)

    (*f layout_elements *)
    let layout_elements page_geom etb =
      let lt  = make_layout_with_geometry etb page_geom in
      let gt  = finalize_geometry [] lt in
      (lt, gt)

    (*f All done *)

end

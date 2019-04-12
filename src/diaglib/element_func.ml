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
open Types

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
        eval : Eval.t_eval;
        content_rt : rt list;
      }

    (*t etb - Bounded-box element type; resolved and desired geometry calculated *)
    type etb = {
        th : th;
        rt : LE.rt;
        properties : (string * t_element_value) list;
        layout_properties  : Layout.t_layout_properties;
        eval : Eval.t_eval;
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
        eval : Eval.t_eval;
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
    let make_et properties et content_et : et =
        let id:string = if List.mem_assoc "id" properties then (List.assoc "id" properties) else "no_id" in
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
      { th=rt.th; rt=rt.rt; properties; layout_properties=rt.layout_properties; eval=rt.eval; content_bbox; element_bbox; min_bbox; layout; content_etb }

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
      { th=etb.th; lt; properties; eval=etb.eval; layout=etb.layout; ltr; content_lt; bbox;}

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

    (*f finalize_value ?default -> 'a rvfn -> 'a evfn -> lt -> string -> 'a *)
    let finalize_value ?default rvfn evfn lt s =
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
    let rec finalize_geometry rev_stack lt : gt =
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
      Layout.render_svg gt.layout gt.ltr gt.th.id (content_svg @ element_svg) 

    (*f All done *)

end

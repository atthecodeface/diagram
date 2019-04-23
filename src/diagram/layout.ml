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

open Types
open Primitives

(*a Types for layout *)
(*t t_layout_properties *)
type t_layout_properties = {
        place             : t_vector option; (* If placed this is where reference point is placed *)
        width             : t_vector option; (* If None then no min/max width; else min/max width *)
        height            : t_vector option; (* If None then no min/max height; else min/max height *)
        expand            : t_vector; (* Amount of slack to take up in each direction, 0 to 1 *)
        anchor            : t_vector; (* If does not fill its outer bbox, then where it is anchored to (-1 top/left, 1 bottom/right) *)
        grid              : t_int4 option;   (* grid elements to cover w=1, h=1 are a single cell *)
        z_index           : float;
        padding           : t_rect;
        border            : t_rect;
        margin            : t_rect;
        border_fill       : Primitives.Color.t;
        border_color      : Primitives.Color.t;
        border_round      : float option; 
        magnets_per_side  : int; 
       (* 
    rotation
    scale (x,y)
*)
}

(*f make_layout_hdr stylesheet styleable - get actual data from the provided properties *)
let make_layout_hdr stylesheet styleable =
  let grid_value_of_n n ints =
    match n with | 0 -> (0,0,0,0)
                 | 1 -> (ints.(0),ints.(0),1,1)
                 | _ -> (ints.(0),ints.(1),1,1)
  in
  let wh_value_of_n n floats =
    match n with | 0 -> (0.,1.E20)
                 | _ -> (floats.(0),1.E20)
  in
  let pbm_value_of_n n floats =
    match n with | 0 -> (0.,0.,0.,0.)
                 | 1 -> (floats.(0),floats.(0),floats.(0),floats.(0))
                 | _ -> (floats.(0),floats.(1),floats.(0),floats.(1))
  in
  let open Properties in
  let magnets_per_side = get_property_int    stylesheet styleable            Attr_names.magnets_per_side in
  let place            = get_property_vector_option stylesheet styleable     Attr_names.place in
  let width            = get_property_vector_option ~value_of_n:wh_value_of_n stylesheet styleable     Attr_names.width in
  let height           = get_property_vector_option ~value_of_n:wh_value_of_n stylesheet styleable     Attr_names.height in
  let expand           = get_property_vector stylesheet styleable            Attr_names.expand in
  let anchor           = get_property_vector stylesheet styleable            Attr_names.anchor in
  let grid             = get_property_int4_option  ~value_of_n:grid_value_of_n stylesheet styleable     Attr_names.grid in
  let z_index          = get_property_float  stylesheet styleable            Attr_names.z_index in
  let padding          = get_property_rect  ~value_of_n:pbm_value_of_n stylesheet styleable            Attr_names.padding in
  let border           = get_property_rect  ~value_of_n:pbm_value_of_n stylesheet styleable            Attr_names.border in
  let margin           = get_property_rect  ~value_of_n:pbm_value_of_n stylesheet styleable            Attr_names.margin in
  let border_fill      = get_property_color  stylesheet styleable            Attr_names.border_fill in
  let border_color     = get_property_color  stylesheet styleable            Attr_names.border_color in
  let border_round     = get_property_float_option  stylesheet styleable     Attr_names.border_round in
(* content_transform and content_inv_transform *)
{
  place; width; height; anchor; expand; grid; z_index;
  padding; border; margin; border_fill; border_color; border_round; magnets_per_side;
}

(*f props_min_max *)
let props_min_max mm_opt m =
  match mm_opt with 
  | None -> m
  | Some (min, max) ->
    let m = if (m<min) then min else m in
    let m = if (m>max) then max else m in
    m

(*f is_placed *)
let is_placed t = 
  match t.grid with | None -> true | _ -> false

(*f expand_bbox - expand a bbox by the padding, border and margin *)
let expand_bbox t bbox = Rectangle.(expand (expand (expand bbox t.padding) t.border) t.margin)

(*a Types *)
(*t t - structure of a desired layout
  props is the layout properties used - these are not recalculated for placement
  grids is a pair of GridDimensions that are the table layout for X and Y
  des_geom is the desired geometry for the layout
    This includes border, padding and margin.
      This is at least the width and height of the table layout.
        The table layout is assumed to have a reference point of 0,0 and bounds +-(w,h)/2
      This is at least the width and height of the non-grid items.
        A non-grid item may have a placement for where is reference point should go
        It will have a reference point at the centre of the bounding box of all placed items.
        If there are no placed items then the bounding box is (0,0,0,0)
      The resulting geometry has a bbox of (px0,py0,px1,py1) and a reference point at the centre of that.
    After border, padding and margin (which affect bounding box and reference point) the width/height is bounded with min/max width/height. If this changes a dimension then the reference point moves by half that amount.
    
 *)
type t = {
    props  : t_layout_properties;
    grids  : Grid.t_placement array;
    des_geom : t_ref_bbox;
  }

(*t t_transform *)
type t_transform = {
translate : t_vector;
scale     : t_vector;
geom      : t_ref_bbox; (* bounding box to be displayed in - do border/fill of this *)
content_geom   : t_ref_bbox; (* bounding box for content to be displayed in *)
    grids  : Grid.t_layout array;
  }

(*a Toplevel Layout module *)
let default_translate = (0.,0.)
let default_scale     = (1.,1.)

(*f create : t_layout_properties -> (t_layout_properties * t_desired_geometry) list -> t

  Create the appropriate layout for the content (not this element) and determine the min_bbox given properties of this
  and the properties and min_bbox of contents

  Start off with no desired placed geometry
  If a child has a grid then add its dimensions to the grid_x and grid_y
  Else if it is placed then its (ref,bbox) contributes as (bbox rel to ref + place)

  Calculate the required width/height for the grid (if any)

  Create final bbox that is centred on placed bbox center; (0,0) if none
  Final geometry is reference point place centre, bbox final bbox
 *)
let build_layout_data acc (cp,des_geom) =
  let (gcl, grl, place_bbox) = acc in
  match cp.grid with
  | None -> (
  (* if it has a placement  then not 0,0 ...*)
    let placement = (0., 0.) in
    let child_bbox = Primitives.Rectangle.translate (Desired_geometry.get_drext des_geom) placement in
    let place_bbox = Primitives.Rectangle.union place_bbox child_bbox in
    (gcl, grl, place_bbox)
  )
  | Some (cs,rs,cw,rw) -> (
    let (_,_,w,h) = Desired_geometry.get_cwh des_geom in
    let gcl = (cs,cw,w)::gcl in
    let grl = (rs,rw,h)::grl in
    (gcl, grl, place_bbox)
  )

let create props children_props_geom =
  let expand_default = 1. in
  let expand = [] in
  let (gcdx, gcdy, place_bbox) = List.fold_left build_layout_data ([],[],Primitives.Rectangle.zeros) children_props_geom in
  let grids  = Array.map (Grid.make_placement expand_default expand) [|gcdx; gcdy|] in
  let grid_sizes  = Array.map Grid.get_placement_size  grids  in
  let (pcx,pcy,pw,ph) = Primitives.Rectangle.get_cwh place_bbox in
  let cont_w = max grid_sizes.(0) pw in
  let cont_h = max grid_sizes.(1) ph in
  let cont_w = props_min_max props.width  cont_w in
  let cont_h = props_min_max props.height cont_h in
  let min_bbox = Primitives.Rectangle.of_cwh (pcx, pcy, cont_w, cont_h) in
  let cont_geom = Desired_geometry.make (pcx,pcy) min_bbox in
  let des_geom = Desired_geometry.(expand (expand (expand cont_geom props.padding) props.border) props.margin) in
  {props; grids; des_geom;}

(*f get_z_index - get the z_index *)
let get_z_index t = t.props.z_index

(*f get_desired_geometry - get the minimum bbox required by the content given grid and placemet (previously created as t) *)
let get_desired_geometry t = t.des_geom

(*f layout_with_geometry
  shrink content bbox by padding/border/margin
  anchor/expand desired geometry into outer geometry and generate geometry in parent coordinates

  The returned content_geom should clearly be inside geom
 *)
let layout_with_geometry t geom =
  let border_geom   = Desired_geometry.(shrink geom        t.props.margin) in
  let padded_geom   = Desired_geometry.(shrink border_geom t.props.border) in
  let internal_geom = Desired_geometry.(shrink padded_geom t.props.padding) in
  let content_geom  = Desired_geometry.fit_within internal_geom t.des_geom t.props.anchor t.props.expand in
  let translate = default_translate in
  let scale     = default_scale in
  let (ccx,ccy,cw,ch) = Desired_geometry.get_cwh content_geom in
  let gx = Grid.make_layout t.grids.(0) ccx cw in
  let gy = Grid.make_layout t.grids.(1) ccy ch in
  let grids = [|gx; gy|] in
  let transform = {translate; scale; geom=geom; content_geom; grids;} in (* bbox is used for the border generation - should include updated grid *)
    let magnets =
      let (x0,y0,x1,y1)=Desired_geometry.get_bbox geom in
      if (t.props.magnets_per_side>1) then (
        let mps = t.props.magnets_per_side-1 in
        let fmps = float mps in
        let dx = (x1 -. x0) /. (float mps) in
        let dy = (y1 -. y0) /. (float mps) in
        let x_of_i i = if (i<=mps) then (float i) else if (i<=2*mps) then fmps            else if (i<=3*mps) then fmps-.(float (i-2*mps)) else 0. in
        let y_of_i i = if (i<=mps) then 0.        else if (i<=2*mps) then (float (i-mps)) else if (i<=3*mps) then fmps                    else fmps-.(float (i-3*mps)) in
        let xs = Array.init (mps*4) (fun i->let x=x_of_i i in x0+.dx*.x) in
        let ys = Array.init (mps*4) (fun i->let y=y_of_i i in y0+.dy*.y) in
        Ev_vectors ((mps*4),xs,ys)
      ) else (
        Ev_vectors (4,[|x0;x0;x1;x1|],[|y0;y1;y1;y0|])
      )
    in
    let layout_pl = [Attr_names.outer_bbox,Ev_rect    (Desired_geometry.get_bbox geom);
                     Attr_names.border_bbox,Ev_rect   (Desired_geometry.get_bbox border_geom);
                     Attr_names.padded_bbox,Ev_rect   (Desired_geometry.get_bbox padded_geom);
                     Attr_names.content_bbox,Ev_rect  (Desired_geometry.get_bbox content_geom);
                     Attr_names.magnets, magnets;
                      ] in
    (transform, content_geom, layout_pl)

let get_geom_element t tr cp des_geom = 
  match cp.grid with
  | None -> (
    tr.content_geom (* as we cannot place things yet *)
  )
  | Some (cs,rs,cw,rw) -> (
    let (x0,x1) = Grid.get_layout_bbox tr.grids.(0) cs cw in
    let (y0,y1) = Grid.get_layout_bbox tr.grids.(1) rs rw in
    let bbox = (x0,y0,x1,y1) in
    let (cx,cy,_,_) = Primitives.Rectangle.get_cwh bbox in
    Printf.printf "get_geom_element %g,%g : %g,%g,%g,%g\n" cx cy x0 y0 x1 y1;
    Desired_geometry.make (cx,cy) bbox
  )

let translate_string tr =
    if tr.translate==default_translate then "" else 
    let (dx,dy) = tr.translate in
    Printf.sprintf "translate(%g %g)" dx dy

let scale_string tr =
    if tr.scale==default_scale then "" else 
    let (dx,dy) = tr.scale in
    Printf.sprintf "scale(%g %g)" dx dy

let add_transform_tag tr tags =
    let t = translate_string tr in
    let s = scale_string tr in
    (Svg.attribute_string "transform" (String.concat " " [t; s])) :: tags

let path_ele t coords = String.concat " " (t::(List.map (Printf.sprintf "%g") coords))

let svg_border_path_coords t (tr:t_transform) =
  let (x0,y0,x1,y1) = Rectangle.(shrink ~scale:(0.5) (shrink (Desired_geometry.get_bbox tr.geom) t.props.margin) t.props.border)  in
  let path_string = 
    match t.props.border_round with
    | None -> Printf.sprintf "M%g %g L%g %g L%g %g L%g %g Z" x0 y0 x1 y0 x1 y1 x0 y1
    | Some r ->
     String.concat " " [
     path_ele "M" [(x0+.r); y0];
     path_ele "L" [(x1-.r); y0];
     path_ele "Q" [x1; y0; x1; (y0+.r)];
     path_ele "L" [x1; (y1-.r)];
     path_ele "Q" [x1; y1; (x1-.r); y1];
     path_ele "L" [(x0+.r); y1];
     path_ele "Q" [x0; y1; x0; (y1-.r)];
     path_ele "L" [x0; (y0+.r)];
     path_ele "Q" [x0; y0; (x0+.r); y0];
    "Z"]
  in
  Svg.attribute_string "d" path_string

let svg_prepend_fill t tr s =
  let coords = svg_border_path_coords t tr in
  let (bw,_,_,_) = t.props.border in
  if (Color.is_none t.props.border_fill) then s else 
    let stroke = Svg.attribute_string "stroke" "none" in
    let stroke_width = Svg.attribute_string "stroke-width" (Printf.sprintf "%g" bw) in
    let fill   = Color.svg_attr "fill" t.props.border_fill in
    let path = Svg.tag "path" [stroke; fill; stroke_width; coords] [] [] in
    path :: s

let svg_append_border t tr s =
  let coords = svg_border_path_coords t tr in
  let (bw,_,_,_) = t.props.border in
  if (Color.is_none t.props.border_color) then s else 
    let stroke = Color.svg_attr "stroke" t.props.border_color in
    let stroke_width = Svg.attribute_string "stroke-width" (Printf.sprintf "%g" bw) in
    let fill   = Svg.attribute_string "fill" "none" in
    let path = Svg.tag "path" [stroke; fill; stroke_width; coords] [] [] in
    s @ [path]

let render_svg t tr id z_index make_element_svg svg_contents =
  let (id_attrs,svg_contents) =
    if t.props.z_index = z_index then (
      let svg_contents = (make_element_svg z_index) @ svg_contents in
      let svg_contents = svg_prepend_fill  t tr svg_contents in
      let svg_contents = svg_append_border t tr svg_contents in
      ([Svg.attribute_string "id" id], svg_contents)
    ) else ( [], svg_contents )
  in
    match svg_contents with 
    | [] -> []
    | s -> [ Svg.tag "g" (add_transform_tag tr id_attrs) s []]

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

(*a Grid and place modules *)
(*m Grid *)
module Grid = struct
  (*t t - structure for a grid - a list of start, span, and height of each cell *)
  type t = {
      cell_data : (int * int * float) list ; (* list of (start row * number of rows (>=1) * height in pixels) *)
      positions : (int * float) list;
    }

  (*f sort_by_start_index - sort the data by starting index *)
  let sort_by_start_index cell_data =
    List.sort (fun (s0,_,_) (s1,_,_) -> compare s0 s1) cell_data

  (*f find_min_size - find the shortest height in cell_data starting
    at the specified row;
    
    If there are any cells that start at a row
    after first_row but all cells starting at first_row span beyond
    those, then first_row can be zero height.

    If there are no such cells then find the cells that have the
    minimum span; then of these we need the largest of their sizes, in
    order to fit that cell in. This will be the min height for first_row
    then.

  *)
  let find_min_size cell_data first_row =
    let acc_if_smaller acc (s0,h0,size) =
      let (min_height, current_next) = acc in
      (* Printf.printf "acc_if_smaller %d %f %d %d %d %f\n" first_row min_height current_next s0 h0 size; *)
      if ((s0 > first_row) && (s0 < current_next)) then
        (0., s0)
      else if (s0==first_row) && (s0+h0<current_next) then
        (size, s0+h0)
      else if (s0==first_row) && (s0+h0==current_next) && (size>min_height) then
        (size, current_next)
      else
        acc
    in
    List.fold_left acc_if_smaller (0.,max_int) cell_data

  (*f remove_rows - remove the span of rows 'first_row' through
    'next_row' given that they have the specified size

    Any cell that starts at first_row can have next_row-first_row rows
    removed from its span: if a cell does not start at first_row then
    it will not overlap with the range; if it does, then remove size
    from its height and changes its start to begin at next_row (since
    the span first_row to next_row had size height).

  *)
  let remove_rows sd first_row next_row row_size =
    let n = next_row - first_row in
    let remove_row acc row =
      let (s0,h0,size) = row in
      if (s0>first_row) then (row::acc)
      else if (h0<=n) then acc
      else if (size<=row_size) then (next_row, h0-n, 0.)::acc
      else (next_row, h0-n, size-.row_size)::acc
    in
    List.fold_left remove_row [] sd

  (*f find_next_row_position - find the minimum height and next given
    the current row, then set the row positions and remove the span
    height from the cell data, and move on 

   *)
  let rec find_next_row_position acc sd first_row current_posn =
    (*
    List.iter (fun (s,h,size) -> Printf.printf "Row %d span %d size %f\n" s h size) sd;
    Printf.printf "\n";
     *)
    if (List.length sd)==0 then acc else (
      let (size, next_row) = find_min_size sd first_row in
      (* Printf.printf "find_min_size %d returned %f %d\n" first_row size next_row; *)
      let posn = current_posn +. size in
      let sd = remove_rows sd first_row next_row size in
      let acc = (next_row, posn)::acc in
      find_next_row_position acc sd next_row posn
    )

  (*f find_row_positions cell_data - find the minimal starting positions for
    each row

   *)
  let find_row_positions cell_data =
    match cell_data with
    | [] -> []
    | _ -> (
      let sd = sort_by_start_index cell_data in
      let (first_row,_,_) = List.hd sd in
      find_next_row_position [(first_row, 0.)] sd first_row 0.
    )

  (*f make - make a Grid.t from input data *)
  let make cell_data = 
    let positions = List.rev (find_row_positions cell_data) in
    { cell_data; positions }

  (*f get_last_index t *)
  let get_last_index t =
    List.fold_left (fun _ (x,_) -> x) 0 t.positions

  (*f get_last_position t *)
  let get_last_position t =
    List.fold_left (fun _ (_,x) -> x) 0. t.positions

  (*f get_position t i *)
  let get_position t i =
    List.fold_left (fun r (x,p) -> (if (i>=x) then p else r)) 0. t.positions
  (*f str *)
  let str t = 
    let str_cell_data = String.concat "\n" (List.map (fun (s,n,sz) -> Printf.sprintf "start %d num %d size %f" s n sz) t.cell_data) in
    let str_positions = String.concat "\n" (List.map (fun (s,p) -> Printf.sprintf "start %d position %f" s p) t.positions) in
    "Cell data:\n" ^ str_cell_data ^ "\nRev positions:\n" ^ str_positions

  (*f All done *)
end

(*m Top level of Layout *)
(*t t_layout_properties *)
type t_layout_properties = {
        place             : t_vector option; (* If placed this is where reference point is placed *)
        width             : t_vector option; (* If None then no min/max width; else min/max width *)
        height            : t_vector option; (* If None then no min/max height; else min/max height *)
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
    anchor - if placed then which part of bbox is placed at the anchor; if grid and does not fill space then where to move it to (x,y 0-1,0-1)
expand - if grid and space available > min space then weight of expansion in x or y or both in call to 
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
  place; width; height; anchor; grid; z_index;
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

(*t t_grid_dimension *)
(*    let gather_place content_layout_bbox_list = 
      let acc_grid acc (layout, bbox) =
        let (w,h) = Primitives.Rectangle.get_wh bbox in
        let (px,py) = layout.place in
        let (ax,ay) = layout.anchor in
        else acc
      in
      List.fold_left acc_grid [] content_layout_bbox_list
 *)

module GridDimension = struct
type t = {
    grid : Grid.t;
    start_index : int;
    last_index : int;
    expansion : float array;
  }

let make expand_default expand grid_cell_data =
    let grid = Grid.make grid_cell_data in
    (* Printf.printf "Grid %s\n" (Grid.str grid); *)
    let find_first_last_index (f,l) (s,n,_) =
      let f = min s f in
      let l = max l (s+n) in
      (f,l)
    in
    (* note: start_index is inclusive and last_index is exclusive *)
    let (start_index,last_index) = List.fold_left find_first_last_index (99999,0) grid_cell_data in
    let start_index = min start_index last_index in
    let n = last_index-start_index in
    let expansion      = Array.make n expand_default in
    let add_expansion (index, amount) =
      let i = index - start_index in
      if ((i>=0) && (i<n)) then expansion.(index-start_index)<-amount
    in
    List.iter add_expansion expand;
    let expand_total = Array.fold_left (fun a b -> a +. b) 0. expansion in
    if (expand_total>0.) then Array.iteri (fun i v -> expansion.(i) <- v /. expand_total) expansion;
    {grid; start_index; last_index; expansion }

let get_span t = (0., Grid.get_last_position t.grid)

type t_layout = {
    start_index : int;
    last_index : int;
    position : float array;
  }

let resize (t:t) w =
  let n = t.last_index - t.start_index in
  let position = Array.init (n+1) (fun i-> Grid.get_position t.grid (i+t.start_index)) in
  let size     = Array.mapi (fun i _ -> position.(i+1) -. position.(i)) t.expansion in
  let span = position.(n) in
  let rec set_position extra i =
    position.(i) <- position.(i)+.extra; (* do this for i==n *)
    if (i<n) then (
      let extra=extra +. (w-.span)*.t.expansion.(i) in
      set_position extra (i+1)
    )
  in
  set_position 0. 0;
  (* Printf.printf "Grid resized %s\n" (Grid.str t.grid); *)
  {start_index=t.start_index; last_index=t.last_index; position;}
  
let get_bbox tl s n =
  let i0 = s - tl.start_index in
  let i1 = i0+n in
  let i0 = max i0 0 in (* i0 >= 0 *)
  let i1 = min i1 (tl.last_index-tl.start_index) in (* i1 <= n *)
  if (i1<=i0) then (0.,0.) else (tl.position.(i0), tl.position.(i1))

let str t = Grid.str t.grid
end

(*m PlaceDimension
  Currently support is for 'absolute' placement - that is at (px,py) in the content coordinates such that anchor point (as proprotion of content bbox) appears at px,py. Hence if anchor point is 0.5,0.5 then the middle of the content is placed at px,py

  fill means grow the bbox

  expand means scale internal coordinates to fill

 *)
module PlaceDimension = struct
type t = {
    expand: bool;
    bbox : float * float;
  }
let make cp_bbox_list index =
  let f acc ((cp,bbox) : (t_layout_properties * t_rect))  =
    if is_placed cp then (
      let (w, h) = Primitives.Rectangle.get_wh bbox in
      let p = match (cp.place,index) with | (None,_) -> 0. | (Some(x,y),0) -> x | (Some(x,y),_) -> y in
      let a = if (index==0) then (fst cp.anchor) else (snd cp.anchor) in
      let s = if (index==0) then w else h in
      let c0 = p -. a*.s in
      let c1 = c0 +. s in
      let (ac0, ac1) = acc in
      (min ac0 c0, max ac1 c1)
    ) else acc
  in 
  let (l,r) = List.fold_left f (0.,0.) cp_bbox_list in
  let bbox = if ((r-.l)<=0.) then (0.,0.) else (l,r) in
  let expand = false in
  { expand; bbox;}
  let get_span t = t.bbox
end

(*a Types *)
(*t t *)
type t = {
    props  : t_layout_properties;
    grids  : GridDimension.t * GridDimension.t;
    places : PlaceDimension.t list;
    min_bbox : t_rect;
  }

(*t t_transform *)
type t_transform = {
translate : t_vector;
scale     : t_vector;
bbox      : t_rect; (* bounding box to be displayed in - do border/fill of this *)
content_bbox      : t_rect; (* bounding box for content to be displayed in *)
    grids  : GridDimension.t_layout * GridDimension.t_layout;
  }

(*a Toplevel Layout module *)
let default_translate = (0.,0.)
let default_scale     = (1.,1.)

(*f create : t_layout_properties -> (t_layout_properties * min_bbox) list -> t

  Create the appropriate layout for the content (not this element) and determine the min_bbox given properties of this
  and the properties and min_bbox of contents
 *)
let create props children_props_bbox =
  let expand_default = 1. in
  let expand = [] in
  let build_grid_data acc ((cp,bbox) : (t_layout_properties * t_rect))  =
    if (is_placed cp) then acc else (
        let (cs,rs,cw,rw) = (match cp.grid with | None->(0,0,0,0) | Some x->x) in
        let (w, h) = Primitives.Rectangle.get_wh bbox in
        let (cl, rl) = acc in
        ((cs,cw,w)::cl), ((rs,rw,h)::rl)
    )
  in
  let (gcdx, gcdy) = List.fold_left build_grid_data ([],[]) children_props_bbox in
  let grids  = List.map (GridDimension.make expand_default expand) [gcdx; gcdy] in
  let places = List.map (fun i -> PlaceDimension.make children_props_bbox i) [0; 1] in
  let grid_spans  = List.map GridDimension.get_span  grids  in
  let place_spans = List.map PlaceDimension.get_span places in
  let (glx,grx) = List.hd  grid_spans in    
  let (gby,gty) = List.nth grid_spans 1 in    
  let (plx,prx) = List.hd  place_spans in    
  let (pby,pty) = List.nth place_spans 1 in
  let grids = List.(hd grids, nth grids 1) in
  (* Printf.printf "grid bbox %f,%f,%f,%f place bbox %f,%f,%f,%f\n" glx gby grx gty plx pby prx pty; *)
  let min_bbox = Primitives.Rectangle.union (glx,gby,grx,gty) (plx,pby,prx,pty) in
  let (mcx,mcy,mw,mh) = Primitives.Rectangle.get_cwh min_bbox in
  let mw = props_min_max props.width  mw in
  let mh = props_min_max props.height mh in
  let min_bbox = Primitives.Rectangle.of_cwh (mcx, mcy, mw, mh) in
  {props; grids; places; min_bbox;}

(*f get_z_index - get the z_index *)
let get_z_index t = t.props.z_index

(*f get_min_bbox - get the minimum bbox required by the content given grid and placemet (previously created as t) *)
let get_min_bbox t = t.min_bbox

(*f layout_within_bbox - If bbox is bigger than min and expanding then expand; remember translation and scaling

shrink content bbox by padding/border/margin?

 let cs = content size (after content transform)
 let ds = display space
 let blank = ds - cs
 let dby = display bottom y
 let content (presumably cby=post-transform) be placed at by = dby + blank*anchor
 hence translate a content y is placed at (content transform y)-cby + dby + blank*anchor
 hence translation to be applited to (content transform y) is dby-cby + (ds-cs)*anchor
 *)
let layout_within_bbox t bbox = 
    let border_bbox = Primitives.Rectangle.(shrink bbox t.props.margin) in
    let padded_bbox = Primitives.Rectangle.(shrink border_bbox t.props.border) in
    let internal_bbox = Primitives.Rectangle.(shrink padded_bbox t.props.padding) in
    (* dcx,dcy , dw,dh is center/size of the bbox to layout in after margin reduction *)
    let (dcx,dcy,dw,dh) = Primitives.Rectangle.get_cwh internal_bbox in
    (* ccx,ccy , cw,ch is center/size of the min bbox for our contents *)
    let (ccx,ccy,cw,ch) = Primitives.Rectangle.get_cwh t.min_bbox in
    (* should determine content layout w/h - assume same as now - but could expand it *)
    (* if expand *)
    let cw=dw in
    let ch=dh in
    (* So spare space can be determined - presumably +ve *)
    let (slack_x,slack_y) = ((dw-.cw),(dh-.ch)) in
    (* adjust ccx and ccy appropriately - depends on anchor coord *)
    (* ax,ay is the anchor coordinates - i.e. where (ccx,ccy) is anchored - ignore for now *)
    let (ax,ay) = (0.,0.) in
    (*  *)
    let ccx = dcx +. slack_x *. ax in
    let ccy = dcy +. slack_y *. ay in
    let content_bbox = Primitives.Rectangle.of_cwh (ccx,ccy,cw,ch) in
    let translate = default_translate in
    let scale     = default_scale in
    let (gx, gy) = t.grids in
    let gx = GridDimension.resize gx dw in
    let gy = GridDimension.resize gy dh in
    let grids = (gx, gy) in
    let transform = {translate; scale; bbox; content_bbox; grids;} in (* bbox is used for the border generation - should include updated grid *)
    (* Printf.printf "Layout_within_bbox %s int bbox %s min bbox %s slack %f,%f returns content %s \n" (Primitives.Rectangle.str bbox) (Primitives.Rectangle.str internal_bbox) (Primitives.Rectangle.str t.min_bbox) slack_x slack_y (Primitives.Rectangle.str content_bbox); *)
    let magnets =
      let (x0,y0,x1,y1)=bbox in
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
    let layout_pl = [Attr_names.outer_bbox,Ev_rect bbox;
                     Attr_names.border_bbox,Ev_rect border_bbox;
                     Attr_names.padded_bbox,Ev_rect padded_bbox;
                     Attr_names.content_bbox,Ev_rect content_bbox;
                     Attr_names.magnets, magnets;
                      ] in
    (transform, content_bbox, layout_pl)

let get_bbox_element t tr cp min_bbox = 
  let (x0,y0,x1,y1) =
    if (is_placed cp) then min_bbox else (
      let (cs,rs,cw,rw) = (match cp.grid with | None->(0,0,0,0) | Some x->x) in
      let (gx, gy) = tr.grids in
      let (x0,x1) = GridDimension.get_bbox gx cs cw in
      let (y0,y1) = GridDimension.get_bbox gy rs rw in
      (* Printf.printf "\nbbox for grid %f,%f, %f,%f, %d,%d,%d,%d\n" x0 y0 x1 y1 cs rs cw rw; *)
      (x0,y0,x1,y1)
    )
  in
  let (dx,dy,_,_) = tr.content_bbox in
  (x0+.dx, y0+.dy, x1+.dx, y1+.dy)

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

let svg_border_path_coords t tr =
  let (x0,y0,x1,y1) = Rectangle.(shrink ~scale:(0.5) (shrink tr.bbox t.props.margin) t.props.border)  in
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

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

  (*f find_min_height - find the shortest height in celL_data starting
    at the specified row;
    
    If there are any cells that start at a row
    after first_row but all cells starting at first_row span beyond
    those, then first_row can be zero height.

    If there are no such cells then find the cells that have the
    minimum span; then of these we need the largest of their sizes, in
    order to fit that cell in. This will be the min height for first_row
    then.

  *)
  let find_min_height cell_data first_row =
    let acc_if_smaller acc (s0,h0,size) =
      let (min_height, current_next) = acc in
      if ((s0 > first_row) && (s0 < current_next)) then
        (0., s0)
      else if (s0==first_row) && (s0+h0<current_next) then
        (size, s0+h0)
      else if (s0==first_row) && (s0+h0==current_next) && (size>min_height) then
        (min_height, current_next)
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
    List.iter (fun (s,h,size) -> Printf.printf "Row %d span %d size %f\n" s h size) sd;
    Printf.printf "\n";
    if (List.length sd)==0 then acc else (
      let (size, next_row) = find_min_height sd first_row in
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
    let positions = find_row_positions cell_data in
    { cell_data; positions }

  (*f get_last_index t *)
  let get_last_index t =
    List.fold_left (fun _ (x,_) -> x) 0 t.positions

  (*f get_last_position t *)
  let get_last_position t =
    List.fold_left (fun _ (_,x) -> x) 0. t.positions

  (*f get_position t i *)
  let get_position t i =
    List.fold_left (fun r (x,p) -> (if (i>=x) then p else r)) t.positions
end

(*m Top level of Layout *)
(*t t_layout_properties *)
type t_layout_properties = {
        padding        : Primitives.t_rect;
        border         : Primitives.t_rect;
        margin         : Primitives.t_rect;
        place          : Primitives.t_vector; (* If placed this is where anchor * bbox is placed*)
        anchor         : Primitives.t_vector; (* If placed, where in bbox the place point is; if grid, where in cell bbox to place bbox *)
        grid           : Primitives.t_int4;   (* grid elements to cover w=1, h=1 are a single cell *)
       (* 
    anchor - if placed then which part of bbox is placed at the anchor; if grid and does not fill space then where to move it to (x,y 0-1,0-1)
expand - if grid and space available > min space then weight of expansion in x or y or both in call to 
    rotation
    scale (x,y)
*)
(* should this have a mutable min bbox that is set when that is calculated ? *)
(* should this have a mutable grid layout ( (row number, posn, size, weight) list, (col number, posn, size, weight) list )  that is set when that is calculated ? *)
(* should this have a mutable output bbox that is set when that is calculated ? *)
}

(*f make_layout_hdr properties - get actual data from the provided properties *)
let make_layout_hdr properties =
        let padding   = Properties.(get_property_rect properties Padding_ Primitives.Rectangle.zeros) in
        let border    = Properties.(get_property_rect properties Border_  Primitives.Rectangle.zeros) in
        let margin    = Properties.(get_property_rect properties Margin_  Primitives.Rectangle.zeros) in
        let place     = Properties.(get_property_vector properties Place_  (0.,0.)) in
        let anchor    = Properties.(get_property_vector properties Anchor_  (0.,0.)) in
        let grid      = Properties.(get_property_int4 properties Grid_  (0,0,0,0)) in
(* content_transform and content_inv_transform *)
{
 padding; border; margin; place; anchor; grid;
}

(*f is_placed *)
let is_placed t = 
  let (_,_,cw,rw) = t.grid in
  if ((cw<=0) || (rw<=0)) then true else false

(*f expand_bbox - expand a bbox by the padding, border and margin *)
let expand_bbox t bbox = Primitives.Rectangle.(expand (expand (expand bbox t.padding) t.border) t.margin)

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
    expand_default : float;
    expand_total : float;
    expand : (int * float) list;
  }

let make grid_cell_data =
    let grid = Grid.make grid_cell_data in
    let find_first_last_index (f,l) (s,n,_) =
      let f = min s f in
      let l = max l (s+n) in
      (f,l)
    in
    (* note: start_index is inclusive and last_index is exclusive *)
    let (start_index,last_index) = List.fold_left find_first_last_index (0,0) grid_cell_data in
    let expand_default = 0. in
    let expand         = [] in
    let add_expansion acc (index, amount) =
      if ((index<start_index) || (index>=last_index)) then acc else (
        let (n,total) = acc in
        (n+1, total+.amount)
      )
    in
    let (n,expand_total) = List.fold_left add_expansion (0,0.) expand in
    let expand_total = expand_total +. (float (n-(last_index-start_index))) *. expand_default in
    {grid; expand_default; expand_total; expand}
let get_span t = (0., Grid.get_last_position t.grid)
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
  let f acc ((cp,bbox) : (t_layout_properties * Primitives.t_rect))  =
    if is_placed cp then (
      let (w, h) = Primitives.Rectangle.get_wh bbox in
      let p = if (index==0) then (fst cp.place)  else (snd cp.place)  in
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

type t = {
    grids  : GridDimension.t list;
    places : PlaceDimension.t list;
  }

type t_transform = {
translate : Primitives.t_vector;
  }
let create box_props children_props_bbox =
  let build_grid_data acc ((cp,bbox) : (t_layout_properties * Primitives.t_rect))  =
    if (is_placed cp) then acc else (
        let (cs,rs,cw,rw) = cp.grid in
        let (w, h) = Primitives.Rectangle.get_wh bbox in
        let (cl, rl) = acc in
        ((cs,cw,w)::cl), ((rs,rw,h)::rl)
    )
  in
  let (gcdx, gcdy) = List.fold_left build_grid_data ([],[]) children_props_bbox in
  let grids  = List.map GridDimension.make [gcdx; gcdy] in
  let places = List.map (fun i -> PlaceDimension.make children_props_bbox i) [0; 1] in
  {grids; places;}

(*f get_min_bbox - get the minimum bbox required by the content given grid and placemet (previously created as t) *)
let get_min_bbox t =
    let grid_spans  = List.map GridDimension.get_span  t.grids  in
    let place_spans = List.map PlaceDimension.get_span t.places in
    let (glx,grx) = List.hd  grid_spans in    
    let (gby,gty) = List.nth grid_spans 1 in    
    let (plx,prx) = List.hd  place_spans in    
    let (pby,pty) = List.nth place_spans 1 in    
    Primitives.Rectangle.union (glx,gby,grx,gty) (plx,pby,prx,pty)

(*f layout_within_bbox - If bbox is bigger than min and expanding then expand; remember translationg and scaling
 *)
let layout_within_bbox t bbox = ({translate=(0.,0.)}, bbox)
let get_bbox_element t tr props min_bbox = 
  if (is_placed props) then min_bbox else ( min_bbox
  )

let render_svg t tr svg_contents = 
    match svg_contents with 
    | [] -> []
    | s -> 
    [Svg.tag "g" [] s]

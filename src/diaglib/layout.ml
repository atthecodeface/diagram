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
module Grid = struct
  type t = {
      cell_data : (int * int * float) list ; (* list of (start row * number of rows (>=1) * height in pixels) *)
    }
  let make cell_data = { cell_data; }
  let sort_by_start_row cell_data =
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

  (*f find_row_positions t - find the minimal starting positions for
    each row

   *)
  let find_row_positions t =
    let sd = sort_by_start_row t.cell_data in
    let (first_row,_,_) = List.hd sd in
    find_next_row_position [(first_row, 0.)] sd first_row 0.

end


(* Move this to layout.ml *)
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
let make_layout_hdr properties =
        let padding   = Properties.(get_property_rect properties Padding_ Primitives.Rectangle.zeros) in
        let border    = Properties.(get_property_rect properties Border_  Primitives.Rectangle.zeros) in
        let margin    = Properties.(get_property_rect properties Margin_  Primitives.Rectangle.zeros) in
        let place     = Properties.(get_property_vector properties Place_  (0.,0.)) in
        let anchor    = Properties.(get_property_vector properties Anchor_  (0.,0.)) in
        let grid      = Properties.(get_property_int4 properties Grid_  (0,0,0,0)) in
{
 padding; border; margin; place; anchor; grid;
}
let expand_bbox t bbox = Primitives.Rectangle.(expand (expand (expand bbox t.padding) t.border) t.margin)


    (* move these two to Layout *)

    let gather_grid content_layout_bbox_list = 
      let acc_grid acc (layout, bbox) =
        let (w,h) = Primitives.Rectangle.get_wh bbox in
        let (cs,rs,cw,rw) = layout.grid in
        if (cw<=0) || (rw<=0) then acc
        else ( ((cs, cw, w), (rs ,rw, h)) :: acc )
      in
      List.fold_left acc_grid [] content_layout_bbox_list

    let gather_place content_layout_bbox_list = 
      let acc_grid acc (layout, bbox) =
        let (w,h) = Primitives.Rectangle.get_wh bbox in
        let (cs,rs,cw,rw) = layout.grid in
        let (px,py) = layout.place in
        let (ax,ay) = layout.anchor in
        if (cw<=0) || (rw<=0) then ((px,py,ax,ay,bbox)::acc)
        else acc
      in
      List.fold_left acc_grid [] content_layout_bbox_list


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

(*a Useful functions *)
(*m LayoutElementFunc - create LayoutElement for aggregate from a LayoutElementType *)
module LayoutElementFunc (E:LayoutElementType) = struct
  type et = E.et
  type rt = E.rt
  type lt = E.lt
  type gt = E.gt
  let styles = E.styles
  let resolve_styles et res = (
      let layout_pl = [Attr_names.padding,Ev_floats (4, res.value_as_floats Attr_names.padding);
                       Attr_names.margin,Ev_floats (4, res.value_as_floats Attr_names.margin);
                       Attr_names.border,Ev_floats (4, res.value_as_floats Attr_names.border);
                       Attr_names.bbox,Ev_rect Primitives.Rectangle.zeros;
                      ] in
      let (rt,pl) = E.resolve_styles et res in
      (rt,pl@layout_pl)
    )
  let get_min_bbox = E.get_min_bbox
  let make_layout_within_bbox et rt bbox = (
      let layout_pl = [Attr_names.bbox,Ev_rect bbox;
                      ] in
      let (lt,pl) = E.make_layout_within_bbox et rt bbox in
      (lt,pl@layout_pl)
    )

  let finalize_geometry = E.finalize_geometry
  let render_svg = E.render_svg
end



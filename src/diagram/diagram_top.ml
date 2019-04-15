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
 * @file     animation.ml
 * @brief    Shared-memory animation viewer using Ogl_gui
 *
 *)

open Structured_doc
open Diaglib

let main () =
    (* let sdoc = (`String (0,diag_test)) in *)
    let sdoc = (`Channel (open_in "examples/pipeline.dml")) in
    let dss = (`Channel (open_in "examples/pipeline.dss")) in
    let stylesheet = create_stylesheet () in
    Stylesheet_ml.stylesheet_add_rules stylesheet dss;
    let page_bbox = (1.,1.,110.,150.) in
    (*let b = from_structured_doc (`String (0,diag_test)) in*)
    let b = from_structured_doc sdoc in
    Format.(pp_set_mark_tags std_formatter true);
    pp_element Format.std_formatter b ;
    Format.print_flush ();
    Printf.printf "\n";
    let gt = layout_elements stylesheet page_bbox b in

    pp_geometry Format.std_formatter gt ;
    Format.print_flush ();
    Printf.printf "\n";

    show_layout gt "";
    let oc = open_out "a.svg" in
    let svg = Svg.svg_doc (render_svg gt 0) page_bbox in
    Svg.svg_print_hdr oc;
    Svg.pretty_print oc svg;
    close_out oc;
    ()

let _ =
  main () ;
  (*test_me ()*)


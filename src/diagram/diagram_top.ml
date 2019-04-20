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

(*a To do

new geometry

Dog-legs or connectors

rounding in paths
shorten in paths
color library

; fonts
; refs
; defs
; reduce coords by marker length
; text anchor vertically
; expand
; fill
font metrics

new eval to have types
 *)

open Diaglib

type args = {
    mutable source          : string option;
    source_is_xml           : bool ref;
    mutable rev_style_files : string list;
    mutable svg_filename    : string option;
  }
let main () =
  let exec = Filename.basename Sys.executable_name in
  let usage = Printf.sprintf "Usage: %s [OPTION]\nPlots something\nOptions:" exec in
  let args = { source=None;
               source_is_xml = ref false;
               rev_style_files = [];
               svg_filename = None;
             } in
  let options = Arg.[
                  "--xml", Set args.source_is_xml, "If used, source is XML format; else HML";
                  "--f",   String (fun s->args.source <- Some s),       "Source filename";
                  "--svg", String (fun s->args.svg_filename <- Some s), "Svg output file";
                ]
  in
  let options = Arg.align options in (* Tidy up documentation in options *)
  Arg.parse options (fun s -> args.rev_style_files <- s::args.rev_style_files) usage;

  let stylesheet = create_stylesheet () in
  List.iter (Stylesheet_ml.add_rules_from_file stylesheet) (List.rev args.rev_style_files);
  let (must_close_f, f) = match args.source with
    | None -> (false, stdin)
    | Some string -> (true, open_in string)
  in
  let b = from_structured_doc (`Channel f) in
  if must_close_f then (close_in f);

  let etb = prepare_elements stylesheet b in
  let (w,h) = prepared_size etb in
  let page_bbox = (0.,0.,w,h) in
  let gt = layout_elements (Desired_geometry.make (w/.2.,h/.2.) page_bbox) etb in
  match args.svg_filename with
  | Some s -> (
    let (must_close_oc,oc) = if String.equal s "-" then (false,stdout) else (true,open_out s) in
    let svg_diagram = render_svg gt in
    let svg = Svg.(svg_doc "1.2" (svg_defs "1.2"::svg_diagram) page_bbox) in
    Svg.svg_print_hdr oc;
    Svg.pretty_print ~extra_indent:"" oc svg;
    if must_close_oc then close_out oc
  )
  | _ -> ()

let _ =
  main () ;


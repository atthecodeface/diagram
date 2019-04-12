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

(*a Stylesheet things *)
(*
let stylesheet = Element.create_stylesheet ()
let sel_true            =  (fun e -> true)
let sel_cbox            =  Stylesheet.se_is_element_id "control"
let sel_type_button     =  Stylesheet.se_is_element_type "text_button"
let sel_cls_rotate      =  Stylesheet.se_has_element_class "rotate"
let sel_state_pressed   =  Stylesheet.se_is_element_state 0 3
let sel_state_hover     =  Stylesheet.se_is_element_state 0 2
let sel_state_enable    =  Stylesheet.se_is_element_state 0 1

let sel_button_rotate = fun e -> (sel_type_button e) && (sel_cls_rotate e)
let sel_hover_button  = fun e -> (sel_type_button e) && (sel_state_hover e)

let _ = 
    Stylesheet.add_style_rule stylesheet [sel_cbox; sel_hover_button]
             [("border_color", Sv_rgb [|1.;1.;1.;|]);
             ];
    Stylesheet.add_style_rule stylesheet [sel_cbox; sel_type_button]
             [("border", Sv_floats (6,[|1.;1.;1.;1.;1.;1.;|]));
             ];
    Stylesheet.add_style_rule stylesheet [sel_true]
             [("margin", Sv_floats (6,[|0.;0.;0.;0.;0.;0.;|]));
             ];
    ()


 *)

(*a HML stuff *)
exception Bad_tag of string
open Structured_doc

let from_structured_doc f =
  let fnt = Font.make  "Arial embedded" 10. 3. 5. in
  let rec read_element_contents opt_nsn_att data rev_acc t =
    match (input t) with
    | `El_start ((ns,name),attrs) -> (
      let e = read_element_contents (Some ((ns,name),attrs)) [] [] t in
      read_element_contents opt_nsn_att [] (e::rev_acc) t
    )
    | `El_end -> (
      let contents = List.rev rev_acc in
      let e = (
          match opt_nsn_att with 
          | None -> Diagram_element.make_box ["id","toplevel"]  contents
          | Some ((ns,name),attrs) ->  (
             let attrs = List.map (fun ((ns,name),value) -> (name,value)) attrs in
             if (String.equal name "box") then (
               Diagram_element.make_box attrs contents
             ) else if (String.equal name "text") then (
               Diagram_element.make_text attrs (De_text.make fnt data)
             ) else if (String.equal name "path") then (
               Diagram_element.make_path attrs (De_path.make ())
             ) else (
               raise (Bad_tag name)
             )
          )
        ) in
         e
    )
    | `Data d -> (
      read_element_contents opt_nsn_att (data@[d]) rev_acc t
    )
    | `Dtd _ -> (
      read_element_contents opt_nsn_att data rev_acc t
    )
  in

  let hml = make_hmlm (Hmlm.make_input ~doc_tag:(("","diag"),[]) f) in
  let diag = (
      try (read_element_contents None [] [] hml)
      with 
      | Xmlm.Error ((l,c),e) -> (
        Printf.printf "Error %s at line %d char %d \n" (Xmlm.error_message e) l c;
        raise Not_found
      )
      | Bad_tag x -> (
         Printf.printf "Bad tag %s \n" x;
         raise Not_found
      )
    ) in
    diag

let create_stylesheet = Diagram_element.create_stylesheet
let pp_element = Diagram_element.pp_element
let pp_geometry = Diagram_element.pp_geometry
let layout_elements = Diagram_element.layout_elements
let show_layout = Diagram_element.show_layout
let render_svg = Diagram_element.render_svg
module Svg=Svg

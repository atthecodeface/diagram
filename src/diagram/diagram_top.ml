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

let test = "
#font id=f0 face=\"Arial embedded\" ascent=10. descent=3. avg_width=5.
#box{ id=b0 border=5 bordercolor=blue
#text id=t0 grid=0,0,1,1 font=f0 size=11 color=black
#text id=t1 grid=1,0,1,1 font=f0 size=11 color=black
#text id=t2 grid=0,1,2,1 border=1 bordercolor=green fillcolor=red font=f0 size=11 color=black
#box}
"
(*
##text id=t0 grid=0,0,1,1 font=f0 size=11 color=black \"Some text\"
##text id=t1 grid=1,0,1,1 font=f0 size=11 color=black \"Some more text\"
##text id=t2 grid=0,1,2,1 border=1 bordercolor=green fillcolor=red font=f0 size=11 color=black \"A long line of text\"
"
 *)

let test_me _ =
  Format.(pp_set_mark_tags std_formatter true);
  let x = make_hmlm (Hmlm.make_input ~doc_tag:(("","svg"),[]) (`String (0,test))) in
  try (pp x) with Xmlm.Error ((l,c),e) -> Printf.printf "Error %s at line %d char %d \n" (Xmlm.error_message e) l c;
  Format.print_flush ();
  Printf.printf "\n"

(*a Top level

#svg version=1.2 xmlns='http://www.w3.org/2000/svg' viewBox='100 100 200 150'
     preserveAspectRatio='xMidYMid' fill-rule='evenodd' stroke-width=1 stroke-linejoin=round
##g transform=translate(105 105)
###g transform=translate(0 0)
####text x=0 y=0 fill=rgb(0,0,0) "Some text"
###g transform=translate(100 0)
####text x=0 y=20 fill=rgb(0,0,0) "Some more text"
###g{ transform=translate(1 21)
#text x=0 y=20 fill=rgb(0,0,0) "Some more text"
#path stroke=none fill=rgb(255,0,0) stroke-width=1
      d='M1.000000 21.000000 L199.000000 21.000000 L199.000000 41.000000 L1.000000 41.000000 Z'
#text x=0 y=42 fill=rgb(0,0,0) "A long line of text"
#path stroke=rgb(255,0,0) fill=none stroke-width=1
      d='M1.000000 21.000000 L199.000000 21.000000 L199.000000 41.000000 L1.000000 41.000000 Z'
###g}
 *)

let diag_test = "
#diag{
#font id=f0 face='Arial embedded' ascent=10. descent=3. avg_width=5.
#box{ id=b0 border=5 bordercolor=blue
##text id=t0 grid=0,0,1,1 font=f0 size=11 color=black 'Some text'
##text id=t1 grid=1,0,1,1 font=f0 size=11 color=black 'Some more text'
##text id=t2 grid=0,1,2,1 border=1 bordercolor=green fillcolor=red font=f0 size=11 color=black 'A long line of text'
#box}
#diag}
"

let diag_test = "
#box{ id=b0 border=5 border_color=blue
#text id=t0 grid=0,0,1,1 font=f0 size=11 font_color=black
#text id=t1 grid=1,0,1,1 font=f0 size=11 font_color=black
#text id=t2 grid=0,1,2,1 border=1 border_color=green fill_color=red font=f0 size=11 font_color=black
#box}
"

let diag_test = "
#box id=b0 border=5 border_color=blue fill_color=yellow
##text id=t0 grid=0,0,1,1 font=f0 size=11 font_color=blue
##text id=t1 grid=1,0,1,1 font=f0 size=11 font_color=black
##text id=t2 grid=0,1,2,1 border=1 border_color=green fill_color=red font=f0 size=11 font_color=black
#box
"

exception Bad_tag of string
open Structured_doc
let from_hml f =
  let fnt = Font.make  "Arial embedded" 10. 3. 5. in
  let rec acc_children rev_acc t =
    match (input t) with
    | `El_start ((ns,name),attrs) -> (
      let c = acc_children [] t in
      let attrs = List.map (fun ((ns,name),value) -> (name,value)) attrs in
      let e = (
          if (String.equal name "box") then (
            Element.make_box attrs "b0" c
          ) else if (String.equal name "text") then (
            Element.make_text attrs "t0" (TextInt.make fnt ["Some text"; ])
          ) else (
            raise (Bad_tag name)
          )
        )
      in
      acc_children (e::rev_acc) t
    )
    | `El_end -> (
      List.rev rev_acc
    )
    | `Data _ -> (
      List.rev rev_acc
    )
    | `Dtd _ -> (
      List.rev rev_acc
    )
  in

  let hml = make_hmlm (Hmlm.make_input ~doc_tag:(("","diag"),[]) f) in
  let diag = (
      try (acc_children [] hml)
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
    List.hd diag

let main () =
    let page_bbox = (100.,100.,200.,150.) in
    let stylesheet = Element.create_stylesheet () in
    let b = from_hml (`String (0,diag_test)) in
    let lt = Element.layout_elements stylesheet page_bbox b in
    Element.show_layout lt "";
    let oc = open_out "a.svg" in
    let svg = Svg.svg_doc (Element.render_svg lt 0) page_bbox in
    Svg.svg_print_hdr oc;
    Svg.pretty_print oc svg;
    close_out oc;
    ()

let _ =
  main () ;
  (*test_me ()*)


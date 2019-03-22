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

#diag{
#font id=f0 face="Arial embedded" ascent=10. descent=3. avg_width=5.
#box{ id=b0 border=5 bordercolor=blue
##text id=t0 grid=0,0,1,1 font=f0 size=11 color=black "Some text"
##text id=t1 grid=1,0,1,1 font=f0 size=11 color=black "Some more text"
##text id=t2 grid=0,1,2,1 border=1 bordercolor=green fillcolor=red font=f0 size=11 color=black "A long line of text"
#box}
#diag}
 *)
let main () =
    let page_bbox = (100.,100.,200.,150.) in
    let stylesheet = Element.create_stylesheet () in
    let f = Font.make  "Arial embedded" 10. 3. 5. in
    let t0 = Element.make_text [("grid","0,0,1,1");] "t0" (TextInt.make f ["Some text"; ]) in
    let t1 = Element.make_text [("grid","1,0,1,1");] "t1" (TextInt.make f ["Some More text"; ]) in
    let t2 = Element.make_text [("grid","0,1,2,1"); "border","1.,1.,1.,1."; "border_color","green"; "fill_color","red";] "t2" (TextInt.make f ["A long line of text"; ]) in
    let b = Element.make_box ["border","5.,5.,5.,5."; "border_color","blue";] "b0" [t0; t1; t2] in
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
  Structured_doc.test_me ()


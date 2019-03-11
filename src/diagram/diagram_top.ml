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

module Option   = Batteries.Option

open Diaglib

let _ =
  let g = Layout.Grid.make [(0,1,0.1);
                            (0,2,0.5);
                            (1,2,0.2);
                            (1,2,0.8);
                            (0,17,0.120);
            ] in
    let r = Layout.Grid.find_row_positions g in
    List.iter (fun (r,s) -> Printf.printf "Row %d at %f\n" r s) r


(*a Top level *)
let main () =
    let f = Font.make  "Arial embedded" 10. 3. 5. in
    let t = Element.make_text [Grid (0,0,1,1);] "t0" (TextInt.make f 11. (Color.None) (Color.black) ["Some text"; ]) in
    let b = Element.make_box Properties.[Border (1.,1.,1.,1.);] "b0" [t] in
    let bbox = Element.get_min_bbox b in
    let (d0,d1,d2,d3) = bbox in
    Printf.printf "Bbox %f,%f %f,%f\n" d0 d1 d2 d3;
    () (*let b = Box.make (Primitives.th_make "f0") *)
let _ =
  main ()



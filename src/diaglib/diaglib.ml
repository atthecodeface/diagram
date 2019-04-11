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
module Properties = Properties
module Color     = Primitives.Color
module Rectangle = Primitives.Rectangle
module Layout = Layout
module Svg = Svg
module type LayoutElementAggrType = Element.LayoutElementAggrType
module ElementFunc            = Element.ElementFunc
module type LayoutElementType = Element.LayoutElementType
module LayoutElementFunc      = Element.LayoutElementFunc
module LayoutElementBase      = Element.LayoutElementBase

(*a Font *)
module Font = struct
    type t = {
    family : string;
    ascent : float;
    descent : float;
    avg_width : float;
      }
    let get_width t s = t.avg_width *. (float (String.length s))
    let get_bbox t s =
      let w = get_width t s in
      (0., w, 0., 10.)
    (*
    let defn_svg t = Svg.tag "font-face" [("font-family", t.family), 
    let use_svg  t = Svg.tag "font-face" [("font-family", t.family), 
     *)
    let make family ascent descent avg_width = 
    { family; ascent; descent; avg_width; }
end

(*a PathInt, TextInt, BoxInt - all LayoutElementType modules *)
(*m PathInt *)
module PathInt : sig
    include LayoutElementType
    val make : unit -> et
end = struct
    include LayoutElementBase
    type et = int
    type rt = {color:string;}
    type lt = Primitives.t_rect
    type gt = {
coords : float array;
      }

    let make _ = 0
    let styles = Stylesheet.Value.[
                   ("coords",  St_float_arr, sv_none_float_arr, true);
                   ("color",   St_rgb,       Sv_rgb [|0.;0.;0.;|], true);
                   ("width",   St_float,     Sv_float (Some 1.), true);
                 ] @ styles
    let resolve_styles et (resolver:Element.t_style_resolver) = 
      let color = resolver.value_as_color_string  "color" in
      let coords = resolver.value_as_floats ~default:[||] "coords" in
      let rt : rt = {color} in
      (rt, ["coords",Element.Ev_floats ((Array.length coords),coords)])

    let r = Rectangle.mk_fixed (0.,0.,100.,20.)
    let get_min_bbox et rt = (0.,0.,100.,20.)
    let make_layout_within_bbox et rt bbox = (bbox,[])
    let finalize_geometry et rt lt (resolver:Element.t_style_resolver) =
      let coords = resolver.value_as_floats "coords" in
      {coords}
    let render_svg et rt lt gt i = 
      let rec make_path acc act i n =
        if (i>=n-1) then acc else (
          let acc = Printf.sprintf "%s %s %f %f" acc act (gt.coords.(i)) (gt.coords.(i+1)) in
          make_path acc "L" (i+2) n
        )
in
    let n = Array.length gt.coords in
    let path = make_path "" "M" 0 n in
      [ Svg.(tag "path" [ FloatAttr("stroke-width", 1.0);
                          StringAttr("stroke", rt.color);
                          StringAttr("fill", "none");
                          StringAttr("d", path);
                    ] [] []);
      ]
end

(*m TextInt *)
module TextInt : sig
    include LayoutElementType
    val make : Font.t -> string list -> et
end = struct 
    include LayoutElementBase
    (* List of (?) text, font, style, base line, min font size, desired font size *)
    (* padding below lowest baseline, above upper baseline, to left of left-most pixel, to right of right-most pixel *)
    type et = {
        font : Font.t;
        text : string list; (* One string per line *)
      }
    type rt = {
        size  : float;
    (* anchor / alignment *)
      }
    type lt = Primitives.t_rect (* Rectangle to place text within *)
    type gt = {
        size  : float;
        color : string;
        x : float;
        y : float;
      }

    let styles = Stylesheet.Value.[
                   ("font_size",  St_float,  Sv_float (Some 12.), true);
                   ("font_color", St_rgb,    Sv_rgb [|0.;0.;0.;|], true);
                 ] @ styles

    let make font text = {font; text}

    let resolve_styles et (resolver:Element.t_style_resolver) =
      let size  = resolver.value_as_float         "font_size" in
      let rt:rt = {size;} in
      (rt, [])

    let r = Rectangle.mk_fixed (0.,0.,100.,20.)
    let get_min_bbox et rt =
      (* min bbox is x 0, width of longest string by font, y of 0, height of num lines * (max height of character + line spacing) *)
     (0.,0.,100.,20.)
    let make_layout_within_bbox et rt bbox = 
      (* bbox is in the coordinate space for us to do a layout in  can now do centering, and could prepare for justification. *)
      Printf.printf "\nText layout bbox %s\n\n" (Rectangle.str bbox);
      (bbox, [])
    let finalize_geometry et (rt:rt) lt (resolver:Element.t_style_resolver) = 
      let color = resolver.value_as_color_string  ~default:"black" "font_color" in
      let (x0,y0,x1,y1) = lt in
      {x=x0; y=y0+.y1; size=rt.size; color;}

    let svg_use et rt lt gt = 
      Svg.(tag "text" [(* font-family, stroke *)
                                     FloatAttr ("x", gt.x);
                                     FloatAttr ("y", gt.y);
                                     FloatAttr ("font-size", gt.size);
                                     StringAttr ("fill", gt.color);
                    ] [] et.text)
    let render_svg et rt lt gt i = 
      let svg = svg_use et rt lt gt in
      [svg]
end

(*m BoxInt *)
module BoxInt : sig
    include LayoutElementType
    val make : unit -> et
end = struct 
    include LayoutElementBase
    type et = int
    type rt = int
    type lt = Primitives.t_rect
    type gt = lt

    let get_min_bbox et rt = Rectangle.zeros

    let make _ = 0
    let resolve_styles et (resolver:Element.t_style_resolver) =
      let rt : rt = 0 in
      (rt, [])
    let make_layout_within_bbox et rt bbox = (bbox, [])
    let finalize_geometry et rt lt resolver = lt
    let render_svg et rt lt gt i = []

end

(*a DiagramElement (using PathInt, TextInt, BoxInt) and Element *)
module Path = LayoutElementFunc(PathInt)
module Text = LayoutElementFunc(TextInt)
module Box  = LayoutElementFunc(BoxInt)

(*m DiagramElement *)
module DiagramElement = struct
  exception Mismatch of string

  (*t et - Basic element type *)
  type et = | EBox  of Box.et
            | EText of Text.et
            | EPath of Path.et

  (*t rt - Additional resolved style structure for the element *)
  type rt = | RBox  of Box.et  * Box.rt
            | RText of Text.et * Text.rt
            | RPath of Path.et * Path.rt

  (*t lt - Additional resolved style structure for the element *)
  type lt = | LBox of  Box.et * Box.rt * Box.lt
            | LText of Text.et * Text.rt * Text.lt
            | LPath of Path.et * Path.rt * Path.lt

  (*t gt - Additional finalized geometry structure for the element *)
  type gt = | GBox  of Box.et * Box.rt * Box.lt * Box.gt
            | GText of Text.et * Text.rt * Text.lt * Text.gt
            | GPath of Path.et * Path.rt * Path.lt * Path.gt

  let styles  = 
    Text.styles @ Path.styles @ Box.styles

  let style_desc = Stylesheet.create_desc [] styles

  let et_is_text = function | EText _ -> true | _ -> false
  let et_is_path = function | EPath _ -> true | _ -> false
  let et_is_box  = function | EBox  _ -> true | _ -> false

  let type_name_et et = 
    match et with
    | EText e      -> "text"
    | EPath e      -> "path"
    | EBox  e      -> "box"

  let et_of_rt rt =
    match rt with
    | RText (e,r) -> EText e
    | RPath (e,r) -> EPath e
    | RBox  (e,r) -> EBox e

  let et_of_lt lt =
    match lt with
    | LText (e,r,l) -> EText e
    | LPath (e,r,l) -> EPath e
    | LBox  (e,r,l) -> EBox e

  let et_of_gt gt =
    match gt with
    | GText (e,r,l,g) -> EText e
    | GPath (e,r,l,g) -> EPath e
    | GBox  (e,r,l,g) -> EBox e

  let type_name_rt rt = type_name_et (et_of_rt rt)

  let type_name_lt lt = type_name_et (et_of_lt lt)

  let type_name_gt gt = type_name_et (et_of_gt gt)

  let resolve_styles et (resolver:Element.t_style_resolver) =
    match et with
    | EText e      -> let (r,pl) = Text.resolve_styles e resolver in (RText (e,r), pl)
    | EPath e      -> let (r,pl) = Path.resolve_styles e resolver in (RPath (e,r), pl)
    | EBox  e      -> let (r,pl) = Box.resolve_styles  e resolver in (RBox  (e,r), pl)

  let get_min_bbox rt = 
    match rt with
    | RText (e,r)      -> Text.get_min_bbox e r
    | RPath (e,r)      -> Path.get_min_bbox e r
    | RBox  (e,r)      -> Box.get_min_bbox  e r

  let make_layout_within_bbox rt (bbox : Primitives.t_rect) = 
    match rt with
    | RText (e,r)  -> let (l,pl) = Text.make_layout_within_bbox e r bbox in (LText (e,r,l), pl)
    | RPath (e,r)  -> let (l,pl) = Path.make_layout_within_bbox e r bbox in (LPath (e,r,l), pl)
    | RBox  (e,r)  -> let (l,pl) = Box.make_layout_within_bbox  e r bbox in (LBox  (e,r,l), pl)

  let finalize_geometry lt res = 
    match lt with
    | LText (e,r,l) -> GText (e,r,l,(Text.finalize_geometry e r l res))
    | LPath (e,r,l) -> GPath (e,r,l,(Path.finalize_geometry e r l res))
    | LBox  (e,r,l) -> GBox  (e,r,l,(Box.finalize_geometry  e r l res))
 
  let render_svg gt zindex = 
    match gt with
    | GText (e,r,l,g) -> Text.render_svg e r l g zindex
    | GPath (e,r,l,g) -> Path.render_svg e r l g zindex
    | GBox  (e,r,l,g) -> []

end

(*m Element - using the DiagramElement *)
module Element = struct
    include ElementFunc(DiagramElement)

    let make_text properties text     = make_et properties (DiagramElement.EText text) []
    let make_path properties path     = make_et properties (DiagramElement.EPath path) []
    let make_box  properties elements = make_et properties (DiagramElement.EBox (BoxInt.make ())) elements
end

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
          | None -> Element.make_box ["id","toplevel"]  contents
          | Some ((ns,name),attrs) ->  (
             let attrs = List.map (fun ((ns,name),value) -> (name,value)) attrs in
             if (String.equal name "box") then (
               Element.make_box attrs contents
             ) else if (String.equal name "text") then (
               Element.make_text attrs (TextInt.make fnt data)
             ) else if (String.equal name "path") then (
               Element.make_path attrs (PathInt.make ())
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


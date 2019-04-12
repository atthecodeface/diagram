open Types
include De_base
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
               (Attr_names.font_size,  St_float,  Sv_float (Some 12.), true);
               (Attr_names.color, St_rgb,         Sv_rgb [|0.;0.;0.;|], true);
             ] @ styles

let make font text = {font; text}

let resolve_styles et (resolver:t_style_resolver) =
  let size  = resolver.value_as_float         Attr_names.font_size in
  let rt:rt = {size;} in
  (rt, [])

let r = Primitives.Rectangle.mk_fixed (0.,0.,100.,20.)
let get_min_bbox et rt =
  (* min bbox is x 0, width of longest string by font, y of 0, height of num lines * (max height of character + line spacing) *)
  (0.,0.,100.,20.)
let make_layout_within_bbox et rt bbox = 
  (* bbox is in the coordinate space for us to do a layout in  can now do centering, and could prepare for justification. *)
  Printf.printf "\nText layout bbox %s\n\n" (Primitives.Rectangle.str bbox);
  (bbox, [])
let finalize_geometry et (rt:rt) lt (resolver:t_style_resolver) = 
  let color = resolver.value_as_color_string  ~default:"black" Attr_names.color in
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

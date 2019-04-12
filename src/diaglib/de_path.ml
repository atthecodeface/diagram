open Types
include De_base
type et = int
type rt = {color:string;}
type lt = t_rect
type gt = {
    coords : float array;
  }

let make _ = 0
let styles = Stylesheet.Value.[
               (Attr_names.coords,  St_float_arr, sv_none_float_arr, true);
               (Attr_names.color,   St_rgb,       Sv_rgb [|0.;0.;0.;|], true);
               (Attr_names.width,   St_float,     Sv_float (Some 1.), true);
             ] @ styles
let resolve_styles et (resolver:t_style_resolver) = 
  let color = resolver.value_as_color_string  Attr_names.color in
  let coords = resolver.value_as_floats ~default:[||] Attr_names.coords in
  let rt : rt = {color} in
  (rt, [Attr_names.coords,Ev_floats ((Array.length coords),coords)])

let r = Primitives.Rectangle.mk_fixed (0.,0.,100.,20.)
let get_min_bbox et rt = (0.,0.,100.,20.)
let make_layout_within_bbox et rt bbox = (bbox,[])
let finalize_geometry et rt lt (resolver:t_style_resolver) =
  let coords = resolver.value_as_floats Attr_names.coords in
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


open Types
include De_base
type et = int
type rt = {color:string; markers:string; stroke_width:float; coords:float array;} (* coords if provided statically *)
type lt = t_ref_bbox
type gt = {
    coords : float array;
  }

let make _ = 0
let styles = Stylesheet.Value.[
               (Attr_names.coords,       St_float_arr, sv_none_float_arr, true);
               (Attr_names.color,        St_rgb,       Sv_rgb [|0.;0.;0.;|], true);
               (Attr_names.stroke_width, St_float,     Sv_float (Some 1.), true);
               (Attr_names.markers,      St_string,    sv_none_string, true);
             ] @ styles

(*f resolve_styles : et -> t_style_resolver -> rt *)
let resolve_styles et (resolver:t_style_resolver) = 
  let markers      = resolver.value_as_string  ~default:"" Attr_names.markers in
  let stroke_width = resolver.value_as_float   Attr_names.stroke_width in
  let color        = resolver.value_as_color_string  Attr_names.color in
  let coords       = resolver.value_as_floats ~default:[||] Attr_names.coords in
  let rt : rt = {color; markers; stroke_width; coords} in
  (rt, [Attr_names.coords,Ev_floats ((Array.length coords),coords)])

(*f bbox_x/y : bbox -> int -> float array -> bbox *)
let rec bbox_x bbox n a = function
  | i when i>=n -> bbox
  | 0 -> [|a.(0);0.;a.(0);0.|]
  | i -> (
     let x = a.(i) in
     let minx = min x bbox.(0) in
     let maxx = max x bbox.(2) in
     bbox_y [|minx; bbox.(1); maxx; bbox.(3); |] n a (i+1)
  )
and bbox_y bbox n a = function
  | i when i>=n -> bbox
  | 1 -> ( [| bbox.(0); a.(1); bbox.(2); a.(1); |] )
  | i -> (
     let y = a.(i) in
     let miny = min y bbox.(1) in
     let maxy = max y bbox.(3) in
     bbox_x [| bbox.(0); miny; bbox.(2); maxy; |] n a (i+1)
  )

(*f get_desired_geometry : et -> rt -> t_ref_bbox *)
let get_desired_geometry et (rt:rt) =
  let bbox = bbox_x Primitives.Rectangle.zeros (Array.length rt.coords) rt.coords 0 in
  Desired_geometry.make Primitives.Vector.zeros bbox

let make_layout_with_geometry et rt geom = (geom,[])
let finalize_geometry et rt lt (resolver:t_style_resolver) =
  let coords = resolver.value_as_floats Attr_names.coords in
  {coords}
let render_svg et rt lt gt z_index = 
  let open Svg in
  let path = svg_path gt.coords in
  let attrs = [] in
  let attrs = StringAttr("d", path)::attrs in
  let attrs = FloatAttr("stroke-width", rt.stroke_width) :: attrs in
  let attrs = StringAttr("stroke", rt.color) :: attrs in
  let attrs = StringAttr("fill", "none") :: attrs in
  let attrs = match List.map (fun m->"url(#"^m^")") (String.split_on_char ' ' rt.markers) with
    | [] -> attrs
    | hd::[] -> StringAttr("marker-start",hd)::StringAttr("marker-end",hd)::attrs
    | st::en::[] -> StringAttr("marker-start",st)::StringAttr("marker-end",en)::attrs
    | st::mid::en::[] -> StringAttr("marker-start",st)::StringAttr("marker-mid",mid)::StringAttr("marker-end",en)::attrs
    | _ -> attrs
  in
  [ Svg.(tag "path" attrs [] []);
  ]


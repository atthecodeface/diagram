open Types
include De_base
type et = int
type rt = {color:string; markers:string; stroke_width:float; coords:float array;} (* coords if provided statically *)
type lt = t_rect
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
  | 0 -> (a.(0),0.,a.(0),0.)
  | i -> (
     let (minx,miny,maxx,maxy) = bbox in
     let x = a.(i) in
     let minx = min x minx in
     let maxx = max x maxx in
     bbox_y (minx,miny,maxx,maxy) n a (i+1)
  )
and bbox_y bbox n a = function
  | i when i>=n -> bbox
  | 1 -> (
    let (minx,miny,maxx,maxy) = bbox in
    minx,a.(1),maxx,a.(1)
  )
  | i -> (
     let (minx,miny,maxx,maxy) = bbox in
     let y = a.(i) in
     let miny = min y miny in
     let maxy = max y maxy in
     bbox_x (minx,miny,maxx,maxy) n a (i+1)
  )

(*f get_desired_geometry : et -> rt -> t_ref_bbox *)
let get_desired_geometry et (rt:rt) =
  let bbox = bbox_x (0.,0.,0.,0.) (Array.length rt.coords) rt.coords 0 in
  Desired_geometry.make (0.,0.) bbox

let make_layout_within_bbox et rt bbox = (bbox,[])
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


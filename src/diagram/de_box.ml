open Types
include De_base
type et = int
type rt = int
type lt = t_ref_bbox
type gt = lt

let get_desired_geometry et rt =
  Desired_geometry.make (0.,0.) Primitives.Rectangle.zeros

let make _ = 0
let resolve_styles et (resolver:t_style_resolver) =
  let rt : rt = 0 in
  (rt, [])
let make_layout_with_geometry et rt geom = (geom, [])
let finalize_geometry et rt lt resolver = lt
let render_svg et rt lt gt i = []

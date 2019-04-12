open Types
include De_base
type et = int
type rt = int
type lt = Primitives.t_rect
type gt = lt

let get_min_bbox et rt = Primitives.Rectangle.zeros

let make _ = 0
let resolve_styles et (resolver:t_style_resolver) =
  let rt : rt = 0 in
  (rt, [])
let make_layout_within_bbox et rt bbox = (bbox, [])
let finalize_geometry et rt lt resolver = lt
let render_svg et rt lt gt i = []

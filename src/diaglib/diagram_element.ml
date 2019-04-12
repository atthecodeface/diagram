open Types

(*a DiagramElement (using PathInt, TextInt, BoxInt) and Element *)
module Path = Element.LayoutElementFunc(De_path)
module Text = Element.LayoutElementFunc(De_text)
module Box  = Element.LayoutElementFunc(De_box)

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

  let resolve_styles et (resolver:t_style_resolver) =
    match et with
    | EText e      -> let (r,pl) = Text.resolve_styles e resolver in (RText (e,r), pl)
    | EPath e      -> let (r,pl) = Path.resolve_styles e resolver in (RPath (e,r), pl)
    | EBox  e      -> let (r,pl) = Box.resolve_styles  e resolver in (RBox  (e,r), pl)

  let get_min_bbox rt = 
    match rt with
    | RText (e,r)      -> Text.get_min_bbox e r
    | RPath (e,r)      -> Path.get_min_bbox e r
    | RBox  (e,r)      -> Box.get_min_bbox  e r

  let make_layout_within_bbox rt (bbox : t_rect) = 
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
include Element_func.ElementFunc(DiagramElement)

let make_text properties text     = make_et properties (DiagramElement.EText text) []
let make_path properties path     = make_et properties (DiagramElement.EPath path) []
let make_box  properties elements = make_et properties (DiagramElement.EBox (De_box.make ())) elements


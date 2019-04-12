open Types

(*a DiagramElement (using PathInt, TextInt, BoxInt) and Element *)
module PathEl = Element.LayoutElementFunc(Path)
module TextEl = Element.LayoutElementFunc(Text)
module BoxEl  = Element.LayoutElementFunc(Box)

(*m DiagramElement *)
module DiagramElement = struct
  exception Mismatch of string

  (*t et - Basic element type *)
  type et = | EBox  of BoxEl.et
            | EText of TextEl.et
            | EPath of PathEl.et

  (*t rt - Additional resolved style structure for the element *)
  type rt = | RBox  of BoxEl.et  * BoxEl.rt
            | RText of TextEl.et * TextEl.rt
            | RPath of PathEl.et * PathEl.rt

  (*t lt - Additional resolved style structure for the element *)
  type lt = | LBox of  BoxEl.et * BoxEl.rt * BoxEl.lt
            | LText of TextEl.et * TextEl.rt * TextEl.lt
            | LPath of PathEl.et * PathEl.rt * PathEl.lt

  (*t gt - Additional finalized geometry structure for the element *)
  type gt = | GBox  of BoxEl.et * BoxEl.rt * BoxEl.lt * BoxEl.gt
            | GText of TextEl.et * TextEl.rt * TextEl.lt * TextEl.gt
            | GPath of PathEl.et * PathEl.rt * PathEl.lt * PathEl.gt

  let styles  = 
    TextEl.styles @ PathEl.styles @ BoxEl.styles

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
    | EText e      -> let (r,pl) = TextEl.resolve_styles e resolver in (RText (e,r), pl)
    | EPath e      -> let (r,pl) = PathEl.resolve_styles e resolver in (RPath (e,r), pl)
    | EBox  e      -> let (r,pl) = BoxEl.resolve_styles  e resolver in (RBox  (e,r), pl)

  let get_min_bbox rt = 
    match rt with
    | RText (e,r)      -> TextEl.get_min_bbox e r
    | RPath (e,r)      -> PathEl.get_min_bbox e r
    | RBox  (e,r)      -> BoxEl.get_min_bbox  e r

  let make_layout_within_bbox rt (bbox : Primitives.t_rect) = 
    match rt with
    | RText (e,r)  -> let (l,pl) = TextEl.make_layout_within_bbox e r bbox in (LText (e,r,l), pl)
    | RPath (e,r)  -> let (l,pl) = PathEl.make_layout_within_bbox e r bbox in (LPath (e,r,l), pl)
    | RBox  (e,r)  -> let (l,pl) = BoxEl.make_layout_within_bbox  e r bbox in (LBox  (e,r,l), pl)

  let finalize_geometry lt res = 
    match lt with
    | LText (e,r,l) -> GText (e,r,l,(TextEl.finalize_geometry e r l res))
    | LPath (e,r,l) -> GPath (e,r,l,(PathEl.finalize_geometry e r l res))
    | LBox  (e,r,l) -> GBox  (e,r,l,(BoxEl.finalize_geometry  e r l res))
 
  let render_svg gt zindex = 
    match gt with
    | GText (e,r,l,g) -> TextEl.render_svg e r l g zindex
    | GPath (e,r,l,g) -> PathEl.render_svg e r l g zindex
    | GBox  (e,r,l,g) -> []

end

(*m Element - using the DiagramElement *)
include Element_func.ElementFunc(DiagramElement)

let make_text properties text     = make_et properties (DiagramElement.EText text) []
let make_path properties path     = make_et properties (DiagramElement.EPath path) []
let make_box  properties elements = make_et properties (DiagramElement.EBox (Box.make ())) elements


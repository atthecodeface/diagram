type t = {
    family : string;
    ascent : float;
    descent : float;
    avg_width : float;
  }

(*
    let defn_svg t = Svg.tag "font-face" [("font-family", t.family), 
    let use_svg  t = Svg.tag "font-face" [("font-family", t.family), 
 *)
let make family ascent descent avg_width = 
  { family; ascent; descent; avg_width; }

let width_of_text font size text = font.avg_width *. size *. (float (String.length text))
let height_of_text font size = (font.ascent +. font.descent) *. size
let text_spacing font size = 1.0

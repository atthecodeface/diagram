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

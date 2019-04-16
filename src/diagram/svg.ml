(* Turn in to structured doc.ml
Add support for xmlm input and output
Add support for human-markup input and output
 *)
type t_attr = | StringAttr of (string * string)
              | FloatAttr of (string * float)
              | FloatsAttr of (string * (float array))
              | RectangleAttr of (string * (float*float*float*float))

type t = {
tag_type : string;
attributes : t_attr list;
cdata    : string list;
contents : t list;
  }
let string_of_float f =
  let s = Printf.sprintf "%f" f in
  let n = String.length s in
  let rec skip_back_zeros n =
    if n<=0 then 1 else (
      if (String.get s n) != '0' then (n+1) else
        skip_back_zeros (n-1)
    )
  in
  let skip_back_trailing_point n =
    if (String.get s (n-1)) == '.' then (n-1) else n
  in
  String.sub s 0 (skip_back_trailing_point (skip_back_zeros (n-1)))

let attribute_string n v = StringAttr (n, v)
let attribute_float  n v = FloatAttr (n, v)
let attribute_floats  n v = FloatsAttr (n, v)
let attribute_rectangle n r = RectangleAttr (n, r)
let attribute_text t = 
  match t with 
  | StringAttr (n,v) -> Printf.sprintf "%s='%s'" n v
  | FloatAttr (n,v) -> Printf.sprintf "%s='%s'" n (string_of_float v)
  | FloatsAttr (n,v) -> Printf.sprintf "%s='%s'" n (String.concat " " (Array.fold_left (fun acc v->acc@[(string_of_float v)]) [] v))
  | RectangleAttr (n,v) -> 
    let (d0, d1, d2, d3) = v in
    Printf.sprintf "%s='%s %s %s %s'" n (string_of_float d0) (string_of_float d1) (string_of_float d2) (string_of_float d3)
let tag tag_type attributes contents cdata = { tag_type; attributes; contents; cdata; }
let pretty_print_indented indent f t s =
    Printf.fprintf f "%s%s\n" indent s
let pretty_print_open_tag indent close_tag f t =
    let open_string = Printf.sprintf "<%s" t.tag_type in
    let close_string = if close_tag then "/>" else ">" in
    let attribute_string_list = List.map attribute_text t.attributes in
    let attribute_string = String.concat " " attribute_string_list in
    let tag_string = String.concat " " [open_string; attribute_string; close_string] in
    pretty_print_indented indent f t tag_string
let pretty_print_close_tag indent f t =
    let close_string = Printf.sprintf "</%s>" t.tag_type in
    pretty_print_indented indent f t close_string
    
let rec pretty_print ?extra_indent:(extra_indent="  ") ?indent:(indent="") f t  =
  match t.cdata with
  | [] -> (
    match t.contents with 
    | [] -> pretty_print_open_tag indent true f t
    | _  ->
       ( pretty_print_open_tag indent false f t ;
         List.iter (fun c -> pretty_print ~extra_indent:extra_indent ~indent:(extra_indent^indent) f c) t.contents;
         pretty_print_close_tag indent f t
       )
  )
  | _ -> (
       ( pretty_print_open_tag indent false f t ;
         List.iter (fun c -> pretty_print ~extra_indent:extra_indent ~indent:(extra_indent^indent) f c) t.contents;
         List.iter (fun i->Printf.fprintf f "%s" i) t.cdata;
         pretty_print_close_tag indent f t
       )
  )
       
let svg_print_hdr f = 
    Printf.fprintf f "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
    Printf.fprintf f "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n";
    ()

let svg_path ?close:(close=false) ?dx:(dx=0.) ?dy:(dy=0.) coords = 
  let rec make_path coords n acc act i =
    if (i>=n-1) then acc else (
      let acc = Printf.sprintf "%s %s %g %g" acc act (coords.(i)+.dx) (coords.(i+1)+.dy) in
      make_path coords n acc "L" (i+2)
    )
  in
  let path = make_path coords (Array.length coords) "" "M" 0 in
  if close then path ^ "z" else path

let svg_defs =
  let circle ?color:(color="context-stroke") cx cy r =
    tag "circle" [FloatAttr("cx",cx); FloatAttr("cy",cy); FloatAttr("r",r); StringAttr("fill",color);] [] []
  in
  let shape ?color:(color="context-stroke") ?dx ?dy coords =
    tag "path" [StringAttr("d",svg_path ~close:true ?dx:dx ?dy:dy coords); StringAttr("fill",color);] [] []
  in
  let triangle ?color:(color="context-stroke") ?dx:(dx=0.) w h =
    tag "path" [StringAttr("d",Printf.sprintf"M %g 0 L %g %g L %g %g z" dx (w+.dx) (h/.2.) dx h); StringAttr("fill",color);] [] []
  in
  let marker id w h rx ry mw mh content =
    tag "marker" [StringAttr("id",id); FloatsAttr("viewBox",[|0.;0.;w;h|]); StringAttr("orient","auto-start-reverse");
                  FloatAttr("refX",rx); FloatAttr("refY",ry);
                  FloatAttr("markerWidth",mw); FloatAttr("markerHeight",mh);] content []
  in
  let markers = [
    marker "stub"    10. 10. 0.05 4.95  1.01 1.01     [shape [|0.;0.;10.;0.;10.;10.;0.;10.;|]; ];
    marker "circ"    10. 10. 7.5 5. 3. 3.    [circle 5. 5. 5.; ]; (* refx=radius*(1-cos(asin(2/marker width))) *)
    marker "extcirc" 10. 10. 0. 5. 3. 3.     [circle 5. 5. 5.; ]; (* ext so refx=0. - cannot do transparent yet (needs two paths) *)
    marker "arr" 10. 10. 5. 5. 4. 3.      [triangle 10. 10.; ]; (* (length-refx)/2 = length/marker width *)
    marker "dblarr" 15. 10. 10. 5. 4. 3.  [triangle 10. 10.; triangle ~dx:5. 10. 10.; ];
    marker "arr2" 10. 10. 5. 5. 4. 3.     [shape [|0.;10.;10.;5.;0.;0.;5.;5.;|]; ];
    marker "dblarr2" 15. 10. 10. 5. 4. 3. [shape [|0.;10.;10.;5.;0.;0.;5.;5.;|]; shape ~dx:5. [|0.;10.;10.;5.;0.;0.;5.;5.;|]; ];
    ] in
  tag "defs" [] markers []
  
let svg_doc contents bbox =
  let attributes = [
      (attribute_string "version" "1.2");
      (attribute_string "xmlns" "http://www.w3.org/2000/svg");
      (attribute_rectangle "viewBox" bbox);
      (attribute_string "preserveAspectRatio" "xMidYMid");
      (attribute_string "fill-rule" "evenodd");
      (attribute_float  "stroke-width" 28.222);
      (attribute_string "stroke-linejoin" "round");
      ] in
    tag "svg" attributes contents []

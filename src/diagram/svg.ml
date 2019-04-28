(* Turn in to structured doc.ml
Add support for xmlm input and output
Add support for human-markup input and output
 *)
type t_attr = | StringAttr of (string * string)
              | FloatAttr of (string * float)
              | FloatsAttr of (string * (float array))
              | RectangleAttr of (string * (float array))

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
  | RectangleAttr (n,r) -> 
    Printf.sprintf "%s='%s %s %s %s'" n (string_of_float r.(0)) (string_of_float r.(1)) (string_of_float r.(2)) (string_of_float r.(3))

let tag tag_type attributes contents cdata = { tag_type; attributes; contents; cdata; }

let pretty_print_indented indent f t s =
    Printf.fprintf f "%s%s\n" indent s

let pretty_print_open_tag ?resolve_attr:(resolve_attr=fun x->x) indent close_tag f t =
    let open_string = Printf.sprintf "<%s" t.tag_type in
    let close_string = if close_tag then "/>" else ">" in
    let attribute_string_list = List.map (fun a -> attribute_text (resolve_attr a))  t.attributes in
    let attribute_string = String.concat " " attribute_string_list in
    let tag_string = String.concat " " [open_string; attribute_string; close_string] in
    pretty_print_indented indent f t tag_string

let pretty_print_close_tag indent f t =
    let close_string = Printf.sprintf "</%s>" t.tag_type in
    pretty_print_indented indent f t close_string

let internal_pretty_print ?extra_indent:(extra_indent="  ") ?initial_indent:(initial_indent="") ~resolve_attr f t =
  let rec print_element t indent =
    match t.cdata with
    | [] -> (
      match t.contents with 
      | [] -> pretty_print_open_tag indent true f t
      | _  ->
         ( pretty_print_open_tag ~resolve_attr:resolve_attr indent false f t ;
           List.iter (fun c -> print_element c (extra_indent^indent)) t.contents;
           pretty_print_close_tag indent f t
         )
    )
    | _ -> (
         ( pretty_print_open_tag ~resolve_attr:resolve_attr indent false f t ;
           List.iter (fun c -> print_element c (extra_indent^indent)) t.contents;
           List.iter (fun i->Printf.fprintf f "%s" i) t.cdata;
           pretty_print_close_tag indent f t
         )
    )
  in
  print_element t initial_indent

let deduplicate_id hash t =
  match t with 
  | StringAttr (n,v) when n="id" -> (
    if (Hashtbl.mem hash v) then (
     let x=1+(Hashtbl.find hash v) in
     Hashtbl.replace hash v (x+1);
     StringAttr (n,Printf.sprintf"%s___%d" v x)
     ) else (
     Hashtbl.replace hash v 0; t
     )
  )
  | _ -> t

let pretty_print ?extra_indent:(extra_indent="  ") ?intial_indent:(initial_indent="") f t  =
  let hashed_ids = Hashtbl.create 1024 in
  internal_pretty_print ~extra_indent ~initial_indent ~resolve_attr:(deduplicate_id hashed_ids) f t
       
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

let svg_defs version =
  let default_color = if version!="2.0" then "black" else "context-stroke" in
  let orient        = if version!="2.0" then StringAttr("orient","auto") else StringAttr("orient","auto-start-reverse") in
  let circle ?color:(color=default_color) cx cy r =
    tag "circle" [FloatAttr("cx",cx); FloatAttr("cy",cy); FloatAttr("r",r); StringAttr("fill",color);] [] []
  in
  let shape ?color:(color=default_color) ?dx ?dy coords =
    tag "path" [StringAttr("d",svg_path ~close:true ?dx:dx ?dy:dy coords); StringAttr("fill",color);] [] []
  in
  let triangle ?color:(color=default_color) ?dx:(dx=0.) w h =
    tag "path" [StringAttr("d",Printf.sprintf"M %g 0 L %g %g L %g %g z" dx (w+.dx) (h/.2.) dx h); StringAttr("fill",color);] [] []
  in
  let rotate x y a c = [tag "g" [StringAttr ("transform",Printf.sprintf "rotate(%g %g %g)" a x y);] c []] in
  let add_markers m id w h rx ry mw mh content =
    let marker = tag "marker" [StringAttr("id",id); FloatsAttr("viewBox",[|0.;0.;w;h|]); orient;
                  FloatAttr("refX",rx); FloatAttr("refY",ry);
                  FloatAttr("markerWidth",mw); FloatAttr("markerHeight",mh);] content []
    in
    let rev_marker = tag "marker" [StringAttr("id","rev_"^id); FloatsAttr("viewBox",[|0.;0.;w;h|]); orient;
                  FloatAttr("refX",w-.rx); FloatAttr("refY",ry);
                  FloatAttr("markerWidth",mw); FloatAttr("markerHeight",mh);] (rotate (w/.2.) (h/.2.) 180. content) []
   in
   marker :: rev_marker :: m
  in
  let m = [] in
  let m = add_markers m "none"    10. 10. 0. 0. 1. 1. [] in
  let m = add_markers m "stub"    10. 10. 0.05 4.95  1.01 1.01     [shape [|0.;0.;10.;0.;10.;10.;0.;10.;|];] in
  let m = add_markers m "circ"    10. 10. 7.5 5. 3. 3.    [circle 5. 5. 5.; ] in (* refx=radius*(1-cos(asin(2/marker width))) *)
  let m = add_markers m "extcirc" 10. 10. 0. 5. 3. 3.     [circle 5. 5. 5.; ] in (* ext so refx=0. - cannot do transparent yet (needs two paths) *)
  let m = add_markers m "arr" 10. 10. 5. 5. 4. 3.      [triangle 10. 10.; ]   in (* (length-refx)/2 = length/marker width *)
  let m = add_markers m "dblarr" 15. 10. 10. 5. 4. 3.  [triangle 10. 10.; triangle ~dx:5. 10. 10.; ] in
  let m = add_markers m "arr2" 10. 10. 5. 5. 4. 3.     [shape [|0.;10.;10.;5.;0.;0.;5.;5.;|]; ] in
  let m = add_markers m "dblarr2" 15. 10. 10. 5. 4. 3. [shape [|0.;10.;10.;5.;0.;0.;5.;5.;|]; shape ~dx:5. [|0.;10.;10.;5.;0.;0.;5.;5.;|]; ] in
  tag "defs" [] m []

let svg_doc version contents bbox =
  let attributes = [
      (attribute_string "version" version);
      (attribute_string "xmlns" "http://www.w3.org/2000/svg");
      (attribute_rectangle "viewBox" bbox);
      (attribute_string "preserveAspectRatio" "xMidYMid");
      (attribute_string "fill-rule" "evenodd");
      ] in
    tag "svg" attributes contents []

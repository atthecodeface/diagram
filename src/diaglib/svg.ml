type t_attr = | StringAttr of (string * string)
              | FloatAttr of (string * float)
              | RectangleAttr of (string * (float*float*float*float))

type t = {
tag_type : string;
attributes : t_attr list;
contents : t list;
  }
let attribute_string n v = StringAttr (n, v)
let attribute_float  n v = FloatAttr (n, v)
let attribute_rectangle n r = RectangleAttr (n, r)
let attribute_text t = 
  match t with 
  | StringAttr (n,v) -> Printf.sprintf "%s='%s'" n v
  | FloatAttr (n,v) -> Printf.sprintf "%s='%f'" n v
  | RectangleAttr (n,v) -> 
    let (d0, d1, d2, d3) = v in
    Printf.sprintf "%s='%f %f %f %f'" n d0 d1 d2 d3
let tag tag_type attributes contents = { tag_type; attributes; contents; }
let pretty_print_indented indent f t s =
    Printf.printf "%s%s\n" indent s
let pretty_print_open_tag indent close_tag f t =
    let open_string = Printf.sprintf "<%s" t.tag_type in
    let close_string = if close_tag then "/>" else ">" in
    let attribute_string_list = List.map attribute_text t.attributes in
    let attribute_string = String.concat " " attribute_string_list in
    let tag_string = String.concat "" [open_string; attribute_string; close_string] in
    pretty_print_indented indent f t tag_string
let pretty_print_close_tag indent f t =
    let close_string = Printf.sprintf "</%s>" t.tag_type in
    pretty_print_indented indent f t close_string
    
let rec pretty_print ?indent:(indent="") f t  =
  match t.contents with 
  | [] -> pretty_print_open_tag indent true f t
  | _  ->
     ( pretty_print_open_tag indent false f t ;
       List.iter (fun c -> pretty_print ~indent:(String.concat " " ["  "; indent]) f c) t.contents;
       pretty_print_close_tag indent f t
     )
       

(*
<svg version="1.2" viewBox="0 0 25400 14288" 
 xmlns="http://www.w3.org/2000/svg"
 xmlns:ooo="http://xml.openoffice.org/svg/export"
 xmlns:xlink="http://www.w3.org/1999/xlink"
 xmlns:presentation="http://sun.com/xmlns/staroffice/presentation"
 xmlns:smil="http://www.w3.org/2001/SMIL20/"
 xmlns:anim="urn:oasis:names:tc:opendocument:xmlns:animation:1.0"
 xml:space="preserve">
 *)

let svg_print_hdr f = 
    Printf.printf "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
    Printf.printf "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n";
    ()

let svg_doc contents =
  let attributes = [
      (attribute_float "version" 1.2);
      (attribute_rectangle "viewBox" (0., 0., 100., 100.));
      (attribute_string "preserveAspectRatio" "xMidYMid");
      (attribute_string "fill-rule" "evenodd");
      (attribute_float  "stroke-width" 28.222);
      (attribute_string "stroke-linejoin" "round");
      ] in
    tag "svg" attributes contents

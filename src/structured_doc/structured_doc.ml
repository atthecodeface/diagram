module Hmlm = Hmlm
open Xmlm
type dtd       = Xmlm.dtd
type name      = Xmlm.name
type attribute = Xmlm.attribute
type tag       = Xmlm.tag
type signal    = Xmlm.signal

let ns_xml   = "xml"
let ns_xmlns = "xmlns"

let name_string (ns,n) =
    if (String.length ns)==0 then n else Printf.sprintf "%s:%s" ns n

let attr_string (n,v) =
    Printf.sprintf "%s='%s'" (name_string n) v


let pp_dtd        = Xmlm.pp_dtd
let pp_name       = Xmlm.pp_name
let pp_attribute  = Xmlm.pp_attribute
let pp_tag        = Xmlm.pp_tag

type input = [ | `Xmlm of Xmlm.input 
               | `Hmlm of Hmlm.input
             ]

let make_xmlm t = `Xmlm t
let make_hmlm t = `Hmlm t

let input = function
  | `Xmlm t -> Xmlm.input t
  | `Hmlm t -> Hmlm.input t

let rec pp t =
    let s = input t in
    match s with
    | `El_start ((ns,name),attrs) -> (
      Format.open_tag (Printf.sprintf "%s:%s%s" ns name (List.fold_left (fun acc attr -> acc ^ " " ^ (attr_string attr)) "" attrs));
      Format.print_string "Stuff";
      pp t ;
      pp t
    )
    | `El_end ->
      Format.close_tag ();
    | _ -> ()

let test = "
#font id=f0 face=\"Arial embedded\" ascent=10. descent=3. avg_width=5.
#box{ id=b0 border=5 bordercolor=blue
#text id=t0 grid=0,0,1,1 font=f0 size=11 color=black
#text id=t1 grid=1,0,1,1 font=f0 size=11 color=black
#text id=t2 grid=0,1,2,1 border=1 bordercolor=green fillcolor=red font=f0 size=11 color=black
#box}
"
(*
##text id=t0 grid=0,0,1,1 font=f0 size=11 color=black \"Some text\"
##text id=t1 grid=1,0,1,1 font=f0 size=11 color=black \"Some more text\"
##text id=t2 grid=0,1,2,1 border=1 bordercolor=green fillcolor=red font=f0 size=11 color=black \"A long line of text\"
"
 *)

let test_me _ =
  Format.(pp_set_mark_tags std_formatter true);
  let x = make_hmlm (Hmlm.make_input ~doc_tag:(("","svg"),[]) (`String (0,test))) in
  try (pp x) with Xmlm.Error ((l,c),e) -> Printf.printf "Error %s at line %d char %d \n" (Xmlm.error_message e) l c;
  Format.print_flush ();
  Printf.printf "\n";

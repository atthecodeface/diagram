(*a Color dictionary *)
let f_black    = [|0.;0.;0.|]
let f_red      = [|1.;0.;0.|]
let f_green    = [|0.;1.;0.|]
let f_blue     = [|0.;0.;1.|]
let f_yellow   = [|1.;1.;0.|]
let f_cyan     = [|0.;1.;1.|]
let f_magenta  = [|1.;0.;1.|]
let f_white    = [|1.;1.;1.|]
let dictionary = [ ("black", f_black);
                         ("red", f_red);
                         ("blue", f_blue);
                         ("green", f_green);
                         ("yellow", f_yellow);
                         ("cyan", f_cyan);
                         ("magenta", f_magenta);
                         ("white", f_white);
                       ]
                        
let from_name name =
  List.assoc_opt name dictionary

let name value = 
  let rec swap_assq_opt x = function
    [] -> None
  | (a,b)::l -> if b==x then Some a else swap_assq_opt x l
  in
  swap_assq_opt value dictionary

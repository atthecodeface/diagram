(** Copyright (C) 2018,  Gavin J Stark.  All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * @file     diaglib.ml
 * @brief    Diagrams for SVG initially
 *
 *)
(*a Import modules *)

(*a Stylesheet things *)
(*
let stylesheet = Element.create_stylesheet ()
let sel_true            =  (fun e -> true)
let sel_cbox            =  Stylesheet.se_is_element_id "control"
let sel_type_button     =  Stylesheet.se_is_element_type "text_button"
let sel_cls_rotate      =  Stylesheet.se_has_element_class "rotate"
let sel_state_pressed   =  Stylesheet.se_is_element_state 0 3
let sel_state_hover     =  Stylesheet.se_is_element_state 0 2
let sel_state_enable    =  Stylesheet.se_is_element_state 0 1

let sel_button_rotate = fun e -> (sel_type_button e) && (sel_cls_rotate e)
let sel_hover_button  = fun e -> (sel_type_button e) && (sel_state_hover e)

let _ = 
    Stylesheet.add_style_rule stylesheet [sel_cbox; sel_hover_button]
             [("border_color", Sv_rgb [|1.;1.;1.;|]);
             ];
    Stylesheet.add_style_rule stylesheet [sel_cbox; sel_type_button]
             [("border", Sv_floats (6,[|1.;1.;1.;1.;1.;1.;|]));
             ];
    Stylesheet.add_style_rule stylesheet [sel_true]
             [("margin", Sv_floats (6,[|0.;0.;0.;0.;0.;0.;|]));
             ];
    ()


 *)

(*a HML stuff *)
exception Failed_to_parse of string
exception Bad_tag of string
open Structured_doc
open Stylesheet
module Rules = struct
type t_style = {
    id : string;
    substyles : t_style list;
    styling : (string * Stylesheet.Value.t_styleable_value) list;
  }

let rec str_style ?indent:(indent="") s =
    let heading = Printf.sprintf "%sStyle %s:" indent s.id in
    let styling_strings = List.map (fun (s,v) -> Printf.sprintf "%s  : %s=%s" indent s (Stylesheet.Value.str v)) s.styling in
    let body_strings = List.map (str_style ~indent:(indent ^ "  ")) s.substyles in
    String.concat "\n" (heading::(styling_strings @ body_strings))

type t_rule = {
    conditions : (string * string) list option; (* None for apply style *)
    styles : string list; (* Style ids to apply *)
  }

let str_rule r =
    Printf.sprintf "Rule:"

type t_ruleset = {
    styles : t_style list;
    rules : t_rule list;
  }

let str_ruleset rs =
  let str_styles = List.map str_style rs.styles in
  let str_rules = List.map str_rule rs.rules in
  String.concat "\n" ("Ruleset:" :: (str_styles @ str_rules))

type t = | Style of t_style
         | Rule of t_rule
         | Ruleset of t_ruleset

(*f make_style : stylesheet -> attrs -> contents -> t_style
attrs allowed are id and styles
 *)
let make_style stylesheet attrs contents = 
  let extract_style = function
    | Style s -> s
    | _ -> raise (Failed_to_parse "styles can only contain styles")
  in
  let substyles = List.map extract_style contents in
  let add_stylings (id,styling) (name,value) =
    if String.equal name "id" then (value,styling) else (
    let sid = Stylesheet.sid_find_exn stylesheet name in
    let stype = Stylesheet.sid_get_type sid in
    let svalue = Stylesheet.Value.from_string stype value in
    (id,(name,svalue)::styling)
    )
  in
  let (id,styling) = List.fold_left add_stylings ("",[]) attrs in
  Style {id; substyles; styling;}

let make contents = 
  let styles = List.fold_left (fun acc -> function | Style s -> (s::acc) | _ -> acc) [] contents in
  let rules  = List.fold_left (fun acc -> function | Rule s -> (s::acc) | _ -> acc) [] contents in
  Ruleset {styles; rules;}

let make_rule attrs contents =
  (* May have style= attribute *)
  (* May have conditions as attr= (for id=, class=, tag=?) and subtree=0/1 *)
  let conditions = None in
  let styles = ["fred"] in
  Rule {conditions; styles}

let make_apply attrs contents = 
  (* Must have style= attribute only *)
  let conditions = None in
  let styles = ["fred"] in
  Rule {conditions; styles}

let str = function
  | Ruleset rs -> str_ruleset rs
  | Style   s  -> str_style s
  | Rule    r  -> str_rule r
end

module Stylesheet_ml = struct
  let rec read_element_contents stylesheet opt_nsn_att rev_acc t =
    match (input t) with
    | `El_start ((ns,name),attrs) -> (
      let e = read_element_contents stylesheet (Some ((ns,name),attrs)) [] t in
      read_element_contents stylesheet opt_nsn_att (e::rev_acc) t
    )
    | `El_end -> (
      let contents = List.rev rev_acc in
      let e = (
          match opt_nsn_att with 
          | None -> Rules.make contents
          | Some ((ns,name),attrs) ->  (
             let attrs = List.map (fun ((ns,name),value) -> (name,value)) attrs in
             if (String.equal name "style") then (
               Rules.make_style stylesheet attrs contents
             ) else if (String.equal name "rule") then (
               Rules.make_rule attrs contents
             ) else if (String.equal name "apply") then (
               Rules.make_apply attrs contents
             ) else (
               raise (Bad_tag name)
             )
          )
        ) in
         e
    )
    | _ -> (
      read_element_contents stylesheet opt_nsn_att rev_acc t
    )

  let read_rules_from_hml stylesheet f =
    let hml = make_hmlm (Hmlm.make_input ~doc_tag:(("","stylesheet"),[]) f) in
    let diag = (
        try (read_element_contents stylesheet None [] hml)
        with 
        | Xmlm.Error ((l,c),e) -> (
          raise (Failed_to_parse (Printf.sprintf "Error %s at line %d char %d \n" (Xmlm.error_message e) l c))
        )
        | Bad_tag x -> (
          raise (Failed_to_parse (Printf.sprintf "Bad tag %s\n" x))
        )
      ) in
    Printf.printf "%s\n" (Rules.str diag);
    diag
end
let stylesheet_add_rules stylesheet f = Stylesheet_ml.read_rules_from_hml stylesheet f

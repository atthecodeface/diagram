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
(*a HML stuff *)
exception Failed_to_parse of string
exception Bad_tag of string
exception Bad_attr of string * string * string
open Structured_doc
open Stylesheet

(*a Rules_ml module *)
module Rules_ml = struct
  (*t t_style - Style in markup: id, substyles and the actual styling *)
  type t_style = {
      id : string;
      substyles : t_style list;
      styling : (string * Stylesheet.Value.t_styleable_value) list;
    }

  (*t t_apply - Apply in markup: list of styles (probably can be at most one long) *)
  type t_apply = {
      styles : string list; (* Style ids to apply *)
    }

  (*t t_rule - Rule in markup: list of conditions for this rule, style applications, and subrules *)
  type t_rule = {
      conditions : (string * string) list;
      subrules : t_rule list;
      applies : t_apply list;
    }

  (*t t_ruleset - Type for the whole ruleset: styles, and rules *)
  type t_ruleset = {
      styles : t_style list;
      rules : t_rule list;
    }

  (*t t - Union of markup elements for 'contents' *)
  type t = | Style of t_style (* Top level, or child o style *)
           | Apply of t_apply (* Top level, or child of rule *)
           | Rule of t_rule (* Top level, or child of rule *)
           | Ruleset of t_ruleset (* Top level only *)

  (*f make_style : stylesheet -> attrs -> contents -> t
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
    {id; substyles; styling;}

  (*f str_style : ?indent -> t_style -> string *)
  let rec str_style ?indent:(indent="") s =
    let heading = Printf.sprintf "%sStyle %s:" indent s.id in
    let styling_strings = List.map (fun (s,v) -> Printf.sprintf "%s  : %s=%s" indent s (Stylesheet.Value.str v)) s.styling in
    let body_strings = List.map (str_style ~indent:(indent ^ "  ")) s.substyles in
    String.concat "\n" (heading::(styling_strings @ body_strings))

  (*f make_apply : attrs -> contents -> t
attrs allowed are style; contents must be empty
   *)
  let make_apply attrs contents = 
    match contents with
    | [] -> ( 
      let find_styles acc (name,value) =
        if String.equal name "style" then [value] else raise (Bad_attr ("apply",name,value))
      in
      (* Must have style= attribute only *)
      let styles = List.fold_left find_styles [] attrs in
      {styles; }
    )
    | _ -> raise (Failed_to_parse "apply must not have children")

  (*f str_apply : ?indent -> t_apply -> string *)
  let str_apply ?indent:(indent="") (r:t_apply) =
    let heading = Printf.sprintf "%sApply:" indent in
    String.concat " " (heading::r.styles)

  (*f make_rule : attrs -> contents -> t
attrs allowed are style
   *)
  let make_rule attrs contents =
    (* May have contents of rules or applies *)
    let extract_rule_applies (rules,rev_applies) = function
      | Apply a -> (rules,(a::rev_applies))
      | Rule r -> ((r::rules),rev_applies)
      | _ -> raise (Failed_to_parse "rule can only contain rule or apply children")
    in
    let (subrules,rev_applies) = List.fold_left extract_rule_applies ([],[]) contents in
    (* May have style= attribute *)
    (* May have conditions as attr= (for id=, class=, tag=?) and subtree=0/1 *)
    let add_applies_conditions (rev_applies,conditions) (name,value) =
      if String.equal name "style" then (
        let apply = {styles=[value]} in
        (apply::rev_applies),conditions
      ) else (
        (rev_applies,(name,value)::conditions)
      )
    in
    let (rev_applies,conditions) = List.fold_left add_applies_conditions (rev_applies,[]) attrs in
    let applies = List.rev rev_applies in
    {conditions; subrules; applies;}

  (*f str_rule : ?indent -> t_rule -> string *)
  let rec str_rule ?indent:(indent="") r =
    let conditions_string = List.map (fun (n,v) -> Printf.sprintf "%s=%s" n v) r.conditions in
    let heading = Printf.sprintf "%sRule:" indent in
    let heading = String.concat " " (heading::conditions_string) in
    let rules_strings   = List.map (str_rule ~indent:(indent ^ "  ")) r.subrules in
    let applies_strings = List.map (str_apply ~indent:(indent ^ "  ")) r.applies in
    String.concat "\n" (heading::(rules_strings @ applies_strings))

  (*f make contents -> t_ruleset *)
  let make contents = 
    let styles = List.fold_left (fun acc -> function | Style s -> (s::acc) | _ -> acc) [] contents in
    let rules  = List.fold_left (fun acc -> function | Rule s -> (s::acc) | _ -> acc) [] contents in
    Ruleset {styles; rules;}

  (*f str_ruleset t_ruleset -> string *)
  let str_ruleset rs =
    let str_styles = List.map str_style rs.styles in
    let str_rules = List.map str_rule rs.rules in
    String.concat "\n" ("Ruleset:" :: (str_styles @ str_rules))

  (*f get_ruleset t -> t_ruleset *)
  let get_ruleset = function
    | Ruleset r -> r
    | _ -> raise (Failed_to_parse "Bug - trying to retreive ruleset from a non-ruleset")

  (*f str : t -> string *)
  let str = function
    | Ruleset rs -> str_ruleset rs
    | Style   s  -> str_style s
    | Apply   s  -> str_apply s
    | Rule    r  -> str_rule r
  (*f read_element_contents *)
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
          | None -> make contents
          | Some ((ns,name),attrs) ->  (
             let attrs = List.map (fun ((ns,name),value) -> (name,value)) attrs in
             if (String.equal name "style") then (
               Style (make_style stylesheet attrs contents)
             ) else if (String.equal name "rule") then (
               Rule (make_rule attrs contents)
             ) else if (String.equal name "apply") then (
               Apply (make_apply attrs contents)
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

  (*f read_rules_from_hml *)
  let read_rules_from_hml stylesheet f =
    let hml = make_hmlm (Hmlm.make_input ~doc_tag:(("","stylesheet"),[]) f) in
    let rules_t = (
        try (read_element_contents stylesheet None [] hml)
        with 
        | Xmlm.Error ((l,c),e) -> (
          raise (Failed_to_parse (Printf.sprintf "Error %s at line %d char %d \n" (Xmlm.error_message e) l c))
        )
        | Bad_tag x -> (
          raise (Failed_to_parse (Printf.sprintf "Bad tag %s\n" x))
        )
      ) in
    (* Printf.printf "%s\n" (str rules_t); *)
    get_ruleset rules_t

  (*f All done *)
end

(*a Rules module *)
module Rules = struct
  (*e Duplicate_style_id *)
  exception Duplicate_style_id of string

  (*e Unknown_style_id *)
  exception Unknown_style_id of string

  (*e Bad_rule_condition *)
  exception Bad_rule_condition of string * string

  (*t t_style *)
  type t_style = {
    id : string; (* for reference *)
    styling : (string * Stylesheet.Value.t_styleable_value) list;
  }

  (*f uniquify_styling : (name * 'a) list -> (name * 'a) list -> (name * 'a) list (input without duplicates, reversed) *)
  let rec uniquify_styling acc = function
    | [] -> acc
    | (name,value)::tl -> (
      let id_matches (s,_) = String.equal s name in
      let acc = if (List.exists id_matches acc) then acc else ((name,value)::acc) in
      uniquify_styling acc tl
    )

  (*f make_styles : (string * value) list -> t_style list -> Rules_ml.t_style -> t_style list *)
  let rec make_styles styling acc (ml_style:Rules_ml.t_style) =
    let id = ml_style.id in
    let styling = (ml_style.styling) @ styling in
    let style = {id; styling} in
    List.fold_left (make_styles styling) (style::acc) ml_style.substyles

  (*t t_style_hash *)
  type t_style_hash = (string, t_style) Hashtbl.t

  (*f make_style_hash : t_style list -> t_style_hash *)
  let make_style_hash styles =
    let style_hash = Hashtbl.create 1024 in
    let add_style s =
    if (Hashtbl.mem style_hash s.id) then (
      raise (Duplicate_style_id s.id)
    ) else (
      Hashtbl.replace style_hash s.id s
    ) in
    List.iter add_style styles;
    style_hash

  let style_find style_hash s =
    if (Hashtbl.mem style_hash s) then (
      Hashtbl.find style_hash s
    ) else (
      raise (Unknown_style_id s)
    )

  (*t t_conditions *)
  type t_match = [ Stylesheet.t_style_match | `Ignored ]
  type t_conditions = {
    mutable subtree : bool;
    mutable classes : t_match;
    mutable id      : t_match;
    mutable tag     : t_match;
    }

  (*f match_of_value - should barf if cm is not `Ignored *)
  let match_of_value r cm value =
    Stylesheet.se_match_of_string value

  let add_selector_of_match acc match_fn = function
    | `Ignored -> acc
    | `Equals f -> (match_fn f)::acc
    | `Matches f -> (match_fn f)::acc

  let make_conditions nv_list =
    let c = {subtree=true; classes=`Ignored; id=`Ignored; tag=`Ignored} in
    let extract_conditions c (name,value) =
      if (String.equal name "tag") then (
        c.tag <- match_of_value "tag" c.tag value; c
      ) else if (String.equal name "id") then (
        c.id <- match_of_value "id" c.id value; c
      ) else if (String.equal name "class") then (
        c.classes <- match_of_value "class" c.classes value; c
      ) else if (String.equal name "subtree") then (
        c
      ) else (
        raise (Bad_rule_condition (name,value))
      )
    in
    List.fold_left extract_conditions c nv_list

  (*f selector_of_conditions *)
  let selector_of_conditions c =
    let selector_fns = [] in
    let selector_fns = add_selector_of_match selector_fns Stylesheet.se_match_element_class c.classes in
    let selector_fns = add_selector_of_match selector_fns Stylesheet.se_match_element_id    c.id in
    let selector_fns = add_selector_of_match selector_fns Stylesheet.se_match_element_type  c.tag in
    let selector = fun t -> List.fold_left (fun acc sel -> acc && (sel t)) true selector_fns in
    (c.subtree, selector)

  (*t t_rule *)
  type t_rule = {
    selectors : Stylesheet.t_style_selector list;
    styles    : t_style list; (* styles are in correct priority order - highest first in list *)
    (* priority ? *)
    }

  (*f make_rules : Stylesheet.t_style_selector list -> t_rules list -> t_style_hash -> Rules_ml.t_rule -> t_rules list *)
  let rec make_rules stack style_hash acc (ml_rule:Rules_ml.t_rule) =
    (* conditions can be 'class', 'id', 'tag', 'subtree' *)
    let conditions = make_conditions ml_rule.conditions in
    let selectors = stack @ [selector_of_conditions conditions] in
    let merge_applies acc (a:Rules_ml.t_apply) = 
      List.rev_append a.styles acc
    in
    (* ml_rule.applies is in reverse priority order *)
    let styles = List.fold_left merge_applies [] ml_rule.applies in
    let styles = List.map (style_find style_hash) styles in
    let rule = {selectors; styles} in
    List.fold_left (make_rules selectors style_hash) (rule::acc) ml_rule.subrules

  (*f add_style_rule *)
  let add_style_rule stylesheet rule =
    let styling = List.fold_left (fun acc r->acc @ r.styling) [] rule.styles in
    let styling = uniquify_styling [] styling in
    Stylesheet.add_style_rule stylesheet rule.selectors styling

  (*t t *)
  type t = {
    styles : t_style list;
    style_hash : t_style_hash;
    rules : t_rule list;
  }

  (*f make : Rules_ml.t_ruleset -> t *)
  let make (rules_ml:Rules_ml.t_ruleset) =
    let styles = List.fold_left (make_styles []) [] rules_ml.styles in
    let style_hash = make_style_hash styles in
    let rules = List.fold_left (make_rules [] style_hash) [] rules_ml.rules in
    {styles; style_hash; rules;}

  (*f add_style_rules *)
  let add_style_rules stylesheet t =
    List.iter (add_style_rule stylesheet) t.rules

  (*f All done *)
end

(*a Stylesheet_ml module *)
module Stylesheet_ml = struct
  let read_rules_from_hml stylesheet f =
    let ruleset_ml = Rules_ml.read_rules_from_hml stylesheet f in
    let rules = Rules.make ruleset_ml in
    rules

  let add_rules stylesheet f =
    let rules = read_rules_from_hml stylesheet f in
    Rules.add_style_rules stylesheet rules

  let add_rules_from_file stylesheet filename =
    let f = open_in filename in
    add_rules stylesheet (`Channel f);
    close_in f
end
let add_rules           = Stylesheet_ml.add_rules
let add_rules_from_file = Stylesheet_ml.add_rules_from_file

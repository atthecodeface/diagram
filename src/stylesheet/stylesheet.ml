(** Copyright (C) 2017-2018,  Gavin J Stark.  All rights reserved.
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
 * @file    stylesheet.ml
 * @brief   A library for having cascading stylesheets
 *
 *)

(*a Notes
CSS notes

font
-family - comma separated list of font names
-size - size or percentage of inheritance
-weight
-stretch
[-thickness?]
line-height

vertical-align

max-width

margin 0 auto !important

padding (top, bottom, left, right)

border (top, bottom, left, right)
border-color
border-style
border-width

box-sizing

display


background

color


value
value_ref
types
style
styleable
stylesheet


*)

(*a Libraries *)
module Styleable_value = Value
module Style_id      = Style.Style_id
module Style_ids     = Style.Style_ids
module Style         = Style.Style
module Styleable_desc        = Styleable.Styleable_desc
module Styleable_desc_built  = Styleable.Styleable_desc_built
open Types

(*a Hack *)
         (*f get_default_value *)
         let get_default_value sid t =
           Style.get_value sid t.default_style

         (*f is_default_inherit *)
         let is_default_inherit sid t =
           Style.get_opt sid t.default_style

         (*f style_id_of_name *)
         let style_id_of_name name t =
           let hash = Style_id.hash_of_string name in
           Style_ids.find_opt_id hash t.ids

         (*f style_id_of_name_exn *)
         let style_id_of_name_exn name t =
           let hash = Style_id.hash_of_string name in
           Style_ids.find_id_exn hash t.ids

         (*f add_styleable *)
         let add_styleable s t =
           t.entity_list <- s :: t.entity_list

         (*f build_desc *)
         let build_desc desc t =
           if (not (List.mem_assoc desc t.built_descs)) then
             (t.built_descs <- (desc,Styleable_desc_built.create desc t.ids)::t.built_descs);
           List.assoc desc t.built_descs

module Styleable = struct
      (*f set_parent*)
      let set_parent p t = t.parent <- Some p
                                            
      (*f get_id *)
      let get_id t = t.id_name

      (*f get_type *)
      let get_type t = t.type_name

      (*f get_nth_sid *)
      let get_nth_sid n t = 
        if n<t.num_base_styles then
          Styleable_desc_built.get_nth_sid n t.desc_built
        else
          t.extra_sids.(n-t.num_base_styles)

      (*f find_sid_index *)
      let find_sid_index sid t =
        match (Styleable_desc_built.find_sid_index sid t.desc_built) with
          Some sid -> Some sid
        | None -> (
          let find_sid acc i =
            let (n, opt_res) = acc in
            if (i=sid) then (n+1, Some n)
            else (n+1, opt_res)
          in
          let (_,opt_index) = Array.fold_left find_sid (0,None) t.extra_sids in
          opt_index
        )

      (*f find_sid_index_exn *)
      let find_sid_index_exn sid t =
        match find_sid_index sid t with 
          None -> raise (Styleable_desc_built.Style_id_not_found_in_binding (Style_id.str sid))
        | Some index -> index

      (*f get_value_ref *)
      let get_value_ref t sheet (s:string) =
        let sid = style_id_of_name_exn s sheet in
        let sindex = find_sid_index_exn sid t in
        t.values.(sindex)

      (*f get_value *)
      let get_value t sheet (s:string) =
        Value_ref.get_value (get_value_ref t sheet s)

      (*f is_element_id *)
      let is_element_id (s:string) t     =
        (*Printf.printf "Is_Element_Id %s : %s\n" t.id_name s;*)
        (t.id_name = s)

      (*f is_element_type *)
      let is_element_type (s:string) t   =
        (*Printf.printf "Is_Element_Type %s : %s : %s\n" t.id_name s t.type_name;*)
        (t.type_name = s)

      (*f is_element_state *)
      let is_element_state state value t =
        if (state>=(Array.length t.state)) then false
        else (
          (*Printf.printf "Is_Element_State %d %d : %d\n" state value t.state.(state);*)
          t.state.(state) = value
        )

      (*f set_element_state *)
      let set_element_state state value t =
        if (state>=(Array.length t.state)) then ()
        else (t.state.(state) <- value)

      (*f has_element_class *)
      let has_element_class (s:string) t =
        (*Printf.printf "Has_Element_Class %s : %s : %d\n" t.id_name s (List.length t.classes);*)
        (List.mem s t.classes) (* maybe not as this would use ==? *)

      (*f create *)
      let create desc sheet type_name name_values style_change_callback children =
        let desc_built = build_desc desc sheet in
        let id_name = 
          if (List.mem_assoc "id" name_values) then (List.assoc "id" name_values) else "no_id"
        in
        let classes = 
          let class_str = if (List.mem_assoc "class" name_values) then (List.assoc "class" name_values) else "" in
          let class_list = String.split_on_char ' ' class_str in
          List.filter (fun x->(x<>"")) class_list
        in
        let count_extra_styles acc nv =
          let (name,_) = nv in
          match style_id_of_name name sheet with
            None -> acc
          | Some sid -> (
            match Styleable_desc_built.find_sid_index sid desc_built with
              None -> (acc+1)
            | Some sid_index -> acc
          )
        in
        let num_extra_styles = List.fold_left count_extra_styles 0 name_values in
        let num_base_styles = (Array.length desc_built.sids) in
        let num_styles = (num_base_styles+num_extra_styles) in
        let t = {
            desc_built;
            num_base_styles;
            num_styles;
            children;
            style_change_callback;
            id_name;
            parent = None;
            type_name;
            classes;
            extra_sids = Array.make num_extra_styles Style_id.dummy;
            state      = Array.make (List.length desc.state_descriptor) 0;
            values     = Array.init num_styles (fun i -> Value_ref.create ());
          }
        in
        add_styleable t sheet;
        let add_extra_style acc nv =
          let (name,_) = nv in
          match style_id_of_name name sheet with
            None -> acc
          | Some sid -> (
            match Styleable_desc_built.find_sid_index sid t.desc_built with
              Some sid_index -> acc
            | None -> (t.extra_sids.(acc) <- sid; acc+1)
          )
        in
        ignore (List.fold_left add_extra_style 0 name_values);
        let set_default_value nv =
          let (name,value) = nv in
          match style_id_of_name name sheet with
            None -> ()
          | Some sid -> (
            match find_sid_index sid t with
              None -> ()
            | Some sid_index -> (
              let stype = Style_id.get_type sid in
              (*Printf.printf "Set default value of %s.%s.%s to be %s\n" t.id_name t.type_name name value;*)
              Value_ref.set_default_from_string t.values.(sid_index) stype value
            )
          )
        in
        List.iter set_default_value name_values;
        let set_inheritance n vr =
          let sid = get_nth_sid n t in
          let di = (is_default_inherit sid sheet) in
          Value_ref.set_default_inherit vr di
        in
        Array.iteri set_inheritance t.values;
        t

      (*f reset_next_values *)
      let rec reset_next_values t =
        Array.iter Value_ref.reset t.values;
        List.iter reset_next_values t.children

      (*f apply_styles *)
      let apply_styles l styles t = 
        let apply_style sid_sv =
          let (sid,sv) = sid_sv in
          match find_sid_index sid t with
            None -> ()
          | Some sindex ->
             Value_ref.apply l sv t.values.(sindex) 
        in
        List.iter apply_style styles

      (*f resolve_next_value *)
      let rec resolve_next_value sid t sheet = 
        let value_ref =
          match find_sid_index sid t with
            None -> (
            if (is_default_inherit sid sheet) then
              Value_ref.Inherit
            else
              Value_ref.Default
          )
          | Some sindex ->
             (
               Value_ref.next_value_ref t.values.(sindex)
             )
        in
        match value_ref with
          Value_ref.Ref v -> v
        | Value_ref.Default -> get_default_value sid sheet
        | Value_ref.Inherit ->
           (
             match t.parent with
               Some p -> resolve_next_value sid p sheet
             | None -> get_default_value sid sheet
           )

      (*f update_current_values_from_next *)
      let rec update_current_values_from_next sheet t =
        let update_nth_value n acc =
          let sid = get_nth_sid n t in
          let value = resolve_next_value sid t sheet in
          if (value == Value_ref.get_value t.values.(n)) then acc
          else (
            (*Printf.printf "update_nth_value %s : %s : %s \n%!" t.id_name (Style_id.str sid) (str_of_svalue value);*)
            Value_ref.set_value t.values.(n) value;
            (sid,value)::acc
          )
        in
        let rec update_values l n acc = 
          if (n>=l) then
            acc
          else
            let next_acc = update_nth_value n acc in
            update_values l (n+1) next_acc
        in
        let changed_sids = update_values t.num_styles 0 [] in
        if changed_sids <> [] then
          (
            (*Printf.printf "Sids changed for this %s.%s\n" t.id_name t.type_name;*)
            t.style_change_callback changed_sids
          );
        List.iter (update_current_values_from_next sheet) t.children

      (*f element_callback_matching_children *)
      let rec element_callback_matching_children selector e t callback = 
        List.iter (fun e -> if (selector e) then callback e) e.children

      (*f element_callback_all_children *)
      let element_callback_all_children _ e t callback = 
        element_callback_matching_children (fun e -> true) e t callback

      (*f element_callback_matching_subelements *)
      let rec element_callback_matching_subelements selector e t callback = 
        element_callback_matching_children selector e t callback;
        element_callback_all_children selector e t (fun e -> element_callback_matching_subelements selector e t callback);
        ()

      (*f element_callback_all_subelements *)
      let element_callback_all_subelements _ e t callback = 
        element_callback_matching_children (fun e -> true) e t callback

      (*f element_callback_matching_tree *)
      let element_callback_matching_tree element_style_selector e t callback = 
        if (element_style_selector e) then (callback e);
        element_callback_matching_subelements element_style_selector e t callback

      (*f All done *)
end
       (*m Style_rule - immutable *)
       module Style_rule = struct
         (*f create *)
         let create selectors styles =
           {
             selectors;
             styles;
           }

         (*f apply *)
         let apply t stylesheet element_callback_matching_tree =
           let apply_style e =
             Styleable.apply_styles (List.length t.selectors) t.styles e
           in
           let rec sel_cbk_for_remaining_rules rules =
             match rules with
             | hd::nxt::tail -> 
                let (sel,cbk) = sel_cbk_for_remaining_rules (nxt::tail) in
                (hd,  fun e -> Styleable.element_callback_matching_subelements sel e stylesheet cbk)
             | hd::tail -> (hd,apply_style)
             | [] -> ((fun e -> true),apply_style)
           in
           let (sel,cbk) = sel_cbk_for_remaining_rules t.selectors in
           element_callback_matching_tree stylesheet sel cbk

       (*f All done *)
       end
       (*m Stylesheet - rules, entities mutable; rest immutable *)
       module Stylesheet : sig 
         (*m sig *)
         (*t type *)
         type t
         val create : unit -> t
         val build  : t -> t_styleable list -> t
         val add_styleable : t_styleable -> t -> unit
         val add_style_defaults : t -> (string * t_styleable_value * bool) list -> unit
         val element_callback_matching_tree : t -> t_style_selector -> t_style_change_fn -> unit
         val add_style_rule : t -> t_style_selector list -> (t_styleable_name * t_styleable_value) list -> unit
         val apply_stylesheet : t -> unit
         val get_default_value  :  t_style_id -> t -> t_styleable_value
         val is_default_inherit :  t_style_id -> t -> bool
         val style_id_of_name     : string -> t -> t_style_id option
         val style_id_of_name_exn : string -> t -> t_style_id
         val build_desc : t_styleable_desc -> t -> t_styleable_desc_built
       end = struct
         (*m struct *)
         (*t type *)
         type t = t_stylesheet

         (*f create *)
         let create () = {
             entity_list = [];
             roots = [];
             ids = Style_ids.create ();
             default_style = Style.create [];
             rules = [];
             built_descs = [];
           }

         (*f build_desc *)
         let build_desc desc t =
           if (not (List.mem_assoc desc t.built_descs)) then
             (t.built_descs <- (desc,Styleable_desc_built.create desc t.ids)::t.built_descs);
           List.assoc desc t.built_descs

         (*f build *)
         let build t roots =
           t.roots <- roots;
           t

         (*f add_styleable *)
         let add_styleable s t =
           t.entity_list <- s :: t.entity_list

         (*f style_id_of_name *)
         let style_id_of_name name t =
           let hash = Style_id.hash_of_string name in
           Style_ids.find_opt_id hash t.ids

         (*f style_id_of_name_exn *)
         let style_id_of_name_exn name t =
           let hash = Style_id.hash_of_string name in
           Style_ids.find_id_exn hash t.ids

         (*f add_style_defaults *)
         let add_style_defaults t nvis =
           let add_id_and_style n_v_i = 
             let (name,svalue,def_inherit)=n_v_i in
             let stype = Styleable_value.stype_of_svalue svalue in
             let sid = Style_id.create name stype in
             Style_ids.add_id (Style_id.hash_of_string name) sid t.ids;
             Style.add_styling sid svalue def_inherit t.default_style
           in
           List.iter add_id_and_style nvis;
           (*Printf.printf "Default %s\n" (Style.str t.default_style);*)
           ()

         (*f get_default_value *)
         let get_default_value sid t =
           Style.get_value sid t.default_style

         (*f is_default_inherit *)
         let is_default_inherit sid t =
           Style.get_opt sid t.default_style

         (*f add_style_rule *)
         let add_style_rule t selectors style_nvs =
           let id_vs = Style_ids.build_id_value_list style_nvs t.ids in
           let rule = Style_rule.create selectors id_vs in
           t.rules <- rule :: t.rules

         (*f element_callback_matching_tree *)
         let element_callback_matching_tree t selector callback =
           List.iter (fun e -> Styleable.element_callback_matching_tree selector e t callback) t.roots

         (*f apply_stylesheet *)
         let apply_stylesheet t =
           (* clear new values, setting all to inherit, and clear longest matching rule
            *)
           List.iter (fun e -> Styleable.reset_next_values e) t.roots;

           (* Apply rules *)
           List.iter (fun r -> Style_rule.apply r t element_callback_matching_tree) t.rules;

           (* resolve next values to current values, determining which ones have changed,
    and callback if any have for each styleable
            *)
           List.iter (fun e -> Styleable.update_current_values_from_next t e) t.roots;
           ()

       (*f All done *)
       end

       let create_desc         = Styleable_desc.create

       let se_create              = Styleable.create
       let se_get_value_ref       = Styleable.get_value_ref
       let se_get_value           = Styleable.get_value
       let se_set_element_state   = Styleable.set_element_state
       let se_set_parent          = Styleable.set_parent
       let se_is_element_id       = Styleable.is_element_id
       let se_is_element_state    = Styleable.is_element_state
       let se_is_element_type     = Styleable.is_element_type
       let se_has_element_class   = Styleable.has_element_class

       let build               = Stylesheet.build
       let apply               = Stylesheet.apply_stylesheet
       let create              = Stylesheet.create
       let add_style_defaults  = Stylesheet.add_style_defaults
       let add_style_rule      = Stylesheet.add_style_rule

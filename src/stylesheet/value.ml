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
 * @file    value.ml
 * @brief   Styleable values
 *
 *)
(*a Helper functions and modules *)
(*f sfmt *)
let sfmt = Printf.sprintf

(*f string_is_none_rex *)
let string_is_none_rex =
  let opt_whitespace = Re.rep Re.space in
  Re.compile (Re.seq [opt_whitespace])

(*f string_as_float_rex *)
let string_as_float_rex =
  let point = Re.char '.' in
  let minus = Re.char '-' in
  let digit = Re.set "0123456789" in
  let float = Re.group (Re.seq [ Re.opt minus; Re.rep1 digit ; Re.opt point ; Re.rep digit] ) in
  let rest  = Re.group (Re.rep Re.any) in
  let opt_whitespace = Re.rep Re.space in
  Re.compile (Re.seq [opt_whitespace ; float ; rest])

(*f string_as_int_rex *)
let string_as_int_rex =
  let prefix = Re.seq [ Re.char '0' ; Re.set "xX" ] in
  let number = Re.group (Re.seq [ Re.opt prefix; Re.rep1 (Re.set "0123456789abcdefABCDEF")] ) in
  let rest  = Re.group (Re.rep Re.any) in
  let opt_whitespace = Re.rep Re.space in
  Re.compile (Re.seq [opt_whitespace ; number ; rest])

(*f extract_first_and_rest *)
let extract_first_and_rest rex s =
  match (Re.exec_opt rex s) with
  | None -> None
  | Some g -> Some ((Re.Group.get g 1),(Re.Group.get g 2))
    
(*f fill_out_array : 'a array -> size -> source_valid -> index_to_fill -> index_source -> 'a array
  Fills out the array so that elements 0 to size-1 contain valid data, assuming that 0 to n-1 initially
  contain valid data, replicating this data across the whole array as required.
 *)
let rec fill_out_array a num n i j =
  if (i=num) then
    a
  else
    (
      a.(i) <- a.(j);
      let next_j = if (j+1=n) then 0 else (j+1) in
      fill_out_array a num n (i+1) next_j
    )

(*f read_floats_from_n : float_array -> max -> string -> number -> (number, remaining_string)
  the float array will be completely valid, even if the string supplies fewer values
 *)
let rec read_floats_from_n floats max string = function
  | n when (n=max) -> (max, string)
  | n -> (
    match extract_first_and_rest string_as_float_rex string with
    | None -> (n, "")
    | Some (s1, s2) -> 
       (
         floats.(n) <- float_of_string s1;
         read_floats_from_n floats max s2 (n+1)
       )
  )

(*f read_floats : string -> number -> float array
  the float array will be completely valid, even if the string supplies fewer values
 *)
let read_floats string num = 
  let floats = Array.make num 0. in
  let (n,_) = read_floats_from_n floats num string 0 in
  fill_out_array floats num (max n 1) n 0

(*f read_float_arr : string  -> float array
  read a float array from the string, as many floats as possible
 *)
let read_float_arr string = 
  let rec acc_float_arrays acc s =
    let max = 10 in
    if (String.length s)=0 then acc else (
      let floats = Array.make max 0. in
      let (n,s) = read_floats_from_n floats max s 0 in
      let (total,arrs) = acc in
      let acc = (total+n,floats::arrs) in
      acc_float_arrays acc s
    )
  in
  let (total, arrs) = acc_float_arrays (0,[]) string in
  (total, Array.(sub (concat arrs) 0 total))

(*f read_ints *)
let read_ints string num = 
  let ints = Array.make num 0 in
  let rec read_ints_from_n n string =
    if (n=num) then
      n
    else
      (
        match extract_first_and_rest string_as_int_rex string with
        | None -> n
        | Some s12 -> 
           (
             let (s1,s2) = s12 in
             ints.(n) <- int_of_string s1;
             read_ints_from_n (n+1) s2
           )
      )
  in
  let n = read_ints_from_n 0 string in
  fill_out_array ints num (max n 1) n 0

(*f read_color *)
let read_color string = 
    match Color.from_name string with
    | Some f -> f
    | None -> read_floats string 3

(*f string_is_none - return True if string is none *)
let string_is_none string =
  match (Re.exec_opt string_is_none_rex string) with
  | None -> true
  | _ -> false

(*f read_tokens *)
let read_tokens string =
  let n = String.length string in
  let rec read_next_token rtl i t nt =
    if (i>=n) then (rtl, t, nt) else (
      let ch = String.get string i in
      if (ch==' ') then (
        if nt then (
          read_next_token rtl (i+1) t nt
        ) else (
          read_next_token (t::rtl) (i+1) "" true
        )
      ) else (
        let s = String.make 1 ch in
        if nt then (
          read_next_token rtl (i+1) s false
        ) else (
          read_next_token rtl (i+1) (t^s) false
        )
      )
    )
  in
  let (rtl, last_token, no_last_token) = read_next_token [] 0 "" true in
  let rtl = if no_last_token then rtl else last_token::rtl in
  List.rev rtl

(*a Types *)
(*e Exception Bad_value*)
exception Bad_value of string

(*t t_styleable_name *)
type t_styleable_name = string

(*t t_styleable_value *)
type t_styleable_value =
  | Sv_float_arr  of int * (float array)
  | Sv_floats     of int * (float array)
  | Sv_float      of float option
  | Sv_int        of int option
  | Sv_ints       of int * (int array)
  | Sv_rgb        of float array
  | Sv_string     of string option
  | Sv_token_list of string list

(*t t_styleable_type *)
type t_styleable_type = St_float_arr | St_floats of int | St_ints of int | St_float | St_int | St_rgb | St_string | St_token_list

(*a Styleable_value module *)
(*v sv_zero, sv_none_ints, sv_none_floats - for use as defaults *)
let sv_zero = Sv_int (Some 0)
let none_floats = Array.make 0 0.
let none_ints   = Array.make 0 0
let sv_none_floats = Sv_floats (0, none_floats)
let sv_none_float_arr = Sv_float_arr (0, none_floats)
let sv_none_ints   = Sv_ints (0, none_ints)
let sv_none_float  = Sv_float None
let sv_none_int    = Sv_int None
let sv_none_rgb    = Sv_rgb none_floats
let sv_none_string = Sv_string None
let sv_none_token_list = Sv_token_list []

(*v one_i_0, one_f_0 - useful float arrays as defaults *)
let one_i_0 = [|0;|]
let one_f_0 = [|0.;|]

(*f stype - find type of an svalue *)
let stype v = 
  match v with
  | Sv_float _         -> St_float
  | Sv_floats (n,_)    -> St_floats n
  | Sv_float_arr (n,_) -> St_float_arr
  | Sv_ints   (n,_)    -> St_ints n
  | Sv_int _           -> St_int
  | Sv_rgb _           -> St_rgb
  | Sv_string _        -> St_string
  | Sv_token_list _    -> St_token_list

(*f is_none - return True if is none *)
let is_none v =
  match v with
  | Sv_float None                            -> true
  | Sv_int None                              -> true
  | Sv_floats (_,f)    when (f==none_floats) -> true
  | Sv_float_arr (_,f) when (f==none_floats) -> true
  | Sv_rgb f           when (f==none_floats) -> true
  | Sv_ints (_,f)      when (f==none_ints)   -> true
  | Sv_string None                           -> true
  | Sv_token_list []                         -> true
  | _ -> false

(*f is_some - return True if is not none *)
let is_some v = not (is_none v)

(*f str - generate string of an svalue *)
let str v =
  if (is_none v) then (
    match v with
    | Sv_float None       -> sfmt "flt:<None>"
    | Sv_int None         -> sfmt "flt:<None>"
    | Sv_floats (n,_)     -> sfmt "f%d:<None>" n
    | Sv_float_arr (_,_)  -> sfmt "fa:<None>"
    | Sv_ints   (n,_)     -> sfmt "i%d:<None>" n
    | Sv_rgb _            -> sfmt "rgb:<None>"
    | Sv_string None      -> sfmt "str:<None>"
    | Sv_token_list []    -> sfmt "tkn:<None>" 
    | _ -> ""
  ) else (
    match v with
    | Sv_float (Some f)   -> sfmt "flt:%f" f
    | Sv_int (Some i)     -> sfmt "int:%d" i
    | Sv_floats (n,f)     -> sfmt "f%d:%s" n (String.concat " " (List.map (sfmt "%f") (Array.to_list f)))
    | Sv_float_arr (n,f)  -> sfmt "fa%d:%s" n (String.concat " " (List.map (sfmt "%f") (Array.to_list f)))
    | Sv_ints   (n,i)     -> sfmt "i%d:%s" n (String.concat " " (List.map (sfmt "%d") (Array.to_list i)))
    | Sv_rgb f            -> sfmt "rgb:%f %f %f" f.(0) f.(1) f.(2)
    | Sv_string (Some s)  -> sfmt "str:'%s'" s
    | Sv_token_list l     -> sfmt "tkn:[%s]" (List.fold_left (fun acc s -> (sfmt "%s %s" acc s)) "" l)
    | _ -> ""
  )

(*f get_color_name *)
let get_color_name v =
  if (is_none v) then  (
    None 
  ) else (
    match v with
    | Sv_rgb f -> Color.name f
    | _ -> None
  )

(*f from_string *)
let rec from_string stype value =
  if (string_is_none value) then (
    match stype with
    | St_ints n     -> sv_none_ints
    | St_floats n   -> sv_none_floats
    | St_float_arr  -> sv_none_float_arr
    | St_rgb        -> sv_none_rgb
    | St_int        -> sv_none_int
    | St_float      -> sv_none_float
    | St_string     -> sv_none_string
    | St_token_list -> sv_none_token_list
  ) else (
    match stype with
    | St_ints n     -> ( let ints   = read_ints   value n in Sv_ints (n,ints) )
    | St_floats n   -> ( let floats = read_floats value n in Sv_floats (n,floats) )
    | St_float_arr  -> ( let (n,floats) = read_float_arr value in Sv_float_arr (n,floats) )
    | St_rgb        -> Sv_rgb (read_color value)
    | St_int        -> ( let ints   = read_ints   value 1 in Sv_int (Some ints.(0)) )
    | St_float      -> ( let floats = read_floats value 1 in Sv_float (Some floats.(0)) )
    | St_string     -> Sv_string (Some value)
    | St_token_list -> ( let tokens = read_tokens value in Sv_token_list tokens)
  )

(*f as_color_string - get a color as a string of an svalue *)
let as_color_string ?default svalue =
  match get_color_name svalue with
  | Some s -> s
  | None -> (
    if (is_none svalue) then (
      match default with | Some s -> s | None -> raise (Bad_value "No default value provided when getting value as_color_string")
    ) else (
      match svalue with
      | Sv_rgb     f    -> sfmt "Rgb(%d,%d,%d)" (int_of_float (255.*.f.(0))) (int_of_float (255.*.f.(1))) (int_of_float (255.*.f.(2)))
      | _               -> "''"
    )
  )

(*f as_floats - get a float array of an svalue *)
let as_floats ?default svalue =
  if (is_none svalue) then (
    match default with | Some f -> f | None -> raise (Bad_value "No default value provided when getting value as_floats")
  ) else (
    match svalue with
    | Sv_floats (n,f)     -> f
    | Sv_float_arr (n,f)  -> f
    | Sv_rgb     f        -> f
    | _                   -> one_f_0
  )

(*f as_float - get a float for an svalue *)
let as_float ?default svalue =
  if (is_none svalue) then (
    match default with | Some f -> f | None -> raise (Bad_value "No default value provided when getting value as_float")
  ) else (
    match svalue with
    | Sv_floats (n,f) -> f.(0)
    | Sv_float_arr (n,f) -> f.(0)
    | Sv_rgb     f -> f.(0)
    | Sv_ints (n,i) -> float i.(0)
    | Sv_float   (Some f) -> f
    | Sv_int     (Some i) -> float i
    | _ -> 0.
  )

(*f as_ints - get an int array for an svalue *)
let as_ints ?default svalue =
  if (is_none svalue) then (
    match default with | Some f -> f | None -> raise (Bad_value "No default value provided when getting value as_ints")
  ) else (
  match svalue with
  | Sv_ints    (_,i) -> i
  | _ -> one_i_0
  )

(*f as_string - get a string an svalue *)
let as_string ?default svalue =
  if (is_none svalue) then (
    match default with | Some f -> f | None -> raise (Bad_value "No default value provided when getting value as_string")
  ) else (
  match svalue with
  | Sv_string (Some s) -> s
  | Sv_token_list l -> String.concat " " l
  | _ -> str svalue
  )

(*f has_token - check if it has a token - only for St_token_list really *)
let has_token svalue str =
  match svalue with
  | Sv_token_list l -> List.fold_left (fun acc s -> if acc then true else (String.equal s str)) false l
  | _ -> false

(*f equals_string - check if it equals a string - only for St_string really *)
let equals_string svalue str =
  match svalue with
  | Sv_string (Some s) -> String.equal s str
  | _ -> false


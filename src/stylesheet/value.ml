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
(*a To do
Colors to come from strings - dictionary of string -> Sv_rgb color value
Repr of value for reading back in - match Sv_rgb with color dictionary
Value can be token list (e.g. for class)
Value can be string
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
    
(*f fill_array *)
let rec fill_array a num n i j =
  if (i=num) then
    a
  else
    (
      a.(i) <- a.(j);
      let next_j = if (j+1=n) then 0 else (j+1) in
      fill_array a num n (i+1) next_j
    )

(*f read_floats *)
let read_floats string num = 
  let floats = Array.make num 0. in
  let rec read_floats_from_n n string =
    if (n=num) then
      n
    else
      (
        match extract_first_and_rest string_as_float_rex string with
        | None -> n
        | Some s12 -> 
           (
             let (s1,s2) = s12 in
             floats.(n) <- float_of_string s1;
             read_floats_from_n (n+1) s2
           )
      )
  in
  let n = read_floats_from_n 0 string in
  fill_array floats num (max n 1) n 0

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
  fill_array ints num (max n 1) n 0


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

(*a Types *)
(*e Exception Bad_value*)
exception Bad_value of string

(*t t_styleable_name *)
type t_styleable_name = string

(*t t_styleable_value *)
type t_styleable_value =
  Sv_floats of int * (float array)
| Sv_float  of float option
| Sv_int    of int option
| Sv_ints   of int * (int array)
| Sv_rgb    of float array

(*t t_styleable_type *)
type t_styleable_type = St_floats of int | St_ints of int | St_float | St_int | St_rgb

(*a Styleable_value module *)
(*v sv_zero, sv_none_ints, sv_none_floats - for use as defaults *)
let sv_zero = Sv_int (Some 0)
let none_floats = Array.make 0 0.
let none_ints   = Array.make 0 0
let sv_none_floats = Sv_floats (0, none_floats)
let sv_none_ints   = Sv_ints (0, none_ints)
let sv_none_float  = Sv_float None
let sv_none_int    = Sv_int None
let sv_none_rgb    = Sv_rgb none_floats

(*v one_i_0, one_f_0 - useful float arrays as defaults *)
let one_i_0 = [|0;|]
let one_f_0 = [|0.;|]

(*f stype - find type of an svalue *)
let stype v = 
  match v with
    Sv_float _ -> St_float
  | Sv_floats (n,_) -> St_floats n
  | Sv_ints   (n,_) -> St_ints n
  | Sv_int _ -> St_int
  | Sv_rgb _ -> St_rgb

(*f is_none - return True if is none *)
let is_none v =
  match v with
  | Sv_float None                              -> true
  | Sv_int None                                -> true
  | Sv_floats (_,f)   when (f==none_floats) -> true
  | Sv_rgb f          when (f==none_floats) -> true
  | Sv_ints (_,f)     when (f==none_ints)   -> true
  | _ -> false

(*f is_some - return True if is not none *)
let is_some v = not (is_none v)

(*f str - generate string of an svalue *)
let str v =
  if (is_none v) then (
    match v with
    | Sv_float None       -> sfmt "flt:<None>"
    | Sv_int None         -> sfmt "flt:<None>"
    | Sv_float (Some f)   -> sfmt "flt:%f" f
    | Sv_floats (n,_)     -> sfmt "f%d:<None>" n
    | Sv_ints   (n,_)     -> sfmt "i%d:<None>" n
    | Sv_rgb _            -> sfmt "rgb:<None>"
    | _ -> ""
  ) else (
    match v with
    | Sv_float (Some f)   -> sfmt "flt:%f" f
    | Sv_int (Some i)     -> sfmt "int:%d" i
    | Sv_floats (n,f)     -> sfmt "f%d:%s" n (String.concat " " (List.map (sfmt "%f") (Array.to_list f)))
    | Sv_ints   (n,i)     -> sfmt "i%d:%s" n (String.concat " " (List.map (sfmt "%d") (Array.to_list i)))
    | Sv_rgb f            -> sfmt "rgb:%f %f %f" f.(0) f.(1) f.(2)
    | _ -> ""
  )

(*f from_string *)
let rec from_string stype value =
  if (string_is_none value) then (
    match stype with
    | St_ints n   -> Sv_ints   (n,none_ints)
    | St_floats n -> Sv_floats (n,none_floats)
    | St_rgb      -> Sv_rgb    none_floats
    | St_int      -> Sv_int None
    | St_float    -> Sv_float None
  ) else (
    match stype with
    | St_ints n   -> ( let ints   = read_ints   value n in Sv_ints (n,ints) )
    | St_floats n -> ( let floats = read_floats value n in Sv_floats (n,floats) )
    | St_rgb      -> Sv_rgb (read_color value)
    | St_int      -> ( let ints   = read_ints   value 1 in Sv_int (Some ints.(0)) )
    | St_float    -> ( let floats = read_floats value 1 in Sv_float (Some floats.(0)) )
  )

(*f as_floats - get a float array of an svalue *)
let as_floats ?default svalue =
  if (is_none svalue) then (
    match default with | Some f -> f | None -> raise (Bad_value "No default value provided when getting value as_floats")
  ) else (
    match svalue with
    | Sv_floats (n,f) -> f
    | Sv_rgb     f    -> f
    | _               -> one_f_0
  )

(*f as_float - get a float for an svalue *)
let as_float ?default svalue =
  if (is_none svalue) then (
    match default with | Some f -> f | None -> raise (Bad_value "No default value provided when getting value as_float")
  ) else (
    match svalue with
    | Sv_floats (n,f) -> f.(0)
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


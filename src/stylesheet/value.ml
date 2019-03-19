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

(*f read_floats *)
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
    None -> None
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
          None -> n
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
          None -> n
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


(*a Types *)
(*t t_styleable_name *)
type t_styleable_name = string

(*t t_styleable_value *)
type t_styleable_value =
  Sv_floats of (int * float array)
| Sv_float of float
| Sv_int  of int
| Sv_ints of (int * int array)
| Sv_rgb of float array

(*t t_styleable_type *)
type t_styleable_type = St_floats | St_ints | St_float_6 | St_float_3 | St_float_2 | St_float_4 | St_float | St_int | St_int_2 | St_int_3 | St_int_4 | St_int_6 | St_rgb

(*a Styleable_value module *)
(*v sv_zero, sv_none - for use as defaults *)
let sv_zero = Sv_int 0
let sv_none = Sv_int 0

(*v one_i_0, one_f_0 - useful float arrays as defaults *)
let one_i_0 = [|0;|]
let one_f_0 = [|0.;|]

(*f stype_of_svalue - find type of an svalue *)
let stype_of_svalue v = 
  match v with
    Sv_float _ -> St_float
  | Sv_floats _ -> St_floats
  | Sv_int _ -> St_int
  | Sv_ints _ -> St_ints
  | Sv_rgb _ -> St_rgb

(*f str_of_svalue - generate string of an svalue *)
let str_of_svalue v =
  match v with
    Sv_float f   -> sfmt "flt:%f" f
  | Sv_int i     -> sfmt "int:%d" i
  | Sv_floats (n,f) -> sfmt "f%d:%s" n (String.concat " " (List.map (sfmt "%f") (Array.to_list f)))
  | Sv_ints   (n,i) -> sfmt "i%d:%s" n (String.concat " " (List.map (sfmt "%d") (Array.to_list i)))
  | Sv_rgb f     -> sfmt "rgb:%f %f %f" f.(0) f.(1) f.(2)

(*f svalue_from_string *)
let rec svalue_from_string ?n stype value =
  let n = match n with | None -> 0 |  Some n -> n in
  match stype with
    St_float   -> ( let floats = read_floats value 1 in Sv_float floats.(0) )
  | St_floats  -> ( let floats = read_floats value n in Sv_floats (n,floats) )
  | St_rgb     -> ( let floats = read_floats value 3 in Sv_rgb floats )
  | St_int     -> ( let ints = read_ints  value 1 in Sv_int ints.(0) )
  | St_ints    -> ( let ints = read_ints  value n in Sv_ints (n,ints) )
  | St_float_2 -> svalue_from_string ~n:2 St_floats value
  | St_float_3 -> svalue_from_string ~n:3 St_floats value
  | St_float_4 -> svalue_from_string ~n:4 St_floats value
  | St_float_6 -> svalue_from_string ~n:6 St_floats value
  | St_int_2   -> svalue_from_string ~n:2 St_ints value
  | St_int_3   -> svalue_from_string ~n:3 St_ints value
  | St_int_4   -> svalue_from_string ~n:4 St_ints value
  | St_int_6   -> svalue_from_string ~n:6 St_ints value

(*f svalue_as_floats - get a float array of an svalue *)
let svalue_as_floats svalue =
  match svalue with
  | Sv_floats (n,f) -> f
  | Sv_rgb     f -> f
  | Sv_float   f -> one_f_0
  | Sv_int     i -> one_f_0
  | Sv_ints    _ -> one_f_0

(*f svalue_as_float - get a float for an svalue *)
let svalue_as_float svalue =
  match svalue with
  | Sv_floats (n,f) -> f.(0)
  | Sv_rgb     f -> f.(0)
  | Sv_float   f -> f
  | Sv_int     i -> float i
  | Sv_ints (n,i) -> float i.(0)

(*f svalue_as_ints - get an int array for an svalue *)
let svalue_as_ints svalue =
  match svalue with
  | Sv_floats  _ -> one_i_0
  | Sv_rgb     _ -> one_i_0
  | Sv_float   _ -> one_i_0
  | Sv_int     _ -> one_i_0
  | Sv_ints    (_,i) -> i


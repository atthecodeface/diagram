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
open Types

let styles = Stylesheet.Value.[ (Attr_names.classes, St_token_list,  sv_none_token_list, false);
                                (Attr_names.eval,    St_string,      sv_none_string, false);
                                (Attr_names.padding,      (St_floats 4),  Sv_floats (4,[|0.;0.;0.;0.;|]), false);
                                (Attr_names.margin,       (St_floats 4),  Sv_floats (4,[|0.;0.;0.;0.;|]), false);
                                (Attr_names.border,       (St_floats 4),  Sv_floats (4,[|0.;0.;0.;0.;|]), false);
                                (Attr_names.anchor,       (St_floats 2),  Sv_floats (2,[|0.5;0.5;|]), false);
                                (Attr_names.expand,       (St_floats 2),  Sv_floats (2,[|0.;0.;|]), false);
                                (Attr_names.place,        (St_floats 2),  sv_none_floats, false);
                                (Attr_names.grid,         (St_ints 4),    sv_none_ints, false);
                                (Attr_names.rotation,     St_float,       sv_none_float, false);
                                (Attr_names.scale,        (St_floats 2),  sv_none_floats, false);
                                (Attr_names.border_color, St_rgb,         sv_none_rgb, true); (* inherit *)
                                (Attr_names.fill_color,   St_rgb,         sv_none_rgb, true); (* inherit *)
             ]


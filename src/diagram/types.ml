
type t_id = int
type t_int4 = int * int * int * int
type t_rect = float * float * float * float
type t_vector = float * float
type t_value = | Scalar of float
               | Rect of t_rect
               | Int4 of t_int4
               | Vector of t_vector
               | String of string

(*t th - Basic header *)
type t_hdr = {
    int_id : t_id;
    mutable parent : t_id;
    id : string;
  }
type t_expr_resolver = t_hdr -> t_value


(*t t_style_resolver *)
type t_style_resolver = {
   value_as_float        : ?default:float -> string -> float;
   value_as_floats       : ?default:float array -> string -> float array;
   value_as_string       : ?default:string -> string -> string;
   value_as_color_string : ?default:string -> string -> string;
  }

(*t t_element_value *)
type t_element_value =
  | Ev_rect       of t_rect
  | Ev_floats     of int * (float array)
  | Ev_float      of float
  | Ev_vector     of float * float
  | Ev_string     of string

type t_element_properties = (string * t_element_value) list

(*m LayoutElementType *)
module type LayoutElementType = sig
    type et   (* base element type *)
    type rt   (* resolved styled of element type - does not include t *)
    type lt   (* layout of element type - does not include rt or t *)
    type gt   (* finalized geometry of element type - does not include lt, rt or t *)

    val styles       : (string * Stylesheet.Value.t_styleable_type * Stylesheet.Value.t_styleable_value * bool) list
    val resolve_styles : et -> t_style_resolver -> (rt * t_element_properties)
    val get_min_bbox : et -> rt -> t_rect
    val make_layout_within_bbox : et -> rt -> t_rect -> (lt * t_element_properties)
    val finalize_geometry : et -> rt -> lt -> t_style_resolver -> gt
    val render_svg   : et -> rt -> lt -> gt -> int -> Svg.t list
end

(*a Aggregate layout element modules *)
(*m LayoutElementAggrType - Aggregate module type for a number of LayoutElementFunc submodules *)
module type LayoutElementAggrType = sig
    type et
    type rt
    type lt
    type gt
    val styles       : (string * Stylesheet.Value.t_styleable_type * Stylesheet.Value.t_styleable_value * bool) list
    val style_desc   : Stylesheet.t_styleable_desc
    val type_name_et  : et -> string
    val type_name_rt  : rt -> string
    val type_name_lt  : lt -> string
    val type_name_gt  : gt -> string
    val resolve_styles : et -> t_style_resolver -> (rt * t_element_properties)
    val get_min_bbox : rt -> t_rect
    val make_layout_within_bbox : rt -> t_rect -> (lt * t_element_properties)
    val finalize_geometry : lt -> t_style_resolver -> gt
    val render_svg   : gt -> int -> Svg.t list
end

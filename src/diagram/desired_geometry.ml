open Types
type t = t_ref_bbox
let make reference bbox =
  { reference; bbox }

let make_wh w h =
  let w = w /. 2. in
  let h = h /. 2. in
  make Primitives.Vector.zeros (Primitives.Rectangle.of_cwh (0.,0.,w,h))
  
let get_ref t = t.reference
              
let get_bbox t = t.bbox

let get_cwh t = Primitives.Rectangle.get_cwh t.bbox

let get_wh t  = Primitives.Rectangle.get_wh t.bbox

let get_c t  = Primitives.Rectangle.get_c t.bbox

let get_drext t =
 Primitives.Rectangle.translate ~scale:(-1.) t.bbox t.reference

let expand ?scale:(scale=1.0) t r =
  let (w,h) = Primitives.Rectangle.get_wh r in
  let reference = Primitives.Vector.add ~scale:scale t.reference [|w;h|] in
  let bbox = Primitives.Rectangle.expand ~scale:scale t.bbox r in
  {reference; bbox}

let shrink = expand ~scale:(-1.)

(* fit_within_dimension
  An outer dimension is effectively an lx, rx, and a ref_x
  The desired dimension is the same

  The initial slack is the outer size minus the desired size
  The desired dimension is first anchored within this slack.
  This is determining a value to add to a desired X to put it
  into the outer coordinate space.
  If anchor is -1 then it is anchored to the left - i.e. no slack is used
  If anchor is 1  then it is anchored to the right - i.e. all the slack is added

  Apply this translation to the desired box, and then expand on both
  sides as required: if expand is 0, then don't change. If expand is
  1. then expand it to the whole of the outer dimension

olr,os,oref etc (4 25) 21 14.5  (-4 4) 8 0 =17 25 0

slack = 13
translation = (14.5 - 0) + 0.5*slack = 21
dlr l+translation = 17
dlr r+translation = 25
do_expand 4  17 = 17
do_expand 25 25 = 25

 *)  
             
let fit_within_dimension outer desired anchor expand i =
  let do_expand tgt v = v +. (tgt-.v)*.(expand.(i)) in

  let olr = Primitives.Rectangle.get_dim outer.bbox i in
  let oref = outer.reference.(i) in
  let dlr = Primitives.Rectangle.get_dim desired.bbox i in
  let dref = desired.reference.(i) in

  let os = Primitives.(Vector.len olr) in
  let ds = Primitives.(Vector.len dlr) in
  let slack = os -. ds in
  let translation = oref -. dref +. anchor.(i) *. slack /. 4. in
  (*
  Printf.printf "a/e %g %g " anchor expand;
  Printf.printf "o0,o1,os,oref etc %g %g %g %g  %g %g %g %g =" o0 o1 os oref d0 d1 ds dref;
   *)
  let d0 = do_expand olr.(0) (dlr.(0) +. translation) in
  let d1 = do_expand olr.(1) (dlr.(1) +. translation) in
  let dref = dref +. translation in
  (*
  Printf.printf "%g %g %g\n" d0 d1 dref;
   *)
  (d0, d1, dref)

(*f fit_within outer desired anchor expand -> t_ref_bbox
  Get a bbox and reference within outer based on desired
 *)        
let fit_within outer desired anchor expand =
  let (x0, x1, xr) = fit_within_dimension outer desired anchor expand 0 in
  let (y0, y1, yr) = fit_within_dimension outer desired anchor expand 1 in
  make (Primitives.Vector.make xr yr) (Primitives.Rectangle.make x0 y0 x1 y1)

let str t =
  Printf.sprintf "Ref %s Bbox %s" (Primitives.Vector.str t.reference) (Primitives.Rectangle.str t.bbox)

open Types
type t = t_ref_bbox
let make reference bbox =
  { reference; bbox }

let make_wh w h =
  let w = w /. 2. in
  let h = h /. 2. in
  make (0.,0.) (-. w, -. h, w, h)
  
let get_ref t = t.reference
              
let get_bbox t = t.bbox

let get_cwh t = Primitives.Rectangle.get_cwh t.bbox

let get_drext t =
  let (xr, yr) = t.reference in
  Primitives.Rectangle.translate t.bbox (-. xr, -. yr)

let expand ?scale:(scale=1.0) t (dx0,dy0,dx1,dy1) =
  let dx0 = dx0 *. scale in
  let dx1 = dx1 *. scale in
  let dy0 = dy0 *. scale in
  let dy1 = dy1 *. scale in
  let (xr, yr) = t.reference in
  let (x0, y0, x1, y1) = t.bbox in
  let reference = (xr-.dx0+.dx1, yr-.dy0+.dy1) in
  let bbox = (x0-.dx0, y0-.dy0, x1+.dx1, y1+.dy1) in
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

o0,o1,os,oref etc 4 25 21 14.5  -4 4 8 0 =17 25 0

slack = 13
translation = (14.5 - 0) + 0.5*slack = 21
d0+translation = 17
d1+translation = 25
do_expand 4  17 = 17
do_expand 25 25 = 25

 *)  
             
let fit_within_dimension outer desired anchor expand f =
  let expand = f expand in
  let anchor = f anchor in
  let (_,_,w,h) = get_cwh outer in
  let (rx,ry) = get_ref outer in
  let os = f (w,h) in
  let oref = f (rx,ry) in
  let (_,_,w,h) = get_cwh desired in
  let (rx,ry) = get_ref desired in
  let ds = f (w,h) in
  let dref = f (rx,ry) in
  let slack = os -. ds in
  let translation = oref -. dref +. anchor *. slack /. 4. in
  let (x0,y0,x1,y1) = outer.bbox in
  let (o0,o1) = (f (x0,y0), f (x1, y1)) in
  let (x0,y0,x1,y1) = desired.bbox in
  let (d0,d1) = (f (x0,y0), f (x1, y1)) in
  (*
  Printf.printf "a/e %g %g " anchor expand;
  Printf.printf "o0,o1,os,oref etc %g %g %g %g  %g %g %g %g =" o0 o1 os oref d0 d1 ds dref;
   *)
  let do_expand tgt v e = v +. (tgt-.v)*.e in
  let d0 = do_expand o0 (d0 +. translation) expand in
  let d1 = do_expand o1 (d1 +. translation) expand in
  let dref = dref +. translation in
  (*
  Printf.printf "%g %g %g\n" d0 d1 dref;
   *)
  (d0, d1, dref)

(*f fit_within outer desired anchor expand -> t_ref_bbox
  Get a bbox and reference within outer based on desired
 *)        
let fit_within outer desired anchor expand =
  let (x0, x1, xr) = fit_within_dimension outer desired anchor expand (fun (a,b)->a) in
  let (y0, y1, yr) = fit_within_dimension outer desired anchor expand (fun (a,b)->b) in
  make (xr,yr) (x0,y0,x1,y1)    

let str t =
  let (xr, yr) = t.reference in
  let (x0, y0, x1, y1) = t.bbox in
  Printf.sprintf "Ref (%g,%g) Bbox (%g,%g,%g,%g)" xr yr x0 y0 x1 y1

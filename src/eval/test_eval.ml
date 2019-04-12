  type test_t = { r : Rpn_eval.t;
             id : string;
             children : test_t list;
           }

  exception Unknown_child of string
let test_assignment _ =
  let find_child r id =
    let rec find_it = function
      | [] -> raise (Unknown_child id)
      | hd::_ when (String.equal id hd.id) -> hd
      | _::tl -> find_it tl
    in
    find_it r.children
  in
  let r1 = Rpn_eval.make "c=2;d=.c ^.e +;" in
  let t1 = {r=r1; id="r1"; children=[]} in
  let r0 = Rpn_eval.make "a=.r1.c;b=.a .r1.d +;e=5;" in
  let t0 = {r=r0; id="top"; children=[t1;]} in
  let al = t0.r in
  List.iter (Printf.printf "Assigns '%s'\n") (Rpn_eval.list_assigns al);
  let tres = Rpn_eval.make_resolver find_child (fun t->t.r) (fun _ s -> None) (fun t->t.id) in
  let v = Rpn_eval.resolve_value tres [t0] 0 ["b"] in
  Printf.printf "Value type %s\n" (Value.str_type v);
  let fa = Value.as_floats v in
  Array.iteri (Printf.printf "%d %f\n") fa;
  ()

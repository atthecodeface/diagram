(*f list_collapse *)
let rec list_collapse f = function
  | [] -> None
  | hd::tl -> match f hd with
              | Some x -> Some x
              | None -> list_collapse f tl

(*f list_find *)
let list_find f = list_collapse (fun x->if (f x) then Some x else None)


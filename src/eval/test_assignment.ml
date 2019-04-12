open Assignment
(*a Test stuff *)
let test_expr _ =
  let e = Expression.( [] |>
    prepend_float 1.0 |>
    prepend_float 2.0 |>
    prepend_function "+"
    ) in
  let e = List.rev e in
  let v = Expression.evaluate (fun _ -> Value.no_value) e in
  let fa = Value.as_floats v in
  Array.iteri (Printf.printf "%d %f\n") fa

let test_tokens _ =
  let str = "a=2;b=a 3 +;" in (* joe = ^^^.fred 34 sqrt +;  *)
  let (tl,ni) = tokenize_string str in
  Printf.printf "Tokenized %s length %d\n" str (String.length str);
  Printf.printf "next index %d\n" ni;
  Printf.printf "Tokens: %s\n" (Token.strl tl);
  let (a,tl) = build_from_tokens tl in
  Printf.printf "Now tokens: %s\n" (Token.strl tl);
  let v = value_of (fun _ -> Value.no_value) a in
  let fa = Value.as_floats v in
  Array.iteri (Printf.printf "%d %f\n") fa;
  ()


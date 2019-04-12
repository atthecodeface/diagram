(*a TokenFunc functor *)
(*m TokenFunc module - takes a TokenizerType module *)
module TokenFunc (T : Tokenizer.TokenizerType) = struct
  (*t t_with_pos *)
  type t_with_pos = int * T.t

  (*t t *)
  type t = T.t

  (*f is_whitespace : int -> bool *)
  let is_whitespace ch = ((ch==9) || (ch==32))

  (*f next_token : string -> len -> ofs -> (T.t * int * int) option *)
  let rec next_token s n i =
    if i>=n then
      None
    else
      let ch = Char.code (String.get s i) in
      if is_whitespace ch then
        next_token s n (i+1)
      else (
        let rec try_token_fn = function
          | [] -> None
          | hd::tl ->
             match hd s n i with
             | Some (t,ni) -> Some (t, i, ni)
             | _ -> try_token_fn tl
        in
        try_token_fn T.tokenizers
      )

  (*f tokenize_string string -> (t list * int) *)
  let tokenize_string s =
    let n = String.length s in
    let rec acc_tokens i rev_tokens = 
      match next_token s n i with
      | None -> (rev_tokens, i)
      | Some (t, i, ni) -> 
         acc_tokens ni ((i,t)::rev_tokens)
    in
    let (rev_tokens, ni) = acc_tokens 0 [] in
    (List.rev rev_tokens, ni)

  (*f str, strl *)
  let str t = T.str t

  (*f strl *)
  let strl tl =
    String.concat "\n" (List.map (fun (i,t) -> str t) tl)

  (*f All done *)
end


(*a TokenFunc module *)
(*m TokenType type *)
module type TokenType = sig
  type t
  type tokenize_fn = string -> int -> int -> (t * int) option
  val tokenizers : tokenize_fn list
  val str : t -> string
  val strl : t list -> string
end

(*m TokenFunc module *)
module TokenFunc (T : TokenType) = struct
  (*t t *)
  type t = T.t

  (*f is_whitespace int -> bool *)
  let is_whitespace ch = ((ch==9) || (ch==32))

  (*f next_token string -> len -> ofs -> (t * int) option *)
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
             | Some x -> Some x
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
      | Some (t,ni) -> 
         acc_tokens ni (t::rev_tokens)
    in
    let (rev_tokens, ni) = acc_tokens 0 [] in
    (List.rev rev_tokens, ni)

  (*f str, strl *)
  let str  = T.str
  let strl = T.strl

  (*f All done *)
end


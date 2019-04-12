(*m TokenizerType type *)
module type TokenizerType = sig
  type t
  type tokenize_fn = string -> int -> int -> (t * int) option
  val tokenizers : tokenize_fn list
  val str : t -> string
end


open Cor.Std;;

module Expr = struct

  module T = struct
    type t = Text_char of char | Meta_char of char with sexp;;
    let compare = compare
    let hash = Hashtbl.hash
  end;;
  include T;;
  include Hashable.Make (T);;

  type expr = t list;;

  let split_raw regexp =
    let rec split str len new_str =
      if len = 0 then new_str
      else begin
	match str.[0] with
	| ')' -> split (String.sub str 1 (len - 1)) (len - 1) (Meta_char ')'::new_str)
	| '*' -> split (String.sub str 1 (len - 1)) (len - 1) (Meta_char '*'::new_str)
	| '+' -> split (String.sub str 1 (len - 1)) (len - 1) (Meta_char '+'::new_str)
	| '?' -> split (String.sub str 1 (len - 1)) (len - 1) (Meta_char '?'::new_str)
	| '(' -> split (String.sub str 1 (len - 1)) (len - 1) (Meta_char '('::new_str)
	| '|' -> split (String.sub str 1 (len - 1)) (len - 1) (Meta_char '|'::new_str)
	| '\\' -> split (String.sub str 2 (len - 2)) (len - 2) (Text_char str.[1]::new_str)
	| default -> split (String.sub str 1 (len - 1)) (len - 1) (Text_char str.[0]::new_str)
      end
    in List.rev (split regexp (String.length regexp) []);;
end;;

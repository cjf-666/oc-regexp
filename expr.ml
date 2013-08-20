module Expr = struct

  type elem = TextChar of char | MetaChar of char;;

  type expr = elem list;;

  let split_raw regexp =
    let rec split str len new_str =
      if len = 0 then new_str
      else begin
	match str.[0] with
	| ')' -> split (String.sub str 1 (len - 1)) (len - 1) (MetaChar ')'::new_str)
	| '*' -> split (String.sub str 1 (len - 1)) (len - 1) (MetaChar '*'::new_str)
	| '+' -> split (String.sub str 1 (len - 1)) (len - 1) (MetaChar '+'::new_str)
	| '?' -> split (String.sub str 1 (len - 1)) (len - 1) (MetaChar '?'::new_str)
	| '(' -> split (String.sub str 1 (len - 1)) (len - 1) (MetaChar '('::new_str)
	| '|' -> split (String.sub str 1 (len - 1)) (len - 1) (MetaChar '|'::new_str)
	| '\\' -> split (String.sub str 2 (len - 2)) (len - 2) (TextChar str.[1]::new_str)
	| default -> split (String.sub str 1 (len - 1)) (len - 1) (TextChar str.[0]::new_str)
      end
    in split regexp (String.length regexp) [];;
end;;

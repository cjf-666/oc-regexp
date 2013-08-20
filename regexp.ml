module Regexp = struct
  let match ~text ~exp =
    let nfa = Autmt.elim_nfa (Autmt.compile ~exp) in
    let rec check_char str len cur_states =
      if String.is_empty str then
	List.mem 
      else
	let rec enum_state states =
	  
	in check_char (String.sub str 1 (len-1)) (enum_state cur_states)
    in check_char ~text (String.length text) [nfa.start];;
    
end;;

open Core.Std;;

module Rep = struct
  type e_t = Eps of int | Chr of char * int;;
  type gr_t = (e_t list * e_t list) array;; (* Eps edges * Chr edges *)

  let edges_to_graph (a_state,(edges:Autmt.frag)) =
    let states = Array.create ~len:(a_state+1) ([],[]) in
    let convert e =
      match e with
      | Autmt.Ord_arr (s,t,ch) -> begin
	match states.(s) with
	| ep_e,ch_e ->
	  if ch = Autmt.epsilon then
	    states.(s) <- t::ep_e,ch_e
	  else
	    states.(s) <- ep_e,(ch,t)::ch_e end
      | _ -> raise (Invalid_argument "e") in
    List.iter ~f:convert edges.Autmt.edge;
  states;;

  let rec add_st aut_graph new_state =
    match aut_graph.(new_state) with
    | [],_ -> new_state::[]
    | ep_e,_  ->
      List.concat (List.map ep_e ~f:(fun x -> add_st aut_graph x));;

  let add_states key aut_graph x =
    match aut_graph.(x) with
    | ep_e,ch_e ->
      List.concat (List.map ~f:(fun (ch,t) -> if ch = key then add_st
    aut_graph t else []) ch_e);;

  let string_match regexp text start =
    let acc_state, edges = Autmt.compile (Expr.split_raw regexp) in
    let init_state = edges.Autmt.start in
    let aut_graph = edges_to_graph (acc_state, edges) in
    let rec scan_string text len cur_states =
      if (List.mem cur_states acc_state) then
	true
      else
	match len with
	| 0 -> false
	| l ->
          let sub_text = String.sub text 1 (l-1) in
	  scan_string sub_text (l-1)
	    (List.concat (List.map ~f:(add_states text.[0] aut_graph)
			    cur_states)) in
    let t_len = String.length text in
    let sub_text = String.sub text start (t_len-start) in
    scan_string sub_text (t_len-start) ((add_st aut_graph init_state)@[]);;
end;;

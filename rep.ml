open Core.Std;;

module Rep = struct
  type e_t = Eps of int | Chr of char * int;;
  type gr_t = (e_t list * e_t list) array;; (* Eps edges * Chr edges *)

  let acc_state = ref 0;;

  let edges_to_graph (a_state,(edges:Autmt.frag)) =
    acc_state := a_state;
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
    List.iter (~f:convert) edges.edge;
  states;;
      
  let string_match regexp text start =
    let aut_graph = edges_to_graph (Autmt.compile (Expr.split_raw
    regexp)) in
    let add_states key x =
      let rec add_st new_state =
	match aut_graph.(new_state) with
	| [],_ -> new_state::[]
	| ep_e,_  ->
	  List.concat (List.map ep_e ~f:(fun x -> add_st x)) in
      match aut_graph.(x) with
      | ep_e,ch_e ->
	List.concat (List.map ~f:(fun (ch,t) -> if ch = key then add_st t else []) ch_e) in
    let rec scan_string text len cur_states =
      match len with
      | 0 -> cur_states
      | l ->
	let sub_text = String.sub text 1 (l-1) in
	scan_string sub_text (l-1)
	  (List.concat (List.map ~f:(add_states text.[0]) cur_states)) in

    let t_len = String.length text in
    let sub_text = String.sub text start (t_len-start) in
    let final_states = scan_string text t_len (Autmt.init_state::[]) in
    List.mem final_states !acc_state;;
end;;

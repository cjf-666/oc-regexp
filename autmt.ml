open Core.Std;;

module Autmt = struct
  type arc = int * int * char;;
  type frag = {start:int; out:arc list};;
  type autometon = int * frag * int;;

  let compile exp =
    let fg_st = Stack.create () in
    let op_st = Stack.create () in

    let priortbl = Char.Table.create () in
    let () = List.iter ~f:(fun (key, data) -> Hashtbl.set priortbl ~key ~data)
      [(')', 0); ('*', 1); ('+', 1); ('?', 1); ('(', 1000); ('|', 2)]
    in

    let rec compile_rec exp len state_id =  
      let id = ref state_id in
      
      let patch start =
	fun s,e,c ->  
	  if e = -1 then (s, start, c) else (s, e, c) in

      let push_op () = 

      	while Option.value  (Hashtbl.find priortbl exp.[0])
	  ~default:0 <= Option.value  (Hashtbl.find priortbl
	  (Stack.top op_st)) ~defualt:0 && Stack.top op_st <> '(' do
       
	  let fg = Stack.pop fg_st in
	  match Stack.top op_st with
	  | '*' -> 
	    Stack.push {start=!id, out=(!id, fg.start, '\000') :: (!id, -1, '\000')
	      :: (List.map ~f:(patch !id) fg.out)} fg_st

	  | '+' ->
	    Stack.push {start=fg.start, out=(!id, fg.start, '\000') :: (!id, -1, 'n\000')
	      :: (List.map ~f:(patch !id) fg.out)} fg_st

	  | '?' ->
	    Stack.push {start=!id, out=(!id, fg.start, '\000') :: (!id, -1, '\000')
	      :: fg.out} fg_st

	  | '|' ->
	    let fg1 = Stack.pop fg_st in
	    Stack.push {start=!id, out=(!id, fg.start, '\000') :: (!id, fg1.start, '\000')
	      :: (List.append fg.out fg1.out)} fg_st;
	    id := !id + 1;
	done;

        if exp.[0] = ')' then Stack.pop op_st
	else Stack.push op_st exp.[0];
	!id in

      let concat_fg exp len =
	let i = ref 0 in
	if exp.[0] = '\\' then i := !i +1;
	Stack.push {start=id, out=(id, -1, exp.[!i]) :: []} fg_st;

	i := !i + 1;
	while !i < len && Hashtbl.find priortbl exp.[!i] = None do
	  id := id + 1;
	  if exp.[0] = '\\' then i := !i +1;
	  let fg = Stack.pop fg_st in
	  Stack.push {start=id, out=(!id, -1, '\000') :: (fg.start, !id, exp.[!i]) :: fg.out} fg_st;
	done;
	(id,String.sub exp !i len-!i)
      in

      if String.is_empty exp then state_id
      else begin
	match exp.[0] with
	| '*' | '+' | '?' | '|' | '(' | ')'->
	  compile_rec (String.sub n_exp 1 len - 1) len-1 (push_op() + 1)
	| '\\' | _ ->
	  let id,n_exp = concat_fg exp len in
	  compile_rec n_exp (String.len n_exp) (!id+1);
      end
    in
    let state_num = compile_rec exp (String.length exp) 0 in
    
    let acc_state =
    while Stack.length fg_st >= 2 do
      let fg1 = Stack.pop fg_st in
      let fg0 = Stack.pop fg_st in
      Stack.push {start.fg0; out=List.append (List.map ~f:(patch
      fg1.start) fg0.out) fg1.out}
    done;
    autometon(state_num,Stack.pop fg_st);;
    
    let elim_nfa (state_num, nfa) =
      let closure = Int.Table.create () in
      let rec flood_fill d_id n_id =
	Hashtbl.set closure ~key:n_id ~data:d_id;
	let rec dfs arc =
	  match arc with
	    [] -> ()
	  | (x,y,c)::rest ->
	    flood_fill d_id y;
	    dfs rest
	in dfs (List.filter ~f:(fun (x,y,c) -> x=n_id && c = '\000')
		  nfa.out) in
      let rec find_closure id new_id =
	if id = state_num then new_id
	else if Hashtbl.find closure id <> None then
	  find_closure (id+1) new_id
	else begin
	  flood_fill new_id id;
	  find_closure (id+1) (new_id+1);
	end;
      in 
      (find_closure nfa.start 0),
      {start=0;
       out=List.map ~f:(fun (x,y,c) -> (Option.value ~default:0 (Hashtbl.find closure x),
					Option.value ~default:0 (Hashtbl.find closure y), c))
	  (List.filter ~f:(fun (x,y,c) -> c <> '\000') nfa.out)};;

end;;

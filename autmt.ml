open Core.Std;;

module Autmt = struct
  type arc = Ord_arr of int * int * char | Dan_arr of int * char;;
  type frag = {start:int; edge:arc list; dan_edge:arc list};;
  type autometon = int * frag * int;;

  let priortbl = Expr.Table.create ();;
  let () = List.iter ~f:(fun (key, data) -> Hashtbl.set priortbl ~key ~data)
    [(Expr.Meta_char ')', 0);
     (Expr.Meta_char '*', 1);
     (Expr.Meta_char '+', 1);
     (Expr.Meta_char '?', 1);
     (Expr.Meta_char '(', 1000);
     (Expr.Meta_char '|', 2)];;

  let fg_st = Stack.create ();;
  let op_st = Stack.create ();;
  
  let epsilon = '\000';;
  let init_state = 0;;

  let patch e = fun (Dan_arr (s,c):arc) -> Ord_arr (s, e, c);;

  let op_oper opr new_st =
    let fg = Option.value (Stack.pop fg_st)
      ~default:{start=0;edge=[];dan_edge=[]} in
    match opr with
    | Expr.Meta_char '*' -> 
      Stack.push fg_st {start = new_st;
			edge = Ord_arr (new_st, fg.start, epsilon)
			:: (fg.edge @ (List.map ~f:(patch new_st) fg.dan_edge));
			dan_edge = Dan_arr (new_st, epsilon)::[]}

    | Expr.Meta_char '+' ->
      Stack.push fg_st {start = fg.start;
			edge = Ord_arr (new_st, fg.start, epsilon)
			:: (fg.edge @ (List.map ~f:(patch new_st) fg.dan_edge));
			dan_edge = Dan_arr (new_st, epsilon)::[]}

    | Expr.Meta_char '?' ->
      Stack.push fg_st {start = new_st;
			edge = Ord_arr (new_st, fg.start, epsilon) :: fg.edge;
			dan_edge = Dan_arr (new_st, epsilon) :: fg.dan_edge}

    | Expr.Meta_char '|' ->
      let fg1 = Option.value (Stack.pop fg_st) ~default:{start=0;edge=[];dan_edge=[]} in 
      Stack.push fg_st {start = new_st;
			edge = Ord_arr (new_st, fg.start, epsilon)
			:: Ord_arr (new_st, fg1.start, epsilon)
			:: fg.edge @ fg1.edge;
			dan_edge = fg.dan_edge @ fg1.dan_edge};;

  let push_op operator state_id =
    let id = ref state_id in  

    while not (Stack.is_empty op_st) && Option.value (Hashtbl.find priortbl operator) ~default:0
      <= Option.value (Hashtbl.find priortbl (Option.value (Stack.top op_st)
						~default:(Expr.Meta_char '|'))) ~default:0
      && Option.value (Stack.top op_st) ~default:(Expr.Meta_char '|') <> (Expr.Meta_char '(') do

      op_oper (Option.value (Stack.pop op_st) ~default:(Expr.Meta_char '|')) !id;

      incr id;
    done;

    if operator = (Expr.Meta_char ')') then begin
      Stack.pop op_st;
      ();
    end
    else if operator = (Expr.Meta_char '|') then
      Stack.push op_st operator
    else begin
      op_oper operator !id;
      incr id;
    end;
    !id;;

  let rec compile_rec exp state_id concat =
    match exp with
    | [] -> state_id
    | (Expr.Meta_char c) as ch :: tail ->
	compile_rec tail (push_op ch state_id) false
    | (Expr.Text_char c) as ch :: tail  ->
	if concat then begin
	  let lst_fg = Option.value (Stack.pop fg_st) ~default:{start=0;edge=[];dan_edge=[]} in
	  Stack.push fg_st {lst_fg with
	    edge = (List.map ~f:(patch state_id) lst_fg.dan_edge) @ lst_fg.edge
	    ;dan_edge = Dan_arr (state_id, c)::[]}
	end
	else
	  Stack.push fg_st {start = state_id;
		     edge = [];
		     dan_edge = Dan_arr (state_id, c)::[]};
	compile_rec tail (state_id + 1) true;;

  let compile exp =
    let last_state = ref (compile_rec exp init_state false) in
    while not (Stack.is_empty op_st) do
      op_oper (Option.value (Stack.pop op_st) ~default:(Expr.Meta_char '|')) !last_state;
      incr last_state
    done;     

    while Stack.length fg_st >= 2 do
      let fg1 = Option.value (Stack.pop fg_st) ~default:{start=0;edge=[];dan_edge=[]} in
      let fg0 = Option.value (Stack.pop fg_st) ~default:{start=0;edge=[];dan_edge=[]} in
      Stack.push fg_st {start = fg0.start;
			edge = (List.map ~f:(patch fg1.start) fg0.dan_edge)
			@ fg1.edge @ fg0.edge;
			dan_edge = fg1.dan_edge}
    done;
    let x = Option.value (Stack.pop fg_st)
    ~default:{start=0;edge=[];dan_edge=[]} in
    !last_state, {x with edge = (List.map ~f:(patch !last_state)
				 x.dan_edge) @ x.edge;
      dan_edge = []};;
end;;

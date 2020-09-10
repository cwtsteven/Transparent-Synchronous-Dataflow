open Map

exception DictionaryException of string
exception ListLengthDifferenceException of string

include Map.Make(struct type t = int let compare = compare end) 

let union_criterion key v1 v2 = 
	begin match v1, v2 with
		| Some x1, None -> v1
		| None, Some x2 -> v2
		| Some x1, Some x2 -> v1
		| None, None -> None
	end 

let union d1 d2 = merge union_criterion d1 d2 

let keycount = ref 0

let generate_key = fun () ->
  let value = !keycount in
  let _ = incr keycount in
  value 
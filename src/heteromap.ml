open Map

exception DictionaryException of string
exception ListLengthDifferenceException of string

module Template = Map.Make(struct type t = int let compare = compare end) 

type t = int Template.t 
type key = Template.key 

let empty = Template.empty 

let add k v s = Template.add k (Obj.magic v) s 

let remove k s = Template.remove k s 

let singleton k v = Template.singleton k (Obj.magic v)

let find k s = Obj.magic (Template.find k s) 

let map f s = Template.map (fun x -> Obj.magic (f (Obj.magic x))) s 

let iter f s = Template.iter (fun k x -> Obj.magic (f k (Obj.magic x))) s 

let fold f s b = Template.fold (fun k x acc -> Obj.magic (f k (Obj.magic x) acc)) s b 

let string_of_key = string_of_int 

let union_criterion key v1 v2 = 
	begin match v1, v2 with
		| Some x1, None -> v1
		| None, Some x2 -> v2
		| Some x1, Some x2 -> v1 
		| None, None -> None
	end 

let union d1 d2 = Template.merge union_criterion d1 d2 

let keycount = ref 0

let generate_key = fun () ->
  let value = !keycount in
  let _ = incr keycount in
  value 

let emptyKeys _ = Array.make 0 0 
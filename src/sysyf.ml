type sysyf_z = SYSYF_Z 
type 'n sysyf_s = SYSYF_S : 'n -> 'n sysyf_s
type vec = float Dictionary.t 
type 'n sym = 'n * vec 

type dkey = Dictionary.key
type ckey = Heteromap.key

module DKeys = Set.Make(struct type t = dkey let compare = compare end)

type _ graph = 
  Thunk : (vec -> (int -> int) -> ckey array -> 'a) * DKeys.t * ckey array -> 'a graph 
| IF_Thunk : (vec -> (int -> int) -> ckey array -> 'a graph) * DKeys.t * ckey array -> 'a graph 
| Cell : int * ckey array -> 'a graph 
| Params : 'n * DKeys.t -> 'n sym graph 


let _cells = ref Heteromap.empty

let _params = ref Dictionary.empty 

let _peekps d = 
  DKeys.fold (fun elt vec -> Dictionary.add elt (Dictionary.find elt !_params) vec) d (Dictionary.empty)

let _lookup k l = Heteromap.find (Array.get l k) !_cells

let rec _peek : type a. vec -> (int -> int) -> ckey array -> a graph -> a = 
  fun ps f l -> function
  | Thunk (t, _, _) -> t ps f l
  | IF_Thunk (t, _, _) -> _peek ps f l (t ps f l) 
  | Cell (k, _) -> let (v,_,_) = !(_lookup (f k) l) in v 
  | Params (n, d) -> n, _peekps d 

let rec _getDict : type a. a graph -> DKeys.t = function
  | Thunk (t, d, l) -> d 
  | IF_Thunk (t, d, l) -> d 
  | Cell (k, l) -> let (_,t,_) = !(_lookup k l) in _getDict t 
  | Params (n, d) -> d 

let _getParents : type a. a graph -> ckey array = function 
  | Thunk (t, d, l)  -> l 
  | IF_Thunk (t, d, l)  -> l
  | Cell (k, l) -> l 
  | Params (n, d) -> Heteromap.emptyKeys() 

let lift = fun t -> Thunk ((fun _ _ _ -> t), DKeys.empty, Heteromap.emptyKeys())  

let lift_list xs = 
  let xs = List.rev_append xs [] in 
  let rec aux ps f l xs acc = 
    match xs with
    | [] -> acc 
    | x :: xs -> aux ps f l xs (_peek ps f l x :: acc) 
  in 
  let rec mergeDict xs acc =
    match xs with 
    | [] -> acc 
    | x :: xs -> mergeDict xs (DKeys.union (_getDict x) acc)
  in 
  let rec mergeParents xs acc = 
    match xs with 
    | [] -> acc 
    | x :: xs -> mergeParents xs (Array.append (_getParents x) acc)
  in
  let d = mergeDict xs DKeys.empty in 
  let l = mergeParents xs (Heteromap.emptyKeys()) in 
  Thunk ((fun ps f l -> aux ps f l xs []), d, l)

let lift_tuple (t, u) = 
  Thunk ((fun ps f l -> _peek ps f l t, _peek ps f l u), 
                        DKeys.union (_getDict t) (_getDict u), 
                        Array.append (_getParents t) (_getParents u))

let peek : type a. a graph -> a = fun g ->
  let f = fun n -> n in 
  match g with  
  | Thunk (t, d, l) -> t (_peekps d) f l 
  | IF_Thunk (t, d, l) -> let ps = _peekps d in _peek ps f l (t ps f l) 
  | Cell (k, l) -> let (v,_,_) = !(_lookup (f k) l) in v 
  | Params (n, d) -> n, _peekps d  




let _print_graph : type a. a graph -> string = function 
| Thunk (t, d, l) -> "Thunk with l = " ^ Array.fold_right (fun x acc -> Heteromap.string_of_key x ^ ", " ^ acc) l "" ^ "\n "
| IF_Thunk (t, d, l) -> "IF_Thunk with l = " ^ Array.fold_right (fun x acc -> Heteromap.string_of_key x ^ ", " ^ acc) l "" ^ "\n " 
| Cell (k,l)         -> "Cell with k = " ^ Heteromap.string_of_key (Array.get l k) ^ "\n "
| Params _           -> "Params\n " 

let _print_parents l = 
  let rec aux curr acc = 
    let len = Array.length curr in 
    if len = 0 then acc
    else let k = curr.(0) in let xs = Array.sub curr 1 (len - 1) in 
         let (v,g,op) = !(Heteromap.find k !_cells) in 
         let acc = "Key = " ^ Heteromap.string_of_key k ^ " where " ^ _print_graph g ^ acc in 
         aux xs acc
  in 
  aux l "" |> print_string 

let print_graph : type a. a graph -> unit = fun t -> 
  let s = _print_graph t in 
  let l = _getParents t in 
  let rec aux parents curr acc = 
    let len = Array.length curr in 
    if len = 0 then acc
    else let k = curr.(0) in let xs = Array.sub curr 1 (len - 1) in 
         if Array.mem k parents 
         then aux parents xs acc
         else let (v,g,op) = !(Heteromap.find k !_cells) in 
              let acc = "Key = " ^ Heteromap.string_of_key k ^ " where " ^ _print_graph g ^ acc in 
              aux (Array.append parents (Array.make 1 k)) (Array.append curr (_getParents g)) acc
  in 
  aux (Heteromap.emptyKeys()) l s |> print_string





let root : type a. a graph -> a graph = function 
  | Cell (k, l) -> let (_,t,_) = !(_lookup k l) in t 
  | _ -> failwith "root: not a cell" 

let apply : type a b. (a -> b) graph -> a graph -> b graph = 
  fun t u ->
  Thunk ((fun ps f l -> (_peek ps f l t) (_peek ps (fun n -> f n + Array.length (_getParents t)) l u)), 
                        DKeys.union (_getDict t) (_getDict u), 
                        Array.append (_getParents t) (_getParents u)) 

let ifthenelse b t1 t2 = let t1 = t1() in let t2 = t2() in 
                         let newDict =  DKeys.union (_getDict b) (DKeys.union (_getDict t1) (_getDict t2)) in 
                         let newParents =  Array.append (_getParents b) (Array.append (_getParents t1) (_getParents t2)) in 
                         IF_Thunk ((fun ps f l -> if _peek ps f l b 
                                                  then Thunk ((fun ps f l -> _peek ps (fun n -> f n + Array.length (_getParents b)) l t1), newDict, newParents)
                                                  else Thunk ((fun ps f l -> _peek ps (fun n -> f n + Array.length (_getParents b) + Array.length (_getParents t1)) l t2), newDict, newParents)),
                                   newDict, 
                                   newParents)

let cell g = 
  let c = ref (peek g, g, None) in
  let k = Heteromap.generate_key() in 
  _cells := Heteromap.add k c (!_cells); 
  Cell (0, Array.make 1 k) 

let rec link cell dep = 
  match cell with
  | Cell (k,l) -> let x = _lookup k l in 
                  let (v, g, _) = !x in 
                  x := (v, dep, None) 
  | IF_Thunk (t,d,l) -> link (t (_peekps d) (fun n -> n) l) dep 
  | _ -> failwith "link: not a cell" 

let _assignps = fun (_,d) (_,v) ->
  let newParams = DKeys.fold (fun elt vec -> Dictionary.add elt (Dictionary.find elt v) vec) d !_params in 
  _params := newParams

let rec assign : type a. a graph -> a -> unit =
  fun cell v -> 
  match cell with
  | Cell (k,l) -> let x = _lookup k l in 
                  let (old_v, g, _) = !x in 
                  x := (v, g, None) 
  | IF_Thunk (t,d,l) -> assign (t (_peekps d) (fun n -> n) l) v
  | Params (n,d) -> _assignps (n,d) v 
  | _ -> failwith "assign: not a cell or parameters" 

let rec set : type a. a graph -> a -> unit =
  fun cell v -> 
  match cell with
  | Cell (k,l) -> let x = _lookup k l in 
                  let (old_v, g, _) = !x in 
                  x := (v, lift v, None)
  | IF_Thunk (t,d,l) -> set (t (_peekps d) (fun n -> n) l) v
  | Params (n,d) -> _assignps (n,d) v 
  | _ -> failwith "set: not a cell or parameters" 

let (<~) = link 

let (<:=) = assign 

let (<:~) = set 

let step () = 
  Heteromap.iter 
    (fun i x -> 
      match !x with 
      | (v, g, None) -> let new_v = peek g in 
                        x := (v, g, Some new_v)
      | _            -> failwith "step: should be None" 
    ) !_cells; 
  let result = Heteromap.fold 
    (fun i x acc -> 
      match !x with
      | (v, g, Some w) -> x := (w, g, None); acc || not (v = w) 
      | _              -> failwith "step: re-eval missing"  
    ) !_cells false
  in 
  result 

let n_step n = 
  if n < 0 then failwith "n_step: cannot accept negative number"
  else 
  let rec aux n b =
    match n with
    | 0 -> b
    | n -> let b = step() in aux (n-1) b 
  in 
  aux n false 

let init _ = 
  _cells := Heteromap.empty

let pc n = 
    let k = Dictionary.generate_key() in 
    _params := Dictionary.add k n !_params;
    Thunk ((fun ps f l -> Dictionary.find k ps), DKeys.singleton k, Heteromap.emptyKeys()) 

let _getAllParents l = 
  let rec aux parents curr = 
    let len = Array.length curr in 
    if len = 0 then parents
    else let k = curr.(0) in let xs = Array.sub curr 1 (len - 1) in 
         if Array.mem k parents 
         then aux parents xs 
         else let (v,g,op) = !(Heteromap.find k !_cells) in 
              aux (Array.append parents (Array.make 1 k)) (Array.append curr (_getParents g))
  in 
  aux (Heteromap.emptyKeys()) l 

let rec _indexOf x a n =
  if a.(n) = x then n 
  else _indexOf x a (n+1);;


let _fusion : type a n. a graph -> n -> (n sym graph -> a graph) * n sym graph = fun term n ->
  let _generateNewl symg l = 
    let allParents = _getAllParents l in 
    let newl = Array.map 
                 (fun k -> let (v,g,op) = !(Heteromap.find k !_cells) in 
                           if Array.length (_getParents g) <> 0 
                           then let c = ref (v,g,op) in
                                let newk = Heteromap.generate_key() in 
                                _cells := Heteromap.add newk c (!_cells); 
                                newk
                           else k
                 ) allParents
    in 
    allParents, newl 
  in

  let rec _change : type a n. a graph -> n sym graph -> ckey array -> ckey array -> a graph = fun g symg allParents newl ->
  match g with 
    | Thunk (t, d, l) -> Thunk ((fun _ f l -> let (n, ps) = peek symg in t ps f l), d, Array.mapi (fun _ k -> Array.get newl (_indexOf k allParents 0)) l) 
    | IF_Thunk (t, d, l) -> IF_Thunk ((fun _ f l -> let (n, ps) = peek symg in _change (t ps f l) symg allParents newl), d, Array.mapi (fun _ k -> Array.get newl (_indexOf k allParents 0)) l) 
    | Cell (i, l) -> Cell (i, Array.mapi (fun _ k -> Array.get newl (_indexOf k allParents 0)) l)
    | Params (n, d) -> Params (n, d) 
  in 

  let l = _getParents term in 
  let d = _getDict term in 
  (fun symg -> 
    let allParents, newl = _generateNewl symg l in 
    Array.iter (fun k -> let x = Heteromap.find k !_cells in let (v,g,op) = !x in x := (v,_change g symg allParents newl,op)) newl;
    _change term symg allParents newl), 
  Params (n, d) 

let plus = fun (p,d1) (_,d2) -> p, Dictionary.mapi (fun k m -> (Dictionary.find k d1) +. m) d2 
let (|+|) = plus 

let mult = fun n (p,d2) -> p, Dictionary.mapi (fun k m -> n *. m) d2 
let (|*|) = mult 

let dot = fun (p,d1) (_,d2) -> Dictionary.fold (fun k m acc -> (Dictionary.find k d1) *. m +. acc) d2 0.0 
let (|.|) = dot 


let fold = fun f symg -> 
  let (p,d) = peek symg in 
  let zeros = Dictionary.map (fun _ -> 0.0) d in 
  let axis k = Dictionary.add k 1.0 zeros in 
  let basis = Dictionary.mapi (fun k _ -> lift (p, axis k)) d in 
  Dictionary.fold (fun _ e acc -> let y = f e in apply (apply (lift plus) y) acc) basis (lift (p,zeros)) 

let print_dict (_,d) = Dictionary.iter (fun k x -> Printf.printf "%.20f, " x) d; Printf.printf "%!" 


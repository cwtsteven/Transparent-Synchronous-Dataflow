open Sysyf
open List

let (+^) = lift (+.)
let (-^) = lift (-.)
let ( *^ ) = lift ( *. ) 
let ( /^ ) = lift ( /. )
let float_of_int' = lift float_of_int

(* create with reset *)
let w_reset reset init t = [%dfg if reset then init else t] 

(* some loss functions *) 
let cross_entropy out exp = 
  let aux out exp = 
    let sum_terms = map2 (fun y y_exp -> if y = 0.0 then 0.0 else y_exp *. log y) out exp in 
    0.0 -. (fold_left (+.) 0.0 sum_terms) 
  in 
  [%dfg (lift aux) out exp] 

let mean_squared_error out exp = 
  let aux out exp = 
    let sq = map2 (fun y y_exp -> (y -. y_exp) *. (y -. y_exp)) out exp in 
    (fold_left (+.) 0.0 sq) /. (float_of_int (length out)) 
  in 
  [%dfg (lift aux) out exp] 

(* accumulation loss *)
let accloss reset loss f v = 
  let s = cell (lift 0.0) in 
  let loss = loss f v in 
  s <~ w_reset reset (lift 0.0) [%dfg s +^ loss];  
  s 


(* numerical gradients graph *) 
let num_grad : type n. (n sym graph -> 'b graph) -> n sym graph 
                       -> ((n sym graph -> 'b graph) -> n sym graph -> float graph) 
                      -> float graph -> n sym graph =
  fun f ps loss delta -> 
  let oldloss = loss f ps in 
  fold (fun e -> let (|+|^),(|*|^),(=^) = lift (|+|), lift (|*|), lift (=) in 
                 let newloss = loss f [%dfg delta |*|^ e |+|^ ps] in 
                 [%dfg if oldloss =^ 0.0 then 0.0 |*|^ e else 
                       (oldloss -^ newloss) /^ oldloss |*|^ e]) ps 

(* feedforward *)
let rec feedforward inp out_exp data layers = 
  match data with 
  | [] -> if layers <= 2 then () else (n_step (layers - 2); ())
  | (xs, ys) :: data -> inp <:= xs; out_exp <:= ys; 
                        (if layers < 2 then () else (step(); ())); 
                        feedforward inp out_exp data layers 


(* gradient descent *)
let gradient_descent ?(debug = false) ps grad rate = 
  let optimise forward = 
    (if debug then (Printf.printf "gradients = "; print_dict (peek grad); Printf.printf "\n%!";
                   Printf.printf "parameters = "; print_dict (peek ps); Printf.printf "\n%!") else ());
    forward (); 
    ps <:~ (peek ps |+| (peek rate |*| peek grad)); 
    (if debug then (Printf.printf "gradients = "; print_dict (peek grad); Printf.printf "\n%!";
                   Printf.printf "parameters = "; print_dict (peek ps); Printf.printf "\n%!") else ())
  in 
  optimise

(* for training *)
let _shuffle ls =
    let nd = map (fun c -> (Random.bits (), c)) ls in
    let sond = sort compare nd in
    map snd sond 

let _split n xs = 
  let rec aux n xs acc =
    match n with 
    | 0 -> (rev_append acc []), xs 
    | n -> match xs with 
           | [] -> acc, xs 
           | x :: xs -> aux (n-1) xs (x :: acc)
  in 
  aux n xs []

(* training algo *)
let train ?(debug = false) ?(shuffle = true) ?(batch_size = 32) ?(optimiser = gradient_descent)
          ps grad forward rate data epochs = 
  let optimise = optimiser ~debug:debug ps grad rate in
  let _data = data in 
  (if debug then (Printf.printf "===================== Training starts =====================\n"; 
                  Printf.printf "Batch size = %d, Epochs = %d\n\n" batch_size epochs; 
                  Printf.printf "%!") else ()); 
  let rec aux = fun data n step -> 
    let data = if shuffle then _shuffle data else data in 
    match n with 
    | 0 -> ()
    | n -> match data with 
           | [] -> (if debug then (Printf.printf "Epoch %d ends =====================.\n\n" (epochs-n+1); Printf.printf "%!") else ()); 
                   aux _data (n-1) 1
           | data -> let (batch, data) = _split batch_size data in 
                     (if debug then Printf.printf "Step %d: " step else ()); 
                     optimise (forward batch); 
                     aux data n (step+1) 
  in 
  aux _data epochs 1 

(* for creating neural networks *)
let input_layer n = 
  let rec aux n acc = 
    match n with
    | 0 -> acc 
    | n -> aux (n-1) (0.0 :: acc)
  in 
  cell (lift (aux n []))

let dot_product xs ys = 
  let rec aux xs ys acc = 
    match xs, ys with 
    | [], [] -> acc 
    | x :: xs, y :: ys -> aux xs ys (x *. y +. acc)
  in 
  aux xs ys 0.0 

let perceptron f inp = 
  let rec create_pcs n acc = 
    match n with 
    | 0 -> acc  
    | n -> create_pcs (n-1) (pc (Random.float 1.0) :: acc) 
  in 
  let pcs = create_pcs (length (peek inp)) [] |> lift_list in 
  let sum = [%dfg (lift dot_product) inp pcs +^ pc 0.0 ] in 
  [%dfg (lift f) sum ]

let dense_layer f n inp = 
  let rec aux n acc = 
    match n with 
    | 0 -> acc 
    | n -> let p = f inp in aux (n-1) (p :: acc)
  in 
  lift_list (aux n [])

let softmax v =
    let exponents = map exp v in
    let sum = fold_left (+.) 0.0 exponents in
    let softmaxes = map (fun x -> x /. sum) exponents in
    softmaxes 

let softmax_layer n inp = 
    let l = dense_layer (perceptron (fun x -> x)) n inp in 
    [%dfg (lift softmax) l] 

let lstm reset inp = 
  let sigmoid x = 1.0 /. (1.0 +. exp (0.0 -. x)) in 
  let cons = lift (cons) in 
  let h = cell [%dfg 0.0] in 
  let c = cell [%dfg 0.0] in 
  let f = perceptron sigmoid ([%dfg cons h inp]) in 
  let i = perceptron sigmoid ([%dfg cons h inp]) in 
  let c' = perceptron tanh ([%dfg cons h inp]) in 
  c <~ w_reset reset [%dfg 0.0] [%dfg f *^ c +^ i *^ c']; 
  let o = perceptron sigmoid ([%dfg cons h inp]) in  
  h <~ (let imme_c = root c in [%dfg o *^ (lift tanh) imme_c]); 
  h 

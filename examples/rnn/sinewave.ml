open Sysyf
open Sysyf_learning
open List

let rec _drop n l = 
  match n with
  | 0 -> l 
  | n -> _drop (n-1) (tl l) 

let x_axis n w = 
  let rec aux acc i = 
  	if i <= 0.0 then acc else aux (i :: acc) (i -. (n /. w)) 
  in 
  aux [] n 

let data = 
  map (fun p -> ([p], [sin p])) (x_axis 6. 5000.) 


let reset = cell (lift false) 

let model = 
	let inp = input_layer 1 in  
	let hidden1 = dense_layer (lstm reset) 1 inp in (* lstm already has a layer of cells embedded *)
	let hidden2 = dense_layer (lstm reset) 2 hidden1 in 
	let out = cell (dense_layer (perceptron (fun x -> x)) 1 hidden2) in 
	let out_exp = lift [0.0] |> cell |> cell |> cell |> cell in 
	inp, out, out_exp, 4 


let _ =	
	let inp, out, out_exp, layers = model in 
	let reset = lift false |> cell in 
  let loss = accloss reset (fun f v -> mean_squared_error (f v) out_exp) in 
  let forward data () = 
      reset <:~ true; n_step (layers+1); reset <:~ false; 
      feedforward inp out_exp data (layers+1) 
  in
  let delta, rate = cell (lift 0.001), cell (lift 0.01) in  
	let%fuse (f, ps) = out in  
  let grad = num_grad f ps loss delta in 
	train ~debug:true ~shuffle:false ~batch_size:32 ps grad forward rate data 1; 

	let test = List.map (fun x -> [x]) (x_axis 6. 5000.)  in 

  let outs = cell (lift []) in 
  let cons = lift (cons) in 
  outs <~ [%dfg cons out outs];
  outs <:= []; 
  feedforward inp out_exp (map (fun x -> (x, [0.])) test) layers; 
  let result = _drop (layers-1) (peek outs) in 
	List.iter (fun x -> print_float (hd x); print_newline()) result 
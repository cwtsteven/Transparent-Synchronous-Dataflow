open Csv
open Sysyf
open Sysyf_learning
open List

let (+^) = lift (+.)
let (-^) = lift (-.)
let ( *^ ) = lift ( *. ) 

let raw_data = tl (Csv.load "examples/linear_regression/data.csv");; 
let data = map (fun (x::y::[]) -> float_of_string x, float_of_string y) raw_data

let _ =  
	let reset = cell (lift false) in 

	let x = cell (lift 0.0) in 
	let y = [%dfg x *^ pc 1.0 +^ pc 0.0] in 
	let y_exp  = cell (lift 0.0) in 

	let mseloss f v = let y = f v in [%dfg (y -^ y_exp) *^ (y -^ y_exp)] in 

	let%fuse (f, ps) = y in 

	let forward data () = 
		reset <:= true; step; 
		feedforward x y_exp data 2 
	in 

	let delta, rate = cell (lift 0.001), cell (lift 0.1) in 

	let grad = num_grad f ps (accloss reset mseloss) delta in 

	train ~debug:true ~batch_size:32 ps grad forward rate data 10000; 

	let result = x <:= 3.; step(); peek y in 
	print_float result; print_newline() 
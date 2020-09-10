open Sysyf
open Sysyf_learning
open List

(* standardising data *)
let standardize_data data =
    let len = length (fst (nth data 0)) in

    let mins = Array.make len infinity in
    let maxs = Array.make len neg_infinity in 

    let compute_ranges datapoint =
        let update_for_datapoint i value =
            let _ = if (value < Array.get mins i) then Array.set mins i value else () 
            in
            if (value > Array.get maxs i) then Array.set maxs i value else ()
        in
        iteri update_for_datapoint (fst datapoint)
    in
    let _ = iter compute_ranges data in

    let standardize_entry i value = 
        let min = Array.get mins i in
        let max = Array.get maxs i in
        if max = min then (max /. min) else
        (value -. min) /. (max -. min)
    in
    let standardize_row row = 
        mapi standardize_entry row
    in
    map (fun (x, y) -> ((standardize_row x), y)) data

let raw_data = tl (Csv.load "examples/neural_network/breastcancerdata.csv");;

let data = 
	let processed_data = map (fun datapoint -> 
		let datapoint = tl datapoint in
		let y = let c = hd datapoint in (if String.equal c "M" then [1.0;0.0] else [0.0;1.0]) in
		let x = map float_of_string (tl datapoint) in 
		(x, y)) raw_data 
	in
	standardize_data processed_data 
(* standardising data *)




let sigmoid x = 1.0 /. (1.0 +. exp (0.0 -. x)) 

let model = 
	let inp = input_layer 30 in 
	let hidden = cell (dense_layer (perceptron sigmoid) 2 inp) in 
	let out = cell (softmax_layer 2 hidden) in 
    let out_exp = cell (cell (cell (lift [0.0;0.0]))) in (* to sync with out *)
    inp, out, out_exp, 3

let _ =	
    let inp, out, out_exp, layers = model in 
    let reset = lift false |> cell in 
    let loss = accloss reset (fun f v -> cross_entropy (f v) out_exp) in 
    let forward data () = 
        reset <:~ true; n_step (layers+1); reset <:~ false; 
        feedforward inp out_exp data (layers+1) 
    in
    let delta, rate = cell (lift 0.001), cell (lift 0.01) in  
    let%fuse (f, ps) = out in 
    let grad = num_grad f ps loss delta in 
    train ~debug:true ps grad forward rate data 10;  
    let (x, y) = hd data in 
	
    forward [(x, y)]; let result = peek out in  
	Printf.printf "Malign: %f, Benign: %f\n" (nth result 0) (nth result 1) 

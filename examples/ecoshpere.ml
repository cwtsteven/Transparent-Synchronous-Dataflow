open Sysyf
open Sysyf_learning

let (+^) = lift (+.)
let (-^) = lift (-.)
let ( *^ ) = lift ( *. ) 
let ( /^ ) = lift ( /. )



let nutrients_consumption_rate_algae = 3.
let algae_consumption_rate_shrimp = 1. 
let waste_consumption_rate_bacteria = 2.
let nutrients_production_ratio = 0.2

let algae_absorption_rate = 0.318
let shrimp_absorption_rate = 0.02 

let a_death_rate = 0.1
let s_death_rate = 0.01
let a_starve_rate = 0.1
let s_starve_rate = 0.5 
let b_starve_rate = 0.2 

let guard x = if x < 0. then 0. else x

let starve_rate require curr rate = 
	let rate = if require <= 0.0 then 0.0 else guard (require -. curr) /. require *. rate in 
	rate 

let algae_growth algae nutrients =
	let require = algae *. nutrients_consumption_rate_algae in 
	let death_rate = a_death_rate +. starve_rate require nutrients a_starve_rate in 
	let consumption = if require > nutrients then nutrients *. 0.7 else require in 
	death_rate, consumption *. algae_absorption_rate, consumption *. (1. -. algae_absorption_rate) 

let shrimp_growth shrimp algae = 
	let require = shrimp *. algae_consumption_rate_shrimp in 
	let death_rate = s_death_rate +. starve_rate require algae s_starve_rate in 
	let consumption = if require > algae then algae *. 0.7 else require in 
	death_rate, consumption *. shrimp_absorption_rate, consumption *. (1. -. shrimp_absorption_rate) 

let bacteria_growth bacteria waste =
	let require = bacteria *. waste_consumption_rate_bacteria in 
	let death_rate = starve_rate require waste b_starve_rate in 
	death_rate, min require waste *. (1. -. nutrients_production_ratio)

let nutrients_growth bacteria waste = 
	let require = bacteria *. waste_consumption_rate_bacteria in 
	min require waste *. nutrients_production_ratio 



let nutrients_eqn reset initial nutrients algae bacteria waste = 
	if reset then
		initial
	else 
		let _,algae_growth,a_waste = algae_growth algae nutrients in 
		let nutrients_growth = nutrients_growth bacteria waste in 
		let b_death_rate,_ = bacteria_growth bacteria waste in 
		let n1 = nutrients +. nutrients_growth -. algae_growth -. a_waste in
		guard n1 

let algae_eqn reset initial algae nutrients shrimp = 
	if reset then
		initial
	else 
		let death_rate, algae_growth,_ = algae_growth algae nutrients in 	
		let _, shrimp_growth,s_waste = shrimp_growth shrimp algae in 	
		let n1 = algae +. algae_growth -. algae *. death_rate -. shrimp_growth -. s_waste in 
		guard n1 

let shrimp_eqn reset initial shrimp algae = 
	if reset then 
		initial 
	else
		let death_rate, shrimp_growth,_ = shrimp_growth shrimp algae in 
		let n1 = shrimp +. shrimp_growth -. shrimp *. death_rate in 
		guard n1 

let bacteria_eqn reset initial bacteria waste =
	if reset then 
		initial 
	else 
		let death_rate, bacteria_growth = bacteria_growth bacteria waste in 
		let n1 = bacteria +. bacteria_growth -. bacteria *. death_rate in 
		guard n1 

let waste_eqn reset initial waste bacteria algae nutrients shrimp = 
	if reset then 
		initial 
	else 
		let a_death_rate,_,a_waste = algae_growth algae nutrients in 
		let s_death_rate,_,s_waste = shrimp_growth shrimp algae in 
		let b_death_rate,bacteria_growth = bacteria_growth bacteria waste in 
		let nutrients_growth = nutrients_growth bacteria waste in 
		let n1 = waste +. algae *. a_death_rate +. shrimp *. s_death_rate +. bacteria *. b_death_rate +. a_waste +. s_waste -. bacteria_growth -. nutrients_growth in 
		guard n1 

let createPrms _ = 
	let init_n = pc (Random.float 1.0 +. 0.1) in 
	let init_a = pc (Random.float 1.0 +. 0.1) in 
	let init_s = pc (Random.float 1.0 +. 0.1) in 
	let init_b = pc (Random.float 1.0 +. 0.1) in 
	let a = ([%dfg init_n *^ 500.0], [%dfg init_a *^ 100.0], [%dfg init_s *^ 1.0], [%dfg init_b *^ 10.0], [%dfg 0.0]) in 
	(*let b = (lift 1500.04, lift 44.16, lift 1.70, lift 20.67, lift 0.0) in *)
	a 

let createCells _ =
	let nutrients = cell (lift 0.0) in 
	let algae = cell (lift 0.0) in 
	let shrimp = cell (lift 0.0) in 
	let bacteria = cell (lift 0.0) in 
	let waste = cell (lift 0.0) in 
	(nutrients, algae, shrimp, bacteria, waste) 

let createModel reset cells prms = 
	let (init_n, init_a, init_s, init_b, init_d) = prms in 
	let (nutrients, algae, shrimp, bacteria, waste) = cells in 
	
	link nutrients [%dfg (lift nutrients_eqn) reset init_n nutrients algae bacteria waste ]; 
	link algae [%dfg (lift algae_eqn) reset init_a algae nutrients shrimp ]; 
	link shrimp [%dfg (lift shrimp_eqn) reset init_s shrimp algae ]; 
	link bacteria [%dfg (lift bacteria_eqn) reset init_b bacteria waste ]; 
	link waste [%dfg (lift waste_eqn) reset init_d waste bacteria algae nutrients shrimp ]; 

	((init_n, init_a, init_s, init_b, init_d), [%dfg nutrients +^ algae +^ shrimp +^ bacteria +^ waste -^ nutrients -^ algae -^ bacteria -^ waste], (nutrients, algae, shrimp, bacteria, waste))


let print_model ((init_n, init_a, init_s, init_b, init_d), _, _) = 
	Printf.printf "Initial values: nutrients: %.2f, algae: %.2f, shrimp: %.2f, bacteria: %.2f, waste: %.2f\n" (peek init_n) (peek init_a) (peek init_s) (peek init_b) (peek init_d);
	Printf.printf "%!"

let print_mass (_, total, (init_n, init_a, init_s, init_b, init_d)) = 
	Printf.printf "Current values: nutrients: %.4f, algae: %.4f, shrimp: %.4f, bacteria: %.4f, waste: %.4f\n" (peek init_n) (peek init_a) (peek init_s) (peek init_b) (peek init_d);
	(*Printf.printf "Total mass: %.2f\n" (peek total);*) 
	Printf.printf "%!" 

let (>^) = lift (>)
let (<=^) = lift (<=)

let simluate () = 
	let reset = cell (lift false) in
	let cells = createCells () in 
	let prms = createPrms () in 
	let model = createModel reset cells prms in 
	let (_,shrimp,_) = model in 
	let visible = [%dfg if shrimp >^ 0.5 then 1.0 else 0.0] in 
	let days_visible = 
	  let s = cell [%dfg 0.0] in 
	  s <~ w_reset reset [%dfg 0.0] [%dfg s +^ visible]; s
	in 

	let delta, rate = cell (lift 0.1), cell (lift 0.1) in 
	let%fuse (f, ps) = days_visible in 

	let loss f v = 
	  let days = f v in 
	  [%dfg if days <=^ 0.0 then 1000000.0 else 1.0 /^ days ]
	in 

	let forward n () = 
		reset <:= true; step; 
		n_step n; ()
	in 

	let grad = num_grad f ps loss delta in 

	let optimise = gradient_descent ~debug:false ps grad rate in 
	let days = loss f ps in 
	for i = 1 to 1000 do 
	  optimise (forward 1000)
	done; 
	print_model model; 
	Printf.printf "Days lasted: %.2f \n\n" (peek days_visible); Printf.printf "%!" 

let rec trial steps = 
	let rec aux = function
	| 0 -> () 
	| n -> Printf.printf ("Trial %d:\n") (steps-n+1); Printf.printf "%!"; 
		   simluate (); 
		   aux (n-1)
	in 
	aux steps

let _ =
	trial 100


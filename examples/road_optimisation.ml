open Sysyf
open Sysyf_learning
open List

type ssegment = { s: float * float; e: float * float; a1: float graph; a2: float graph; cp: float graph }
type bsegment = { s: float * float; e: float * float; a1: float graph; a2: float graph; cp: float graph; 
				  cps: float graph; cpe: float graph} 
type segment = S of ssegment | B of bsegment 

let (+^) = lift (+.)
let (-^) = lift (-.)
let ( *^ ) = lift ( *. ) 
let ( /^ ) = lift ( /. )

(* gravity *)
let g = 9.8 (* m/s**2 *)

(* car properties *)
let max_acc = 11.0 (* m/s**2 *)
let max_dec = -2.0 (* m/s**2 *)
let max_spd = 53.0 (* m/s *)
let car_width = 1.90 (* m *) 
let c_g = 0.5 (* center of gravity (m) *)

(* initial speed *)
let init_spd = 0.0  (* m/s *) 

(* return value for tipping *)
let max_float = 100000.


let bezier (p0x, p0y) (p1x, p1y) (p2x, p2y) (p3x, p3y) t =
	let aux p0 p1 p2 p3 t = (1.0-.t) ** 3.0 *. p0 
							+. 3.0 *. ((1.0-.t) ** 2.0) *. t *. p1 
							+. 3.0 *. (1.0-.t) *. (t ** 2.0) *. p2 
							+. (t ** 3.0) *. p3 in 
	(aux p0x p1x p2x p3x t, aux p0y p1y p2y p3y t) 

let dBezier p0 p1 p2 p3 t = 3.0 *. ((p3 -. 3.0 *. p2 +. 3.0 *. p1 -. p0) *. (t ** 2.0) +. (2.0 *. p2 -. p1 +. 2.0 *. p0) *. t +. p1 -. p0) 

let dBezier2 p0 p1 p2 p3 t = 6.0 *. ((p3 -. 3.0 *. p2 +. 3.0 *. p1 -. p0) *. t +. p2 -. 2.0 *. p1 +. p0) 

let radius (p0x, p0y) (p1x, p1y) (p2x, p2y) (p3x, p3y) t = 
	let x' = dBezier p0x p1x p2x p3x t in  
	let x'' = dBezier2 p0x p1x p2x p3x t in 
	let y' = dBezier p0y p1y p2y p3y t in 
	let y'' = dBezier2 p0y p1y p2y p3y t in 
	let c = (abs_float (x' *. y'' -. x'' *. y')) in 
	if c == 0.0 then 
		failwith "radius: divide by zero. " 
	else abs_float (((x' ** 2.0 +. y' ** 2.0) ** (3.0 /. 2.0)) /. c)

let overturningSpd d r h = sqrt (d *. g *. r /. (2.0 *. h))

let distance (x0, y0) (x1, y1) = sqrt ((x1 -. x0) ** 2.0 +. (y1 -. y0) ** 2.0)

let point (x0, y0) (x1, y1) t = (1.0 -. t) *. x0 +. t *. x1, (1.0 -. t) *. y0 +. t *. y1



let calculateVT u a d = 
	let v2 = u ** 2.0 +. 2.0 *. a *. d in 
	if v2 < 0.0 || (u == 0.0 && a == 0.0) then
		(u, max_float)
	else 
		let v = sqrt v2 in 
		if v < max_spd then 
			(v, if a == 0.0 then d /. u else (v -. u) /. a) 
		else 
			let v = max_spd in 
			let t_toMaxSpd = if a == 0.0 then 0.0 else (v -. u) /. a in 
			let d_toMaxSpd = (u +. v) *. t_toMaxSpd /. 2.0 in 
			let t_toEnd = (d -. d_toMaxSpd) /. v in 
			(v, t_toMaxSpd +. t_toEnd) 

let feedbackmax v v_max = if v > v_max then (v -. v_max) /. v_max else 0.0 
let feedbackmim v v_min = if v < v_min then (v_min -. v) /. v_min else 0.0
let penaltyS a1 a2 cp = 1. +. feedbackmax a1 max_acc +. feedbackmim a1 max_dec +. feedbackmax a2 max_acc +. feedbackmax cp 1.0 +. feedbackmim cp 0.0
let penaltyB a1 a2 cp cps cpe angles anglee = 
	1. +. feedbackmax a1 max_acc +. feedbackmim a1 max_dec +. feedbackmax a2 max_acc +. feedbackmax cp 1.0 +. feedbackmim cp 0.0 
	+. feedbackmim cps 0.0 +. feedbackmim cpe 0.0 +. feedbackmax angles Float.pi +. feedbackmim angles (2. *. Float.pi /. 3.) +. feedbackmax anglee Float.pi +. feedbackmim anglee (2. *. Float.pi /. 3.)

let simulateStraightSeg s e a1 a2 cp u = 
	(*
	let a1 = if a1 > max_acc then max_acc else if a1 < max_dec then max_dec else a1 in 
	let a2 = if a2 > max_acc then max_acc else if a2 < max_dec then max_dec else a2 in 
	let cp = if cp > 1.0 then 1.0 else if cp < 0.0 then 0.0 else cp in 
	*)
	let d = distance s e in 
	let (v_i, t1) = calculateVT u a1 (cp *. d) in 
	let (v, t2) = calculateVT v_i a2 ((1.0 -. cp) *. d) in 
	[v; t1 +. t2; (t1 +. t2) *. penaltyS a1 a2 cp]

let angleOf3points (x0,y0) (x1,y1) (x2,y2) = 
	Float.abs (atan2 (y2 -. y1) (x2 -. x1) -. atan2 (y0 -. y1) (x0 -. x1)) 

let simulateBezierSeg s e a1 a2 cp cps cpe u = 
	(*
	let a1 = if a1 > max_acc then max_acc else if a1 < max_dec then max_dec else a1 in 
	let a2 = if a2 > max_acc then max_acc else if a2 < max_dec then max_dec else a2 in 
	let cp = if cp > 1.0 then 1.0 else if cp < 0.0 then 0.0 else cp in 
	let cps = if cps < 1.0 then 1.0 else cps in 
	let cpe = if cpe < 1.0 then 1.0 else cpe in 
    *)
    let p0 = s in let p3 = e in 
    let (x0, y0) = s in 
    let (x1, y1) = e in 
    let p1 = point s (x1, y0) cps in 
    let p2 = point e (x0, y1) cpe in 
    let bt = bezier p0 p1 p2 p3 in 
    let delta = 0.01 in  

    let angles = angleOf3points (x0-.0.000001, y0) s (bezier s p1 p2 e 0.000001) in 
    let anglee = angleOf3points (bezier s p1 p2 e (1.-.0.000001)) e (x1+.0.000001, y1) in

	let simulateStep u t = 
		let a = if t >= cp then a1 else a2 in 
		let b0 = bt t in 
		let b1 = bt (t +. delta) in  
		let d = distance b0 b1 in 
		let r = radius p0 p1 p2 p3 t in 

		if u >= overturningSpd car_width r c_g then 
			u, max_float 
		else calculateVT u a d 
	in 

	let rec aux t (u, tu) = 
		if t >= 1.0 
		then u, tu 
		else 
			let v, tv = simulateStep u t in 
			aux (t +. delta) (v, tu +. tv)
	in 

	let (v, t) = aux 0.0 (u, 0.0) in 
	[v; t; t *. penaltyB a1 a2 cp cps cpe angles anglee]  


let createPrmsStraigt (s, e) = S {s=s; e=e; a1=[%dfg pc (Random.float 1.0) *^ (lift max_acc)]; a2=[%dfg pc (Random.float 1.0) *^ (lift max_dec)]; cp=pc (Random.float 1.0)}
let createPrmsSandB (s, e) (s1, e1) = 
	let sseg = S {s=s; e=e; a1=[%dfg pc (Random.float 1.0) *^ (lift max_acc)]; a2=[%dfg pc (Random.float 1.0) *^ (lift max_dec)]; cp=pc (Random.float 1.0)} in
	let bseg = B {s=e; e=s1; a1=[%dfg pc (Random.float 1.0) *^ (lift max_acc)]; a2=[%dfg pc (Random.float 1.0) *^ (lift max_dec)]; cp=pc (Random.float 1.0); cps=pc (Random.float 1.0 +. 1.); cpe=pc (Random.float 1.0 +. 1.)} in  
	bseg, sseg

(* create parameters *)
let createPrms segs = 
	let rec aux acc = function
	| [] -> failwith "Error: no segments found. "
	| p :: [] -> createPrmsStraigt p :: acc 
	| p0 :: p1 :: segs -> let (bseg, sseg) = createPrmsSandB p0 p1 in 
					 	  aux (bseg :: sseg :: acc) (p1 :: segs)
	in 
	List.rev_append (aux [] segs) [] 

let createModelS (seg : ssegment) acc = 
	[%dfg (lift simulateStraightSeg) (lift seg.s) (lift seg.e) seg.a1 seg.a2 seg.cp ((lift hd) acc) ]
let createModelB seg acc = 
	[%dfg (lift simulateBezierSeg) (lift seg.s) (lift seg.e) seg.a1 seg.a2 seg.cp seg.cps seg.cpe ((lift hd) acc) ]
let aggregate result acc = 
	[%dfg (lift cons) ((lift hd) result) ((lift tl) ((lift map2) (+^) result acc)) ]

(* create model *)
let createModel segs = 
	let aux acc = function
		| S seg -> let result = createModelS seg acc in aggregate result acc
		| B seg -> let result = createModelB seg acc in aggregate result acc
	in 
	List.fold_left aux (lift [init_spd; 0.0; 0.0]) segs 


let print_segment seg = 
	match seg with 
	| S seg -> let a1 = if peek seg.a1 > max_acc then max_acc else if peek seg.a1 < max_dec then max_dec else peek seg.a1 in 
			   let a2 = if peek seg.a2 > max_acc then max_acc else if peek seg.a2 < max_dec then max_dec else peek seg.a2 in 
			   let cp = if peek seg.cp > 1.0 then 1.0 else if peek seg.cp < 0.0 then 0.0 else peek seg.cp in 
			   Printf.printf "start: (%.2f, %.2f); end: (%.2f, %.2f); a1: %.2f; a2: %.2f; cp: %.2f.\n" (fst seg.s) (snd seg.s) (fst seg.e) (snd seg.e) a1 a2 cp; Printf.printf "%!"
	| B seg -> let a1 = if peek seg.a1 > max_acc then max_acc else if peek seg.a1 < max_dec then max_dec else peek seg.a1 in 
			   let a2 = if peek seg.a2 > max_acc then max_acc else if peek seg.a2 < max_dec then max_dec else peek seg.a2 in 
			   let cp = if peek seg.cp > 1.0 then 1.0 else if peek seg.cp < 0.0 then 0.0 else peek seg.cp in 
			   let cps = if peek seg.cps < 1.0 then 1.0 else peek seg.cps in 
			   let cpe = if peek seg.cpe < 1.0 then 1.0 else peek seg.cpe in 
			   let (x0, y0) = seg.s in 
			   let (x1, y1) = seg.e in 
			   let p1 = point seg.s (x1, y0) cps in 
			   let p2 = point seg.e (x0, y1) cpe in 
			   Printf.printf "start: (%.2f, %.2f); end: (%.2f, %.2f); a1: %.2f; a2: %.2f; cp: %.2f; p1: (%.2f, %.2f); p2: (%.2f, %.2f).\n" (fst seg.s) (snd seg.s) (fst seg.e) (snd seg.e) a1 a2 cp (fst p1) (snd p1) (fst p2) (snd p2); Printf.printf "%!"


let simulate segments =
	let segs = createPrms segments in 
	let model = createModel segs in 
	let delta, rate = cell (lift 0.01), cell (lift 0.1) in 
	let%fuse (f, ps) = [%dfg (lift nth) model 2] in 
	let loss f v = f v in 
	let grad = num_grad f ps loss delta in 
	let optimise = gradient_descent ~debug:true ps grad rate in 
	for i = 1 to 1000 do 
		optimise (fun x -> x)
	done; 
	List.map print_segment segs; 
	(Printf.printf "Time used: %.2fs \n\n" (peek [%dfg (lift nth) model 1])); Printf.printf "%!"

let rec trial segments steps = 
	let rec aux = function 
	| 0 -> () 
	| n -> (Printf.printf ("Trial %d:\n") (steps-n+1)); Printf.printf "%!"; 
		   simulate segments; 
		   aux (n-1)
   in 
   aux steps

let _ = 
	let segments = [((0.0, 0.0), (1000.0, 0.0)); ((1500.0, 1000.0), (2500.0, 1000.0))] in 
	trial segments 1000
	
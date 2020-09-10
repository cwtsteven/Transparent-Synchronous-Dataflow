open Sysyf

val w_reset : bool graph -> 'a graph -> 'a graph -> 'a graph

val cross_entropy : float list graph -> float list graph -> float graph

val mean_squared_error : float list graph -> float list graph -> float graph

val num_grad : ('n sym graph -> 'b graph) -> 'n sym graph 
			   -> (('n sym graph -> 'b graph) -> 'n sym graph -> float graph) 
			   -> float graph -> 'n sym graph

val accloss : bool graph -> (('n sym graph -> 'b graph) -> 'n sym graph -> float graph) 
              -> ('n sym graph -> 'b graph) -> 'n sym graph 
              -> float graph

val feedforward : 'a graph -> 'b graph -> ('a * 'b) list -> int -> unit 

val gradient_descent : ?debug:bool 
					   -> 'n sym graph 
					   -> 'n sym graph 
					   -> float graph 
					   -> ((unit -> unit) -> unit) 

val train : ?debug:bool -> ?shuffle:bool -> ?batch_size:int 
			-> ?optimiser:(?debug:bool 
					       -> 'n sym graph 
					       -> 'n sym graph 
					       -> float graph 
					       -> ((unit -> unit) -> unit) ) 
		    -> 'n sym graph 
		    -> 'n sym graph 
		    -> ('c list -> unit -> unit) 
		    -> float graph 
		    -> 'c list
		    -> int 
		    -> unit 
(*
val predict : 'b graph -> int -> (('a * 'b) list -> int -> unit) -> 'a -> 'b 
val predict_multiple_inputs : 'b graph -> int -> (('a * 'b) list -> int -> unit) -> 'a list -> 'b 
val predict_multiple_inputs_outputs : 'b graph -> int -> (('a * 'b) list -> int -> unit) -> 'a list -> 'b list 
*)

val input_layer : int -> float list graph
val perceptron : (float -> float) -> float list graph -> float graph 
val dense_layer : (float list graph -> float graph) -> int -> float list graph -> float list graph 
val softmax_layer : int -> float list graph -> float list graph
val lstm : bool graph -> float list graph -> float graph 

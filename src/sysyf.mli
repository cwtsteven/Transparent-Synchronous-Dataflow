type sysyf_z = SYSYF_Z 
type 'n sysyf_s = SYSYF_S : 'n -> 'n sysyf_s 
type 'n sym
type 'a graph  

(* liftings *)
val lift : 'a -> 'a graph
val lift_list : 'a graph list -> 'a list graph
val lift_tuple : 'a graph * 'b graph -> ('a * 'b) graph

(* core for tsd *)
val peek : 'a graph -> 'a 
val root : 'a graph -> 'a graph 
val apply : ('a -> 'b) graph -> 'a graph -> 'b graph
val ifthenelse : bool graph -> (unit -> 'a graph) -> (unit -> 'a graph) -> 'a graph
val cell : 'a graph -> 'a graph 

val link : 'a graph -> 'a graph -> unit
val (<~) : 'a graph -> 'a graph -> unit
val assign : 'a graph -> 'a -> unit
val (<:=) : 'a graph -> 'a -> unit 
val set : 'a graph -> 'a -> unit
val (<:~) : 'a graph -> 'a -> unit 
val init : unit -> unit 
val step : unit -> bool 
val n_step : int -> bool

(* creating provisional constants *)
val pc : float -> float graph 

(* parameter tensors management *)
val _fusion : 'a graph -> 'n -> ('n sym graph -> 'a graph) * 'n sym graph (* not to be used by programmers *)

(* algebraic operators for symmetric tensor *)
val plus : 'n sym -> 'n sym -> 'n sym
val (|+|) : 'n sym -> 'n sym -> 'n sym

val mult : float -> 'n sym -> 'n sym
val (|*|) : float -> 'n sym -> 'n sym 

val dot : 'n sym -> 'n sym -> float
val (|.|) : 'n sym -> 'n sym -> float

val fold : ('n sym graph -> 'n sym graph) -> 'n sym graph -> 'n sym graph 

val print_dict : 'n sym -> unit 

val print_graph : 'a graph -> unit


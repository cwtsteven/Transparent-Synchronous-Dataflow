type key
type t
val empty : t
val add : key -> 'a -> t -> t
val remove : key -> t -> t
val singleton : key -> 'a -> t
val union : t -> t -> t
val find : key -> t -> 'a
val map : ('a -> 'b) -> t -> t
val iter : (key -> 'a -> 'b) -> t -> unit
val fold : (key -> 'a -> 'b -> 'b) -> t -> 'b -> 'b
val generate_key : unit -> key
val emptyKeys : unit -> key array  
val string_of_key : key -> string
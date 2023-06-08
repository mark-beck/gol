type t = Cell.t array array

val rows : t -> int
val cols : t -> int
val create_clear : int -> int -> t
val map2d : ('a -> 'b) -> 'a array array -> 'b array array
val create_random : ?prob:int -> int -> int -> t
val get_2d : int * int -> 'a array array -> 'a option
val count_nbs : t -> int -> int -> int
val run_step : t -> t

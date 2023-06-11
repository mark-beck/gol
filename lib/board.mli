type t = Cell.t array array

val rows : t -> int
val cols : t -> int
val create_clear : int -> int -> t
val create_random : ?prob:int -> int -> int -> t
val count_nbs : t -> int -> int -> int -> int

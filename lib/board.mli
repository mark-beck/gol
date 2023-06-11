type t

val rows : t -> int
val cols : t -> int
val create_clear : int -> int -> t
val create_random : ?prob:int -> int -> int -> t
val count_nbs : t -> int -> int -> int -> int
val get : int * int -> t -> int option
val map : (int -> int) -> t -> t
val mapi : (int * int -> int -> int) -> t -> t

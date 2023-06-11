type t

val run_step : t -> Board.t -> Board.t
val color : t -> int -> string
val states : t -> int
val gol : t
val seeds : t
val bb : t

type t

val run_step : t -> Board.t -> Cell.t array array
val color : t -> int -> string
val gol : t
val seeds : t
val bb : t

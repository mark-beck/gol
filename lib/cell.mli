type t = { state : int; x : int; y : int; width : int }

val state : t -> int
val empty : unit -> t
val is_overlapping : t -> Raylib.Vector2.t -> bool
val draw : t -> unit

val vec2d_in_rect : Raylib.Vector2.t -> Raylib.Rectangle.t -> bool
val get_2d : int * int -> 'a array array -> 'a option
val map2d : ('a -> 'b) -> 'a array array -> 'b array array
val parse_color : string -> Raylib.Color.t
val id : 'a -> 'a
val oor : 'a option -> 'a option -> 'a option

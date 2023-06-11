(** Utility functions *)

val vec2d_in_rect : Raylib.Vector2.t -> Raylib.Rectangle.t -> bool
(** [vec2d_in_rect v r] checks if vector [v] is within the rectangle [r]. It returns true if [v] is inside [r], and false otherwise. *)

val get_2d : int * int -> 'a array array -> 'a option
(** [get_2d (x, y) a] retrieves the element at position (x, y) in the 2D array [a]. It returns [Some a.(x).(y)] if the coordinates (x, y) are within the bounds of [a], and [None] otherwise. *)

val map2d : ('a -> 'b) -> 'a array array -> 'b array array
(** [map2d f a] applies the function [f] to each element of the 2D array [a]. It returns a new 2D array where [f] is applied to every element in [a]. *)

val parse_color : string -> Raylib.Color.t
(** [parse_color s] parses the string representation [s] and returns the corresponding Raylib color value. *)

val id : 'a -> 'a
(** [id x] is the identity function that returns the input [x] unchanged. *)

val oor : 'a option -> 'a option -> 'a option
(** [oor a b] returns [a] if it is not [None], and [b] otherwise. It performs an "option or" operation, where the first non-None value is selected. *)

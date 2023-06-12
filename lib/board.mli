(** A module for representing and manipulating a 2D bopard of states. *)

type t

val rows : t -> int
(** [rows b] is the number of rows in [b]. *)

val cols : t -> int
(** [cols b] is the number of columns in [b]. *)

val create_clear : int -> int -> t
(** [create_clear r c] is a board with [r] rows and [c] columns, all
    of whose cells are in state 0. *)

val create_random : ?prob:int -> int -> int -> t
(** [create_random ?prob r c] is a board with [r] rows and [c]
    columns, whose cells are in state 1 with probability [prob], otherwise in state 0.
    The default value of [prob] is 20. *)

val count_nbs : t -> int -> int -> int -> int
(** [count_nbs b r c s] is the number of neighbors of cell [(r,c)] in
    board [b] that are in state [s]. *)

val count_nbs_2 : t -> int -> int -> int -> int array

val get : int * int -> t -> int option
(** [get (r,c) b] is the state of cell [(r,c)] in board [b], or [None]
    if [(r,c)] is not a valid cell in [b]. *)

val map : (int -> int) -> t -> t
(** [map f b] is a board with the same dimensions as [b], whose cells
    are in state [f s] if the corresponding cell in [b] is in state
    [s]. *)

val mapi : (int * int -> int -> int) -> t -> t
(** [mapi f b] is a board with the same dimensions as [b], whose cells
    are in state [f (r,c) s] if the corresponding cell in [b] is in
    state [s]. *)

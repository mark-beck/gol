(** This module provides an interface for working with cellular automata and includes predefined automata implementations. *)

type t
(** The type representing a specific ruleset. *)

val run_step : t -> Board.t -> Board.t
(** [run_step ruleset board] runs a single step of the automaton using the provided [ruleset] on the given [board]. It returns the updated board after applying the rules. *)

val color : t -> int -> string
(** [color ruleset state] returns the color associated with the given [state] in the specified [ruleset]. *)

val states : t -> int
(** [states ruleset] returns the number of states in the specified [ruleset]. *)

val gol : t
(** Conway's Game of Life automaton. *)

val seeds : t
(** Seeds automaton. *)

val bb : t
(** Brian's Brain automaton. *)

val wireworld : t
(** Wireworld automaton. *)

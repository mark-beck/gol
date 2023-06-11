(** Module containing the state of the game. *)

type t = {
  tick_count : float;
  last_gamestep : float;
  paused : bool;
  step_intervall : float;
  reset : bool;
  random : bool;
  last_info : float;
  rows : int;
  cols : int;
  selected_color : int;
}
(** The state of the game. *)

val default : t
(** The default state of the game. *)

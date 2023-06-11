(** [obj.mli] contains the interface for the [obj] module. *)

(** [baseObj] is the base class for all drawable objects in the game. *)
class virtual baseObj :
  object
    method virtual check_hover : Gamestate.t -> Raylib.Vector2.t -> unit
    method virtual recompute_dimensions : int -> int -> unit
    method virtual render : unit
    method virtual tick : Gamestate.t -> Gamestate.t
  end

(** [boardObj] is the class for the board object. It contains the board
    itself, the rule, and can render and update the board. *)
class boardObj :
  int
  -> int
  -> int
  -> int
  -> Board.t
  -> Rule.t
  -> object
       method check_hover : Gamestate.t -> Raylib.Vector2.t -> unit
       method clear_board : int -> int -> unit
       method random_board : int -> int -> unit
       method recompute_dimensions : int -> int -> unit
       method render : unit
       method rule : Rule.t
       method tick : Gamestate.t -> Gamestate.t
     end

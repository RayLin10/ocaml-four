(** Board animation and messages when a game reaches an ending state. *)

val print_message : Board.state -> unit
(** [print_message s] prints a corresponding end of game message when given a
    state that is not [Continue]. *)

val drop_animation : Board.t -> Board.state -> unit
(** [drop_animation b] prints out a number of boards in succession that
    represents the tokens falling out of the board. *)

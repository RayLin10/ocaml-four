(** Representation of dynamic board state.

    This module represents the board in a OCaml (Connect) Four game. It includes
    most of the methods that print, modify, or consume the board. *)

(** The type [slot] represents what is in a slot of a OCaml Four board. It can
    also be used for player representation. *)
type slot =
  | Red
  | Blue
  | Empty

type t
(** The abstract type of values representing a OCaml Four board. *)

exception FullColumn
(** Raised when there is an attempt to place a token into a full column. *)

val max_row : int
(** The maximum number of rows in a OCaml Four board. *)

val max_col : int
(** The maximum number of columns in a OCaml Four board. *)

val empty_board : unit -> t
(** [empty_board ()] is a value of type [t] that represents an empty board. *)

val print_board : t -> unit
(** [print_board b] prints the board [b]. *)

val place_piece : t -> int -> slot -> int * int
(** [place_piece b c p] places a token of the player [p] into column [c] of
    board [b] and returns (r, c) which represents the row [r] and column [c]
    that the token is placed into.

    Raises: [FullColumn] if the column [c] is full. *)

val tokens_drop : t -> t
(** [tokens_drop b] is the new board [b'] which is the board [b] with all tokens
    moved down a row and the tokens in the last row gone. *)

val check_empty : t -> bool
(** [check_empty b] is the boolean [e] that represents whether the board [b] is
    empty or not. *)

val string_of_board : t -> string
(** [string_of_board b] is a string [s] representing the board formatted as a
    grid *)

(** The type [state] represents the state of an OCaml Four board. *)
type state =
  | RedWin
  | BlueWin
  | Tie
  | Continue

val check_state : t -> slot -> int -> int -> state
(** [check_state b] is the state that the board [b] is in. *)

val valid_moves : t -> int list
(** [valid_moves b] is the list [lst] of valid moves for board [b]. *)

val copy : t -> t
(** [copy b] returns a copy of the board [b]. *)

val print_state : state -> string
(** [print_state state] returns the state as a string *)
(** Parsing of player commands. *)

(** The type [command] represents the result of a player command. *)
type command =
  | ValidColumn of int
  | Quit

exception Empty
(** Raised when an empty command is parsed. *)

exception InvalidColumn
(** Raised when an invalid column command is parsed. *)

exception Malformed
(** Raised when a malformed command is parsed. *)

val parse : string -> command
(** [parse str] parses a player's input into a [command] as follows. A string of
    a valid integer is parsed into a [command] of [ValidColumn]. ["Quit"] and
    some other variations of it are parsed into the [command] [Quit].

    - [parse "4"] is [ValidColumn \[4\]]
    - [parse "quit"] is [Quit]

    Raises: [Empty] when [""] is parsed.

    Raises: [InvalidColumn] when ["i"] is parsed and [i] is outside the range of
    valid columns (1-7).

    Raises: [Malformed] when the string parsed is neither an integer or some
    variation of ["Quit"]. *)

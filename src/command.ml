open Board

type command =
  | ValidColumn of int
  | Quit

exception Empty
exception InvalidColumn
exception Malformed

let valid_quit = [ "quit"; "exit"; "q" ]

let parse (str : string) : command =
  match str with
  | "" -> raise Empty
  | q when List.mem (String.lowercase_ascii str) valid_quit -> Quit
  | _ -> begin
      match int_of_string str with
      | exception Failure _ -> raise Malformed
      | i when i >= 1 && i <= max_col -> ValidColumn (int_of_string str)
      | i -> raise InvalidColumn
    end
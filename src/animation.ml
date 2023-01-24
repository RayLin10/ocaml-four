open Unix
open Board
open ANSITerminal

let print_message (s : state) =
  let terminal_length = fst (ANSITerminal.size ()) in
  match s with
  | RedWin ->
      let message = "Red Won. Congratulations!" in
      let indent_length = (terminal_length - String.length message) / 2 in
      let indent = String.make indent_length ' ' in
      ANSITerminal.print_string [ ANSITerminal.red ] (indent ^ message);
      print_endline "\n"
  | BlueWin ->
      let message = "Blue Won. Congratulations!" in
      let indent_length = (terminal_length - String.length message) / 2 in
      let indent = String.make indent_length ' ' in
      ANSITerminal.print_string [ ANSITerminal.blue ] (indent ^ message);
      print_endline "\n"
  | Tie ->
      let message = "The game has ended in a tie." in
      let indent_length = (terminal_length - String.length message) / 2 in
      let indent = String.make indent_length ' ' in
      Stdlib.print_string (indent ^ message);
      print_endline "\n"
  | Continue -> failwith "Invalid Input"

let drop_animation (board : t) (s : state) =
  let b = ref board in
  let finished = ref false in
  while not !finished do
    ANSITerminal.erase Screen;
    print_board !b;
    print_endline "\n";
    print_message s;
    sleepf 1.;
    b := tokens_drop !b;
    finished := check_empty !b
  done;
  ANSITerminal.erase Screen;
  print_board !b;
  print_endline "\n";
  print_message s
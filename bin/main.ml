open ANSITerminal
open Unix
open Game
open Board
open Ai
open Command
open Animation

let next_player (p : slot) =
  match p with
  | Red -> Blue
  | Blue -> Red
  | Empty -> failwith "Invalid Input"

let turn_message (p : slot) =
  let terminal_length = fst (ANSITerminal.size ()) in
  match p with
  | Red ->
      let message = "Red's Turn" in
      let indent_length = (terminal_length - String.length message) / 2 in
      let indent = String.make indent_length ' ' in
      ANSITerminal.print_string [ ANSITerminal.red ] (indent ^ message);
      print_endline "\n"
  | Blue ->
      let message = "Blue's Turn" in
      let indent_length = (terminal_length - String.length message) / 2 in
      let indent = String.make indent_length ' ' in
      ANSITerminal.print_string [ ANSITerminal.blue ] (indent ^ message);
      print_endline "\n"
  | Empty -> failwith "Invalid Input"

let print_center (s : string) =
  let terminal_length = fst (ANSITerminal.size ()) in
  let str_length = String.length s in
  let indent_length = (terminal_length - str_length) / 2 in
  if indent_length >= 0 then
    let indent = String.make indent_length ' ' in
    print_endline (indent ^ s)
  else print_endline s

let choose_message = "Please choose a column to place your piece (1-7):\n"
let quit_message = "Thanks for playing.\n"
let full_column_message = "The column is full. Please pick another column.\n"
let empty_message = "You can't do nothing.\n"
let invalid_column_message = "That isn't a valid column.\n"

let malformed_message =
  "The only valid inputs are \"Quit\" or an integer representing a column.\n"

let rec main () =
  ANSITerminal.erase Screen;
  print_endline "";
  print_center "Welcome to OCaml Four!\n";
  print_center "Please choose a game mode (PvP, AI):\n";
  let ai = ref None in
  Stdlib.print_string "> ";
  begin
    match read_line () |> String.lowercase_ascii with
    | exception End_of_file -> ()
    | "pvp" -> ai := Some false
    | "ai" -> ai := Some true
    | _ ->
        print_endline "";
        print_center "That is not a valid input. Please try again.";
        sleepf 2.;
        main ()
  end;
  ANSITerminal.erase Screen;
  let board = empty_board () in
  print_board board;
  print_endline "\n";
  turn_message Red;
  print_center choose_message;
  Stdlib.print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | cmd -> loop cmd board Red (Option.get !ai)

and loop (cmd : string) (board : t) (p : slot) (ai : bool) =
  match parse cmd with
  | Quit ->
      ANSITerminal.erase Screen;
      print_board board;
      print_endline "\n";
      print_center quit_message;
      exit 0
  | ValidColumn i -> begin
      match place_piece board (i - 1) p with
      | exception FullColumn -> begin
          ANSITerminal.erase Screen;
          print_endline "\n";
          print_board board;
          print_endline "\n";
          turn_message p;
          print_center full_column_message;
          print_center choose_message;
          Stdlib.print_string "> ";
          match read_line () with
          | exception End_of_file -> ()
          | cmd -> loop cmd board p ai
        end
      | r, c -> begin
          ANSITerminal.erase Screen;
          print_endline "\n";
          print_board board;
          print_endline "\n";
          let state = check_state board p r c in
          match state with
          | RedWin | BlueWin | Tie ->
              print_message state;
              sleepf 3.;
              drop_animation board state;
              play_again ()
          | Continue ->
              if ai then begin
                let ai_slot = next_player p in
                let move = mcts board ai_slot Continue in
                let r', c' = place_piece board move ai_slot in
                ANSITerminal.erase Screen;
                print_board board;
                let state' = check_state board ai_slot r' c' in
                match state' with
                | RedWin | BlueWin | Tie ->
                    print_endline "\n";
                    print_message state';
                    sleepf 3.;
                    drop_animation board state';
                    play_again ()
                | Continue -> begin
                    print_endline "\n";
                    turn_message p;
                    print_center choose_message;
                    Stdlib.print_string "> ";
                    match read_line () with
                    | exception End_of_file -> ()
                    | cmd -> loop cmd board p ai
                  end
              end
              else begin
                turn_message (next_player p);
                print_center choose_message;
                Stdlib.print_string "> ";
                match read_line () with
                | exception End_of_file -> ()
                | cmd -> loop cmd board (next_player p) ai
              end
        end
    end
  | exception Empty -> begin
      ANSITerminal.erase Screen;
      print_board board;
      print_endline "\n";
      turn_message p;
      print_center empty_message;
      print_center choose_message;
      Stdlib.print_string "> ";
      match read_line () with
      | exception End_of_file -> ()
      | cmd -> loop cmd board p ai
    end
  | exception InvalidColumn -> begin
      ANSITerminal.erase Screen;
      print_board board;
      print_endline "\n";
      turn_message p;
      print_center invalid_column_message;
      print_center choose_message;
      Stdlib.print_string "> ";
      match read_line () with
      | exception End_of_file -> ()
      | cmd -> loop cmd board p ai
    end
  | exception Malformed -> begin
      ANSITerminal.erase Screen;
      print_board board;
      print_endline "\n";
      turn_message p;
      print_center malformed_message;
      print_center choose_message;
      Stdlib.print_string "> ";
      match read_line () with
      | exception End_of_file -> ()
      | cmd -> loop cmd board p ai
    end

and play_again () =
  let valid_yes = [ "yes"; "y" ] in
  let valid_no = [ "no"; "n" ] in
  print_center "Would you like to play again? (Y/n):\n";
  Stdlib.print_string "> ";
  let str = read_line () |> String.lowercase_ascii in
  match str with
  | exception End_of_file -> ()
  | y when List.mem str valid_yes -> main ()
  | n when List.mem str valid_no -> exit 0
  | _ ->
      print_endline "";
      print_center "That is not a valid input. Please try again.";
      sleepf 2.;
      ANSITerminal.erase Screen;
      play_again ()

let () = main ()
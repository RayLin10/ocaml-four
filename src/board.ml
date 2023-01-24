open ANSITerminal

type slot =
  | Red
  | Blue
  | Empty

type t = slot array array

exception FullColumn

let max_row = 6
let max_col = 7
let row () = Array.make 7 Empty
let empty_board () = [| row (); row (); row (); row (); row (); row () |]
let column_label = "  1   2   3   4   5   6   7  "
let row_border = "+---+---+---+---+---+---+---+"
let column_border = "|"
let token = " ● "
let empty = "   "
let board_str_length = String.length row_border

let print_board (board : t) =
  let terminal_length = fst (ANSITerminal.size ()) in
  let indent_length = (terminal_length - board_str_length) / 2 in
  let indent = String.make indent_length ' ' in
  print_endline (indent ^ column_label);
  for y = 0 to max_row - 1 do
    print_endline (indent ^ row_border);
    for x = 0 to max_col - 1 do
      if x = 0 then Stdlib.print_string indent;
      Stdlib.print_string column_border;
      begin
        match board.(y).(x) with
        | Red -> ANSITerminal.print_string [ ANSITerminal.red ] token
        | Blue -> ANSITerminal.print_string [ ANSITerminal.blue ] token
        | Empty -> Stdlib.print_string empty
      end;
      if x = max_col - 1 then print_endline column_border
    done;
    if y = max_row - 1 then print_endline (indent ^ row_border)
  done

let place_piece (board : t) (c : int) (color : slot) =
  let row = ref (max_row - 1) in
  while !row > 0 && board.(!row).(c) <> Empty do
    row := !row - 1
  done;
  if board.(!row).(c) = Empty then begin
    board.(!row).(c) <- color;
    (!row, c)
  end
  else raise FullColumn

let tokens_drop (board : t) =
  let new_board = empty_board () in
  for y = 0 to max_row - 2 do
    for x = 0 to max_col - 1 do
      let cur_slot = board.(y).(x) in
      new_board.(y + 1).(x) <- cur_slot
    done
  done;
  new_board

let check_empty (board : t) =
  let empty = ref true in
  let c = ref 0 in
  while !c < max_col && !empty do
    if board.(max_row - 1).(!c) != Empty then empty := false;
    c := !c + 1
  done;
  !empty

type state =
  | RedWin
  | BlueWin
  | Tie
  | Continue

let check_tie (board : t) =
  let tie = ref true in
  let c = ref 0 in
  while !c < max_col && !tie do
    if board.(0).(!c) = Empty then tie := false;
    c := !c + 1
  done;
  !tie

let horizontal_helper (board : t) (p : slot) (row : int) (col : int) =
  let result = ref false in
  if col + 3 < max_col then begin
    let cur = ref col in
    let counter = ref 0 in
    while !counter < 4 && !cur < max_col do
      (* Check Connect Four to the Right of Piece *)
      if board.(row).(!cur) = p then begin
        cur := !cur + 1;
        counter := !counter + 1
      end
      else counter := 5
    done;
    if !counter = 4 then result := true else result := false
  end
  else result := false;
  !result

let horizontal_check (board : t) (p : slot) (row : int) (col : int) =
  let result = ref false in
  let c = ref (col - 3) in
  while !result = false && !c <= col do
    if !c >= 0 && !c + 3 < max_col then begin
      result := horizontal_helper board p row !c;
      c := !c + 1
    end
    else c := !c + 1
  done;
  !result

let vertical_helper (board : t) (p : slot) (row : int) (col : int) =
  let result = ref false in
  if row + 3 < max_row then begin
    let cur = ref row in
    let counter = ref 0 in
    while !counter < 4 && !cur < max_row do
      (* Check Connect Four to the below of Piece *)
      if board.(!cur).(col) = p then begin
        cur := !cur + 1;
        counter := !counter + 1
      end
      else counter := 5
    done;
    if !counter = 4 then result := true else result := false
  end
  else result := false;
  !result

let vertical_check (board : t) (p : slot) (row : int) (col : int) =
  let result = ref false in
  let r = ref (row - 3) in
  while !result = false && !r <= row do
    if !r >= 0 && !r + 3 < max_row then begin
      result := vertical_helper board p !r col;
      r := !r + 1
    end
    else r := !r + 1
  done;
  !result

let diagonal_down_helper (board : t) (p : slot) (row : int) (col : int) =
  let result = ref false in
  if row + 3 < max_row && col + 3 < max_col then begin
    let cur_row, cur_col = (ref row, ref col) in
    let counter = ref 0 in
    while !counter < 4 && !cur_row < max_row && !cur_col < max_col do
      (* Check Connect Four to the below right of Piece *)
      if board.(!cur_row).(!cur_col) = p then begin
        cur_row := !cur_row + 1;
        cur_col := !cur_col + 1;
        counter := !counter + 1
      end
      else counter := 5
    done;
    if !counter = 4 then result := true else result := false
  end
  else result := false;
  !result

let diagonal_down_check (board : t) (p : slot) (row : int) (col : int) =
  let result = ref false in
  let r, c = (ref (row - 3), ref (col - 3)) in
  while !result = false && !r <= row && !c <= col do
    if !r >= 0 && !r + 3 < max_row && !c >= 0 && !c + 3 < max_col then begin
      result := diagonal_down_helper board p !r !c;
      r := !r + 1;
      c := !c + 1
    end
    else begin
      r := !r + 1;
      c := !c + 1
    end
  done;
  !result

let diagonal_up_helper (board : t) (p : slot) (row : int) (col : int) =
  let result = ref false in
  if row - 3 >= 0 && col + 3 < max_col then begin
    let cur_row, cur_col = (ref row, ref col) in
    let counter = ref 0 in
    while !counter < 4 && !cur_row > 0 && !cur_col < max_col do
      (* Check Connect Four to the below right of Piece *)
      if board.(!cur_row).(!cur_col) = p then begin
        cur_row := !cur_row - 1;
        cur_col := !cur_col + 1;
        counter := !counter + 1
      end
      else counter := 5
    done;
    if !counter = 4 then result := true else result := false
  end
  else result := false;
  !result

let diagonal_up_check (board : t) (p : slot) (row : int) (col : int) =
  let result = ref false in
  let r, c = (ref (row + 3), ref (col - 3)) in
  while !result = false && !r >= row && !c <= col do
    if !r < max_row && !r - 3 > 0 && !c >= 0 && !c + 3 < max_col then begin
      result := diagonal_up_helper board p !r !c;
      r := !r - 1;
      c := !c + 1
    end
    else begin
      r := !r - 1;
      c := !c + 1
    end
  done;
  !result

let win_state (p : slot) =
  match p with
  | Red -> RedWin
  | Blue -> BlueWin
  | Empty -> failwith "Invalid Input"

let check_state (board : t) (p : slot) (row : int) (col : int) =
  if
    horizontal_check board p row col
    || vertical_check board p row col
    || diagonal_down_check board p row col
    || diagonal_up_check board p row col
  then win_state p
  else if check_tie board then Tie
  else Continue

let string_of_board b : string =
  let result = ref "" in
  Array.iter
    (fun row ->
      Array.iter
        (fun slot ->
          match slot with
          | Red -> result := !result ^ "R"
          | Blue -> result := !result ^ "B"
          | Empty -> result := !result ^ "_")
        row)
    b;
  !result

(* AI Helper Functions *)

let valid_moves (board : t) =
  let moves = ref [] in
  for x = 0 to max_col - 1 do
    if board.(0).(x) = Empty then moves := x :: !moves
  done;
  !moves

let copy (board : t) =
  let row_copy (r : slot array) = Array.copy r in
  [|
    row_copy board.(0);
    row_copy board.(1);
    row_copy board.(2);
    row_copy board.(3);
    row_copy board.(4);
    row_copy board.(5);
  |]

let print_state state =
  match state with
  | RedWin -> "RedWin"
  | BlueWin -> "BlueWin"
  | Tie -> "Tie"
  | Continue -> "Continue"

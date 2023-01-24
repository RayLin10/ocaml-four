open Board

let next_player (p : slot) =
  match p with
  | Red -> Blue
  | Blue -> Red
  | Empty -> failwith "Invalid Input"

let max_simulation =
  250000 (* Higher -> Greater Difficulty and Computation Time *)

let ucb_constant = sqrt 2.

type node = {
  board : t;
  turn : slot;
  state : state;
  parent : node option;
  prev : int option;
  mutable children : node list;
  mutable visits : float;
  mutable wins : float;
  mutable unexplored_moves : int list;
}

let create_node (board : t) (turn : slot) (state : state) (parent : node option)
    (prev : int option) =
  {
    board;
    turn;
    state;
    parent;
    prev;
    children = [];
    visits = 0.;
    wins = 0.;
    unexplored_moves = valid_moves board;
  }

let ucb_value (n : node) =
  let p = n.parent in
  (n.wins /. n.visits)
  +. sqrt (ucb_constant *. log ((Option.get p).visits /. n.visits))

let select (n : node) =
  let cur = ref n in
  while
    List.length !cur.children <> 0 && List.length !cur.unexplored_moves = 0
  do
    let best_child = ref None in
    let best_ucb = ref neg_infinity in
    List.iter
      (fun child ->
        let child_ucb = ucb_value child in
        if child_ucb > !best_ucb then begin
          best_child := Some child;
          best_ucb := child_ucb
        end)
      !cur.children;
    cur := Option.get !best_child
  done;
  !cur

let expand (n : node) =
  let possible_moves = n.unexplored_moves in
  let next_move =
    List.nth possible_moves (Random.int (List.length possible_moves))
  in
  let next_board = copy n.board in
  let r, c = place_piece next_board next_move n.turn in
  let child =
    create_node next_board (next_player n.turn)
      (check_state next_board n.turn r c)
      (Some n) (Some c)
  in
  n.children <- child :: n.children;
  n.unexplored_moves <- List.filter (fun e -> e <> next_move) n.unexplored_moves;
  (child, (r, c))

let simulate (board : t) (player : slot) (row : int) (col : int) =
  let p = ref player in
  let r = ref row in
  let c = ref col in
  let state = ref (check_state board (next_player !p) !r !c) in
  while !state = Continue do
    let possible_moves = valid_moves board in
    let next_move =
      List.nth possible_moves (Random.int (List.length possible_moves))
    in
    let next_r, next_c = place_piece board next_move !p in
    r := next_r;
    c := next_c;
    state := check_state board !p !r !c;
    p := next_player !p
  done;
  !state

let back_propagate (n : node option) (s : state) =
  let cur = ref n in
  while !cur <> None do
    let cur_node = Option.get !cur in
    cur_node.visits <- cur_node.visits +. 1.;
    begin
      match s with
      | BlueWin ->
          if cur_node.turn = Red then cur_node.wins <- cur_node.wins +. 1.
      | RedWin ->
          if cur_node.turn = Blue then cur_node.wins <- cur_node.wins +. 1.
      | _ -> ()
    end;
    cur := (Option.get !cur).parent
  done

let best_move (n : node) =
  let chosen_move = ref None in
  let highest_visits = ref neg_infinity in
  List.iter
    (fun child ->
      if child.visits > !highest_visits then begin
        chosen_move := Some child.prev;
        highest_visits := child.visits
      end)
    n.children;
  Option.get !chosen_move

let mcts (board : t) (p : slot) (s : state) =
  Random.self_init ();
  let root = create_node board p s None None in
  for x = 0 to max_simulation - 1 do
    let selection = select root in
    if selection.state = Continue then
      let new_node, (r, c) = expand selection in
      let state_res = simulate (copy new_node.board) new_node.turn r c in
      back_propagate (Some new_node) state_res
    else back_propagate (Some selection) selection.state
  done;
  Option.get (best_move root)
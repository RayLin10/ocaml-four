(** Method(s) for the AI based on Monte Carlo Tree Search. *)

val mcts : Board.t -> Board.slot -> Board.state -> int
(** [mcts b p st] is the integer corresponding to the best move [c] for the
    board [b] in the state [st] when it is [p]'s turn based on Monte Carlo Tree
    Search. *)

open Regex

type state = Nfa.state BatSet.t
type states = state BatSet.t
type edge = state * alphabet * state

type t

(* Create a new DFA with a start state *) 
val create_new_dfa : state -> t 

(* Get initial states *)
val get_initial_state : t -> state

(* Check if a state is final *)
val is_final_state : t -> state -> bool

(* Add a state to DFA *)
val add_state : t -> state -> t

(* Add a state as a final state *)
val add_final_state : t -> state -> t 

(* Add an edge to DFA *)
val add_edge : t -> edge -> t

(* Get the next state *)
val get_next_state : t -> state -> alphabet -> state

(* print DFA *)
val print : t -> unit 

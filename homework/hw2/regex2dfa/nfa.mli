open Regex 

type state = int
type states = state BatSet.t  (* set of states *)

type edge = state * alphabet * state   (* non-epsilon edge *)
type eps_edge = state * state          (* epsilon edge *)

type t

(* Create a new NFA with a single start state *)
val create_new_nfa : unit -> t

(* Get the initial state of NFA *)
val get_initial_state : t -> state

(* Get the set of all states of NFA *)
val get_states : t -> states

(* Get the set of final states *)
val get_final_states : t -> states

(* Check if a state is final *)
val is_final_state : t -> state -> bool

(* Create a state and add the state to NFA. 
 * The new state and the updated NFA are returned *)
val create_state : t -> (state * t) 

(* Add a state as a final state *)
val add_final_state : t -> state -> t 

(* Add a non-epsilon transition *)
val add_edge : t -> edge -> t

(* Add an epsilon transition *)
val add_epsilon_edge : t -> eps_edge -> t

(* Get the next state along non-epsilon transitions *)
val get_next_state : t -> state -> alphabet -> states

(* Get the next state along epsilon transitions *)
val get_next_state_epsilon : t -> state -> states

(* Add a set of states into NFA *)
val add_states : t -> states -> t

(* Get all edges in NFA *)
val get_edges : t -> (edge list * eps_edge list)

(* Add a set of edges into NFA *)
val add_edges : t -> (edge list * eps_edge list) -> t

(* Print NFA *)
val print : t -> unit

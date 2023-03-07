open Regex

let sidx = ref 0
let new_state() = sidx := !sidx + 1; !sidx

type state = int
type states = state BatSet.t 
type delta = (state * alphabet, state BatSet.t) BatMap.t 
type e_delta = (state, state BatSet.t) BatMap.t
type final = state BatSet.t
type init = state

type edge = state * Regex.alphabet * state   (* non-epsilon edge *)
type eps_edge = state * state          (* epsilon edge *)

type t = states * delta * e_delta * init * final 

(* create a new NFA with a single start state *)
let create_new_nfa () = 
  let is = new_state () in
    (BatSet.singleton is, BatMap.empty, BatMap.empty, is, BatSet.empty)

let string_of_set set =
  "{ " ^ (BatSet.fold (fun s str -> str ^ string_of_int s ^ ", ") set "") ^ " }"

let print : t -> unit 
=fun (states, delta, e_delta, is, final) ->
    prerr_endline ("* States: " ^ string_of_set states);
    prerr_endline ("* Initial State : " ^ string_of_int is); 
    prerr_endline ("* Final States: " ^ string_of_set final);
    prerr_endline "* Transition";
    prerr_endline "- non-epsilon: ";
    BatMap.iter (fun (s,a) states -> 
      prerr_endline (string_of_int s ^ " -> " ^ string_of_set states ^ " on " ^ string_of_alphabet a)
    ) delta;
    prerr_endline "- epsilon ";
    BatMap.iter (fun s states ->
        prerr_endline (string_of_int s ^ " -> " ^ string_of_set states)) e_delta;
    prerr_endline ""

(* get the initial state *)
let get_initial_state (_,_,_,s,_) = s

(* Get the set of all states *)
let get_states : t -> state BatSet.t 
=fun (states,_,_,_,_) -> states

(* Get the final states *)
let get_final_states : t -> states 
=fun (_,_,_,_,final) -> final

(* Check if a state is final *)
let is_final_state : t -> state -> bool
=fun (_,_,_,_,final) s -> BatSet.mem s final


let update_delta (s,a) s' d = 
  let old = try BatMap.find (s,a) d with _ -> BatSet.empty in
    BatMap.add (s,a) (BatSet.add s' old) d

let update_edelta s s' d = 
  let old = try BatMap.find s d with _ -> BatSet.empty in
    BatMap.add s (BatSet.add s' old) d

(* Create a state and add the state to NFA. The new state and the updated NFA * are returned *)
let create_state : t -> (state * t) 
=fun (states, delta, e_delta, init, final) ->
  let s = new_state () in
    (s, (BatSet.add s states, delta, e_delta, init, final))

(* Designate a state as a final state *)
let add_final_state : t -> state -> t 
=fun (states, delta, e_delta, init, final) s -> 
  (BatSet.add s states, delta, e_delta, init, BatSet.add s final)

(* Add a non-epsilon transition *)
let add_edge : t -> edge -> t
=fun (states, delta, e_delta, init, final) (s,a,s') ->
  if not (BatSet.mem s states) || not (BatSet.mem s' states) 
  then raise (Failure "Nfa.add_edge: states not found")
  else (states, update_delta (s,a) s' delta, e_delta, init, final)

(* Add an epsilon transition *)
let add_epsilon_edge : t -> eps_edge -> t
=fun (states, delta, e_delta, init, final) (s,s') ->
  if not (BatSet.mem s states) || not (BatSet.mem s' states) 
  then raise (Failure "Nfa.add_edge: states not found")
  else (states, delta, update_edelta s s' e_delta, init, final)

(* Get the next state along non-epsilon transitions *)
let get_next_state : t -> state -> alphabet -> state BatSet.t
=fun (_, delta, _, _, _) s a -> 
  try BatMap.find (s,a) delta
  with _ -> BatSet.empty

(* Get the next state along epsilon transitions *)
let get_next_state_epsilon : t -> state -> states
=fun (_, _, e_delta, _, _) s -> 
  try BatMap.find s e_delta
  with _ -> BatSet.empty

(* Add a set of states into NFA *)
let add_states : t -> states -> t
=fun (states,delta,e_delta,is,fs) states' ->
  (BatSet.union states states', delta, e_delta, is, fs)

(* Get the set of all edges in NFA. A pair of non-epsilon edges and epsilon
 * edges is returned *)
let get_edges : t -> edge list * eps_edge list
=fun (_,d,ed,_,_) -> 
  (BatMap.foldi (fun (s,a) states l -> 
     BatSet.fold (fun s' l -> (s,a,s')::l) states l) d [],
   BatMap.foldi (fun s states l -> 
     BatSet.fold (fun s' l -> (s,s')::l) states l) ed [])

(* Add a set of edges into NFA.  *)
let add_edges : t -> edge list * eps_edge list -> t
=fun (states, delta, e_delta, is, fs) (edges,e_edges) ->
  (states, 
   List.fold_right (fun (s,a,s') -> update_delta (s,a) s') edges delta, 
   List.fold_right (fun (s,s') -> update_edelta s s') e_edges e_delta, 
   is, fs)

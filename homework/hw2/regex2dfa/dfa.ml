open Regex

type state = Nfa.state BatSet.t (* A DFA state is a set of NFA states *)
type states = state BatSet.t 
type delta = (state * alphabet, state) BatMap.t 
type final = state BatSet.t
type init = state

type edge = state * alphabet * state

type t = states * delta * init * final 

let string_of_set set =
  "{ " ^ (BatSet.fold (fun s str -> str ^ string_of_int s ^ ", ") set "") ^ " }"

let string_of_states states = 
  BatSet.fold (fun s str -> str ^ string_of_set s ^ ", ")  states ""

let print : t -> unit 
=fun (states, delta, is, final) ->
    prerr_endline ("* States: " ^ string_of_states states);
    prerr_endline ("* Initial State : " ^ string_of_set is); 
    prerr_endline ("* Final States : " ^ string_of_states final);
    prerr_endline "* Transition";
    BatMap.iter (fun (s,a) s' -> 
      prerr_endline (string_of_set s ^ " -> " ^ string_of_set s' ^ " on " ^ string_of_alphabet a)
    ) delta;
    prerr_endline ""


(* Create a new DFA with a start state *) 
let create_new_dfa : state -> t 
=fun is -> (BatSet.singleton is, BatMap.empty, is, BatSet.empty)

let get_initial_state (_,_,s,_) = s

let add_state : t -> state -> t
=fun (states, delta, is, final) s -> (BatSet.add s states, delta, is, final)

let add_final_state : t -> state -> t 
=fun (states, delta, init, final) s -> 
  (BatSet.add s states, delta, init, BatSet.add s final)

let add_edge : t -> edge -> t
=fun (states, delta, init, final) (s,a,s') ->
  if not (BatSet.mem s states) || not (BatSet.mem s' states) 
  then raise (Failure "Dfa.add_edge: states not found")
  else (states, BatMap.add (s,a) s' delta, init, final)

let get_next_state : t -> state -> alphabet -> state
=fun (_, delta, _, _) s a -> 
  try BatMap.find (s,a) delta
  with _ -> raise (Failure "Dfa.get_next_state: Not found")

let is_final_state : t -> state -> bool
=fun (_,_,_,final) s -> BatSet.mem s final



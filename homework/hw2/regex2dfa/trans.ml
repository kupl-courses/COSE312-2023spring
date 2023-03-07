open Regex 

exception Not_implemented

let regex2nfa : Regex.t -> Nfa.t 
=fun _ -> raise Not_implemented (* TODO *)

let nfa2dfa : Nfa.t -> Dfa.t
=fun _ -> raise Not_implemented (* TODO *)
 
(* Do not modify this function *)
let regex2dfa : Regex.t -> Dfa.t
=fun regex -> 
  let nfa = regex2nfa regex in
  let dfa = nfa2dfa nfa in
    dfa

let run_dfa : Dfa.t -> alphabet list -> bool
=fun _ _ -> raise Not_implemented (* TODO *)

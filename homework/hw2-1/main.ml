open Regex
open Hw1

let testcases : (Regex.t * alphabet list) list = 
  [ 
    (Empty, []);
    (Epsilon, []);
    (Alpha A, [A]);
    (Alpha A, [B]);
    (OR (Alpha A, Alpha B), [B]);
    (CONCAT (STAR (Alpha A), Alpha B), [B]);
    (CONCAT (STAR (Alpha A), Alpha B), [A;B]);
    (CONCAT (STAR (Alpha A), Alpha B), [A;A;B]);
    (CONCAT (STAR (Alpha A), Alpha B), [A;B;B]);
    (CONCAT (CONCAT (STAR (CONCAT (Alpha A, Alpha A)), STAR (CONCAT (Alpha B, Alpha B))), Alpha B), [B]);
    (CONCAT (CONCAT (STAR (CONCAT (Alpha A, Alpha A)), STAR (CONCAT (Alpha B, Alpha B))), Alpha B), [A;A;B]);
    (CONCAT (CONCAT (STAR (CONCAT (Alpha A, Alpha A)), STAR (CONCAT (Alpha B, Alpha B))), Alpha B), [B;B;B]);
    (CONCAT (CONCAT (STAR (CONCAT (Alpha A, Alpha A)), STAR (CONCAT (Alpha B, Alpha B))), Alpha B), [A;A;A;A;B;B;B]);
    (CONCAT (CONCAT (STAR (CONCAT (Alpha A, Alpha A)), STAR (CONCAT (Alpha B, Alpha B))), Alpha B), [A;A;A;B;B;B])
  ]

let match_regex : Regex.t -> alphabet list -> bool
=fun regex input -> Hw1.run_dfa (Hw1.regex2dfa regex) input

(* run testcases *)
let _ = 
  List.iter (fun (regex, str) -> 
    prerr_endline (string_of_bool (match_regex regex str)) 
  ) testcases

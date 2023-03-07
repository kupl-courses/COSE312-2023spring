type symbol = T of string | N of string | Epsilon | End
type production = (symbol * symbol list) list
type cfg = symbol list * symbol list * symbol * production

let check_LL1 : cfg -> bool
=fun _ -> false (* TODO *)

let parse : cfg -> symbol list -> bool
=fun _ _ -> false (* TODO *)

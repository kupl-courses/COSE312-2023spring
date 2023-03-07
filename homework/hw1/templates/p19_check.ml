exception NotImplemented;;

type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
and var = string;;

let check : exp -> bool
= fun exp -> raise NotImplemented;; (* TODO *)

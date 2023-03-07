exception NotImplemented;;

type formula =
  | True
  | False
  | Not of formula
  | AndAlso of formula * formula
  | OrElse of formula * formula
  | Imply of formula * formula
  | Equal of exp * exp

and exp =
  | Num of int
  | Plus of exp * exp
  | Minus of exp * exp;;

let eval : formula -> bool
= fun f -> raise NotImplemented;; (* TODO *)

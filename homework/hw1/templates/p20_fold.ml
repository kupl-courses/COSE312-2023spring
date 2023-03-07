exception NotImplemented;;

let rec fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
= fun f lst init ->
    match lst with
    | [] -> init
    | hd::tl -> f hd (fold_right f tl init)

let rec fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
= fun f init lst ->
    match lst with
    | [] -> init
    | hd::tl -> fold_left f (f init hd) tl

let rec length : 'a list -> int
= fun l -> raise NotImplemented;; (* TODO *)

let rec reverse : 'a list -> 'a list
= fun l -> raise NotImplemented;; (* TODO *)

let rec is_all_pos : 'a list -> bool
= fun l -> raise NotImplemented;; (* TODO *)

let rec map : ('a -> 'b) -> 'a list -> 'b list
= fun f l -> raise NotImplemented;; (* TODO *)

let rec filter : ('a -> bool) -> 'a list -> 'a list
= fun p l -> raise NotImplemented;; (* TODO *)
exception NotImplemented;;

type btree = Empty | Node of int * btree * btree

let mem : int -> btree -> bool
= fun n t -> raise NotImplemented;; (* TODO *)

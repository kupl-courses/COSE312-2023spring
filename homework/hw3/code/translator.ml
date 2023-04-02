(*
open Spy
open Lib.Util
open Spvm
*)

let temp_var_index = ref 0
let label_index = ref 1
let new_temp_var() = temp_var_index := !temp_var_index + 1; ".t" ^ (string_of_int !temp_var_index)
let new_label() = label_index := !label_index + 1; !label_index

exception Not_Implemented of string 
exception Error of string (* raise when syntax is beyond Spy *)

let translate : Spy.program -> Spvm.program
=fun _ -> raise (Not_Implemented "Spy.program")
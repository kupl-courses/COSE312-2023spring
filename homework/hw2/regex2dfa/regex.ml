type alphabet = A | B

let string_of_alphabet a = match a with A -> "a" | B -> "b"

type t = 
  | Empty 
  | Epsilon
  | Alpha of alphabet
  | OR of t * t
  | CONCAT of t * t
  | STAR of t

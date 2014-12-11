type n =
  | Z
  | S of n

let z = Z
let one = S(Z)
let two = S(S(Z))

open Format

let rec print fp = function
  | Z -> fprintf fp "Z@?"
  | S(n) -> fprintf fp "S(%a)@?" print n

type t =
| Tv
| Ti of int

type e =
| EBin of (e * string * e)
| EInt of int

type r =
| RL of t * string
| RN of t * string

type v =
| VBin of r * string * r * r
| VPrint of r


module FM4FUNTypesAST

type C =
    | Assign of (string * a)
    | ArrAssign of (string * a * a)
    | Skip
    | Seq of (C * C)
    | If of GC
    | Do of GC

and GC =
    | Cond of (b * C)
    | Conc of (GC * GC)


and a =
    | N of int
    | X of string
    | ArrRead of string * a
    | Add of (a * a)
    | Sub of (a * a)
    | Prod of (a * a)
    | Div of (a * a)
    | Neg of a
    | Exp of (a * a)
    | ParA of a

and b =
    | Bool of bool
    | SingleAnd of (b * b)
    | SingleOr of (b * b)
    | DoubleAnd of (b * b)
    | DoubleOr of (b * b)
    | NegB of b
    | Eq of (a * a)
    | Neq of (a * a)
    | Gt of (a * a)
    | Geq of (a * a)
    | Lt of (a * a)
    | Leq of (a * a)
    | ParB of b

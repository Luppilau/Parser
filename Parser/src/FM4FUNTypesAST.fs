// This file implements a module where we define a data type "expr"
// to store represent arithmetic expressions
module FM4FUNTypesAST

type expr =
  | Num of float
  | TimesExpr of (expr * expr)
  | DivExpr of (expr * expr)
  | PlusExpr of (expr * expr)
  | MinusExpr of (expr * expr)
  | PowExpr of (expr * expr)
  | UPlusExpr of (expr)
  | UMinusExpr of (expr)

type C =
  | Assignment of ( x * C)
  | ArrAssignment of ( ArrVal * a )
  | Skip
  | Chain of (C * C)
  | If of GC
  | Do of GC

type GC = 
  | Match of (b * C)
  | Conc of (GC * GC)

type a = 
  | n of int
  | x of x
  | ArrVal of Arrval
  | Add of (a * a)
  | Sub of (a * a)
  | Prod of (a * a)
  | Div of (a * a)
  | Neg of a
  | Exp of (a * a)
  | Par of a

type b = 
  | true
  | false
  | SingleAnd of (b * b)
  | SingleOr of (b * b)
  | DoubleAnd of (b * b)
  | DoubleOr of (b * b)
  | Neg of b
  | Eq of (a * a)
  | Neq of (a * a)
  | Gt of (a * a)
  | Geq of (a * a)
  | Lt of (a * a)
  | Leq of (a * a)
  | Par of b

type x = 
  | String

type A =
  | String

type ArrVal =
  | A * a
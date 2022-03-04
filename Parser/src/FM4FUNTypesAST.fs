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
  | 

type x = 
  | String

type A =
  | String

type ArrVal =
  | A * a
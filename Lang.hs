-- Team Members:
-- Hannah Vaughan
-- Cole Swanson
-- Melanie Gutzmann

module QueueLang where

data Value = I Int | B Bool | T Value Value

data Expr = Add | Mul | Div | Equ | If Prog Prog

data Stmt = While Expr Cmd | Begin [Cmd]

data Cmd = Push Value | E Expr | S Stmt

type Queue = [Value]
type Prog = [Cmd]
type Domain = Queue -> Maybe Queue
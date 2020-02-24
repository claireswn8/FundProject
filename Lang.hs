-- Team Members:
-- Hannah Vaughan
-- Cole Swanson
-- Melanie Gutzmann

module QueueLang where

data Value = I Int | B Bool | T Value Value
   deriving(Eq, Show)

data Expr = Add | Mul | Div | Equ | If Prog Prog
   deriving(Eq, Show)

data Stmt = While Expr Cmd | Begin [Cmd]
   deriving(Eq, Show)

data Cmd = Push Value | E Expr | S Stmt
   deriving(Eq, Show)

type Queue = [Value]

type Prog = [Cmd]

type Domain = Queue -> Maybe Queue


cmd :: Cmd -> Domain
cmd (Push v) q =Just (q ++ [v])
cmd (E e) q = expr e q
cmd (S s) q = stmt s q

expr :: Expr -> Domain
expr Add q = case q of 
                ((I i) : (I j) : qs) -> Just ( qs ++ [I (i+j)] )
                _                    -> Nothing
expr Mul q = case q of
              ((I i) : (I j) : qs) -> Just (qs ++ [I (i*j)])
              _                    -> Nothing  
expr Equ q = case q of 
               ((I i) : (I j) : qs) -> Just (qs ++ [B (i == j)])
               ((B a) : (B b) : qs)  -> Just (qs ++ [B (a == b)])
               _                    -> Nothing 
expr (If t f) q = case q of
                  (B True : qs)  -> prog t qs
                  (B False : qs) -> prog f qs
                  _              -> Nothing 

stmt :: Stmt -> Domain
stmt (While e c) q = case (expr e q) of 
                     (Just ((B True):qs)) -> case (cmd c qs) of
                                             (Just q) -> stmt (While e c) q
                                             _        -> Nothing
                     (Just (_:qs))         -> Just (qs)
                     _                     -> Nothing
stmt (Begin (c:cs)) q = case (c:cs) of
                     [] -> Just q
                     _  -> case (cmd c q) of
                           (Just q) -> stmt (Begin cs) q
                           _        -> Nothing
 

prog :: Prog -> Domain
prog [] q      = Just q
prog (c:cs) q  = case (cmd c q) of
                 (Just q) -> prog cs q 
                 _        -> Nothing     
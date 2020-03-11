-- Team Members:
-- Hannah Vaughan
-- Cole Swanson
-- Melanie Gutzmann

module StackLang where


data Value = D Double
           | B Bool
           | T Value Value
           | F Cmd 
   deriving (Eq, Show)

data Expr = Add
          | Mul
          | Div
          | Equ
          | If Prog Prog
          | Less
   deriving (Eq, Show)

data Stmt = While Expr Cmd
          | Begin Prog
   deriving (Eq, Show)

data Cmd = Push Value
         | Pop
         | E Expr
         | S Stmt
         | Call FuncName
   deriving (Eq, Show)

type Stack = [Value]

type Prog = [Cmd]

type FuncName = String

type Func = (FuncName, [Cmd])

type Domain = Stack -> [Func] -> Maybe Stack

cmd :: Cmd -> Domain
cmd (Pop)     []     _ = Nothing
cmd (Pop)     (q:qs) _ = Just qs
cmd (Push v)  q      _ = Just (v : q)
cmd (E e)     q     fs = expr e q fs
cmd (S s)     q     fs = stmt s q fs
cmd (Call fn) q     fs = case lookupFunc fn fs of 
                              Just cmds -> prog cmds q fs
                              _         -> Nothing



safeDiv :: Double -> Double -> Maybe Double
safeDiv _ 0 = Nothing
safeDiv x y = Just (x / y)

tupleDiv :: Value -> Value -> Maybe Value
tupleDiv (T a b) (T c d) = case (a, b, c, d) of
                              (_, D 0, _, D 0)     -> Nothing
                              (D a, D b, D c, D d) -> case (safeDiv a c, safeDiv b d) of
                                                         (Nothing, _)     -> Nothing
                                                         (_, Nothing)     -> Nothing
                                                         (Just x, Just y) -> Just (T (D x) (D y))
                              _                    -> Nothing
tupleDiv _        _      = Nothing

tupleEqu :: Value -> Value -> Bool
tupleEqu (T a b) (T c d) = case (a, b, c, d) of
                              (D a, D b, D c, D d) -> a == c && b == d
                              (B a, B b, B c, B d) -> a == c && b == d
                              (D a, B b, D c, B d) -> a == c && b == d
                              (B a, D b, B c, D d) -> a == c && b == d
                              _                    -> False
tupleEqu _       _       = False

tupleLess :: Value -> Value -> Bool
tupleLess (T a b) (T c d) = case (a, b, c, d) of
                              (D a, D b, D c, D d) -> a < c && b < d
                              _                    -> False
tupleLess _       _       = False


expr :: Expr -> Domain
expr Add q fs = case q of 
                  (D i   : D j   : qs) -> Just (D (i + j) : qs)
                  ( F f : qs )         -> case (prog [f] qs fs) of 
                                          Just q  -> expr Add q fs
                                          Nothing -> Nothing  
                  (a : F f : qs )      -> case (prog [f] qs fs) of 
                                          Just q  -> expr Add (a : q) fs
                                          Nothing -> Nothing  
                  (T v w : T y z : qs) -> case (v, w, y, z) of
                                             (D v, D w, D y, D z) -> Just (T (D (v + y)) (D (w + z)) : qs)
                                             _                    -> Nothing
                  _                    -> Nothing
expr Mul q fs = case q of
                  (D i   : D j   : qs) -> Just (D (i * j) : qs)
                  ( F f : qs )         -> case (prog [f] qs fs) of 
                                          Just q  -> expr Mul q fs
                                          Nothing -> Nothing 
                  (a : F f : qs )      -> case (prog [f] qs fs) of 
                                          Just q  -> expr Mul (a : q) fs
                                          Nothing -> Nothing   
                  (T v w : T y z : qs) -> case (v, w, y, z) of
                                             (D v, D w, D y, D z) -> Just (T (D (v * y)) (D (w * z)) : qs)
                                             _                    -> Nothing
                  _                    -> Nothing
expr Div q fs = case q of
                  (D i   : D j   : qs) -> case safeDiv i j of
                                          (Just k) -> Just (D k : qs)
                                          _        -> Nothing
                  ( F f : qs )         -> case (prog [f] qs fs) of 
                                          Just q  -> expr Div q fs
                                          Nothing -> Nothing
                  (a : F f : qs )      -> case (prog [f] qs fs) of 
                                          Just q  -> expr Div (a : q) fs
                                          Nothing -> Nothing  
                  (T v w : T y z : qs) -> case tupleDiv (T v w) (T y z) of
                                             (Just (T a b)) -> Just (T a b : qs)
                                             _              -> Nothing
                  _                    -> Nothing
expr Equ q fs = case q of
                  (D i   : D j   : qs) -> Just (B (i == j) : qs)
                  (B a   : B b   : qs) -> Just (B (a == b) : qs)
                  ( F f : qs )         -> case (prog [f] qs fs) of 
                                          Just q  -> expr Equ q fs
                                          Nothing -> Nothing
                  (a : F f : qs )      -> case (prog [f] qs fs) of 
                                          Just q  -> expr Equ (a : q) fs
                                          Nothing -> Nothing  
                  (T v w : T y z : qs) -> Just (B (tupleEqu (T v w) (T y z)) : qs)
                  _                    -> Nothing
expr Less q fs = case q of 
                     (D i   : D j   : qs) -> Just (B (i < j) : qs)
                     ( F f : qs )         -> case (prog [f] qs fs) of 
                                             Just q  -> expr Less q fs
                                             Nothing -> Nothing
                     (a : F f : qs )      -> case (prog [f] qs fs) of 
                                             Just q  -> expr Less (a : q) fs
                                             Nothing -> Nothing  
                     (T v w : T y z : qs) -> Just (B (tupleLess (T v w) (T y z)) : qs) 
                     _                    -> Nothing                
expr (If t f) q fs = case q of
                        (B True : qs)  -> prog t qs fs
                        (B False : qs) -> prog f qs fs
                        (F func : qs)  -> case (prog [func] qs fs) of
                                          Just q  -> expr (If t f) q fs
                                          Nothing -> Nothing
                        _              -> Nothing 

stmt :: Stmt -> Domain
stmt (While e c) q fs = case (expr e q fs) of 
                           (Just ((B True):qs)) -> case (cmd c qs fs) of
                                                      Just q -> stmt (While e c) q fs
                                                      _      -> Nothing
                           (Just (_:qs))        -> Just (qs)
                           _                    -> Nothing
stmt (Begin (c:cs)) q fs = case (cmd c q fs) of
                              Just q -> stmt (Begin cs) q fs
                              _      -> Nothing
stmt (Begin []) q _ = Just q
 
-- Takes the name of a function and a list of functions, and returns the list of commands associated
-- with the function, if it exists. If the function doesn't exist, it returns Nothing.
lookupFunc :: FuncName -> [Func] -> Maybe [Cmd]
lookupFunc fn []             = Nothing
lookupFunc fn ((n, cmds):fs) = if n == fn then Just cmds
                               else lookupFunc fn fs

prog :: Prog -> Domain
prog  []     q _  = Just q
prog  (c:cs) q fs = case cmd c q fs of
                              Just q -> prog cs q fs
                              _      -> Nothing

-- Syntactic Sugar --

true :: Cmd
true = Push (B True)

false :: Cmd
false = Push (B True)

greaterequ :: Cmd
greaterequ = S (Begin [E Less, notl])

inc :: Cmd
inc = S (Begin [Push (D 1), E Add])

dec :: Cmd
dec = S (Begin [Push (D (-1)), E Add])

notl :: Cmd
notl = E (If [Push (B False)] [Push (B True)])

andl :: Cmd
andl = E (If [E (If [true] [false])] [E (If [false] [false])]) 

orl :: Cmd
orl = E (If [E (If [true] [true])] [E (If [true] [false])])

-- Library-Level Functions --

-- | Returns x% of y
percent :: Double -> Double -> Prog
<<<<<<< HEAD
percent x y = [Push (D 100), Push (D x), E Div, Push (D y), E Mul]
=======
percent x y = [Push (D 100), Push (D x), E Div, Push (D y), E Mul]
>>>>>>> Merge branch 'percentages' of https://github.com/Coleswn8/FundProject into percentages

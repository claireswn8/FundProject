-- Team Members:
-- Hannah Vaughan
-- Cole Swanson
-- Melanie Gutzmann

module MathLang where

-- Our "Prelude", which contains library-level definitions
mathlude :: [Func]
mathlude = [("factorial", [S (Begin [Push (B False), Swap, E Dup, Push (I 2), 
            S (While Less [E Dup, Push (I 1), minus, absval, E Dup, Push (I 2)]), 
            S (While IsType [E Mul]), Swap, Pop ])]),
            ("percent", [Push (D 100), Swap, E Div, E Mul])
           ]

data Value = I Int
           | D Double
           | B Bool
           | T Value Value
           | C Cmd 
           | F FuncName
   deriving (Eq, Show)

data Expr = Add
          | Mul
          | Div
          | Equ
          | If Prog Prog
          | Less
          | Dup
          | ExprList [Expr]
          | IsType
          | Mod
   deriving (Eq, Show)

data Stmt = While Expr Prog
          | Begin Prog
   deriving (Eq, Show)

data Cmd = Push Value
         | Pop
         | ExtractTuple Int
         | E Expr
         | S Stmt
         | Call FuncName
         | CallStackFunc
         | Swap
   deriving (Eq, Show)

type Stack = [Value]

type Prog = [Cmd]

type FuncName = String

type Func = (FuncName, [Cmd])

type Domain = Stack -> [Func] -> Maybe Stack

cmd :: Cmd -> Domain
cmd (Pop)            []     _  = Nothing
cmd (Pop)            (q:qs) _  = Just qs
cmd (Push v)         q      _  = Just (v : q)
cmd (ExtractTuple _) []     _  = Just []
cmd (ExtractTuple n) (q:qs) _  = case q of
                                    T v w -> case n of
                                                0 -> Just (v : qs)
                                                1 -> Just (w : qs)
                                                2 -> Just (v : w : qs)
                                                _ -> Nothing
                                    _     -> Nothing
cmd (E e)            q     fs = expr e q fs
cmd (S s)            q     fs = stmt s q fs
cmd (Call fn)        q     fs = case lookupFunc fn fs of 
                                    Just cmds -> prog cmds q fs
                                    _         -> Nothing
-- Allows functions to be passed from the stack to other functions.
cmd (CallStackFunc) []     _  = Nothing
cmd (CallStackFunc) (q:qs) fs = case q of 
                                    (F fn) -> case lookupFunc fn fs of
                                                Just cmds -> prog cmds qs fs
                                                _         -> Nothing
                                    _      -> Nothing
-- Swaps the position of the first two elements of the stack, if they exist
cmd (Swap)           q     fs = case q of
                                    []             -> Nothing
                                    [q1]           -> Nothing
                                    (q1 : q2 : qs) -> Just (q2 : q1 : qs)


safeDiv :: Value -> Value -> Maybe Value
safeDiv x y = case y of
                  (I 0) -> Nothing
                  (D 0) -> Nothing
                  _     -> case (x, y) of
                              (I x, I y) -> Just (I (x `div` y))
                              (D x, D y) -> Just (D (x / y))
                              _          -> Nothing

tupleDiv :: Value -> Value -> Maybe Value
tupleDiv (T a b) (T c d) = case (a, b, c, d) of
                              (_, _, I 0, I 0)     -> Nothing
                              (_, _, D 0, D 0)     -> Nothing
                              (I a, I b, I c, I d) -> case (safeDiv (I a) (I c), safeDiv (I b) (I d)) of
                                                         (Nothing, _)     -> Nothing
                                                         (_, Nothing)     -> Nothing
                                                         (Just x, Just y) -> Just (T x y)
                              (D a, D b, D c, D d) -> case (safeDiv (D a) (D c), safeDiv (D b) (D d)) of
                                                         (Nothing, _)     -> Nothing
                                                         (_, Nothing)     -> Nothing
                                                         (Just x, Just y) -> Just (T x y)
                              _                    -> Nothing
tupleDiv _        _      = Nothing

tupleEqu :: Value -> Value -> Bool
tupleEqu (T a b) (T c d) = case (a, b, c, d) of
                              (I a, I b, I c, I d) -> a == c && b == d
                              (D a, D b, D c, D d) -> a == c && b == d
                              (B a, B b, B c, B d) -> a == c && b == d
                              (I a, B b, I c, B d) -> a == c && b == d
                              (B a, I b, B c, I d) -> a == c && b == d
                              (I a, D b, I c, D d) -> a == c && b == d
                              (D a, B b, D c, B d) -> a == c && b == d
                              (B a, D b, B c, D d) -> a == c && b == d
                              (D a, I b, D c, I d) -> a == c && b == d
                              _                    -> False
tupleEqu _       _       = False

tupleLess :: Value -> Value -> Bool
tupleLess (T a b) (T c d) = case (a, b, c, d) of
                              (I a, I b, I c, I d) -> a < c && b < d
                              (D a, D b, D c, D d) -> a < c && b < d
                              (I a, D b, I c, D d) -> a < c && b < d
                              (D a, I b, D c, I d) -> a < c && b < d
                              _                    -> False
tupleLess _       _       = False


expr :: Expr -> Domain
expr Add q fs = case q of
                  (I i : [])           -> Just ([I i])
                  (D i : [])           -> Just ([D i])
                  (T v w : [])         -> Just ([T v w])
                  (I i : I j : qs)     -> Just (I (i + j) : qs)
                  (D i : D j : qs)     -> Just (D (i + j) : qs)
                  (C f : qs)           -> case (prog [f] qs fs) of 
                                             Just q  -> expr Add q fs
                                             Nothing -> Nothing  
                  (a : C f : qs)       -> case (prog [f] qs fs) of 
                                             Just q  -> expr Add (a : q) fs
                                             Nothing -> Nothing  
                  (T v w : T y z : qs) -> case (v, w, y, z) of
                                             (I v, I w, I y, I z) -> Just (T (I (v + y)) (I (w + z)) : qs)
                                             (D v, D w, D y, D z) -> Just (T (D (v + y)) (D (w + z)) : qs)
                                             (I v, D w, I y, D z) -> Just (T (I (v + y)) (D (w + z)) : qs)
                                             (D v, I w, D y, I z) -> Just (T (D (v + y)) (I (w + z)) : qs)
                                             _                    -> Nothing
                  _                    -> Nothing
expr Mul q fs = case q of
                  (I i : [])           -> Just ([I i])
                  (D i : [])           -> Just ([D i])
                  (T v w : [])         -> Just ([T v w])
                  (I i : I j : qs)     -> Just (I (i * j) : qs)
                  (D i : D j : qs)     -> Just (D (i * j) : qs)
                  (C f : qs)           -> case (prog [f] qs fs) of 
                                             Just q  -> expr Mul q fs
                                             Nothing -> Nothing 
                  (a : C f : qs)       -> case (prog [f] qs fs) of 
                                             Just q  -> expr Mul (a : q) fs
                                             Nothing -> Nothing   
                  (T v w : T y z : qs) -> case (v, w, y, z) of
                                             (I v, I w, I y, I z) -> Just (T (I (v * y)) (I (w * z)) : qs)
                                             (D v, D w, D y, D z) -> Just (T (D (v * y)) (D (w * z)) : qs)
                                             (I v, D w, I y, D z) -> Just (T (I (v * y)) (D (w * z)) : qs)
                                             (D v, I w, D y, I z) -> Just (T (D (v * y)) (I (w * z)) : qs)
                                             _                    -> Nothing
                  _                    -> Nothing
expr Div q fs = case q of
                  (I i : [])           -> Just ([I i])
                  (D i : [])           -> Just ([D i])
                  (T v w : [])         -> Just ([T v w])
                  (I i : I j : qs)     -> case safeDiv (I i) (I j) of
                                             (Just k) -> Just (k : qs)
                                             _        -> Nothing
                  (D i : D j : qs)     -> case safeDiv (D i) (D j) of
                                             (Just k) -> Just (k : qs)
                                             _        -> Nothing
                  (C f : qs)           -> case (prog [f] qs fs) of 
                                             Just q  -> expr Div q fs
                                             Nothing -> Nothing
                  (a : C f : qs)       -> case (prog [f] qs fs) of 
                                             Just q  -> expr Div (a : q) fs
                                             Nothing -> Nothing  
                  (T v w : T y z : qs) -> case tupleDiv (T v w) (T y z) of
                                             (Just (T a b)) -> Just (T a b : qs)
                                             _              -> Nothing
                  _                    -> Nothing
expr Equ q fs = case q of
                  (I i : [])           -> Just ([B (i == 0)])
                  (D i : [])           -> Just ([B (i == 0)])
                  (B b : [])           -> Just ([B (b == False)]) 
                  (T a b : [])         -> case (a, b) of
                                             (I a, I b)     -> Just ([B (a == 0 && b == 0)])
                                             (D a, D b)     -> Just ([B (a == 0 && b == 0)])
                                             (B a, B b)     -> Just ([B (a == False && b == False)])
                                             (I a, B b)     -> Just ([B (a == 0 && b == False)])
                                             (B a, I b)     -> Just ([B (a == False && b == 0 )])
                                             (D a, B b)     -> Just ([B (a == 0 && b == False)])
                                             (B a, D b)     -> Just ([B (a == False && b == 0)])
                                             (I a, D b)     -> Just ([B (a == 0 && b == 0)])
                                             (D a, I b)     -> Just ([B (a == 0 && b == 0)])
                                             (T v w, T y z) -> Just ([B (tupleEqu (T v w) (T y z))])
                                             _              -> Nothing
                  (I i : I j : qs)     -> Just (B (i == j) : qs)
                  (D i : D j : qs)     -> Just (B (i == j) : qs)
                  (B a : B b : qs)     -> Just (B (a == b) : qs)
                  (C f : qs)           -> case (prog [f] qs fs) of 
                                             Just q  -> expr Equ q fs
                                             Nothing -> Nothing
                  (a : C f : qs)       -> case (prog [f] qs fs) of 
                                             Just q  -> expr Equ (a : q) fs
                                             Nothing -> Nothing  
                  (T v w : T y z : qs) -> Just (B (tupleEqu (T v w) (T y z)) : qs)
                  _                    -> Nothing
expr Less q fs = case q of 
                     (I i : [])           -> Just ([B (i < 0)])
                     (D i : [])           -> Just ([B (i < 0)])
                     (T v w : [])         -> Just ([B (tupleLess (T v w) (T (I 0) (I 0)))])
                     (I i : I j : qs)     -> Just (B (i < j) : qs)
                     (D i : D j : qs)     -> Just (B (i < j) : qs)
                     (C f : qs)           -> case (prog [f] qs fs) of 
                                                Just q  -> expr Less q fs
                                                Nothing -> Nothing
                     (a : C f : qs)       -> case (prog [f] qs fs) of 
                                                Just q  -> expr Less (a : q) fs
                                                Nothing -> Nothing  
                     (T v w : T y z : qs) -> Just (B (tupleLess (T v w) (T y z)) : qs) 
                     _                    -> Nothing                
expr (If t f) q fs = case q of
                        (B True  : qs) -> prog t qs fs
                        (B False : qs) -> prog f qs fs
                        (I n     : qs) -> if n > 0 then prog t qs fs else prog f qs fs
                        (D n     : qs) -> if n > 0 then prog t qs fs else prog f qs fs
                        (C func  : qs) -> case (prog [func] qs fs) of
                                             Just q  -> expr (If t f) q fs
                                             Nothing -> Nothing
                        _              -> Nothing 
-- Duplicates the top value on the stack.
expr (Dup) q fs = case q of
                     []     -> Just []
                     (v:qs) -> Just (v : v : qs)
-- Represents a list of expressions to evaluate against the stack, in order.
expr (ExprList el) q fs = case el of 
                              []       -> Just q
                              (e : es) -> case expr e q fs of
                                             Just q2 -> expr (ExprList es) q2 fs
                                             _       -> Nothing
-- Checks if the top two values on the stack are the same type. Does not consume the values.
expr (IsType) q fs = case q of
                        []             -> Nothing
                        [v1]           -> Nothing
                        (v1 : v2 : vs) -> case (v1, v2) of
                                             (I i1, I i2)       -> Just (B True  : q)
                                             (D i1, D i2)       -> Just (B True  : q)
                                             (B i1, B i2)       -> Just (B True  : q)
                                             (T i1 i2, T i3 i4) -> Just (B True  : q)
                                             (C i1, C i2)       -> Just (B True  : q)
                                             (F i1, F i2)       -> Just (B True  : q)
                                             _                  -> Just (B False : q)
expr (Mod) q fs = case q of 
                     (I i : [])       -> Just ([I (1 `mod` i)])
                     (I i : I j : qs) -> Just (I (j `mod` i) : qs)
                     _                -> Nothing


stmt :: Stmt -> Domain
stmt (While e c)    q fs = case (expr e q fs) of 
                              (Just ((B True):qs)) -> case (prog c qs fs) of
                                                         Just q -> stmt (While e c) q fs
                                                         _      -> Nothing
                              (Just (_:qs))        -> Just (qs)
                              _                    -> Nothing
stmt (Begin (c:cs)) q fs = case (cmd c q fs) of
                              Just q -> stmt (Begin cs) q fs
                              _      -> Nothing
stmt (Begin [])     q _  = Just q
 
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

-- Runs a Prog with the MathLude
run :: Prog -> [Func] -> Maybe Stack
run [] fs = prog [] [] (fs ++ mathlude)
run x  fs = prog x [] (fs ++ mathlude)

-- Syntactic Sugar --

true :: Cmd
true = Push (B True)

false :: Cmd
false = Push (B True)

greaterequ :: Expr
greaterequ = ExprList [Less, notl]

inc :: Cmd
inc = S (Begin [Push (D 1), E Add])

dec :: Cmd
dec = S (Begin [Push (D (-1)), E Add])

notl :: Expr
notl = If [Push (B False)] [Push (B True)]

andl :: Cmd
andl = E (If [E (If [true] [false])] [E (If [false] [false])]) 

orl :: Cmd
orl = E (If [E (If [true] [true])] [E (If [true] [false])]) 

minus :: Cmd
minus = S (Begin [Push (I (-1)), E Mul, E Add])

absval :: Cmd
absval = S (Begin [E Dup, Push (I 0), E Less, E (If [] [Push (I (-1)), E Mul])])

-- Good Examples --

-- Example 1: Deconstruct an integer into its digits.
-- run using 'run int2digit_example i2d_functions' or for custom arguments 'prog int2digit_example [I 2837] i2d_functions`
int2digit_example :: Prog
int2digit_example = [Push (I 235234)] ++ int2digit

int2digit :: Prog
int2digit = [Call "preprocessing",
                  S (While Less [Call "deconstruct"]),
                  Push (F "cleanup"), CallStackFunc]

i2d_functions :: [Func]
i2d_functions = [  ("preprocessing", [E Dup, Push (I 0)]),
               ("deconstruct", [E Dup, Push (I 10), E Mod, Swap, Push (I 10), Swap, E Div, E Dup, Push (I 0)]),
               ("cleanup", [Pop])
            ]
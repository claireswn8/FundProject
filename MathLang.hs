-- Team Members:
-- Hannah Vaughan
-- Cole Swanson
-- Melanie Gutzmann

module MathLang where


data Value = I Int
           | B Bool
           | T Value Value
           | C Cmd 
           | F FuncName
           | Counter
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
   deriving (Eq, Show)

data Stmt = While Expr Prog
          | Begin Prog
   deriving (Eq, Show)

data Cmd = Push Value
         | Pop
         | E Expr
         | S Stmt
         | Call FuncName
         | CallStackFunc
         | Swap
         | Set
   deriving (Eq, Show)

data Ref = RF Func | RC Int

type Stack = [Value]

type Prog = [Cmd]

type FuncName = String

type Func = (FuncName, [Cmd])

type Domain = Stack -> [Ref] -> Maybe (Stack, [Ref])

cmd :: Cmd -> Domain
cmd (Pop)     []     fs = Nothing
cmd (Pop)     (q:qs) fs = Just (qs, fs)
cmd (Push v)  q      fs = Just ((v : q), fs)
cmd (E e)     q      fs = expr e q fs
cmd (S s)     q      fs = case stmt s q fs of
                              Just (q', fs') -> Just(q', fs)
                              _              -> Nothing
cmd (Call fn) q      fs = case lookupFunc fn fs of 
                              Just cmds -> prog cmds q fs
                              _         -> Nothing
-- Allows functions to be passed from the stack to other functions.
cmd (CallStackFunc) (q:qs) fs = case q of 
                                    (F fn) -> case lookupFunc fn fs of
                                                   Just cmds -> prog cmds qs fs
                                                   _         -> Nothing
                                    _      -> Nothing
-- Swaps the position of the first two elements of the stack, if they exist
cmd (Swap)           q     fs = case q of
                                    []             -> Nothing
                                    [q1]           -> Nothing
                                    (q1 : q2 : qs) -> Just ((q2 : q1 : qs), fs)
cmd Set             (q:qs) fs = case q of
                                    I i           -> case fs of
                                                     []            -> Just (qs, [RC i])
                                                     _             -> Just (qs, (findSetC i fs))
                                    _             -> Nothing
cmd Set              []    fs = Nothing


safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

tupleDiv :: Value -> Value -> Maybe Value
tupleDiv (T a b) (T c d) = case (a, b, c, d) of
                              (_, I 0, _, I 0)     -> Nothing
                              (I a, I b, I c, I d) -> case (safeDiv a c, safeDiv b d) of
                                                         (Nothing, _)     -> Nothing
                                                         (_, Nothing)     -> Nothing
                                                         (Just x, Just y) -> Just (T (I x) (I y))
                              _                    -> Nothing
tupleDiv _        _      = Nothing

tupleEqu :: Value -> Value -> Bool
tupleEqu (T a b) (T c d) = case (a, b, c, d) of
                              (I a, I b, I c, I d) -> a == c && b == d
                              (B a, B b, B c, B d) -> a == c && b == d
                              (I a, B b, I c, B d) -> a == c && b == d
                              (B a, I b, B c, I d) -> a == c && b == d
                              _                    -> False
tupleEqu _       _       = False

tupleLess :: Value -> Value -> Bool
tupleLess (T a b) (T c d) = case (a, b, c, d) of
                              (I a, I b, I c, I d) -> a < c && b < d
                              _                    -> False
tupleLess _       _       = False


expr :: Expr -> Domain
expr Add q fs = case q of 
                  (Counter : qs)       -> expr Add ((lookupC fs): qs) fs
                  (a : Counter : qs)   -> expr Add (a:(lookupC fs):qs) fs 
                  (I i : I j : qs)     -> Just ((I (i + j) : qs),fs)
                  (C f : qs)           -> case (prog [f] qs fs) of 
                                             Just (q, fs)  -> expr Add q fs
                                             Nothing -> Nothing  
                  (a : C f : qs)       -> case (prog [f] qs fs) of 
                                             Just (q, fs)  -> expr Add (a : q) fs
                                             Nothing -> Nothing  
                  (T v w : T y z : qs) -> case (v, w, y, z) of
                                             (I v, I w, I y, I z) -> Just ((T (I (v + y)) (I (w + z)) : qs), fs)
                                             _                    -> Nothing
                  _                    -> Nothing
expr Mul q fs = case q of
                  (Counter : qs)       -> expr Mul ((lookupC fs): qs) fs
                  (a : Counter : qs)   -> expr Mul (a:(lookupC fs):qs) fs 
                  (I i : I j : qs)     -> Just ((I (i * j) : qs), fs)
                  (C f : qs)           -> case (prog [f] qs fs) of 
                                             Just (q, fs)  -> expr Mul q fs
                                             Nothing -> Nothing 
                  (a : C f : qs)       -> case (prog [f] qs fs) of 
                                             Just (q,fs)  -> expr Mul (a : q) fs
                                             Nothing -> Nothing   
                  (T v w : T y z : qs) -> case (v, w, y, z) of
                                             (I v, I w, I y, I z) -> Just ((T (I (v * y)) (I (w * z)) : qs), fs)
                                             _                    -> Nothing
                  _                    -> Nothing
expr Div q fs = case q of
                  (Counter : qs)       -> expr Div ((lookupC fs): qs) fs
                  (a : Counter : qs)   -> expr Div (a:(lookupC fs):qs) fs 
                  (I i : I j : qs)     -> case safeDiv i j of
                                             Just k -> Just ((I k : qs), fs)
                                             _        -> Nothing
                  (C f : qs)           -> case (prog [f] qs fs) of 
                                             Just (q, fs)  -> expr Div q fs
                                             Nothing -> Nothing
                  (a : C f : qs)       -> case (prog [f] qs fs) of 
                                             Just (q, fs)  -> expr Div (a : q) fs
                                             Nothing -> Nothing  
                  (T v w : T y z : qs) -> case tupleDiv (T v w) (T y z) of
                                             (Just (T a b)) -> Just ((T a b : qs), fs)
                                             _              -> Nothing
                  _                    -> Nothing
expr Equ q fs = case q of 
                  (Counter : qs)       -> expr Equ ((lookupC fs): qs) fs
                  (a : Counter : qs)   -> expr Equ (a:(lookupC fs):qs) fs 
                  (I i : I j : qs)     -> Just ((B (i == j) : qs), fs)
                  (B a : B b : qs)     -> Just ((B (a == b) : qs), fs)
                  (C f : qs)           -> case (prog [f] qs fs) of 
                                          Just (q, fs)  -> expr Equ q fs
                                          Nothing -> Nothing
                  (a : C f : qs)       -> case (prog [f] qs fs) of 
                                          Just (q, fs)  -> expr Equ (a : q) fs
                                          Nothing -> Nothing  
                  (T v w : T y z : qs) -> Just ((B (tupleEqu (T v w) (T y z)) : qs), fs)
                  _                    -> Nothing
expr Less q fs = case q of 
                     (Counter : qs)       -> expr Less ((lookupC fs): qs) fs
                     (a : Counter : qs)   -> expr Less (a:(lookupC fs):qs) fs 
                     (I i : I j : qs)     -> Just ((B (i < j) : qs), fs)
                     (C f : qs)           -> case (prog [f] qs fs) of 
                                             Just (q, fs)  -> expr Less q fs
                                             Nothing -> Nothing
                     (a : C f : qs)       -> case (prog [f] qs fs) of 
                                             Just (q, fs)  -> expr Less (a : q) fs
                                             Nothing -> Nothing  
                     (T v w : T y z : qs) -> Just ((B (tupleLess (T v w) (T y z)) : qs), fs) 
                     _                    -> Nothing                
expr (If t f) q fs = case q of
                        (B True  : qs) -> prog t qs fs
                        (B False : qs) -> prog f qs fs
                        (I n     : qs) -> if n > 0 then prog t qs fs else prog f qs fs
                        (C func  : qs) -> case (prog [func] qs fs) of
                                             Just (q, fs)  -> expr (If t f) q fs
                                             Nothing -> Nothing
                        _              -> Nothing 
-- Duplicates the top value on the stack.
expr (Dup) q fs = case q of 
                     []      -> Just ([], fs)
                     (v:qs)  -> Just ((v : v : qs), fs)
-- Represents a list of expressions to evaluate against the stack, in order.
expr (ExprList el) q fs = case el of 
                              []       -> Just (q, fs)
                              (e : es) -> case expr e q fs of
                                             Just (q2, fs) -> expr (ExprList es) q2 fs
                                             _       -> Nothing
-- Checks if the top two values on the stack are the same type. Does not consume the values.
expr (IsType) q fs = case q of 
                            []             -> Nothing
                            [v1]           -> Nothing
                            (v1 : v2 : vs) -> case (v1, v2) of
                                                   (I i1, I i2)       -> Just ((B True  : q), fs)
                                                   (B i1, B i2)       -> Just ((B True  : q), fs)
                                                   (T i1 i2, T i3 i4) -> Just ((B True  : q), fs)
                                                   (C i1, C i2)       -> Just ((B True  : q), fs)
                                                   (F i1, F i2)       -> Just ((B True  : q), fs)
                                                   _                  -> Just ((B False : q), fs)



stmt :: Stmt -> Domain
stmt (While e c)    q fs = case (expr e q fs) of 
                           (Just (((B True):qs), fs)) -> case (prog c qs fs) of
                                                      Just (q, fs) -> stmt (While e c) q fs
                                                      _      -> Nothing
                           (Just ((_:qs), fs))        -> Just (qs, fs)
                           _                    -> Nothing
stmt (Begin (c:cs)) q fs = case (cmd c q fs) of
                           Just (q, fs') -> stmt (Begin cs) q fs'
                           _      -> Nothing
stmt (Begin [])     q fs  = Just (q, fs)
 
-- Takes the name of a function and a list of functions, and returns the list of commands associated
-- with the function, if it exists. If the function doesn't exist, it returns Nothing.
lookupFunc :: FuncName -> [Ref] -> Maybe [Cmd]
lookupFunc fn []             = Nothing
lookupFunc fn (RF (n, cmds):fs) = if n == fn then Just cmds
                               else lookupFunc fn fs

findSetC :: Int -> [Ref] -> [Ref]
findSetC i []       = [RC i]
findSetC i (r:rs)   = case r of
                     RC j  -> (RC i) : rs
                     _     -> r : (findSetC i rs)

lookupC :: [Ref] -> Value
lookupC []         = I 0
lookupC (r:rs)   = case r of
                      RC i  -> I i
                      _    -> lookupC rs

prog :: Prog -> Domain
prog  []     q fs  = Just (q, fs)
prog  (c:cs) q fs = case cmd c q fs of
                        Just (q, fs') -> prog cs q fs'
                        _      -> Nothing

run :: Prog -> [Ref] -> Maybe Stack
run [] _ = Nothing
run c fs = case prog c [] fs of
         Just(q, fs)       -> Just q
         _                 -> Nothing

-- Syntactic Sugar --

true :: Cmd
true = Push (B True)

false :: Cmd
false = Push (B False)

greaterequ :: Expr
greaterequ = ExprList [Less, notl]

inc :: Cmd
inc = S (Begin [Push (I 1), E Add])

dec :: Cmd
dec = S (Begin [Push (I (-1)), E Add])

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

-- Library-level or possibly syntactic sugar?

factorial :: Cmd
factorial = S (Begin [Push (B False), Swap, E Dup, Push (I 2), 
            S (While Less [E Dup, Push (I 1), minus, absval, E Dup, Push (I 2)]), 
            S (While IsType [E Mul]), Swap, Pop ])

for :: Int -> [Cmd] -> Cmd
for i c = S (Begin [Push (I i), Set, Push Counter, Push (I 0), S (While Less [Push Counter, dec, Set, S (Begin c), Push Counter, Push(I 0)])])

summation :: Int -> Int -> [Cmd] -> Cmd
summation l h c = S(Begin [Push (I l), Set, Push (I (h+1)), Push (I l), S (While Less 
                   [Push Counter, S (Begin c), Push (I (h+1)), Push Counter, inc, Set, Push Counter]), 
                   for (h-l) [E Add] ])
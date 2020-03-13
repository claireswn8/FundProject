-- Team Members:
-- Hannah Vaughan
-- Cole Swanson
-- Melanie Gutzmann

module MathLang where

-- Our "Prelude", which contains library-level definitions
mathlude :: [Ref]
mathlude = [RF ("factorial", [S (Begin [Push (B False), Swap, E Dup, Push (I 2), 
            S (While Less [E Dup, Push (I 1), minus, absval, E Dup, Push (I 2)]), 
            S (While IsType [E Mul]), Swap, Pop ])]),
            RF ("percent", [Push (D 100), Swap, E Div, E Mul])
           ]

data Value = I Int
           | D Double
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
cmd (ExtractTuple _) []     fs  = Just ([], fs)
cmd (ExtractTuple n) (q:qs) fs  = case q of
                                    T v w -> case n of
                                                0 -> Just ((v : qs), fs)
                                                1 -> Just ((w : qs), fs)
                                                2 -> Just ((v : w : qs), fs)
                                                _ -> Nothing
                                    _     -> Nothing
cmd (S s)     q      fs = case stmt s q fs of
                              Just (q', fs') -> Just(q', fs)
                              _              -> Nothing
cmd (Call fn) q      fs = case lookupFunc fn fs of 
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
                                    (q1 : q2 : qs) -> Just ((q2 : q1 : qs), fs)
cmd Set             (q:qs) fs = case q of
                                    I i           -> case fs of
                                                     []            -> Just (qs, [RC i])
                                                     _             -> Just (qs, (findSetC i fs))
                                    _             -> Nothing
cmd Set              []    fs = Nothing


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
                  (Counter : qs)       -> expr Add ((lookupC fs): qs) fs
                  (a : Counter : qs)   -> expr Add (a:(lookupC fs):qs) fs 
                  (I i : I j : qs)     -> Just ((I (i + j) : qs), fs)
                  (D i : D j : qs)     -> Just ((D (i + j) : qs), fs)
                  (I i : [])           -> Just ([I i], fs)
                  (D i : [])           -> Just ([D i], fs)
                  (T v w : [])         -> case (v, w) of
                                         (I i, I j)           -> Just (([T (I i) (I j)]), fs)
                                         _                    -> Nothing
                  (C f : qs)           -> case (prog [f] qs fs) of 
                                             Just (q, fs)  -> expr Add q fs
                                             Nothing -> Nothing  
                  (a : C f : qs)       -> case (prog [f] qs fs) of 
                                             Just (q, fs)  -> expr Add (a : q) fs
                                             Nothing -> Nothing  
                  (T v w : T y z : qs) -> case (v, w, y, z) of
                                             (I v, I w, I y, I z) -> Just ((T (I (v + y)) (I (w + z)) : qs), fs)
                                             (D v, D w, D y, D z) -> Just ((T (D (v + y)) (D (w + z)) : qs), fs)
                                             (I v, D w, I y, D z) -> Just ((T (I (v + y)) (D (w + z)) : qs), fs)
                                             (D v, I w, D y, I z) -> Just ((T (D (v + y)) (I (w + z)) : qs), fs)
                                             _                    -> Nothing
                  _                    -> Nothing
expr Mul q fs = case q of
                  (Counter : qs)       -> expr Mul ((lookupC fs): qs) fs
                  (a : Counter : qs)   -> expr Mul (a:(lookupC fs):qs) fs 
                  (I i : I j : qs)     -> Just ((I (i * j) : qs), fs)
                  (D i : D j : qs)     -> Just ((D (i * j) : qs), fs)
                  (I i : [])           -> Just ([I 0], fs)
                  (D i : [])           -> Just ([D i], fs)
                  (T v w : [])         -> case (v, w) of
                                         (I i, I j)            -> Just ([I 0], fs)

                  (C f : qs)           -> case (prog [f] qs fs) of 
                                             Just (q, fs)  -> expr Mul q fs
                                             Nothing -> Nothing 
                  (a : C f : qs)       -> case (prog [f] qs fs) of 
                                             Just (q,fs)  -> expr Mul (a : q) fs
                                             Nothing -> Nothing   
                  (T v w : T y z : qs) -> case (v, w, y, z) of
                                             (I v, I w, I y, I z) -> Just ((T (I (v * y)) (I (w * z)) : qs), fs)
                                             (D v, D w, D y, D z) -> Just ((T (D (v * y)) (D (w * z)) : qs), fs)
                                             (I v, D w, I y, D z) -> Just ((T (I (v * y)) (D (w * z)) : qs), fs)
                                             (D v, I w, D y, I z) -> Just ((T (D (v * y)) (I (w * z)) : qs), fs)
                                             _                    -> Nothing
                  _                    -> Nothing
expr Div q fs = case q of
                  (I i : [])           -> Just ([I i], fs)
                  (D i : [])           -> Just ([D i], fs)
                  (Counter : qs)       -> expr Div ((lookupC fs): qs) fs
                  (a : Counter : qs)   -> expr Div (a:(lookupC fs):qs) fs 
                  (I i : I j : qs)     -> case safeDiv (I i) (I j) of
                                             Just k -> Just ((k : qs), fs)
                  (D i : D j : qs)     -> case safeDiv (D i) (D j) of
                                             Just k -> Just ((k : qs), fs)

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
                  (D i : D j : qs)     -> Just ((B (i == j) : qs), fs)
                  (B a : B b : qs)     -> Just ((B (a == b) : qs), fs)
                  (I i : [])           -> Just ([B (i == 0)], fs)
                  (D i : [])           -> Just ([B (i == 0)], fs)
                  (B b : [])           -> Just ([B (b == False)], fs) 
                  (T a b : [])         -> case (a, b) of
                                          (I a, I b)     -> Just ([B (a == 0 && b == 0)], fs)
                                          (D a, D b)     -> Just ([B (a == 0 && b == 0)], fs)
                                          (B a, B b)     -> Just ([B (a == False && b == False)], fs)
                                          (I a, B b)     -> Just ([B (a == 0 && b == False)], fs)
                                          (B a, I b)     -> Just ([B (a == False && b == 0 )], fs)
                                          (D a, B b)     -> Just ([B (a == 0 && b == False)], fs)
                                          (B a, D b)     -> Just ([B (a == False && b == 0)], fs)
                                          (I a, D b)     -> Just ([B (a == 0 && b == 0)], fs)
                                          (D a, I b)     -> Just ([B (a == 0 && b == 0)], fs)
                                          (T v w, T y z) -> Just ([B (tupleEqu (T v w) (T y z))], fs)
                                          _              -> Nothing
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
                     (D i : D j : qs)     -> Just ((B (i < j) : qs), fs)
                     (I i : [])           -> Just ([B (i < 0)], fs)
                     (D i : [])           -> Just ([B (i < 0)], fs)
                     (T v w : [])         -> Just ([B (tupleLess (T v w) (T (I 0) (I 0)))], fs)
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
                        (D n     : qs) -> if n > 0 then prog t qs fs else prog f qs fs
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
                                                   (D i1, D i2)       -> Just ((B True  : q), fs)
                                                   (B i1, B i2)       -> Just ((B True  : q), fs)
                                                   (T i1 i2, T i3 i4) -> Just ((B True  : q), fs)
                                                   (C i1, C i2)       -> Just ((B True  : q), fs)
                                                   (F i1, F i2)       -> Just ((B True  : q), fs)
                                                   _                  -> Just ((B False : q), fs)
expr (Mod) q fs = case q of 
                        (I i : [])       -> Just ([I (1 `mod` i)], fs)
                        (I i : I j : qs) -> Just ((I (j `mod` i) : qs), fs)
                        _                -> Nothing

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
run [] fs = case prog [] [] (fs ++ mathlude) of
         Just(q, fs)       -> Just q
         _                 -> Nothing
run c fs = case prog c [] (fs++mathlude) of
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

for :: Int -> [Cmd] -> Cmd
for i c = S (Begin [Push (I i), Set, Push Counter, Push (I 0), S (While Less [Push Counter, dec, Set, S (Begin c), Push Counter, Push(I 0)])])

summation :: Int -> Int -> [Cmd] -> Cmd
summation l h c = S(Begin [Push (I l), Set, Push (I (h+1)), Push (I l), S (While Less 
                   [Push Counter, S (Begin c), Push (I (h+1)), Push Counter, inc, Set, Push Counter]), 
                   for (h-l) [E Add] ])

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
            
-- Bad Examples --

-- This program is an example of a mismatched types error in MathLang.
-- The I 10 and B False parameters on the stack cannot be added by the Add expression, 
-- and the program returns an error value of Nothing. 
mismatchtype :: Prog
mismatchtype = [Push (I 10), Push(B False), E Add]
-- Expected Output: Nothing

-- This program is an example of a mismatched types error with tuples in MathLang.
-- The I 10 and B False parameters on the stack cannot be added by the Add expression, 
-- and the program returns an error value of Nothing. 
tuplemismatchtype :: Prog
tuplemismatchtype = [Push (T (I 10) (I 20)), Push (T (B True) (I 10)), E Add]
-- Expected Output: Nothing

-- This program is an example of an invalid argument to ExtractTuple error in MathLang.
-- ExtractTuple can only accept integer values from 0 to 2, and values outside of this range will return an
-- error value of Nothing. 
invalidextracttuple :: Prog
invalidextracttuple = [Push (T (I 10) (I 20)), ExtractTuple 5]
-- Expected Output: Nothing

-- This program is an example of a missing function definition error in MathLang.
-- Named functions in MathLang must be defined before the program is run, and only defined functions can
-- be called when running a program. The "badfunc" function was not defined on program start, and the program
-- returns an error value of Nothing.   
missingfunctiondefinition :: Prog
missingfunctiondefinition = [Call "badfunc"]
-- Expected Output: Nothing

-- This program is an example of a divide by zero error in MathLang.
-- Division in MathLang expects a non-zero divisor, and if a divisor is equal to zero the
-- program will return an error value of Nothing. 
dividebyzero :: Prog
dividebyzero = [Push (I 0), Push (I 10), E Div]
-- Expected Output: Nothing

-- This program is an example of an empty stack pop error in MathLang.
-- Popping from the stack in MathLang expects a value on the stack. a program 
-- attempting to pop an empty stack returns an error value of Nothing.  
emptystackpop :: Prog
emptystackpop = [Pop]
-- Expected Output: Nothing

-- This program is an example of a not enough arguments error in MathLang.
-- The Swap and IsType operations on the stack that require two items on the stack will
-- return an error value of Nothing if there are not enough items on the stack. 
-- Note that this behavior only occurs for Swap and IsType, while expressions such as Add will 
-- generate default arguments. 
notenoughargumentsswap :: Prog
notenoughargumentsswap = [Push (I 10), Swap]
-- Expected Output: Nothing
notenoughargumentsistype :: Prog
notenoughargumentsistype = [Push (I 10), Swap]





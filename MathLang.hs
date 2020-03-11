-- Team Members:
-- Hannah Vaughan
-- Cole Swanson
-- Melanie Gutzmann

module MathLang where


data Value = I Int
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
                  (I i : [])           -> Just ([I i])
                  (T v w : [])         -> case (v, w) of
                                         (I i, I j)           -> Just ([T (I i) (I j)])
                                         _                    -> Nothing
                  (I i : I j : qs)     -> Just (I (i + j) : qs)
                  (F f : qs)           -> case (prog [f] qs fs) of 
                                             Just q  -> expr Add q fs
                                             Nothing -> Nothing  
                  (a : F f : qs)       -> case (prog [f] qs fs) of 
                                             Just q  -> expr Add (a : q) fs
                                             Nothing -> Nothing  
                  (T v w : T y z : qs) -> case (v, w, y, z) of
                                             (I v, I w, I y, I z) -> Just (T (I (v + y)) (I (w + z)) : qs)
                                             _                    -> Nothing
                  _                    -> Nothing
expr Mul q fs = case q of
                  (I i : [])           -> Just ([I 0])
                  (T v w : [])         -> case (v, w) of
                                         (I i, I j)            -> Just ([I 0])
                  (I i : I j : qs)     -> Just (I (i * j) : qs)
                  (F f : qs)           -> case (prog [f] qs fs) of 
                                             Just q  -> expr Mul q fs
                                             Nothing -> Nothing 
                  (a : F f : qs)       -> case (prog [f] qs fs) of 
                                             Just q  -> expr Mul (a : q) fs
                                             Nothing -> Nothing   
                  (T v w : T y z : qs) -> case (v, w, y, z) of
                                             (I v, I w, I y, I z) -> Just (T (I (v * y)) (I (w * z)) : qs)
                                             _                    -> Nothing
                  _                    -> Nothing
expr Div q fs = case q of
                  (I i : I j : qs)     -> case safeDiv i j of
                                             (Just k) -> Just (I k : qs)
                                             _        -> Nothing
                  (F f : qs)           -> case (prog [f] qs fs) of 
                                             Just q  -> expr Div q fs
                                             Nothing -> Nothing
                  (a : F f : qs)       -> case (prog [f] qs fs) of 
                                             Just q  -> expr Div (a : q) fs
                                             Nothing -> Nothing  
                  (T v w : T y z : qs) -> case tupleDiv (T v w) (T y z) of
                                             (Just (T a b)) -> Just (T a b : qs)
                                             _              -> Nothing
                  _                    -> Nothing
expr Equ q fs = case q of 
                  (I i : [])           -> Just ([B (i == 0)])
                  (B b : [])           -> Just ([B (b == False)]) 
                  (T a b : [])         -> case (a, b) of
                                          (I a, I b) -> Just ([B (a == 0 && b == 0)])
                                          (B a, B b) -> Just ([B (a == False && b == False)])
                                          (I a, B b) -> Just ([B (a == 0 && b == False)])
                                          (B a, I b) -> Just ([B (a == False && b == 0)])
                  (I i : I j : qs)     -> Just (B (i == j) : qs)
                  (B a : B b : qs)     -> Just (B (a == b) : qs)
                  (F f : qs)           -> case (prog [f] qs fs) of 
                                          Just q  -> expr Equ q fs
                                          Nothing -> Nothing
                  (a : F f : qs)       -> case (prog [f] qs fs) of 
                                          Just q  -> expr Equ (a : q) fs
                                          Nothing -> Nothing  
                  (T v w : T y z : qs) -> Just (B (tupleEqu (T v w) (T y z)) : qs)
                  _                    -> Nothing
expr Less q fs = case q of 
                     (I i : [])           -> Just ([B (i < 0)])
                     (T v w : [])         -> Just ([B (tupleLess (T v w) (T (I 0) (I 0)))])
                     (I i : I j : qs)     -> Just (B (i < j) : qs)
                     (F f : qs)           -> case (prog [f] qs fs) of 
                                             Just q  -> expr Less q fs
                                             Nothing -> Nothing
                     (a : F f : qs)       -> case (prog [f] qs fs) of 
                                             Just q  -> expr Less (a : q) fs
                                             Nothing -> Nothing  
                     (T v w : T y z : qs) -> Just (B (tupleLess (T v w) (T y z)) : qs) 
                     _                    -> Nothing                
expr (If t f) q fs = case q of
                        (B True  : qs) -> prog t qs fs
                        (B False : qs) -> prog f qs fs
                        (I n     : qs) -> if n > 0 then prog t qs fs else prog f qs fs
                        (F func  : qs) -> case (prog [func] qs fs) of
                                             Just q  -> expr (If t f) q fs
                                             Nothing -> Nothing
                        _              -> Nothing 

stmt :: Stmt -> Domain
stmt (While e c)    q fs = case (expr e q fs) of 
                           (Just ((B True):qs)) -> case (cmd c qs fs) of
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

-- Syntactic Sugar --

true :: Cmd
true = Push (B True)

false :: Cmd
false = Push (B True)

greaterequ :: Cmd
greaterequ = S (Begin [E Less, notl])

inc :: Cmd
inc = S (Begin [Push (I 1), E Add])

dec :: Cmd
dec = S (Begin [Push (I (-1)), E Add])

notl :: Cmd
notl = E (If [Push (B False)] [Push (B True)])

andl :: Cmd
andl = E (If [E (If [true] [false])] [E (If [false] [false])]) 

orl :: Cmd
orl = E (If [E (If [true] [true])] [E (If [true] [false])]) 
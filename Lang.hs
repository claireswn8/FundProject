-- Team Members:
-- Hannah Vaughan
-- Cole Swanson
-- Melanie Gutzmann

module StackLang where


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
          | Call FuncName
   deriving (Eq, Show)

data Stmt = While Expr Cmd
          | Begin Prog
   deriving (Eq, Show)

data Cmd = Push Value
         | Pop
         | E Expr
         | S Stmt
   deriving (Eq, Show)

type Stack = [Value]

type Prog = [Cmd]

type FuncName = String

-- functions can take parameters simply by calling functions like 'pop' on the stack,
-- which will return the first value to the calling function
type Func = (FuncName, [Cmd])

type Domain = Stack -> Maybe Stack

cmd :: [Func] -> Cmd -> Stack -> (Maybe Stack, Maybe Value)
cmd _  (Pop) []     = (Nothing, Nothing)
cmd _  (Pop) (q:qs) = (Just qs, Just q)
cmd _  (Push v)  q  = (Just (v : q), Nothing)
cmd fs (E e)     q  = (expr fs e q, Nothing)
cmd fs (S s)     q  = (stmt fs s q, Nothing)



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


expr :: [Func] -> Expr -> Domain
expr fs Add q = case q of 
                (I i   : I j   : qs) -> Just (I (i + j) : qs)
                ( F f : qs )         -> case (prog fs [f] qs) of 
                                        Just q  -> expr fs Add q
                                        Nothing -> Nothing  
                (a : F f : qs )      -> case (prog fs [f] qs) of 
                                        Just q  -> expr fs Add (a : q)
                                        Nothing -> Nothing  
                (T v w : T y z : qs) -> case (v, w, y, z) of
                                          (I v, I w, I y, I z) -> Just (T (I (v + y)) (I (w + z)) : qs)
                                          _                    -> Nothing
                _                    -> Nothing
expr fs Mul q = case q of
                (I i   : I j   : qs) -> Just (I (i * j) : qs)
                ( F f : qs )         -> case (prog fs [f] qs) of 
                                        Just q  -> expr fs Mul q
                                        Nothing -> Nothing 
                (a : F f : qs )      -> case (prog fs [f] qs) of 
                                        Just q  -> expr fs Mul (a : q)
                                        Nothing -> Nothing   
                (T v w : T y z : qs) -> case (v, w, y, z) of
                                          (I v, I w, I y, I z) -> Just (T (I (v * y)) (I (w * z)) : qs)
                                          _                    -> Nothing
                _                    -> Nothing
expr fs Div q = case q of
               (I i   : I j   : qs) -> case safeDiv i j of
                                       (Just k) -> Just (I k : qs)
                                       _        -> Nothing
               ( F f : qs )         -> case (prog fs [f] qs) of 
                                       Just q  -> expr fs Div q
                                       Nothing -> Nothing
               (a : F f : qs )      -> case (prog fs [f] qs) of 
                                        Just q  -> expr fs Div (a : q)
                                        Nothing -> Nothing  
               (T v w : T y z : qs) -> case tupleDiv (T v w) (T y z) of
                                          (Just (T a b)) -> Just (T a b : qs)
                                          _              -> Nothing
               _                    -> Nothing
expr fs Equ q = case q of 
               (I i   : I j   : qs) -> Just (B (i == j) : qs)
               (B a   : B b   : qs) -> Just (B (a == b) : qs)
               ( F f : qs )         -> case (prog fs [f] qs) of 
                                       Just q  -> expr fs Equ q
                                       Nothing -> Nothing
               (a : F f : qs )      -> case (prog fs [f] qs) of 
                                        Just q  -> expr fs Equ (a : q)
                                        Nothing -> Nothing  
               (T v w : T y z : qs) -> Just (B (tupleEqu (T v w) (T y z)) : qs)
               _                    -> Nothing
expr fs Less q = case q of 
               (I i   : I j   : qs) -> Just (B (i < j) : qs)
               ( F f : qs )         -> case (prog fs [f] qs) of 
                                       Just q  -> expr fs Less q
                                       Nothing -> Nothing
               (a : F f : qs )      -> case (prog fs [f] qs) of 
                                        Just q  -> expr fs Less (a : q)
                                        Nothing -> Nothing  
               (T v w : T y z : qs) -> Just (B (tupleLess (T v w) (T y z)) : qs) 
               _                    -> Nothing                
expr fs (If t f) q = case q of
                  (B True : qs)  -> prog fs t qs
                  (B False : qs) -> prog fs f qs
                  (F func : qs)  -> case (prog fs [func] qs) of
                                    Just q  -> expr fs (If t f) q
                                    Nothing -> Nothing
                  _              -> Nothing 
expr fs (Call fn) q = case lookupFunc fn fs of 
                              Just cmds -> prog fs cmds q
                              _         -> Nothing

stmt :: [Func] -> Stmt -> Domain
stmt fs (While e c) q = case (expr fs e q) of 
                     (Just ((B True):qs)) -> case (cmd fs c qs) of
                                             (Just q, _ ) -> stmt fs (While e c) q
                                             _        -> Nothing
                     (Just (_:qs))        -> Just (qs)
                     _                    -> Nothing
stmt fs (Begin (c:cs)) q = case (cmd fs c q) of
                           (Just q, _) -> stmt fs (Begin cs) q
                           _        -> Nothing
stmt _ (Begin []) q = Just q
 
-- Takes the name of a function and a list of functions, and returns the list of commands associated
-- with the function, if it exists. If the function doesn't exist, it returns Nothing.
lookupFunc :: FuncName -> [Func] -> Maybe [Cmd]
lookupFunc fn []             = Nothing
lookupFunc fn ((n, cmds):fs) = if n == fn then Just cmds
                               else lookupFunc fn fs

prog :: [Func] -> Prog -> Domain
prog  _     []  q  = Just q
prog  fs (c:cs) q  = case cmd fs c q of
                              (Just q, _) -> prog fs cs q 
                              _           -> Nothing

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
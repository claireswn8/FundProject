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
   deriving (Eq, Show)

data Stmt = While Expr Cmd
          | Begin Prog
   deriving (Eq, Show)

data Cmd = Push Value
         | E Expr
         | S Stmt
   deriving (Eq, Show)

type Stack = [Value]

type Prog = [Cmd]

type Domain = Stack -> Maybe Stack

cmd :: Cmd -> Domain
cmd (Push v) q = Just (v : q)
cmd (E e)    q = expr e q
cmd (S s)    q = stmt s q

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
expr Add q = case q of 
                (I i   : I j   : qs) -> Just (I (i + j) : qs)
                ( F f : qs )         -> case (prog [f] qs) of 
                                        Just q  -> expr Add q
                                        Nothing -> Nothing  
                (a : F f : qs )      -> case (prog [f] qs) of 
                                        Just q  -> expr Add (a : q)
                                        Nothing -> Nothing  
                (T v w : T y z : qs) -> case (v, w, y, z) of
                                          (I v, I w, I y, I z) -> Just (T (I (v + y)) (I (w + z)) : qs)
                                          _                    -> Nothing
                _                    -> Nothing
expr Mul q = case q of
                (I i   : I j   : qs) -> Just (I (i * j) : qs)
                ( F f : qs )         -> case (prog [f] qs) of 
                                        Just q  -> expr Mul q
                                        Nothing -> Nothing 
                (a : F f : qs )      -> case (prog [f] qs) of 
                                        Just q  -> expr Mul (a : q)
                                        Nothing -> Nothing   
                (T v w : T y z : qs) -> case (v, w, y, z) of
                                          (I v, I w, I y, I z) -> Just (T (I (v * y)) (I (w * z)) : qs)
                                          _                    -> Nothing
                _                    -> Nothing
expr Div q = case q of
               (I i   : I j   : qs) -> case safeDiv i j of
                                       (Just k) -> Just (I k : qs)
                                       _        -> Nothing
               ( F f : qs )         -> case (prog [f] qs) of 
                                       Just q  -> expr Div q
                                       Nothing -> Nothing
               (a : F f : qs )      -> case (prog [f] qs) of 
                                        Just q  -> expr Div (a : q)
                                        Nothing -> Nothing  
               (T v w : T y z : qs) -> case tupleDiv (T v w) (T y z) of
                                          (Just (T a b)) -> Just (T a b : qs)
                                          _              -> Nothing
               _                    -> Nothing
expr Equ q = case q of 
               (I i   : I j   : qs) -> Just (B (i == j) : qs)
               (B a   : B b   : qs) -> Just (B (a == b) : qs)
               ( F f : qs )         -> case (prog [f] qs) of 
                                       Just q  -> expr Equ q
                                       Nothing -> Nothing
               (a : F f : qs )      -> case (prog [f] qs) of 
                                        Just q  -> expr Equ (a : q)
                                        Nothing -> Nothing  
               (T v w : T y z : qs) -> Just (B (tupleEqu (T v w) (T y z)) : qs)
               _                    -> Nothing
expr Less q = case q of 
               (I i   : I j   : qs) -> Just (B (i < j) : qs)
               ( F f : qs )         -> case (prog [f] qs) of 
                                       Just q  -> expr Less q
                                       Nothing -> Nothing
               (a : F f : qs )      -> case (prog [f] qs) of 
                                        Just q  -> expr Less (a : q)
                                        Nothing -> Nothing  
               (T v w : T y z : qs) -> Just (B (tupleLess (T v w) (T y z)) : qs) 
               _                    -> Nothing                
expr (If t f) q = case q of
                  (B True : qs)  -> prog t qs
                  (B False : qs) -> prog f qs
                  (F func : qs)  -> case (prog [func] qs) of
                                    Just q  -> expr (If t f) q
                                    Nothing -> Nothing
                  _              -> Nothing 

stmt :: Stmt -> Domain
stmt (While e c) q = case (expr e q) of 
                     (Just ((B True):qs)) -> case (cmd c qs) of
                                             (Just q) -> stmt (While e c) q
                                             _        -> Nothing
                     (Just (_:qs))        -> Just (qs)
                     _                    -> Nothing
stmt (Begin (c:cs)) q = case (cmd c q) of
                           (Just q) -> stmt (Begin cs) q
                           _        -> Nothing
stmt (Begin []) q = Just q
 


prog :: Prog -> Domain
prog [] q      = Just q
prog (c:cs) q  = case cmd c q of
                     (Just q) -> prog cs q 
                     _        -> Nothing


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
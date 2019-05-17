module Direct where

type Var = String
data Op = Add | Sub | Mul | Mod | Leq deriving (Eq, Show)

data Type = TInt
          | TBool
          | Fun Type Type
        deriving (Eq, Show)


data Lambda = Abs Var Type Lambda
            | App Lambda Lambda
            | Ref Var
            | Oprt Op Lambda Lambda
            | LInt Int
            | LBool Bool
            | Fix Lambda
            | If Lambda Lambda Lambda
         deriving (Eq, Show)

-- | Typing Environment:

type Env a = [(Var, a)]


typecheck :: Env Type -> Lambda -> Maybe Type
typecheck env (LInt a)
    = Just TInt

typecheck env (LBool b)
    = Just TBool

typecheck env (Abs v t e)
    = case (typecheck ((v, t):env) e) of
        Nothing  -> Nothing
        Just et  -> (Just . Fun t) et

typecheck env (Ref v)
    = lookup v env

typecheck env (App l r)
    = case t1 of
        Nothing          -> Nothing
        Just (Fun a b)   -> if (Just a) == t2 then Just b else Nothing
    where
      t1 = typecheck env l
      t2 = typecheck env r

typecheck env (Fix e1)
    = case t of
        Nothing -> Nothing
        Just (Fun a b) -> if a == b then Just a else Nothing 
    where
      t = typecheck env e1

typecheck env (Oprt op l r)
    = case op of
      Add -> case (typecheck env l, typecheck env r) of
               (Just TInt, Just TInt) -> Just TInt
               _                      -> Nothing
      Sub -> case (typecheck env l, typecheck env r) of
               (Just TInt, Just TInt) -> Just TInt
               _                      -> Nothing
      Mul -> case (typecheck env l, typecheck env r) of
               (Just TInt, Just TInt) -> Just TInt
               _                      -> Nothing
      Mod -> case (typecheck env l, typecheck env r) of
               (Just TInt, Just TInt) -> Just TInt
               _                      -> Nothing
      Leq -> case (typecheck env l, typecheck env r) of
               (Just TInt, Just TInt) -> Just TBool
               _                      -> Nothing

typecheck env (If tcond tthen telse)
    = let tc = typecheck env in
        case (tc tcond, tc tthen, tc telse) of
            (Just TBool, tp1, tp2) -> if tp1 == tp2 then tp2 else Nothing
            _ -> Nothing




-- | Testcase Lambdaessions:

-- testEnv = []

testEnv :: Env Type
testEnv =
    [("two", TInt)
    ,("odd", Fun TInt TBool)
    ,("add", Fun TInt (Fun TInt TInt))]

runTest :: Lambda -> Maybe Type -> Bool
runTest e mt = typecheck testEnv e == mt



-- Good cases!

ex0 = Ref "x"

ex1, ex2, ex3 :: Lambda
ex1 = App (Ref "odd") (Ref "two")
ex2 = Abs "x" TInt (App (App (Ref "add") (Ref "two")) (Ref "x"))
aoo = App (Abs "x" TInt  (App (Ref "x") (Ref "y"))) (Ref "y")
ex3 = App ex2 (Ref "two")

ty1, ty2, ty3 :: Type
ty1 = TBool
ty2 = Fun TInt TInt
ty3 = TInt

good = zipWith runTest [ex1,ex2,ex3] (map Just [ty1,ty2,ty3])


-- Bad cases!

ex4, ex5, ex6 :: Lambda
ex4 = App (Ref "even") (Ref "two")
ex5 = App (Ref "add") ex1
ex6 = App (Ref "odd") ex2

bad = map (\e -> runTest e Nothing) [ex4,ex5,ex6]



-- | Evaluation function. Part of the runtime evaluation of
--   lambda calculus involves the creation of closures, environments
--   which hold the local variables in scope. Here there are
--   two possible values which reduction may converge on, VInt and VClosure.

data Value = VInt Int
           | VBool Bool
           | VClosure (Env Value) Var Lambda
        deriving (Eq, Show)
{-
instance Show Value where
  show (VInt a)
    = show a
  show (VBool b)
    = show b
  show (VClosure env x e)
    = "(\\" ++ x ++ ". " ++ show e ++ ")"
-}


-- | The evaluator function simply maps the local scope and a term
--   to the final value. Whenever a variable is referred to it is looked up
--   in the environment. Whenever a lambda is entered it extends
--   the environment with the local scope of the closure.

eval :: Env Value -> Lambda -> Value
eval env (Ref n)         = case lookup n env of
                             Just x  -> x
                             Nothing -> error ("unbound variable:" ++ n)
eval env (Abs x _ e)    = VClosure env x e
eval env (App l r)
  = case (eval env l, eval env r) of
      (VClosure env' x e, v) -> eval ((x,v):env') e

eval env (LInt x)  = VInt x
eval env (LBool x) = VBool x

eval env (Oprt op l r)
  = case op of
      Add -> case (eval env l, eval env r) of
               (VInt i, VInt j) -> VInt (i + j)
      Sub -> case (eval env l, eval env r) of
               (VInt i, VInt j) -> VInt (i - j)
      Mul -> case (eval env l, eval env r) of
               (VInt i, VInt j) -> VInt (i * j)
      Mod -> case (eval env l, eval env r) of
               (VInt i, VInt j) -> VInt (mod i j)
      Leq -> case (eval env l, eval env r) of
               (VInt i, VInt j) -> VBool (i <= j)

eval env (If test' cond1 cond2)
  = case (eval env test') of
      VBool b -> if b then (eval env cond1) else (eval env cond2)

eval env (Fix e) = eval env (App e (Fix e))

someFun = typecheck []

doAll :: Lambda -> Maybe Value
doAll l = case someFun l of
            Just t  -> Just (eval [] l)
            Nothing -> Nothing


-- factorial
fact' :: Lambda
fact' = Abs "f" (Fun TInt TInt) (Abs "n" TInt (If (Oprt Leq (Ref "n") (LInt 0))(LInt 1)(Oprt Mul (Ref "n")(App (Ref "f")(Oprt Sub (Ref "n")(LInt 1))))))

fix :: Lambda
fix = App (Fix fact') (LInt 5)

-- |  >>> doAll ex'
--  Just (VInt 2)
ex' :: Lambda
ex' = App (Abs "x" TInt (Ref "x")) (LInt 2)

-- |  >>> doAll even'
--  Just (VBool True)
even' :: Lambda
even' = App (Abs "x" TInt (If (Oprt Leq (Oprt Mod (Ref "x") (LInt 2)) (LInt 0))
          (LBool True) (LBool False))) (LInt 8)

-- |  >>> doAll exNot
--  Just (VBool False)
exnotB :: Lambda
exnotB = Abs "x" TBool (If (Ref "x") (LBool False) (LBool True))
exNot :: Lambda
exNot = App exnotB (LBool True)

-- |  >>> doAll exOr
--  Just (VBool False)
exId :: Lambda
exId = Abs "y" TBool (If (Ref "x")(LBool True)(Ref "y"))
exorB :: Lambda
exorB = Abs "x" TBool (exId)
exOr :: Lambda
exOr = App (App (exorB) (LBool False))(LBool False)

-- |  >>> doAll exc
--  Nothing
exc :: Lambda
exc = App (Abs "x" TInt (Ref "y")) (LInt 1)

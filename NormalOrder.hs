-- | operation in Normal Order Evaluation
--   limitation: Do not support renaming for free variable 

module NormalOrder where 

import qualified Data.Map as Map
import Control.Monad.Writer
import Syntax 
import PrettyPrint
import Data.List

-- | Evaluate an expression to normal form using normal order evaluation.
--   Note that this function will not terminate if the reduction never
--   reaches a normal form!
eval :: Expr -> Expr
eval e = case step' [] e of
           Nothing -> e
           Just e' -> eval e'

-- | Do one step normal order reduction and return the result expr
--   
--   >>> step [] (App (Abs "x" (App (Ref "x") (Ref "x") )) ( App (Abs "y" (Ref "y")) (Ref "z")))
--   Just (App (App (Abs "y" (Ref "y")) (Ref "z")) (App (Abs "y" (Ref "y")) (Ref "z")))
--
--   >>> step [] (App (App (Abs "y" (Ref "y")) (Ref "z")) (App (Abs "y" (Ref "y")) (Ref "z")))
--   Just (App (Ref "z") (App (Abs "y" (Ref "y")) (Ref "z")))
--   
--   >>> step [] ((App (Ref "z") (App (Abs "y" (Ref "y")) (Ref "z"))))
--   Just (App (Ref "z") (Ref "z"))
-- 
--   >>> step [] (App (Abs "x" (App (Ref "x") (Ref "y"))) (Ref "y"))
--   Just (App (Ref "y") (Ref "y"))
-- 
--   >>> step [] (App ( Abs "y" (Abs "x" (App (Ref "x") (Ref "y"))) )(Ref "w"))
--   Just (Abs "x" (App (Ref "x") (Ref "w")))

step' :: EvalScope -> Expr -> Maybe Expr 
step' env (App (Abs x e) r) = Just $ sub ((x,r):env) e 
step' env (App l r) = case step' env l of 
                                Just l' -> Just (App l' r)
                                Nothing -> fmap (App l) (step' env r)
step' env expr@(Ref x)   = Nothing
step' env (Abs x e) = fmap (Abs x) (step' env e) 

-- | perfrom one step reduction 
step :: Expr ->  Expr 
step e = case step' [] e of
           Nothing -> e
           Just e' -> e'

-- | Variable substitution based EvalScope 
sub :: EvalScope -> Expr -> Expr
sub env expr@(Ref x) = case lookup x env of 
                            Just ev -> ev 
                            Nothing -> expr
sub env epxr@(Abs x e) = Abs x (sub env e)   
sub env (App l r) = App (sub env l) (sub env r)                         


-- 
--  * evaluate the lambda expression 
--    and keep track of each result of reduction and corresponding redex
--

evalWithLogs :: Expr -> Writer Logs Expr  
evalWithLogs e = case stepWithRedex [] e of   
                    (Just e',log) -> do 
                        tell [(e, (head log))]
                        evalWithLogs e'
                    _            -> do
                        return e
                        
                    -- (_, [])       -> do 
                    --     return e
                    -- (Nothing,_) -> do 
                    --     return e 


stepWithRedex :: EvalScope -> Expr -> (Maybe Expr, [Redex]) 
stepWithRedex env expr@(App (Abs x e) r) = let res  = Just $ sub ((x,r):env) e 
                                               red = [expr]
                                           in (res, red)
stepWithRedex env expr@(Ref x)   = (Nothing, [])
stepWithRedex env (Abs x e) = let (e', red) = stepWithRedex env e 
                            in  (fmap (Abs x) e', red)
stepWithRedex env (App l r) = case stepWithRedex env l of 
                                (Just l',red) -> (Just (App l' r), red )
                                (Nothing,_) -> let (r', red) = stepWithRedex env r
                                               in ( fmap (App l) r', red)
                                  -- liftM ??

-- 
-- capture-avoiding substution 
--

-- | rename variable to avoid variable capture problem
rename :: Expr -> Expr
rename = subv []

-- | substitude the bound variables for avoiding variable
subv :: RenameMapping -> Expr -> Expr
subv m expr@(App (Abs x e) r) = let fv = free r
                                    m' = zip fv (zipWith (++) fv (map show [1..(length fv)] ))
                                    e' = subv m' e
                                in case lookup x m' of
                                  Just nx -> (App (Abs nx e') r)
                                  Nothing -> (App (Abs x e') r)
subv m expr@(Ref x) = case lookup x m of 
                            Just nv -> Ref nv 
                            Nothing -> expr
subv m (Abs x e) = case lookup x m of 
                            Just nv -> Abs nv (subv m e)  
                            Nothing ->  Abs x (subv m e)  
subv m (App l r) = App (subv m l) (subv m r)   

-- Given an expression e, the following rules define FV(e), the set of free variables in e:
-- If e is a variable x, then FV(e) = {x}.
-- If e is of the form λx.y, then FV(e) = FV(y) - {x}.
-- If e is of the form xy, then FV(e) = FV(x) ∪ FV(y)
--  get all the free variable in the expression
free :: Expr -> [Var] 
free (Ref x)   = [x]
free (Abs x y) = delete x (free y) 
free (App x y) = nub (free x ++ free y)


-- | evaluate a lambda expression by using normal order 
evalLambda :: Expr -> IO ()
evalLambda expr = let expr' = rename expr
                  in case expr' == expr  of  
                    True -> printWriter $ evalWithLogs expr 
                    False -> do 
                       putStrLn $ prettyExpr expr 
                       putStrLn $ "=" ++ (prettyEval $ evalWithLogs $ rename expr)



evaltest = evalWithLogs (App (Abs "x" (App (Ref "x") (Ref "x") )) ( App (Abs "y" (Ref "y")) (Ref "z")))
logs = snd $ runWriter $ evalWithLogs (App (Abs "x" (App (Ref "x") (Ref "x") )) ( App (Abs "y" (Ref "y")) (Ref "z")))
results = fst $ runWriter $ evalWithLogs (App (Abs "x" (App (Ref "x") (Ref "x") )) ( App (Abs "y" (Ref "y")) (Ref "z")))

-- | operation in Normal Order Evaluation
--   limitation: Do not support renaming for free variable 

module NormalOrder where 

import qualified Data.Map as Map
import Control.Monad.Writer
import Syntax 
-- import Control.Monad.State

-- | Evaluate an expression to normal form using normal order evaluation.
--   Note that this function will not terminate if the reduction never
--   reaches a normal form!
eval :: Expr -> Expr
eval e = case step [] e of
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

step :: EvalScope -> Expr -> Maybe Expr 
step env (App (Abs x e) r) = Just $ sub ((x,r):env) e 
step env (App l r) = case step env l of 
                                Just l' -> Just (App l' r)
                                Nothing -> fmap (App l) (step env r)
step env expr@(Ref x)   = Nothing
step env (Abs x e) = fmap (Abs x) (step env e) 

-- | Variable substitution based EvalScope 
sub :: EvalScope -> Expr -> Expr
sub env expr@(Ref x) = case lookup x env of 
                            Just ev -> ev 
                            Nothing -> expr
sub env epxr@(Abs x e) = Abs x (sub env e)   
sub env (App l r) = App (sub env l) (sub env r)                         


-- | Trace the corresponding expr and redux in that reduction
-- stepWithLog :: EvalScope -> Expr -> Writer Logs (Maybe Expr) 
-- stepWithLog env expr@(App (Abs x e) r) = do 
--     tell [(expr, expr)]
--     return $ Just $ sub ((x,r):env) e 
-- stepWithLog env expr@(Ref x)   = return Nothing
-- stepWithLog env (Abs x e) = do 
--     e' <- (stepWithLog env e) 
--     return $ fmap (Abs x) e' 
-- stepWithLog env (App l r) = do 
--     x <- stepWithLog env l 
--     y <- stepWithLog env r 
--     apply l r x y

-- apply :: Expr -> Expr -> Maybe Expr -> Maybe Expr -> Writer Logs (Maybe Expr)
-- apply l r (Just x)  _        = return $ Just (App x r)
-- apply l r Nothing   y = return $ fmap (App l ) y


evalWithLogs :: Expr -> Writer Logs Expr  
evalWithLogs e = case stepWithRedex [] e of
                    (_, [])       -> do 
                        return e
                    (Nothing,_) -> do 
                        return e
                    (Just e',log) -> do 
                        tell [(e, (head log))]
                        evalWithLogs e'
                        

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


test1 = evalWithLogs (App (Abs "x" (App (Ref "x") (Ref "x") )) ( App (Abs "y" (Ref "y")) (Ref "z")))




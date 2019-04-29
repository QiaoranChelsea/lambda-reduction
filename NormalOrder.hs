-- | operation in Normal Order Evaluation
--   limitation: Do not support renaming for free variable 

module NormalOrder where 

import qualified Data.Map as Map
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
--   >>> step [] (App (Abs "x" (App (Ref "x") (Ref "y"))) (Ref "y"))
--   Just (App (Ref "y") (Ref "y"))
-- 
--   >>> step [] (App ( Abs "y" (Abs "x" (App (Ref "x") (Ref "y"))) )(Ref "w"))
--   Just (Abs "x" (App (Ref "x") (Ref "w")))
--   
--   >>> step [] (App (Abs "x" (App (Ref "x") (Ref "x") )) ( App (Abs "y" (Ref "y")) (Ref "z")))
--   Just (App (App (Abs "y" (Ref "y")) (Ref "z")) (App (Abs "y" (Ref "y")) (Ref "z")))
--
--   >>> step [] (App (App (Abs "y" (Ref "y")) (Ref "z")) (App (Abs "y" (Ref "y")) (Ref "z")))
--   Just (App (Ref "z") (App (Abs "y" (Ref "y")) (Ref "z")))
--   
--   >>> step [] ((App (Ref "z") (App (Abs "y" (Ref "y")) (Ref "z"))))
--   Just (App (Ref "z") (Ref "z"))

step :: EvalScope -> Expr -> Maybe Expr 
step env (App (Abs x e) r) = Just$ sub ((x,r):env) e 
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







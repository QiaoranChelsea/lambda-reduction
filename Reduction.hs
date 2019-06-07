-- | operation in Normal Order Evaluation
module Reduction where 

import qualified Data.Map as Map
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Syntax 
import Data.List

--
-- ** simple evaluation without log redexes 
--

-- | Evaluate an expression to normal form using normal order evaluation.
--   Note that this function will not terminate if the reduction never
--   reaches a normal form!
eval :: Expr -> Expr
eval e = case step' [] e of
           Nothing -> e
           Just e' -> eval e'

-- | Do one step normal order reduction and return the result expr
step' :: EvalScope -> Expr -> Maybe Expr 
step' env (App (Abs x e) r) = Just $ sub ((x,r):env) e 
step' env (App l r)         = case step' env l of 
                                Just l' -> Just (App l' r)
                                Nothing -> fmap (App l) (step' env r)
step' env expr@(Ref x)      = Nothing
step' env (Abs x e)         = fmap (Abs x) (step' env e) 

-- | perfrom one step reduction 
step :: Expr ->  Expr 
step e = case step' [] e of
           Nothing -> e
           Just e' -> e'

-- | Variable substitution based EvalScope 
sub :: EvalScope -> Expr -> Expr
sub env expr@(Ref x)   = case lookup x env of 
                            Just ev -> ev 
                            Nothing -> expr
sub env epxr@(Abs x e) = Abs x (sub env e)   
sub env (App l r)      = App (sub env l) (sub env r)                         

--
-- ** Evaluation with log (log redex choosen for each reduction )
--

type EvalStep a = State [Redex] a

-- | original plain impelmentation 
evalWithLogs :: Expr -> Writer Logs Expr  
evalWithLogs e = let (e', red) = stepWithState e 
                 in  if null red -- no redex found
                        then return e 
                        else do tell [(e, head red)] -- log (lambda expression, and its redex 
                                evalWithLogs e'

stepWithState :: Expr -> (Expr, [Redex])
stepWithState expr  = runState (stepWithState' [] expr) []

-- | use only State with simple value 

stepWithState' :: EvalScope -> Expr -> EvalStep Expr
stepWithState' env expr@(App (Abs x e) r) = do put [expr]
                                               return (sub ((x,r):env) e)  
stepWithState' env expr@(Ref x)  = do put []  
                                      return expr                                
stepWithState' env (Abs x e) = do e' <- stepWithState' env e 
                                  return (Abs x e')
stepWithState' env (App l r) = do l' <- stepWithState' env l 
                                  m <- get 
                                  if null m -- no red found 
                                      then do r' <- stepWithState' env r
                                              return ( App l r')
                                      else return (App l' r)
                        
-- 
-- ** capture-avoiding substution/Rename 
--

-- | rename variable to avoid variable capture problem
rename :: Expr -> Expr
rename = subv []

-- | substitude the bound variables for avoiding variable capture
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
                            Nothing -> Abs x (subv m e)  
subv m (App l r) = App (subv m l) (subv m r)   

-- | use monad 
captureAvoidRename :: Expr -> (Expr, RenameLog)
captureAvoidRename expr = fst $ runState (runWriterT (captureAvoidSub expr)) []

captureAvoidSub :: Expr -> Rename Expr 
captureAvoidSub expr@(App (Abs x e) r) = do updateFVMapping r
                                            m <- get
                                            e' <- captureAvoidSub e
                                            case lookup x m of
                                              Just nx -> return (App (Abs nx e') r)
                                              Nothing -> return (App (Abs x e') r)
captureAvoidSub expr@(Ref x) = do m <- get 
                                  case lookup x m of 
                                    Just nv -> return (Ref nv) 
                                    Nothing -> return (expr)
captureAvoidSub (Abs x e) = do  m <- get
                                e' <- captureAvoidSub e
                                case lookup x m of 
                                  Just nv -> return (Abs nv e')
                                  Nothing -> return (Abs x e')
captureAvoidSub (App l r) = do l' <- captureAvoidSub l
                               r' <- captureAvoidSub r
                               return (App l' r')

-- | update the variable rename mapping  
updateFVMapping :: Expr -> Rename ()
updateFVMapping e = do let fv = free e 
                           m = zip fv (zipWith (++) fv (map show [1..(length fv)] ))
                       put m 


-- | get all the free variable in the expression

-- Given an expression e, the following rules define FV(e), the set of free variables in e:
-- If e is a variable x, then FV(e) = {x}.
-- If e is of the form λx.y, then FV(e) = FV(y) - {x}.
-- If e is of the form xy, then FV(e) = FV(x) ∪ FV(y)

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


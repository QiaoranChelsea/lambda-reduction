module Main where 

import Reduction
import EvalView
import Syntax
import Draw
import Control.Monad

import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Trans.Maybe


-- To Do : 1. cut the operation if exceed some depth 

main :: IO ()
main = return ()

printResultView :: Expr -> IO ()
printResultView = putStrLn . drawResultView . initView

viewResults :: EvalView -> IO ()
viewResults = putStrLn . drawResultView 

view :: EvalView -> IO ()
view = putStrLn . drawAllView

-- | evaluate a lambda expression by using normal order 
evalLambda :: Expr -> IO ()
evalLambda expr = let expr' = fst $ captureAvoidRename expr
                  in case expr' == expr  of  
                    True -> printWriter $ evalWithLogs expr 
                    False -> do 
                       putStrLn $ prettyExpr expr 
                       putStrLn $ "=" ++ (prettyEval $ evalWithLogs $ expr')


--
-- Examples
--

-- | (λx . x x) ((λy . y) z)
lambda1 = (App (Abs "x" (App (Ref "x") (Ref "x") )) ( App (Abs "y" (Ref "y")) (Ref "z")))

-- | (λx.λy. x) y u 
lambda2 = App (App (Abs "x" (Abs "y" (Ref "x"))) (Ref "y")) (Ref "u")

-- | (λx.xy) y
lambda3 = App (Abs "x" (App (Ref "x") ( Ref "y"))) (Ref "y")

-- | (λx. (λy. y x) (λz. z)) (λw.w) 
lambda4 = App (Abs "x" (App (Abs "y" (App (Ref "y")(Ref "x") )  ) (Abs "z" (Ref "z") ))) (Abs "w" (Ref "w") )

-- | λx. (λy. (λx. y x)) x
lambda5 = Abs "x" (App (Abs "y" (Abs "x" (App (Ref "y") (Ref "x") ) ) ) (Ref "x") )
 
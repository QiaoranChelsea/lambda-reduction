module Main where 

import NormalOrder
import EvalView
import Syntax
import PrettyPrint

-- TODO: 1. cut the operation if exceed some depth
main :: IO ()
main = return ()

printResultView :: Expr -> IO ()
printResultView = putStrLn . drawResultView . initView

viewResults :: EvalView -> IO ()
viewResults = putStrLn . drawResultView 

view :: EvalView -> IO ()
view = putStrLn . drawAllView

viewLayer :: EvalLayer -> IO ()
viewLayer = putStrLn . drawOneLayer 
--
-- small test suite
--

-- | (\x . x x) ((\y . y) z)
lambda1 = (App (Abs "x" (App (Ref "x") (Ref "x") )) ( App (Abs "y" (Ref "y")) (Ref "z")))
lambda1' = (App (Abs "z" (App (Ref "z") (Ref "z") )) ( App (Abs "y" (Ref "y")) (Ref "z")))

-- | (\ x. (\ y. y x) (\ z. z)) (\ z.z) 
lambda2 = App (Abs "x" (App (Abs "y" (App (Ref "y")(Ref "x") )  ) (Abs "z" (Ref "z") ))) (Abs "z" (Ref "z") )

lambda3' = (App (Abs "x" (Abs "y" (Ref "x"))) (Ref "y"))
-- | (\x.\y. x) y u 
lambda3 = App (App (Abs "x" (Abs "y" (Ref "x"))) (Ref "y")) (Ref "u")

-- | (\x.xy) y
lambda4 = App (Abs "x" (App (Ref "x") ( Ref "y"))) (Ref "y")

-- | (\x.xx)((\y.y)z)
lambda5 = App (Abs "x" (App (Ref "x") (Ref "x"))) (App (Abs "y" (Ref "y")) (Ref "z"))
lambda5' = (App (App (Abs "y" (Ref "y")) (Ref "z")) (App (Abs "y" (Ref "y")) (Ref "z"))) 
lambda5'' = (App (App (Abs "x" (Ref "x")) (Ref "z")) (App (Abs "y" (Ref "y")) (Ref "z"))) 
red5 = App (Abs "y" (Ref "y")) (Ref "z") 
 
-- | (\x.xxx)((\y.y)z) -- bad loop 
-- lambda6 = App (Abs "x" (App (App (Ref "x") (Ref "x")) (Ref "x"))) (App (Abs "y" (Ref "y")) (Ref "z"))
lambda6' = (App (App (App (Abs "y" (Ref "y")) (Ref "z")) (App (Abs "y" (Ref "y")) (Ref "z"))) (App (Abs "y" (Ref "y")) (Ref "z")))
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


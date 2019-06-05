module Main where 

import NormalOrder
import EvalView
import Syntax
import Draw
import Control.Monad


-- TODO: 1. cut the operation if exceed some depth 
--       2. Assume the original var do not have int in the Name 
main :: IO ()
main = return ()

printResultView :: Expr -> IO ()
printResultView = putStrLn . drawResultView . initView

viewResults :: EvalView -> IO ()
viewResults = putStrLn . drawResultView 

view :: EvalView -> IO ()
view = putStrLn . drawAllView

-- viewLayer :: EvalLayer -> IO ()
-- viewLayer = putStrLn . drawOneLayer 
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

lambda7 =(App (App (Abs "y" (App (Ref "y") (Ref "y") )) (Ref "z")) (App (Abs "y" (Ref "y")) (Ref "z"))) 
lambda8 = Abs "y" (App lambda5' lambda5')
big = App lambda5' lambda6'
test1 = let v = initView lambda5 
        in view v
test2 = let v = initView lambda5'
        in view v


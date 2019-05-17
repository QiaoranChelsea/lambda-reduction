module ViewGraph where 


import Syntax 
import Data.Graph
import NormalOrder

-- | filter out all redexes in current level lambda expression
getRedexes :: (Expr -> Bool) -> Expr -> [Expr]
getRedexes f  (Ref x) = []
getRedexes f  expr@(App l r) = let lred = getRedexes f l 
                                   rred = getRedexes f r 
                               in if f expr 
                                  then [expr]++lred++rred 
                                  else lred++rred 
getRedexes f (Abs x e) = getRedexes f e

-- | check if current lambda expression is a redex
isRedex :: Expr -> Bool 
isRedex (Ref _)           = False 
isRedex (App (Abs x e) r) = True 
isRedex (Abs x e)         = isRedex e
isRedex (App l r)         = isRedex l && isRedex r 


-- | reduce the Expr based on the Redex choosed 
lfReduce :: Expr -> Redex -> Expr 
lfReduce e@(App l r) red | e == red = step' e 
                         | l == red = App (step' l) r 
                         | r == red = App l (step' r)
lfReduce (Abs x body) red = Abs x (lfReduce body red)
lfReduce e   _   = e 

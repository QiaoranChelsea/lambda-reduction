module EvalView where 

import Syntax 
import NormalOrder


-- | get all redexes in current level lambda expression
getRedexes :: Expr -> [Redex]
getRedexes = filterRedex isRedex

-- filter out all redexes in current level lambda expression
filterRedex :: (Expr -> Bool) -> Expr -> [Expr]
filterRedex f  (Ref x) = []
filterRedex f  expr@(App l r) = let lred = filterRedex f l 
                                    rred = filterRedex f r 
                                in if f expr 
                                   then [expr]++lred++rred 
                                   else lred++rred 
filterRedex f (Abs x e) = filterRedex f e

-- check if current lambda expression is a redex
isRedex :: Expr -> Bool 
isRedex (App (Abs x e) r) = True 
isRedex _         = False

-- | Reduce the Expr based on the Redex choosed 
--   * NOTE: lfReduce only reduce the first seen Redex from left
lfReduce :: Expr -> Redex -> Expr 
lfReduce e@(App l r) red | e == red = step e 
                         | l == red = App (step l) r 
                         | r == red = App l (step r)
lfReduce (Abs x body) red = Abs x (lfReduce body red)
lfReduce e   _   = e 

-- | init a evaluation graph based on input lambda expression
initView :: Expr -> EvalView
initView = transLayer2View. initOneLayer 

-- init one layer of evaluation tree
initOneLayer :: Expr -> EvalLayer
initOneLayer e = let reds = getRedexes e
                     result = map (lfReduce e) reds 
                 in Layer e (zip result reds)

-- translate One Layer to View Tree 
transLayer2View :: EvalLayer -> EvalView
transLayer2View (Layer e []) = Leaf e
transLayer2View (Layer e xs) = let sub = map (\(expr, red) -> (transLayer2View (initOneLayer expr), red )) xs
                               in Node e sub

-- | get top level redexes 
redexes :: EvalView -> EvalView 
redexes (Node e children) =  Node e $ map (\(eview,red)-> (getRoot eview,red)) children
        where getRoot (Node e sub) = Leaf e 

-- | Produce view that shows result nth redex
reduceWith :: Int -> EvalView -> EvalView 
reduceWith i (Node e children) = let (eview,red) = children !! i 
                                     subview = initView $ getRootExp eview 
                                 in Node e [(subview, red)]
             where getRootExp (Node e sub) = e 
                   getRootExp (Leaf e)     = e
                                     
                    




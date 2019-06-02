module EvalView where 

import Syntax 
import NormalOrder
import Data.List
import Control.Monad.State



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

type Rename a b = State a b

-- | rename Expr for the duplicate redexes
--   * Note that the duplicate redex will only happen when the expr is (App l r) 
renameForDupRedex :: Expr -> Expr 
renameForDupRedex e = let (e',m) = subvForDupRedex [] e 
                      in e'

-- substitude bound variable for avoiding duplicate redexes 
subvForDupRedex :: RenameMapping -> Expr -> (Expr , RenameMapping) 
subvForDupRedex m expr@(Ref x) = case lookup x m of 
                                   Just nv -> (Ref nv, m) 
                                   Nothing -> (expr, m)
subvForDupRedex m (Abs x e) = let (e',m')= (subvForDupRedex m e)
                              in case lookup x m of 
                                   Just nv -> (Abs nv e' , m' )
                                   Nothing -> (Abs x e' , m' ) 
subvForDupRedex m (App l r) = let (l',m2) = subvForDupRedex m l 
                                  (r',m3) = (subvForDupRedex m2 r)  
                                  bv = nub (bound l' )
                                  m4 = zip bv (zipWith (++) bv (map show [(1+length m3) .. length m3+(length bv)] ))
                              in (App l' r', m4)

                                    
-- get all bound variable in the expression
bound :: Expr -> [Var] 
bound (Ref x)   = []
bound (Abs x y) = x : (bound y) 
bound (App x y) = (bound x ++ bound y)

-- | init a evaluation graph based on input lambda expression
initView :: Expr -> EvalView
initView = transLayer2View. initOneLayer 

-- init one layer of evaluation tree
-- 
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

-- | Produce view that shows result from nth redex
reduceWith :: Int -> EvalView -> EvalView 
reduceWith i (Node e children) = let (eview,red) = children !! i 
                                     subview = initView $ getRootExp eview 
                                 in Node e [(subview, red)]
             where getRootExp (Node e sub) = e 
                   getRootExp (Leaf e)     = e
                                     
                    




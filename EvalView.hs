-- | This module impelemnts functions that relates to the explanation view tree.
-- Contents in this module 
-- 1. Operation User could use for evaluation tree
-- 2. Helper functions to impelment the view
-- 3. Rename the bound variable for avoiding duplicate redexes 
module EvalView where 

import Syntax 
import Reduction
import Data.List
import Control.Monad.State
import Control.Monad.Writer


--
-- * Operations 
--

-- | init a evaluation graph based on input lambda expression
initView :: Expr -> EvalView
initView = transLayer2View. initOneLayer 

-- init one layer of evaluation tree
initOneLayer :: Expr -> EvalLayer
initOneLayer e = let (e1,log1) = captureAvoidRename e
                     (e2,log2) = dupRedexRename e1
                     reds = getRedexes e2
                     result = map (lfReduce e2) reds   
                 in Layer e (zip result reds) (log1++log2)

-- translate One Layer to View Tree 
transLayer2View :: EvalLayer -> EvalView
transLayer2View (Layer e [] m) = Leaf e
transLayer2View (Layer e xs m) = let sub = map (\(expr, red) -> (transLayer2View (initOneLayer expr), red )) xs
                               in Node e sub m

-- | get top level redexes 
redexes :: EvalView -> EvalView 
redexes (Node e children m) =  Node e (map (\(eview,red)-> (getRoot eview,red)) children) m
        where getRoot (Node e sub m) = Leaf e 
              getRoot (Leaf e)       = Leaf e

-- | Produce view that shows result from nth redex
reduceWith :: Int -> EvalView -> EvalView 
reduceWith i (Node e children m) = let (eview,red) = children !! i 
                                       subview = initView $ getRootExp eview 
                                   in Node e [(subview, red)] m
             where getRootExp (Node e sub m) = e 
                   getRootExp (Leaf e)     = e

--
-- ** Helper functions (getRedexes, lfReduce)
--

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

--
-- * Reduce the Expr based on the Redex choosed 
--   * NOTE: lfReduce only reduce the first seen Redex from left
lfReduce :: Expr -> Redex -> Expr 
lfReduce e@(App l r) red | e == red = step e 
                         | elem red (getRedexes l) = App (lfReduce l red) r 
                         | elem red (getRedexes r) = App l (lfReduce r red)
lfReduce (Abs x body) red = Abs x (lfReduce body red)
lfReduce e   _   = e 
                                    

-- 
--  * Rename the bound variable for avoiding duplicate redexes 
--    refactor with State and Writer monad 
--

-- | Rename the bound variable for avoiding duplicate redexes 
dupRedexRename:: Expr -> (Expr, RenameLog) 
dupRedexRename ex = fst $ runState (runWriterT (dupRedexSub ex)) []
                
-- substitude the duplicated bound variable in expression
dupRedexSub :: Expr -> Rename Expr 
dupRedexSub e@(Ref x) = do m <- get
                           case lookup x m of                    
                             Just nv -> do return (Ref nv)
                             Nothing -> return e
dupRedexSub (Abs x e) = do m <- get 
                           e' <- dupRedexSub e 
                           case lookup x m of 
                             Just nv -> do logName (x,nv)
                                           return  (Abs nv e')
                             Nothing -> do return (Abs x e')                
dupRedexSub expr@(App l r) = do  l' <-  dupRedexSub l 
                                 updateState l 
                                 r' <- dupRedexSub r  
                                 updateState r 
                                 return (App l' r')

-- | update the renamming mapping state
updateState :: Expr ->  Rename ()
updateState expr = do m <- get 
                      let bv = (nub (bound expr))
                          nameMapping = newM bv (length m) 
                      put $ (nameMapping ++ m)




-- | new variable name
newV :: Var -> Int -> Var
newV v i =  v++ (show i)

-- | new rename mapping 
newM :: [Var] -> Int  -> [(Var, Var)]
newM bv i = [ (v, newV v i) | v <- bv]
                                     
                    
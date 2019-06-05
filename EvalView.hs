module EvalView where 

import Syntax 
import NormalOrder
import Data.List
import Control.Monad.State
import Control.Monad.Writer



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
                         | elem red (getRedexes l) = App (lfReduce l red) r 
                         | elem red (getRedexes r) = App l (lfReduce r red)
lfReduce (Abs x body) red = Abs x (lfReduce body red)
lfReduce e   _   = e 
                                    
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
initOneLayer e = let (e1,log1) = captureAvoidRename e
                     (e2,log2) = renameDupBV e1
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
-- * refactor with State and Writer monad 
--

ex =  App (Abs "x" (App (Ref "x") (Ref "x"))) (App (Abs "y" (Ref "y")) (Ref "z"))

-- substitude bound variable for avoiding duplicate redexes 
foo :: Expr ->  ((Expr, RenameLog), RenameMapping)
foo ex  = runState (runWriterT (subDupBV ex)) []

renameDupBV:: Expr -> (Expr, RenameMapping) 
renameDupBV ex = fst $ runState (runWriterT (subDupBV ex)) []
                 

-- | log new rename mapping in Log 
logName :: (Var,Var) -> Rename ()
logName vpair = do  tell [vpair]  


subDupBV :: Expr -> Rename Expr 
subDupBV e@(Ref x) = do m <- get
                        case lookup x m of                    
                          Just nv -> do return (Ref nv)
                          Nothing -> return e
subDupBV (Abs x e) = do m <- get 
                        e' <- subDupBV e 
                        case lookup x m of 
                          Just nv -> do logName (x,nv)
                                        return  (Abs nv e')
                          Nothing -> do return (Abs x e')                
subDupBV expr@(App l r) = do l' <-  subDupBV l 
                             updateState l 
                             r' <- subDupBV r  
                             updateState r 
                             return (App l' r')

updateState :: Expr ->  Rename ()
updateState expr = do m <- get 
                      let bv = (nub (bound expr))
                          nameMapping = newM bv (length m) 
                      put $ (nameMapping ++ m)
                                 
-- The censor function takes a function and a Writer and produces a new Writer whose output is the same but whose log entry has been modified by the function.
-- runEval :: Expr -> (Expr, RenameLog)
-- runEval x = runState (runWriterT (foo x)) (EvalState 0)            

newV :: Var -> Int -> Var
newV v i =  v++ (show i)

newM :: [Var] -> Int  -> [(Var, Var)]
newM bv i = [ (v, newV v i) | v <- bv]
                                     
                    
-- | rename Expr for the duplicate redexes
--   * Note that the duplicate redex will only happen when the expr is (App l r) 
renameForDupRedex :: Expr -> (Expr, RenameMapping) 
renameForDupRedex e = let (e',m,log) = subvForDupRedex [] e 
                      in (e',log)

-- substitude bound variable for avoiding duplicate redexes 
subvForDupRedex :: RenameMapping -> Expr -> (Expr , RenameMapping, RenameMapping) 
subvForDupRedex m expr@(Ref x) = case lookup x m of 
                                   Just nv -> (Ref nv, m, (x,nv): m) 
                                   Nothing -> (expr, m,  m)
subvForDupRedex m (Abs x e) = let (e',m',log)= (subvForDupRedex m e)
                              in case lookup x m' of 
                                   Just nv -> (Abs nv e' , m',(x,nv):log)
                                   Nothing -> (Abs x e' , m', log ) 
subvForDupRedex m (App l r) = let (l',m2,log1) = subvForDupRedex m l 
                                  (r',m3,log2) = (subvForDupRedex m2 r)  
                                  log = log1 ++ log2
                                  bv = nub (bound l') \\ (map snd log)
                                  m4 = zip bv (zipWith (++) bv (map show [1 + (length log ) .. length log+(length bv)] ))
                              in (App l' r', m4, nub (log1++log2))





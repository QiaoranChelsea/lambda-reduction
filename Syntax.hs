module Syntax where 


import Control.Monad.Writer


-- | Variable Name 
type Depth = Int 
type Var = String 
    
-- | Named lambda calculus terms.      
data Expr = Ref Var       -- ^ variable reference
         | App Expr Expr  -- ^ application
         | Abs Var Expr   -- ^ lambda abstraction
  deriving (Eq)

type EvalScope = [(Var, Expr)]

-- | old var,new var
type RenameMapping = [(Var, Var)]
type Redex = Expr

-- | cur whole expr + redux
type Logs = [(Expr, Redex)]

-- | Graph for Eval Trace, Pair shows the child with its Redex
data EvalView = Leaf Expr | Node Expr [(EvalView, Redex)] RenameMapping
    deriving (Show)

-- data EvalViewLog = State EvalView (Expr,Expr)

-- | One layer of the Evaluation 
-- |  [(Expr, Redex, Expr)]=> resultExpr + redex + originalExpr
data EvalLayer = Layer Expr [(Expr, Redex)] RenameMapping
    deriving (Show)




instance Show Expr where
  show = prettyExpr


prettyExpr :: Expr -> String 
prettyExpr (Ref v)     =  v 
prettyExpr (App l r )  = concat ["(", prettyExpr l, " ", prettyExpr r, ")"] 
prettyExpr (Abs v e)   = concat ["(λ", v, ". ", prettyExpr e, ")"] 


prettyLogs :: Logs -> String 
prettyLogs []            = ""
prettyLogs ((e,red):xs)  = prettyExpr e ++ "\t --"  ++ prettyExpr red ++ "\n" ++ "=> " ++ prettyLogs xs 

prettyEval :: Writer Logs Expr -> String 
prettyEval w = let logs = snd $ runWriter w 
                   results = fst $ runWriter w
               in prettyLogs logs ++ prettyExpr results 

printWriter :: Writer Logs Expr -> IO ()
printWriter w = putStrLn $ prettyEval w 

prettyRenameMapping :: RenameMapping -> String 
prettyRenameMapping [] = ""
prettyRenameMapping ((x,nv):xs) = concat [nv, "<-", x, ";"] ++ prettyRenameMapping xs 


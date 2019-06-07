module Syntax where 

import Control.Monad.Writer
import Control.Monad.State


-- | Variable Name 
type Var = String 
    
-- | Named lambda calculus terms.      
data Expr = Ref Var       -- ^ variable reference
         | App Expr Expr  -- ^ application
         | Abs Var Expr   -- ^ lambda abstraction
  deriving (Eq)

-- | Evaluation Scope
type EvalScope = [(Var, Expr)]

-- | Mapping from old var -> new var
type RenameMapping = [(Var, Var)]
type Redex = Expr

-- | Current whole expr + redux
type Logs = [(Expr, Redex)]

-- | Rose Tree for Eval Trace, Pair shows the result expresssion with its Redex
data EvalView = Leaf Expr | Node Expr [(EvalView, Redex)] RenameMapping
    deriving (Show)

-- | One layer of the Evaluation 
-- |  [(Expr, Redex, Expr)]=> resultExpr + redex + originalExpr
data EvalLayer = Layer Expr [(Expr, Redex)] RenameMapping
    deriving (Show)

-- | Log the variable name which have been actually renamed 
type RenameLog = RenameMapping

-- | Wrap up the state and writer monad 
type Rename a = WriterT RenameLog (State RenameMapping ) a 


-- 
-- ** Pretty Print the Syntax 
--
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


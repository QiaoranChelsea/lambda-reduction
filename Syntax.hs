module Syntax where 

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

-- | Graph for Eval Trace, Pair shows the child with its path
data EvalGraph = Node Expr [(EvalGraph, Expr)]



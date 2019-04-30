module Syntax where 

-- | Variable Name 
type Depth = Int 
type Var = String 
    
-- | Named lambda calculus terms.      
data Expr = Ref Var       -- ^ variable reference
         | App Expr Expr  -- ^ application
         | Abs Var Expr   -- ^ lambda abstraction
  deriving (Eq,Show)

type EvalScope = [(Var, Expr)]

type Redex = Expr 
-- | cur whole expr + redux
type Logs = [(Expr, Redex)]
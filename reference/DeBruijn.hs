-- | This module illustrates beta-reduction on nameless lambda calculus
--   terms using de Bruijn indexes.
module DeBruijn where

-- | The de Bruijn index of a variable.
type Var = Int

-- | Nameless lambda calculus terms. Note that we can check
--   alpha-equivalence with plain old Haskell (==). This is
--   a complicated and expensive operation in named lambda
--   calcuses.
data Exp = App Exp Exp   -- ^ application
         | Abs Exp       -- ^ lambda abstraction
         | Ref Var       -- ^ variable reference
  deriving (Eq,Show)
 

-- | Do one step of normal order reduction and return the result.
--   If no redex is found, return Nothing.
--
--   The first case matches a redex and does a substitution. The rest of
--   the cases implement a search for next redex.
--
--   >>> step (App (Abs (Ref 0)) (Ref 1))
--   Just (Ref 1)
--
--   >>> step (App (Abs (Abs (App (Ref 0) (Ref 1)))) (Ref 2))
--   Just (Abs (App (Ref 0) (Ref 3)))
--
--   >>> step (App (Abs (Abs (App (Ref 2) (Ref 1)))) (Ref 0))
--   Just (Abs (App (Ref 1) (Ref 1)))
--
step :: Exp -> Maybe Exp
step (App (Abs e) r) = Just (sub 0 r e)  -- found a redex, do beta reduction!
step (App l r)       = case step l of
                         Just l' -> Just (App l' r)
                         Nothing -> fmap (App l) (step r)
step (Abs e)         = fmap Abs (step e)
step (Ref _)         = Nothing


-- | Evaluate an expression to normal form using normal order evaluation.
--   Note that this function will not terminate if the reduction never
--   reaches a normal form!
eval :: Exp -> Exp
eval e = case step e of
           Nothing -> e
           Just e' -> eval e'


-- | Variable substitution. `sub v e1 e2` substitues e1 for every v in e2.
--
--   Both the abstraction and reference cases are interesting.
--
--   Each time we enter a new abstraction in e2, we must:
--    1. Increment the v that we're looking for.
--    2. Increment all of the free variables in e1 since now the references
--       will have to skip over one more lambda to get to the lambda they
--       refer to.
--
--   For each variable reference v' in e2, there are three possibilities:
--     1. It's the variable v we're looking for, in which case we replace it.
--     2. It's a variable bound in e2 (v' < v), in which case we do nothing.
--     3. It's a variable that is free in e2 (v' > v), in which case we
--        decrement it since now the reference has to skip over one less
--        lambda (i.e. the lambda that we beta-reduced away) to get to the
--        lambda it refers to.
--   
--   >>> sub 0 (Ref 1) (Abs (Ref 1))
--   Abs (Ref 2)
--
sub :: Var -> Exp -> Exp -> Exp
sub v e (App l r) = App (sub v e l) (sub v e r)
sub v e (Abs e')  = Abs (sub (v+1) (inc 0 e) e')
sub v e (Ref v')
    | v' == v = e           -- found a v, replace it!
    | v' < v  = Ref v'      -- not v and bound in e2, leave it alone
    | v' > v  = Ref (v'-1)  -- not v and free in e2, decrement it


-- | Increment the free variables in an expression.
-- 
--   The argument d (for "depth") indicates the number of abstractions we
--   have recursed into so far. A variable that is smaller than the depth
--   is not free, and so should not be incremented.
--
--   >>> inc 0 (Ref 0)
--   Ref 1
--
--   >>> inc 0 (App (Ref 1) (Abs (Ref 0)))
--   App (Ref 2) (Abs (Ref 0))
--
inc :: Int -> Exp -> Exp
inc d (App l r) = App (inc d l) (inc d r)
inc d (Abs e)   = Abs (inc (d+1) e)
inc d (Ref v)   = Ref (if v < d then v else v+1)

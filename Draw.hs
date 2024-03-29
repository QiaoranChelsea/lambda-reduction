module Draw where 

import Syntax
import Control.Monad.Writer

-- | Draw EvalView which only shows the result of reduction
drawResultView :: EvalView -> String 
drawResultView = unlines . drawView' False

-- | draw view  which shows both result and Redex 
drawAllView :: EvalView -> String 
drawAllView = unlines . drawView' True

-- | drawView with True ==> view with redex 
--   drawView with False ==> view without redex      
drawView' ::  Bool -> EvalView -> [String]
drawView' b (Node e xs m) = prettyExpr e : drawSubTrees xs
  where
    drawSubTrees [] = []
    drawSubTrees [(v,red)] =
        "|" : shift "`- " "   " (showRedex b red ++ showRename m ++ drawView' b v )
    drawSubTrees ((v,red):ts) =
        "|" : shift "+- " "|  " (showRedex b red++ showRename m ++ drawView' b v )   ++ drawSubTrees ts
    shift first other = zipWith (++) (first : repeat other)
drawView' b (Leaf e) = [prettyExpr e]

-- | Show the rename mapping during the reduction
showRename :: RenameMapping -> [String] 
showRename m = if null m then [] else ["RENAME: "++ prettyRenameMapping m]

-- | Show the redex choosen during the reduction
showRedex :: Bool -> Redex -> [String]
showRedex b red = if b then ["REDEX:" ++ prettyExpr red] else []

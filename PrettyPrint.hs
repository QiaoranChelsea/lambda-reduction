module PrettyPrint where 


import Syntax
import Control.Monad.Writer

-- data EvalView = Leaf Expr | Node Expr [(EvalView, Redex)]


drawResultView :: EvalView -> String 
drawResultView = unlines . drawResultView'

drawResultView' :: EvalView -> [String]
drawResultView' (Node e xs m) = prettyExpr e : drawSubTrees xs
  where
    drawSubTrees [] = []
    drawSubTrees [(v,red)] =
        "|" : shift "`- " "   " (showRename m ++ drawResultView' v)
    drawSubTrees ((v,red):ts) =
        "|" : shift "+- " "|  " (showRename m ++ drawResultView' v) ++ drawSubTrees ts
    shift first other = zipWith (++) (first : repeat other)
    showRename = \m -> (if null m then [] else ["RENAME: "++ prettyRenameMapping m])
drawResultView' (Leaf e) = [prettyExpr e]


drawAllView :: EvalView -> String 
drawAllView = unlines . drawAllView'

drawAllView' :: EvalView -> [String]
drawAllView' (Node e xs m) = prettyExpr e : drawSubTrees xs
  where
    drawSubTrees [] = []
    drawSubTrees [(v,red)] =
        "|" : shift "`- " "   " (["REDEX:" ++ prettyExpr red] ++ showRename m ++ drawAllView' v  )
    drawSubTrees ((v,red):ts) =
        "|" : shift "+- " "|  " (["REDEX:" ++ prettyExpr red] ++showRename m ++ drawAllView' v )   ++ drawSubTrees ts
    shift first other = zipWith (++) (first : repeat other)
    showRename = \m -> (if null m then [] else ["RENAME: "++ prettyRenameMapping m])
drawAllView' (Leaf e) = [prettyExpr e]

-- | data EvalLayer = Layer Expr [(Expr, Redex)] RenameMapping
drawOneLayer :: EvalLayer -> String 
drawOneLayer = unlines . drawOneLayer'

drawOneLayer' :: EvalLayer -> [String] 
drawOneLayer' (Layer e xs m) = prettyExpr e : drawSubTrees xs
  where
    drawSubTrees [] = []
    drawSubTrees [(v,red)] =
        "|" : shift "`- " "   " (["REDEX:" ++ prettyExpr red] ++ showRename m ++  [prettyExpr v]  )
    drawSubTrees ((v,red):ts) =
        "|" : shift "+- " "|  " (["REDEX:" ++ prettyExpr red] ++showRename m ++ [prettyExpr v]  )   ++ drawSubTrees ts
    shift first other = zipWith (++) (first : repeat other)
    showRename = \m -> (if null m then [] else ["RENAME: "++ prettyRenameMapping m])

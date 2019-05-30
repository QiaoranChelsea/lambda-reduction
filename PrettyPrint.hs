module PrettyPrint where 


import Syntax
import Control.Monad.Writer

-- data EvalView = Leaf Expr | Node Expr [(EvalView, Redex)]


drawResultView :: EvalView -> String 
drawResultView = unlines . drawResultView'

drawResultView' :: EvalView -> [String]
drawResultView' (Node e xs) = prettyExpr e : drawSubTrees xs
  where
    drawSubTrees [] = []
    drawSubTrees [(v,red)] =
        "|" : shift "`- " "   " (drawResultView' v)
    drawSubTrees ((v,red):ts) =
        "|" : shift "+- " "|  " (drawResultView' v) ++ drawSubTrees ts
    shift first other = zipWith (++) (first : repeat other)
drawResultView' (Leaf e) = [prettyExpr e]


drawAllView :: EvalView -> String 
drawAllView = unlines . drawAllView'

drawAllView' :: EvalView -> [String]
drawAllView' (Node e xs) = prettyExpr e : drawSubTrees xs
  where
    drawSubTrees [] = []
    drawSubTrees [(v,red)] =
        "|" : shift "`- " "   " (["*" ++ prettyExpr red++"*"] ++ drawAllView' v  )
    drawSubTrees ((v,red):ts) =
        "|" : shift "+- " "|  " (["*" ++ prettyExpr red++"*"] ++ drawAllView' v )   ++ drawSubTrees ts
    shift first other = zipWith (++) (first : repeat other)
drawAllView' (Leaf e) = [prettyExpr e]

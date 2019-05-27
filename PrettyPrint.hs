module PrettyPrint where 


import Syntax
import Control.Monad.Writer

-- data EvalView = Leaf Expr | Node Expr [(EvalView, Redex)]


drawView :: EvalView -> String 
drawView = unlines . drawView'

drawView' :: EvalView -> [String]
drawView' (Node e xs) = prettyExpr e : drawSubTrees xs
  where
    drawSubTrees [] = []
    drawSubTrees [(v,red)] =
        "|" : shift "`- " "   " (drawView' v)
    drawSubTrees ((v,red):ts) =
        "|" : shift "+- " "|  " (drawView' v) ++ drawSubTrees ts
    shift first other = zipWith (++) (first : repeat other)
drawView' (Leaf e) = [prettyExpr e]


-- -- | Neat 2-dimensional drawing of a tree.
-- drawTree :: Tree String -> String
-- drawTree  = unlines . draw

-- -- | Neat 2-dimensional drawing of a forest.
-- drawForest :: Forest String -> String
-- drawForest  = unlines . map drawTree

-- draw :: EvalLayer -> [String]
-- draw (Node x ts0) = x : drawSubTrees ts0
--   where
--     drawSubTrees [] = []
--     drawSubTrees [t] =
--         "|" : shift "`- " "   " (draw t)
--     drawSubTrees (t:ts) =
--         "|" : shift "+- " "|  " (draw t) ++ drawSubTrees ts

-- --     shift first other = zipWith (++) (first : repeat other)



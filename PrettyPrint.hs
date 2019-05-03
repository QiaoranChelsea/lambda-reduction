module PrettyPrint where 

import Syntax
import Control.Monad.Writer
-- print current expression and the selected redex
-- challenge: pretty print the parentheses

-- type Logs = [(Expr, Redex)]

prettyExpr :: Expr -> String 
prettyExpr (Ref v)     =  v 
-- prettyExpr (App l (Ref v2)) = prettyExpr l ++ " " ++ v2
-- prettyExpr (App (Ref v1) r) = v1 ++ " " ++ prettyExpr r 
prettyExpr (App l r )  = "(" ++ prettyExpr l ++ ") (" ++ prettyExpr r ++ ")"
prettyExpr (Abs v e)   = "\\" ++ v ++ " . "++ prettyExpr e


prettyLogs :: Logs -> String 
prettyLogs []            = ""
prettyLogs ((e,red):xs)  = prettyExpr e ++ "\t --"  ++ prettyExpr red ++ "\n" ++ "=> " ++ prettyLogs xs 

prettyEval :: Writer Logs Expr -> String 
prettyEval w = let logs = snd $ runWriter w 
                   results = fst $ runWriter w
               in prettyLogs logs ++ prettyExpr results 

printWriter :: Writer Logs Expr -> IO ()
printWriter w = putStrLn $ prettyEval w 


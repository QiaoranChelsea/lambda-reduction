# Lambda-Reduction Explanation Tree 

## Steps to run
`> ghci Main.hs`

## Examples 
Example lambda expression
```
> lambda1
((λx. (x x)) ((λy. y) z))
> lambda2
(((λx. (λy. x)) y) u)
>lambda3
((λx. (x y)) y)
> lambda4
((λx. ((λy. (y x)) (λz. z))) (λw. w))
> lambda5
(λx. ((λy. (λx. (y x))) x))
```

Initialize a view 
```
> let v = initView lambda1
```

View entire evaluation tree
```
> view v
((λx. (x x)) ((λy. y) z))
|
+- REDEX:((λx. (x x)) ((λy. y) z))
|  (((λy. y) z) ((λy. y) z))
|  |
|  +- REDEX:((λy. y) z)
|  |  RENAME: y1<-y;
|  |  (z ((λy1. y1) z))
|  |  |
|  |  `- REDEX:((λy1. y1) z)
|  |     (z z)
|  |
|  `- REDEX:((λy1. y1) z)
|     RENAME: y1<-y;
|     (((λy. y) z) z)
|     |
|     `- REDEX:((λy. y) z)
|        (z z)
|
`- REDEX:((λy. y) z)
   ((λx. (x x)) z)
   |
   `- REDEX:((λx. (x x)) z)
      (z z)
```


View just the evolution results
```
> viewResults v
((λx. (x x)) ((λy. y) z))
|
+- (((λy. y) z) ((λy. y) z))
|  |
|  +- RENAME: y1<-y;
|  |  (z ((λy1. y1) z))
|  |  |
|  |  `- (z z)
|  |
|  `- RENAME: y1<-y;
|     (((λy. y) z) z)
|     |
|     `- (z z)
|
`- ((λx. (x x)) z)
   |
   `- (z z)
```

Look for the top level redexes
```
> view $ redexes v
((λx. (x x)) ((λy. y) z))
|
+- REDEX:((λx. (x x)) ((λy. y) z))
|  (((λy. y) z) ((λy. y) z))
|
`- REDEX:((λy. y) z)
   ((λx. (x x)) z)
```

Look for the evaluation about the i-th redex (NOTE:index start with 0 )
```
> view $ reduceWith 1 $ redexes v
((λx. (x x)) ((λy. y) z))
|
`- REDEX:((λy. y) z)
   ((λx. (x x)) z)
   |
   `- REDEX:((λx. (x x)) z)
      (z z)
``` 

## Other Examples
Evaluate Lambda in Normal Order 
```
> evalLambda lambda1
((λx. (x x)) ((λy. y) z))	 --((λx. (x x)) ((λy. y) z))
=> (((λy. y) z) ((λy. y) z))	 --((λy. y) z)
=> (z ((λy. y) z))	 --((λy. y) z)
=> (z z)
```

Rename the bound variable to avoid variable capture
```
> captureAvoidRename lambda2
((((λx. (λy1. x)) y) u),[])
> captureAvoidRename lambda5
((λx. ((λy. (λx1. (y x1))) x)),[("x","x1")])

> evalLambda lambda2
(((λx. (λy. x)) y) u)
=(((λx. (λy1. x)) y) u)	 --((λx. (λy1. x)) y)
=> ((λy1. y) u)	 --((λy1. y) u)
=> y
```

## how FP has affected the project
1. Used state monad to keep track of redex for each reduction so that I just modify the state whenever I really need to do, instead of explicitly point out the state in every cases. 
2. Used writer monad to log the result expression with correspoding redex through entire reduction, making life easier.
3. Wrap the writer monad and state monad so that I'm able to log the rename occurance and also keep track of the renamming mapping at the same time. 
4. Define instance of type class Show for Expr so that I don't need to manually call prettyExpr anymore

## limitation
1. The impelementation couldn't cut out the infinite loop automatically. The use should use ctrl-c to end the computation.
2. The new name used for the renaming such as y1,y2,y3 couldn't be continuous. For example, if we have a lambda caculus express as "((((λy. y) z) ((λy. y) z)) ((λy. y) z))", the result of renaming to avoid duplicate redexes would be ((((λy. y) z) ((λy1. y1) z)) ((λy4. y4) z)) where the new variable name y1,y4 is not continuous.


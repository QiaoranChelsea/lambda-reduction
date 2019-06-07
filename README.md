# Lambda-Reduction Explanation Tree 

## Steps to run
> ghci Main.hs

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
```



# Lambda-Reduction Explanation  

## Examples 
example lambda expression
```
> lambda 5
((λx. (x x)) ((λy. y) z))
```

Initialize a view 
```
> let v = initView lambda5
```

View entire evaluation tree
```
> view v
((λx. (x x)) ((λy. y) z))
|
+- *((λx. (x x)) ((λy. y) z))*
|  (((λy. y) z) ((λy. y) z))
|  |
|  +- *((λy. y) z)*
|  |  (z ((λy. y) z))
|  |  |
|  |  `- *((λy. y) z)*
|  |     (z z)
|  |
|  `- *((λy. y) z)*
|     (z ((λy. y) z))
|     |
|     `- *((λy. y) z)*
|        (z z)
|
`- *((λy. y) z)*
   ((λx. (x x)) z)
   |
   `- *((λx. (x x)) z)*
      (z z)
```


View just the evolution results
```
> viewResults v
((λx. (x x)) ((λy. y) z))
|
+- (((λy. y) z) ((λy. y) z))
|  |
|  +- (z ((λy. y) z))
|  |  |
|  |  `- (z z)
|  |
|  `- (z ((λy. y) z))
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
+- *((λx. (x x)) ((λy. y) z))*
|  (((λy. y) z) ((λy. y) z))
|
`- *((λy. y) z)*
   ((λx. (x x)) z)
```

Look for the evaluation about the i-th redex
```
> view $ reduceWith 1 $ redexes v
((λx. (x x)) ((λy. y) z))
|
`- *((λy. y) z)*
   ((λx. (x x)) z)
   |
   `- *((λx. (x x)) z)*
      (z z)
``` 

## Other Examples
Evaluate Lambda in Normal Order 
```
> evalLambda lambda5
((λx. (x x)) ((λy. y) z))	 --((λx. (x x)) ((λy. y) z))
=> (((λy. y) z) ((λy. y) z))	 --((λy. y) z)
=> (z ((λy. y) z))	 --((λy. y) z)
=> (z z)
```


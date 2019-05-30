# Lambda-Reduction Explanation Tree 

## Examples 

Initialize a view 
```
let v = initView lambda5
```

View entire evaluation tree
```
view v
```


View just the evolution results
```
viewResults v
```

Look for the top level redexes
```
view $ redexes v
```

Look for the evaluation about the i-th redex
```
view $ reduceWith 1 $ redexes v
``` 



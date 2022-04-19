---
title: "R and Python Notebook"
output: html_notebook
---

Reticulate is powerful! 


```r
library(reticulate)
use_condaenv(condaenv = "r-reticulate", conda = "e:/miniconda/_conda.exe")
```



```r
df <- data.frame(c(1, 2, 3), c(4, 5, 6))
dim(df)
```

```
## [1] 3 2
```


```python
import pandas as pd
df = pd.DataFrame([[1, 2, 3], [4, 5, 6], [7, 8, 9]])
df.shape
```

```
## (3, 3)
```

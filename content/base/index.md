---
title: "Score cutoff analysis"
subtitle: "Credit risk series (Part 2)"
summary: "How to perform cutoff analysis when deploying credit risk scorecards"
author: "royr2"
date: 2021-09-18
categories: ["R", "Credit Risk Analytics", "Scorecards", "Credit decisioning"]
tags: ["R", "dplyr", "credit scoring", "credit risk"]  
comments: true
---



INTRODUCTION HERE

## Packages
Let's get package installation out of the way first.


```r
# Pacman is a package management tool 
install.packages("pacman")
```


```r
library(pacman)
## Warning: package 'pacman' was built under R version 4.1.1

# p_load automatically installs packages if needed
p_load(dplyr, magrittr, knitr, scales)
```

## Sample dataset

Here's some sample data to play around with. The data set is small sample of the **Lending Club** dataset available on [kaggle](https://www.kaggle.com/wordsforthewise/lending-club).

The file is available for download [here](https://github.com/royr2/blog/blob/main/download/credit_sample.csv).



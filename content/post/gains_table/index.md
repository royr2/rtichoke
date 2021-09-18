---
title: "Measuring model performance using a gains table"
subtitle: "Credit risk series (Post #1)"
summary: "How to create a gains table to measure the performance of a binary classification model"
author: "royr2"
date: 2021-09-06
categories: ["R", "Credit Risk Analytics"]
tags: ["R", "dplyr", "credit scoring", "credit risk"]  
comments: true
---



Modellers/analysts developing credit scores generally use something known as the `gains table` (or a `ks table`) to measure and quantify the performance of such models. We'll explore how to build such a table in this post. 

The idea is to first discretise the population under consideration (say the testing or validation set) into groups based on the model's output (probability/log odds/scores). Typically, this is done in a way such that each group represents 10% of the total population (or deciles). Then, summary statistics are generated for each group and  cumulative distributions of events and non-events are analysed and the model's performance is quantified. 

We'll use `dplyr` in this post. Doing it this way is nice since it's easier to read and the code would need minimal changes if using `sparklyr` (say within a big data environment where one might need to run this directly on a *hadoop* table).


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

[Here's](https://github.com/royr2/blog/blob/main/download/credit_sample.csv) some sample data to play around with. The data set is small sample of the **Lending Club** dataset available on [Kaggle](https://www.kaggle.com/wordsforthewise/lending-club).






```r
sample <- read.csv("credit_sample.csv")
```


```r
dim(sample)
## [1] 10000   153
```


```r
class(sample)
## [1] "data.frame"
```

## Defining a target
First, we need to create a target (outcome) to model for. Since this is a credit risk use case, we are looking to create a target which identifies borrowers who defaulted on (or missed) their payments consecutively.


```r
unique(sample$loan_status)
## [1] "Fully Paid"                                         
## [2] "Current"                                            
## [3] "Charged Off"                                        
## [4] "Late (31-120 days)"                                 
## [5] "Late (16-30 days)"                                  
## [6] "In Grace Period"                                    
## [7] "Does not meet the credit policy. Status:Fully Paid" 
## [8] "Does not meet the credit policy. Status:Charged Off"
```


```r
# For simplicity we'll just use 
# 1. "Charged Off"
# 2. "Does not meet the credit policy. Status:Charged Off"
codes <- c("Charged Off", "Does not meet the credit policy. Status:Charged Off")

# For details on the %<>% operator please look at the 
# documentation for the magrittr package. 
sample %<>% mutate(bad_flag = ifelse(loan_status %in% codes, 1, 0))
```

Let's also check for overall event rates.

```r
sample %>% 
  summarise(events = sum(bad_flag == 1), 
            non_events = sum(bad_flag == 0)) %>% 
  mutate(event_rate = events/(events + non_events))
##   events non_events event_rate
## 1   1162       8838     0.1162
```

## Building a model
Next lets build a quick and dirty model, the output of which we will use to build the gains table.


```r
# Check out available features (not shown here for brevity)
colnames(sample)
```

We'll need to do some data cleaning first.

```r
# Replace all NA values with a default value
sample[is.na(sample)] <- -1

sample %<>% 
  
  # Remove cases where home ownership and payment plan are not reported
  filter(! home_ownership %in% c("", "NONE"),
         pymnt_plan != "") %>% 
  
  # Convert these two variables into factors
  mutate(home_ownership = factor(home_ownership), 
         pymnt_plan = factor(pymnt_plan))
```


```r
# Train Test split
idx <- sample(1:nrow(sample), size = 0.7 * nrow(sample), replace = F)
train <- sample[idx,]
test <- sample[-idx,]
```


```r
dim(train)
## [1] 6999  153
dim(test)
## [1] 3000  153
```


```r
# Using a GLM model for simplicity
mdl <- glm(
  formula = bad_flag ~ 
    loan_amnt + term + mths_since_last_delinq + total_pymnt + 
    home_ownership + acc_now_delinq + 
    inq_last_6mths + delinq_amnt + 
    mths_since_last_record + mths_since_recent_revol_delinq + 
    mths_since_last_major_derog + mths_since_recent_inq + 
    mths_since_recent_bc + num_accts_ever_120_pd,
  family = "binomial", 
  data = train
)
```

While we are at it, let's also attach the model predictions to the test dataset.

```r
test$pred <- predict(mdl, newdata = test)
```

## Creating the Gains Table
The table has a few important components:

- Bins in decreasing/increasing order of model output (probability/log odds/scores)
- Population percentages contained in each bin 
- Observed event rates in each bin 
- Cumulative events and non events distributions


```r
# Discretise predictions based on quantiles
q <- quantile(test$pred, probs = seq(0, 1, length.out = 11))

# Add bins to test dataset
test$bins <- cut(test$pred, breaks = q, include.lowest = T, right = T, ordered_result = T)
```

{{< alert "Note that the output from cut is arranged in increasing order of value" >}}


```r
levels(test$bins)
##  [1] "[-5.27,-3.52]" "(-3.52,-3]"    "(-3,-2.72]"    "(-2.72,-2.48]"
##  [5] "(-2.48,-2.29]" "(-2.29,-2.1]"  "(-2.1,-1.89]"  "(-1.89,-1.65]"
##  [9] "(-1.65,-1.29]" "(-1.29,12.8]"
```

Using the bins we created above, we can now start to put the table together


```r
# Start with the test dataset and summarise
gains_table <- test %>% 
  group_by(bins) %>% 
  summarise(total = n(), 
            events = sum(bad_flag == 1), 
            non_events = sum(bad_flag == 0))
```

At this point the table should look something like this:

```r
kable(gains_table)
```



|bins          | total| events| non_events|
|:-------------|-----:|------:|----------:|
|[-5.27,-3.52] |   300|     10|        290|
|(-3.52,-3]    |   300|      8|        292|
|(-3,-2.72]    |   300|     13|        287|
|(-2.72,-2.48] |   300|     20|        280|
|(-2.48,-2.29] |   300|     26|        274|
|(-2.29,-2.1]  |   300|     37|        263|
|(-2.1,-1.89]  |   300|     54|        246|
|(-1.89,-1.65] |   300|     57|        243|
|(-1.65,-1.29] |   300|     64|        236|
|(-1.29,12.8]  |   300|     71|        229|

Next, we'll add the event rate columns. Let's also make the table presentable - I'll use the `percent()` function in the `scales` package to show numbers as percentages. 

```r
gains_table %<>%
  mutate(event_rate = percent(events / total, 0.1, 100))

kable(gains_table)
```



|bins          | total| events| non_events|event_rate |
|:-------------|-----:|------:|----------:|:----------|
|[-5.27,-3.52] |   300|     10|        290|3.3%       |
|(-3.52,-3]    |   300|      8|        292|2.7%       |
|(-3,-2.72]    |   300|     13|        287|4.3%       |
|(-2.72,-2.48] |   300|     20|        280|6.7%       |
|(-2.48,-2.29] |   300|     26|        274|8.7%       |
|(-2.29,-2.1]  |   300|     37|        263|12.3%      |
|(-2.1,-1.89]  |   300|     54|        246|18.0%      |
|(-1.89,-1.65] |   300|     57|        243|19.0%      |
|(-1.65,-1.29] |   300|     64|        236|21.3%      |
|(-1.29,12.8]  |   300|     71|        229|23.7%      |

To this we'll add some columns quantifying how events and non events are distributed across each bin.

```r
gains_table %<>%
  mutate(pop_pct = percent(total/sum(total), 0.1, 100), 
         
         # Not formatting these as percentages just yet
         c.events_pct = cumsum(events) / sum(events),
         c.non_events_pct = cumsum(non_events) / sum(non_events))

kable(gains_table) 
```



|bins          | total| events| non_events|event_rate |pop_pct | c.events_pct| c.non_events_pct|
|:-------------|-----:|------:|----------:|:----------|:-------|------------:|----------------:|
|[-5.27,-3.52] |   300|     10|        290|3.3%       |10.0%   |    0.0277778|        0.1098485|
|(-3.52,-3]    |   300|      8|        292|2.7%       |10.0%   |    0.0500000|        0.2204545|
|(-3,-2.72]    |   300|     13|        287|4.3%       |10.0%   |    0.0861111|        0.3291667|
|(-2.72,-2.48] |   300|     20|        280|6.7%       |10.0%   |    0.1416667|        0.4352273|
|(-2.48,-2.29] |   300|     26|        274|8.7%       |10.0%   |    0.2138889|        0.5390152|
|(-2.29,-2.1]  |   300|     37|        263|12.3%      |10.0%   |    0.3166667|        0.6386364|
|(-2.1,-1.89]  |   300|     54|        246|18.0%      |10.0%   |    0.4666667|        0.7318182|
|(-1.89,-1.65] |   300|     57|        243|19.0%      |10.0%   |    0.6250000|        0.8238636|
|(-1.65,-1.29] |   300|     64|        236|21.3%      |10.0%   |    0.8027778|        0.9132576|
|(-1.29,12.8]  |   300|     71|        229|23.7%      |10.0%   |    1.0000000|        1.0000000|

Almost done - just need a few more columns namely:

- A column computing the difference between the two cumulative distribution columns we created previously. The maximum value of this column will become the primary performance metric known as the `KS statistic`.
- Two additional columns computing `event capture rates` and `cumulative event rates`.


```r
gains_table %<>%
  mutate(ks = round(abs(c.events_pct - c.non_events_pct), 2), 
         cap_rate = percent(cumsum(events)/sum(events), 1, 100), 
         c_event_rate = percent(cumsum(events)/cumsum(total), 0.1, 100), 
         
         # Format pending columns
         c.events_pct = percent(c.events_pct, 0.1, 100),
         c.non_events_pct = percent(c.non_events_pct, 0.1, 100))

kable(gains_table)
```



|bins          | total| events| non_events|event_rate |pop_pct |c.events_pct |c.non_events_pct |   ks|cap_rate |c_event_rate |
|:-------------|-----:|------:|----------:|:----------|:-------|:------------|:----------------|----:|:--------|:------------|
|[-5.27,-3.52] |   300|     10|        290|3.3%       |10.0%   |2.8%         |11.0%            | 0.08|3%       |3.3%         |
|(-3.52,-3]    |   300|      8|        292|2.7%       |10.0%   |5.0%         |22.0%            | 0.17|5%       |3.0%         |
|(-3,-2.72]    |   300|     13|        287|4.3%       |10.0%   |8.6%         |32.9%            | 0.24|9%       |3.4%         |
|(-2.72,-2.48] |   300|     20|        280|6.7%       |10.0%   |14.2%        |43.5%            | 0.29|14%      |4.2%         |
|(-2.48,-2.29] |   300|     26|        274|8.7%       |10.0%   |21.4%        |53.9%            | 0.33|21%      |5.1%         |
|(-2.29,-2.1]  |   300|     37|        263|12.3%      |10.0%   |31.7%        |63.9%            | 0.32|32%      |6.3%         |
|(-2.1,-1.89]  |   300|     54|        246|18.0%      |10.0%   |46.7%        |73.2%            | 0.27|47%      |8.0%         |
|(-1.89,-1.65] |   300|     57|        243|19.0%      |10.0%   |62.5%        |82.4%            | 0.20|62%      |9.4%         |
|(-1.65,-1.29] |   300|     64|        236|21.3%      |10.0%   |80.3%        |91.3%            | 0.11|80%      |10.7%        |
|(-1.29,12.8]  |   300|     71|        229|23.7%      |10.0%   |100.0%       |100.0%           | 0.00|100%     |12.0%        |

## Creating a function
Finally, we can encapsulate all of the above code in a single function. Note that we actually do not need the full test/train dataset, just the actual classes and predicted outcomes (log odds/probability/score).


```r
gains_table <- function(act, pred, increasing = T, nBins = 10){
  
  q <- quantile(pred, probs = seq(0, 1, length.out = nBins + 1))
  bins <- cut(pred, breaks = q, include.lowest = T, right = T, ordered_result = T)
  
  df <- data.frame(act, pred, bins)
  
  df %>% 
    
    group_by(bins) %>% 
    summarise(total = n(), 
              events = sum(act == 1), 
              non_events = sum(act == 0)) %>% 
    mutate(event_rate = percent(events / total, 0.1, 100)) %>% 
    
    # This odd looking format is to ensure that the if-else 
    # condition is part of the dplyr chain
    {if(increasing == TRUE){
      arrange(., bins)
    }else{
      arrange(., desc(bins))
    }} %>% 
    
    mutate(pop_pct = percent(total/sum(total), 0.1, 100), 
           c.events_pct = cumsum(events) / sum(events),
           c.non_events_pct = cumsum(non_events) / sum(non_events), 
           ks = round(abs(c.events_pct - c.non_events_pct), 2), 
           cap_rate = percent(cumsum(events)/sum(events), 1, 100), 
           c_event_rate = percent(cumsum(events)/cumsum(total), 0.1, 100), 
           c.events_pct = percent(c.events_pct, 0.1, 100),
           c.non_events_pct = percent(c.non_events_pct, 0.1, 100))
}
```

It is worth noting here that since the capture rate is being computed from **top to bottom**, it is important that the table is arranged in an appropriate manner. That is, when modelling for `bads`, the table should be arrange in **descending** order of the model output (i.e. higher event rates at the top) and vice versa. 

Also, if you are planning on using this with `sparklyr`, consider looking into the `ft_quantile_discretizer()` function. It would replace `cut()` here.


```r
# Test the function
tab <- gains_table(test$bad_flag, test$pred, F, 10)
kable(tab)
```



|bins          | total| events| non_events|event_rate |pop_pct |c.events_pct |c.non_events_pct |   ks|cap_rate |c_event_rate |
|:-------------|-----:|------:|----------:|:----------|:-------|:------------|:----------------|----:|:--------|:------------|
|(-1.29,12.8]  |   300|     71|        229|23.7%      |10.0%   |19.7%        |8.7%             | 0.11|20%      |23.7%        |
|(-1.65,-1.29] |   300|     64|        236|21.3%      |10.0%   |37.5%        |17.6%            | 0.20|38%      |22.5%        |
|(-1.89,-1.65] |   300|     57|        243|19.0%      |10.0%   |53.3%        |26.8%            | 0.27|53%      |21.3%        |
|(-2.1,-1.89]  |   300|     54|        246|18.0%      |10.0%   |68.3%        |36.1%            | 0.32|68%      |20.5%        |
|(-2.29,-2.1]  |   300|     37|        263|12.3%      |10.0%   |78.6%        |46.1%            | 0.33|79%      |18.9%        |
|(-2.48,-2.29] |   300|     26|        274|8.7%       |10.0%   |85.8%        |56.5%            | 0.29|86%      |17.2%        |
|(-2.72,-2.48] |   300|     20|        280|6.7%       |10.0%   |91.4%        |67.1%            | 0.24|91%      |15.7%        |
|(-3,-2.72]    |   300|     13|        287|4.3%       |10.0%   |95.0%        |78.0%            | 0.17|95%      |14.2%        |
|(-3.52,-3]    |   300|      8|        292|2.7%       |10.0%   |97.2%        |89.0%            | 0.08|97%      |13.0%        |
|[-5.27,-3.52] |   300|     10|        290|3.3%       |10.0%   |100.0%       |100.0%           | 0.00|100%     |12.0%        |
## Interpretation
Some notes on how to interpret such a table:

- Since scoring models aim to risk-rank borrowers, the first thing to look for is whether or not the event rates are consistently increasing  (or decreasing) across bins. If not, when using the actual model, one might not be able to confidently conclude if borrower A is better (or worse) than borrower B. 
- If bin sizes are not consistent (in this case ~10%) it would imply that the model is assigning the same output/score to a lot of borrowers (clumping). This could pose issues later on (say when deciding cutoffs). The ideal solution is to add additional variables that can help differentiate between good and bad borrowers. 
- While the ideal cutoff would be the bin where the `KS` statistic is at its maximum, additional aspects like capture rates and approval rates should be taken into account. 
- Typically, analysts would look for a model which achieves the maximum value of the `KS` statistic within the first 2/3 deciles. That way, when creating underwriting policies, you would only end up rejecting 20%-30% of the applicant pool.


*Thoughts? Comments? Helpful? Not helpful? Like to see anything else added in here? Let me know!*

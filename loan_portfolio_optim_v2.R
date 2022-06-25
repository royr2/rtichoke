pacman::p_load(bezier, dplyr, reshape2, ggplot2, data.table, magrittr, rpart, partykit)

df <- fread("c:/Users/royr2/Downloads/accepted_2007_to_2018Q4.csv")
names(df)

df %>%

  mutate(dti_bins = cut(dti, c(-Inf, 0, 30, Inf))) %>%

  mutate(mths_since_last_delinq_bins = cut(mths_since_last_delinq, c(-Inf, 0, 12, Inf))) %>%

  filter(! is.na(dti_bins) & ! is.na(mths_since_last_delinq_bins)) %>%

  group_by(dti_bins, mths_since_last_delinq_bins) %>%

  tally()


unique(df$mths_since_last_delinq)


df[,bad_flag := ifelse(loan_status %in% c("Fully Paid", "Current", "Does not meet the credit policy. Status:Fully Paid"), 1, 0)]
df[,bad_flag := as.factor(bad_flag)]

mdl <- rpart(bad_flag ~ dti + mths_since_last_delinq, data = df, control = rpart.control(cp = 0.001))
rpart.plot::rpart.plot(mdl)

mdl <- ctree(bad_flag ~ dti + mths_since_last_delinq, data = df, control = ctree_control(maxdepth = 2))
plot(mdl)

dim(df)

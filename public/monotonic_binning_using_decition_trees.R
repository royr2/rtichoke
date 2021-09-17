library(pacman)
p_load(rpart, partykit, data.table, recipes, ggplot2)

load("E:/Github/scoretools/data/accepted_base.rda")

codes <- c("Charged Off", "Does not meet the credit policy. Status:Charged Off")

df <- accepted_base %>%
  data.frame() %>%
  mutate(bad_flag = ifelse(loan_status %in% codes, 1, 0)) %>%
  select_if(is.numeric)

rec <- recipe(bad_flag ~ ., data = df) %>%
  step_impute_median(all_predictors())

rec <- prep(rec, training = df)
model_data <- bake(rec, new_data = df)


x <- model_data$int_rate
y <- model_data$bad_flag

direction <- ifelse(cor(x, y, method = "spearman") > 0, 1, -1)

mdl <- xgboost(data = model_data %>%
          select(loan_amnt) %>%
          as.matrix(),
        label = model_data[["bad_flag"]],
        nrounds = 1,
        params = list(monotone_constraints = direction, max_depth = 20))

xgboost::xgb.plot.tree(model = mdl)
splits <- xgboost::xgb.model.dt.tree(model = mdl)
cuts <- c(-Inf, sort(splits$Split), Inf)

data.frame(target = model_data$bad_flag,
           buckets = cut(model_data$loan_amnt, breaks = cuts, include.lowest = T, right = T, ordered_result = T)) %>%
  group_by(buckets) %>%
  summarise(total = n(),
            events = sum(target == 1)) %>%
  mutate(pct = events/total) %>%
  ggplot(aes(x = buckets, y = pct)) + geom_col()


mdl <- ctree(bad_flag ~ loan_amnt, data = model_data, )
partykit::data_party(party = mdl) %>%
  data.frame() %>%
  group_by(X.fitted.) %>%
  summarise(min(loan_amnt), max(loan_amnt))

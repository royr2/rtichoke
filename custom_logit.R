pacman::p_load(dplyr, data.table, skimr, ROCR)

df <- fread("download/credit_sample.csv")
df[is.na(dti),dti := 0]
mdl <- glm(bad_flag ~ inq_last_6mths + dti, family = "binomial", data = df)

obj <- function(pars){

  fit <- pars[1] + pars[2] * df$inq_last_6mths + pars[3] * df$dti

  pi <- exp(fit) / (1 + exp(fit))
  pi[pi > 1] <- 1
  pi[pi < 0] <- 0

  # val <- -sum(df$bad_flag * log(pi ** 2) + (1 - df$bad_flag) * log(1 - pi))
  val <- - (pROC::auc(df$bad_flag, pi)[1] * 1000)

  return(val)

}

out <- optim(par = c(0, 0, 0), fn = obj)
out
coef(mdl)
N
pred <- out$par[1] + out$par[2] * df$inq_last_6mths
pred <- exp(pred)/ (1 + exp(pred))
pred_mdl <- predict(mdl, type = "response")

pROC::auc(df$bad_flag, pred)
pROC::auc(df$bad_flag, pred_mdl)

ppred <- prediction(pred, df$bad_flag)
perf <- performance(ppred,"tpr","fpr")

ppred_mdl <- prediction(pred_mdl, df$bad_flag)
perf_mdl <- performance(ppred_mdl,"tpr","fpr")


plot(perf@x.values[[1]], perf@y.values[[1]], type = 'l')
lines(perf_mdl@x.values[[1]], perf_mdl@y.values[[1]], type = 'l', col = 'red')




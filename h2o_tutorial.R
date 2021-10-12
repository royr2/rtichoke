library(pacman)

p_load(h2o)

model_data <- read.csv("download/credit_sample.csv")
dim(model_data)

h2o.init()

h2o.df <- as.h2o(model_data)

h2o.
N

pacman::p_load(bezier, dplyr, reshape2, ggplot2, data.table, magrittr)

# Create a loan book ----------------------------------------------------------
# Dimensions: Bureau history, thick/thin file, known bad/ known good
cred_hist <- c("LTEQ12", "GT12")
thick_thin <- c("thick", "thin")
kb_kg <- c("KB", "KG")
cred_score <- seq(300, 900, by = 1)

# Number of samples
n_samples <- 1e6

# Combinations
combinations <- expand.grid(cred_hist = cred_hist,
                            thick_thin = thick_thin,
                            kb_kg = kb_kg)

# Remove infeasible combinations
combinations %<>%
  mutate(include = case_when(
    cred_hist == "GT12" & thick_thin == "thin" ~ "exclude",
    TRUE ~ "include")) %>%
  filter(include == "include") %>%
  select(-include)

# Generate synthetic Data -----------------------------------------------------

# Ranking:
# 1: GT12 + Thick + KG
# 2: LTEQ12 + Thick + KG
# 3: LTEQ12 + Thin + KG (ideally should be indeterminate)
# 4: GT12 + Thick + KB
# 5: LTEQ12 + Thick + KB
# 6: LTEQ12 + Thin + KB

combinations %<>%
  mutate(ranking = case_when(
    cred_hist == "GT12" & thick_thin == "thick" & kb_kg == "KG" ~ 1,
    cred_hist == "LTEQ12" & thick_thin == "thick" & kb_kg == "KG" ~ 2,
    cred_hist == "LTEQ12" & thick_thin == "thin" & kb_kg == "KG" ~ 3,
    cred_hist == "GT12" & thick_thin == "thick" & kb_kg == "KB" ~ 4,
    cred_hist == "LTEQ12" & thick_thin == "thick" & kb_kg == "KB" ~ 5,
    cred_hist == "LTEQ12" & thick_thin == "thin" & kb_kg == "KB" ~ 6
  )) %>%
  arrange(ranking)

# Assign population percentage
combinations %<>%
  mutate(pop = c(0.30, 0.25, 0.15, 0.12, 0.10, 0.08) * n_samples)
  # mutate(pop = rep(1/6, 6) * n_samples)

# Assign min/max scores
combinations %<>%
  mutate(score_min = c(720, 700, 670, 620, 450, 300),
         score_max = c(900, 750, 700, 670, 620, 450))

# Scaling function
scale_func <- function(vec) (vec - min(vec))/(max(vec) - min(vec))

# Simulate

graphics.off()

beta = 22; mu = 10
p <- c()

for(i in 1:6){

  vec <- combinations$score_min[i]:combinations$score_max[i]

  x <- seq(-10, 50, length.out = length(vec))
  z <- (x - mu)/beta
  g <- (1/beta) * exp(-(z ** 2 + exp(-z)))

  plot(vec, g)

  p_sample <- sample(vec, size = combinations$pop[i], replace = T, prob = g)

  p <- c(p, p_sample)
}

hist(p)

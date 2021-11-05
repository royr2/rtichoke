pacman::p_load(data.table, pso, ggplot2, dplyr)

df <- fread("c:/Users/royr2/Desktop/TickerMania/open_prices.csv")

daily_returns <- df %>%
  select(-date) %>%
  apply(., 2, function(x)x/lag(x) - 1) %>%
  na.omit()

N <- ncol(daily_returns)
mu <- apply(daily_returns, 2, mean)
cov_mat <- cov(daily_returns)

# mu <- mu[1:2]
# cov_mat <- cov_mat[1:2, 1:2]

obj_func <- function(wts,
                     lambda1 = 10,
                     lambda2 = 100,
                     n_dim = NA){

  mu <- mu[1:n_dim]
  cov_mat <- cov_mat[1:n_dim, 1:n_dim]

  wts <- matrix(wts, nrow = length(wts))

  port_ret <- matrix(mu, nrow = 1) %*% wts
  port_cov <- t(wts) %*% cov_mat %*% wts
  port_sd <- sqrt(port_cov)

  obj <- (port_ret - lambda1 * port_cov) - lambda2 * (sum(wts) - 1)^2

  return(-obj)
}

pso_optim <- function(obj_func,
                      c1 = 0.05,
                      c2 = 0.05,
                      w = 0.8,
                      init_fact = 0.1,
                      n_particles = 20,
                      n_dim = 2,
                      n_iter = 50,
                      upper = 1,
                      lower = 0,
                      n_avg = 10,
                      ...
){

  X <- matrix(runif(n_particles * n_dim), nrow = n_particles)
  X <- X * (upper - lower) + lower

  dX <- matrix(runif(n_particles * n_dim) * init_fact, ncol = n_dim)
  dX <- dX * (upper - lower) + lower

  pbest <- X
  pbest_obj <- apply(X, 1, obj_func, n_dim = n_dim)

  gbest <- pbest[which.min(pbest_obj),]
  gbest_obj <- min(pbest_obj)

  loc_df <- data.frame(X, iter = 0, obj = pbest_obj)
  iter <- 1

  while(iter < n_iter){

    dX <- w * dX + c1*runif(1)*(pbest - X) + c2*runif(1)*t(gbest - t(X))

    X <- X + dX

    obj <- apply(X, 1, obj_func, n_dim = n_dim)

    idx <- which(obj <= pbest_obj)
    pbest[idx,] <- X[idx,]
    pbest_obj[idx] <- obj[idx]

    idx <- which.min(pbest_obj)
    gbest <- pbest[idx,]
    gbest_obj <- min(pbest_obj)

    # Update iteration
    iter <- iter + 1
    loc_df <- rbind(loc_df, data.frame(X, iter = iter, obj = pbest_obj))
  }

  gbest <- loc_df %>%
    arrange(desc(iter)) %>%
    group_by(iter) %>%
    filter(row_number() <= n_avg) %>%
    ungroup() %>%
    select(-iter, -obj) %>%
    apply(., 2, mean)

  lst <- list(X = loc_df, obj = gbest_obj, obj_loc = gbest)
  return(lst)
}

out <- pso_optim(obj_func,
                 n_particles = 50,
                 n_dim = 3,
                 n_iter = 200,
                 c1 = 0.02, c2 = 0.02, w = 0.05, init_fact = 0.01,
                 lambda1 = 1e2,
                 lambda2 = 1e2,
                 n_avg = 20)

barplot(out$obj_loc)
sum(out$obj_loc)

out$X %>%
  mutate(obj = apply(.[,c(1, 3)], 1, obj_func, n_dim = 2)) %>%
  rgl::plot3d(x = .$X1, y = .$X2, z = .$obj, col = rainbow(nrow(.)))

cols <- RColorBrewer::brewer.pal(n = 3, "Set1")

df <- expand.grid(x1 = seq(0, 1, by = 0.01),
                  x2 = seq(0, 1, by = 0.01),
                  x3 = seq(0, 1, by = 0.01))

df$obj <- apply(df, 1, obj_func, n_dim = 3)

df %>%
  filter(x1 + x2 == 1) %>%
  ggplot(aes(y = -obj)) +
  geom_point(aes(x = x1), col = cols[1], alpha = 0.2) +
  geom_point(aes(x = x2), col = cols[2], alpha = 0.2) +
  geom_point(aes(x = x3), col = cols[3], alpha = 0.2)

g2 <- out$X %>%
  ggplot(aes(y = obj)) +
  geom_point(aes(x = X1), col = cols[1], alpha = 0.2) +
  geom_point(aes(x = X2), col = cols[2], alpha = 0.2) +
  geom_point(aes(x = X3), col = cols[3], alpha = 0.2)





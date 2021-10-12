library(ggplot2)
library(dplyr)
library(gganimate)

f_xy <- function(x, y){
  # (x - 3.14)^2 + (y - 2.72)^2  + sin(3 * x + 2) + sin(4 * y - 1.73)
  # sin(3*3.14* x)^2 + (x - 1)^2 *(1+sin(3 * 3.14 * y)^2) + (y-1)^2*(1+sin(2*3.14*y)^2)
  # (x**2 + y - 11)**2 + (x + y**2 -7)**2

  -20 * exp(-0.2 * sqrt(x^2 + y^2)) - exp(cos(x) + cos(y))

}

x <- seq(-10, 10, length.out = 100)
y <- seq(-10, 10, length.out = 100)

grid <- expand.grid(x, y, stringsAsFactors = F)
grid$z <- f_xy(grid[,1], grid[,2])

grid %>%
  ggplot() +
  geom_contour_filled(aes(x = Var1, y = Var2, z = z), color = "black") +
  scale_fill_viridis_d()

n_part <- 20
# X <- matrix(runif(n_part * 2) * 5, ncol = 2)
X <- cbind(sample(x, n_part, replace = F), sample(y, n_part, replace = F))
V <- matrix(runif(n_part * 2) * 0.1, ncol = 2)

pbest <- X
pbest_obj <-f_xy(X[,1], X[,2])

gbest <- pbest[which.min(pbest_obj),]
gbest_obj <- min(pbest_obj)

c1 <- c2 <- 0.1
w <- 0.8

# One iteration
V <- w * V + c1*runif(1)*(pbest - X) + c2*runif(1)*(gbest - X)
X <- X + V
obj = f_xy(X[,1], X[,2])

idx <- which(obj >= pbest_obj)
pbest[idx,] <- X[idx,]
pbest_obj[idx] <- obj[idx]

idx <- which.min(pbest_obj)
gbest <- pbest[idx,]
gbest_obj <- min(pbest_obj)


# Final function
pso_optim <- function(obj_func,
                      c1 = 0.05,
                      c2 = 0.05,
                      w = 0.8,
                      n_particles = 20,
                      v_init_fact = 0.1,
                      n_iter = 50, ...){

  x <- seq(min(x), max(x), length.out = 100)
  y <- seq(min(y), max(y), length.out = 100)

  X <- cbind(sample(x, n_particles, replace = F),
             sample(y, n_particles, replace = F))

  V <- matrix(runif(n_particles * 2) * v_init_fact, ncol = 2)

  pbest <- X
  pbest_obj <- obj_func(x = X[,1], y = X[,2])

  gbest <- pbest[which.min(pbest_obj),]
  gbest_obj <- min(pbest_obj)

  loc_df <- data.frame(X, iter = 0)
  iter <- 1

  while(iter < n_iter){

    V <- w * V + c1*rchisq(1, 1, 0)*(pbest - X) + c2*rchisq(1, 1, 0)*(gbest - X)
    X <- X + V

    obj <- obj_func(x = X[,1], y = X[,2])

    idx <- which(obj <= pbest_obj)
    pbest[idx,] <- X[idx,]
    pbest_obj[idx] <- obj[idx]

    idx <- which.min(pbest_obj)
    gbest <- pbest[idx,]
    gbest_obj <- min(pbest_obj)

    # Update iteration
    iter <- iter + 1
    loc_df <- rbind(loc_df, data.frame(X, iter = iter))
    print(gbest)
  }

  lst <- list(X = loc_df, obj = gbest_obj, obj_loc = paste0(gbest, collapse = ","))
  return(lst)
}


out <- pso_optim(f_xy,
                 x = x,
                 y = y,
                 c1 = 0.01,
                 c2 = 0.05,
                 n_particles = 50,
                 w = 0.05,
                 v_init_fact = 0.1,
                 n_iter = 100)

ggplot(out$X) +
  geom_tile(data = grid, aes(x = Var1, y = Var2, fill = z), )  +
  geom_point(aes(X1, X2)) +
  labs(x = "X", y = "Y") +
  transition_time(iter) +
  ease_aes("linear")

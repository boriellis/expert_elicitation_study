

#seeing what a distribution of values between 0-1 would produce like it might for a cell in the raster

# uniform distribution (complete disagreement amoung experts)
set.seed(123)  
x <- runif(25, min = 0, max = 1)

# summary stats
m <- mean(x)
ci <- t.test(x)$conf.int  # 95% CI

# plot histogram with mean & CI
hist(x, breaks = 10)
abline(v = m, col = "red", lwd = 2)
abline(v = ci, col = "blue", lty = 2, lwd = 2)

m; ci



#normal distribution - more agreement amoung experts
library(truncnorm)

set.seed(123)
x <- rtruncnorm(25, a = 0, b = 1, mean = 0.8, sd = 0.1)

summary(x)
# summary stats
m <- mean(x)
ci <- t.test(x)$conf.int  # 95% CI

# plot histogram with mean & CI
hist(x, breaks = 10)
abline(v = m, col = "red", lwd = 2)
abline(v = ci, col = "blue", lty = 2, lwd = 2)

m; ci


#bimodal:

library(truncnorm)

set.seed(123)

n <- 25

# mixture weights (you can adjust)
w1 <- 0.5   # 50% near 0
w2 <- 0.5   # 50% near 0.8

x <- c(
  rtruncnorm(round(n * w1), a = 0, b = 1, mean = 0.05, sd = 0.05),
  rtruncnorm(round(n * w2), a = 0, b = 1, mean = 0.8,  sd = 0.10)
)

logit_x <- log(x / (1 - x))
mean_logit_x <- mean(logit_x)
sd_logit_x <- sd(logit_x)
lwr_logit_x <- mean_logit_x - 1.96 * sd_logit_x
upr_logit_x <- mean_logit_x + 1.96 * sd_logit_x
inv_logit <- function(x) exp(x) / (1 + exp(x))
mean_x <- inv_logit(mean_logit_x)
lwr_x <- inv_logit(lwr_logit_x)
upr_x <- inv_logit(upr_logit_x)

# hist(x, breaks = 10, main = "Bimodal Distribution (Truncated Normals)")
# 
# m <- mean(x)
# ci <- t.test(x)$conf.int  # 95% CI

# plot histogram with mean & CI
hist(x, breaks = 10)
abline(v = mean_x, col = "red", lwd = 2)
abline(v = c(lwr_x, upr_x), col = "blue", lty = 2, lwd = 2)

c(mean_x, lwr_x, upr_x)



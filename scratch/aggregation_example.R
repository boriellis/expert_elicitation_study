#this is from chatgpt using the shelf package. HAVEN'T TRIED IT YET!
library(SHELF)

#3 experts, each provides a mean, lower bound, and upper bound and a confidence that the interval is within the bounds
experts <- data.frame(
  L = c(8, 5, 11),
  M = c(10, 9, 12),
  U = c(12, 20, 13),
  lower_p = c(0.05, 0.10, 0.025),   # expert-specific lower percentile
  upper_p = c(0.95, 0.90, 0.975)    # expert-specific upper percentile
)

# Helper: fit Normal params from quantiles
fit_normal_from_quantiles <- function(L, M, U, pL, pU) {
  zL <- qnorm(pL); zU <- qnorm(pU)
  mean <- M
  sd <- (U - L) / (zU - zL)
  list(mean = mean, sd = sd)
}

# Helper: fit Lognormal params from quantiles (works if L>0)
fit_lognormal_from_quantiles <- function(L, M, U, pL, pU) {
  if(any(c(L, M, U) <= 0)) stop("Lognormal fit requires positive L, M, U")
  zL <- qnorm(pL); zU <- qnorm(pU)
  mu <- log(M)
  sigma <- (log(U) - log(L)) / (zU - zL)
  list(mu = mu, sigma = sigma)
}

# Fit normals and lognormals for each expert
normal_fits <- lapply(1:nrow(experts), function(i) {
  fit_normal_from_quantiles(experts$L[i], experts$M[i], experts$U[i],
                            experts$lower_p[i], experts$upper_p[i])
})

lognormal_fits <- lapply(1:nrow(experts), function(i) {
  fit_lognormal_from_quantiles(experts$L[i], experts$M[i], experts$U[i],
                               experts$lower_p[i], experts$upper_p[i])
})

# Evaluate densities on a grid
x <- seq(0, 30, length = 2000)   # widen range if needed

# Expert normal densities
norm_densities <- sapply(normal_fits, function(f) {
  dnorm(x, mean = f$mean, sd = f$sd)
})

# Expert lognormal densities (on same x)
ln_densities <- sapply(lognormal_fits, function(f) {
  dlnorm(x, meanlog = f$mu, sdlog = f$sigma)
})

# Linear pooling (equal weights)
pooled_norm <- rowMeans(norm_densities)
pooled_ln   <- rowMeans(ln_densities)

# Compute pooled quantiles via numeric CDF
dx <- x[2] - x[1]
pooled_norm_cdf <- cumsum(pooled_norm) * dx
pooled_ln_cdf   <- cumsum(pooled_ln) * dx

get_q <- function(x_grid, cdf, p) x_grid[which.min(abs(cdf - p))]

cat("---- Normal pooling results ----\n")
cat("Pooled normal median:", get_q(x, pooled_norm_cdf, 0.5), "\n")
cat("Pooled normal 90% CI:",
    get_q(x, pooled_norm_cdf, 0.05), "to", get_q(x, pooled_norm_cdf, 0.95), "\n\n")

cat("---- Lognormal pooling results ----\n")
cat("Pooled lognormal median:", get_q(x, pooled_ln_cdf, 0.5), "\n")
cat("Pooled lognormal 90% CI:",
    get_q(x, pooled_ln_cdf, 0.05), "to", get_q(x, pooled_ln_cdf, 0.95), "\n")

# Plot (two panels)
par(mfrow = c(2,1), mar = c(4,4,2,1))
matplot(x, norm_densities, type = "l", lty = 1, col = 1:ncol(norm_densities),
        ylab = "Density", xlab = "Value", main = "Individual expert Normal PDFs")
lines(x, pooled_norm, lwd = 2) ; legend("topright", legend = c(paste0("E",1:ncol(norm_densities)), "Pooled"),
                                        col = c(1:ncol(norm_densities), "black"), lwd = c(rep(1,ncol(norm_densities)),2))

matplot(x, ln_densities, type = "l", lty = 1, col = 1:ncol(ln_densities),
        ylab = "Density", xlab = "Value", main = "Individual expert Lognormal PDFs")
lines(x, pooled_ln, lwd = 2) ; legend("topright", legend = c(paste0("E",1:ncol(ln_densities)), "Pooled"),
                                      col = c(1:ncol(ln_densities), "black"), lwd = c(rep(1,ncol(ln_densities)),2))

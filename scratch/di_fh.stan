data {
  int<lower=1> N;
  int<lower=1> E;
  int<lower=1> M;
  array[M] int<lower=1, upper=N> obs_id;
  array[M] int<lower=1, upper=E> expert_id;
  vector<lower=0, upper=1>[M] fh_best;
  vector<lower=0, upper=1>[N] fh_actual;
}

parameters {
  // Beta part (continuous, for fh_actual > 0)
  vector[E] beta_0;
  simplex[E] beta_1;
  real<lower=0> phi;
  real<lower=0> sigma;

  // Bernoulli part (probability of nonzero)
  vector[E] gamma_0;
  simplex[E] gamma_1;
  real<lower=0> sigma_gamma;
}

model {
  // Priors — Beta part
  sigma ~ exponential(1);
  beta_0 ~ normal(0, sigma);
  beta_1 ~ dirichlet(rep_vector(1.0, E));
  phi ~ exponential(1);

  // Priors — Bernoulli part
  sigma_gamma ~ exponential(1);
  gamma_0 ~ normal(0, sigma_gamma);
  gamma_1 ~ dirichlet(rep_vector(1.0, E));

  // Accumulate linear predictors
  vector[N] lp_beta    = rep_vector(0.0, N);
  vector[N] lp_hurdle  = rep_vector(0.0, N);
  for (m in 1:M) {
    int n = obs_id[m];
    int e = expert_id[m];
    lp_beta[n]   += (beta_0[e]  + fh_best[m]) * beta_1[e];
    lp_hurdle[n] += (gamma_0[e] + fh_best[m]) * gamma_1[e];
  }

  // Likelihood
  for (n in 1:N) {
    real p_nonzero = inv_logit(lp_hurdle[n]);
    if (fh_actual[n] == 0) {
      target += log1m(p_nonzero);
    } else {
      real mu = inv_logit(lp_beta[n]);
      target += log(p_nonzero)
                + beta_lpdf(fh_actual[n] | mu * phi, (1 - mu) * phi);
    }
  }
}

generated quantities {
  vector[N] lp_beta_rep   = rep_vector(0.0, N);
  vector[N] lp_hurdle_rep = rep_vector(0.0, N);
  for (m in 1:M) {
    int n = obs_id[m];
    int e = expert_id[m];
    lp_beta_rep[n]   += (beta_0[e]  + fh_best[m]) * beta_1[e];
    lp_hurdle_rep[n] += (gamma_0[e] + fh_best[m]) * gamma_1[e];
  }
  vector[N] fh_pred;
  for (n in 1:N) {
    real p_nonzero = inv_logit(lp_hurdle_rep[n]);
    if (bernoulli_rng(p_nonzero) == 0) {
      fh_pred[n] = 0;
    } else {
      real mu = inv_logit(lp_beta_rep[n]);
      fh_pred[n] = beta_rng(mu * phi, (1 - mu) * phi);
    }
  }
}

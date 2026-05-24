data {
  int<lower=1> E; // n experts
  int<lower=1> M; // n species
  vector<lower=0, upper=1>[M] Y;
  matrix<lower=0, upper=1>[M, E] X;
}

parameters {
  simplex[E] beta;
  real<lower=0> phi;
}

transformed parameters {
  vector[M] mu_logit;
  vector<lower=0, upper=1>[M] mu;
  
  for (i in 1:M) { // doing the weighted average
    mu_logit[i] = 0.0;
    for (j in 1:E) {
      mu_logit[i] += beta[j] * logit(X[i, j]);
    }
    mu[i] = inv_logit(mu_logit[i]);
  }
}

model {
  beta ~ dirichlet(rep_vector(1.0, E));
  // flat prior for phi

  for (i in 1:M) {
    Y[i] ~ beta(mu[i] * phi, (1 - mu[i]) * phi); #every time we iterate the MC chain we get new betas (weighted avg), apply them to get the new estimate - this is where the model thinks about how close the expert is to the actual 
  }
}

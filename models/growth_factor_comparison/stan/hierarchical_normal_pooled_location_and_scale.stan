
data{
  
  int n;
  int n_state;
  int state[n];
  real y[n];
  
}

parameters {
  
  real alpha;
  real<lower=0> gamma;
  
  real<lower=0> nu;
  
  real mu[n_state];
  real<lower=0> sigma[n_state];
  
}

model {
  
  mu ~ normal(alpha, gamma);
  sigma ~ chi_square(nu);
  
  for (i in 1:n) 
    y[i] ~ normal(mu[state[i]], sigma[state[i]]);
  
}

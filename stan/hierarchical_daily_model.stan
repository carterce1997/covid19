
data {
  int n;
  int n_Region;
  int Region[n];
  real DaysOut[n];
  int y[n];
}

parameters {
  
  real m_mu;
  real<lower=0> m_sigma;
  
  real s_mu;
  real<lower=0> s_sigma;
  
  real<lower=0> A[n_Region];
  real m[n_Region];
  real<lower=0> s[n_Region];
  real<lower=0> phi[n_Region];
  
}

transformed parameters {
  
  real mu[n];
  
  for (i in 1:n) {
    
    mu[i] = A[Region[i]] * exp(- (DaysOut[i] - m[Region[i]]) / s[Region[i]]) / (1 + exp(- (DaysOut[i] - m[Region[i]]) / s[Region[i]])) ^ 2;
    
  }
  
}

model {
  
  m_mu ~ cauchy(0, 10);
  m_sigma ~ cauchy(0, 10);
  
  m ~ normal(m_mu, m_sigma);
  s ~ cauchy(s_mu, s_sigma);
  A ~ cauchy(0, 10);
  phi ~ cauchy(0, 10);
  
  for (i in 1:n)
    target += neg_binomial_2_lpmf(y[i] | mu[i], phi[Region[i]]);
    
}

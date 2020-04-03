


data {
  int n;
  int n_Region;
  int Region[n];
  real DaysOut[n];
  int cumu_n[n];
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
    
    mu[i] = A[Region[i]] / (1 + exp(- (DaysOut[i] - m[Region[i]]) / s[Region[i]]));
    
  }
  
}

model {
  
  m_mu ~ cauchy(7, 10);
  m_sigma ~ cauchy(0, 10);
  
  m ~ normal(m_mu, m_sigma);
  s ~ cauchy(s_mu, s_sigma);
  A ~ cauchy(0, 20);
  phi ~ cauchy(0, 1);
  
  for (i in 1:n)
    target += neg_binomial_2_lpmf(cumu_n[i] | mu[i], phi[Region[i]]);

}

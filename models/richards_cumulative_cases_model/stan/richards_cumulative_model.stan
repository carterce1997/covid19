

data {
 int n;
 real t[n];
 int cumu_y[n];
}

parameters {
  
  real<lower=0> A; // effective carrying capacity
  real m; // curve location
  real<lower=0> s; // curve flatness
  
  real<lower=0> nu; // curve shape
  
  real<lower=0> phi; // auxillary
}

transformed parameters {
  
  real mu[n];
  
  for (i in 1:n)
    mu[i] = A / (1 + exp(- (t[i] - m) / s)) ^ (1 / nu);
    
  
}

model {
  
  nu ~ cauchy(1, 1);
  m ~ cauchy(0, 10);
  s ~ cauchy(0, 10);
  phi ~ cauchy(0, 3);
  A ~ cauchy(0, 20);
  
  cumu_y ~ neg_binomial_2(mu, phi);
  
}

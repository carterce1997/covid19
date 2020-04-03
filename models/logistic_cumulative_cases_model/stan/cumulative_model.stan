

data {
 int n;
 real t[n];
 int cumu_y[n];
}

parameters {
  real<lower=0> A;
  real m;
  real<lower=0> s;
  real<lower=0> phi;
}

transformed parameters {
  
  real mu[n];
  
  for (i in 1:n) {
    
    mu[i] = A / (1 + exp(- (t[i] - m) / s));
    
  }
  
}

model {
  
  m ~ cauchy(0, 20);
  s ~ cauchy(0, 20);
  A ~ cauchy(0, 20);
  phi ~ cauchy(0, 3);

  cumu_y ~ neg_binomial_2(mu, phi);
  
}

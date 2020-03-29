
data {
  
  int n;
  real y[n];
  real cumu_y[n];
  
}

parameters {
  
  real<lower=0> s;
  real<upper=1/max(cumu_y)> inv_A;
  real<lower=0> sigma;

}

transformed parameters {
  
  real mu[n];
  real A;
  
  for (i in 1:n)
    mu[i] = (cumu_y[i] / s) * (1 - cumu_y[i] * inv_A);
    
  A = 1 / inv_A;
  
}

model {
  
  s ~ cauchy(0, 1);
  sigma ~ cauchy(0, 1);
  inv_A ~ cauchy(0, 1);
  
  y ~ normal(mu, sigma);
  
}

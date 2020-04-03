
data {
  
  int n;
  real cumu_y[n];
  real y[n];
  
}

parameters {
  
  real<lower=max(cumu_y)> A;
  real<lower=0> a;
  real<lower=0> inv_nu;
  real<lower=0> sigma;
  
}

transformed parameters {
  
  real mu[n];

  for (i in 1:n)
    mu[i] = a * cumu_y[i] * (1 - (cumu_y[i] / A) ^ inv_nu);
  
}

model {
  
  inv_nu ~ normal(1, 1);
  a ~ cauchy(0, 5);
  sigma ~ cauchy(0, 5);
  A ~ cauchy(0, 10);
  
  y ~ normal(mu, sigma);
  
}

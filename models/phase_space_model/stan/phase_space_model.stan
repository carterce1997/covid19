
data {
  
  int n;
  real y[n];
  real cumu_y[n];
  
}

transformed data {
  
  real log_y[n];
  
  for (i in 1:n) 
    log_y[i] = log(y[i]);
  
}

parameters {
  
  real<lower=0> s;
  real<lower=max(cumu_y)> A;
  real<lower=0> sigma;

}

transformed parameters {
  
  real mu[n];
  // real A;
  
  for (i in 1:n)
    mu[i] = log(cumu_y[i] / s) + log(1 - cumu_y[i] / A);
    
  // A = 1 / inv_A;
  
}

model {
  
  // s ~ cauchy(0, 1);
  sigma ~ cauchy(0, 10);
  A ~ cauchy(max(cumu_y), max(cumu_y));
  // inv_A ~ cauchy(0, 1);
  
  y ~ normal(mu, sigma);
  
}

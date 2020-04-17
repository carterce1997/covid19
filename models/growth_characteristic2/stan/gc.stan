
data {
  
  int n;
  real y[n];
  real x[n];
  
}

parameters {
  
  real<lower=0> A;
  real<lower=0> v;
  real<lower=0> r;
  
  real<lower=0> sigma;
  
}

model {
  
  r ~ cauchy(0, 1);
  v ~  cauchy(1, .1);

  for (i in 1:n)
    y[i] ~ normal(r * (1 - (x[i] / A) ^ v), sigma);
  
}

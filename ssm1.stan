//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  int<lower=0, upper=1> cat[N];
  vector[N] eyex;
  vector[N] eyey;
  vector[N] AU45;
  vector[N] AU12;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector[N] mu;
  real<lower=0> sigma_S;
}

transformed parameters{
  vector<lower=0, upper=1>[N] p;
  p = inv_logit(mu + eyex + eyey + AU45 + AU12);
  //p = inv_logit(mu+eyex);
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  for (i in 2:N)
    mu[i] ~ normal(mu[i-1], sigma_S);
    cat ~ bernoulli(p);
}


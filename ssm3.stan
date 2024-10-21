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
  array[N] int<lower=0> cat;
  array[N] int binom_size;
  vector[N] eyex;
  vector[N] eyey;
  vector[N] faceP;
  vector[N] faceY;
  vector[N] faceR;
  vector[N] rH;
  vector[N] lH;
  vector[N] AU45;
  vector[N] AU12;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector[N] mu;
  real<lower=0> sigma_S;
  //vector<lower=0>[N] prob;
  real Intercept;
  real b_x;
  real b_y;
  real b_P;
  real b_Y;
  real b_R;
  real b_r;
  real b_l;
  real b_45;
  real b_12;
}

transformed parameters{
  vector<lower=0, upper=1>[N] prob;
  //prob = inv_logit(mu + b_x*eyex + b_y*eyey + b_P*faceP + b_Y*faceY+ b_R*faceR + b_r*rH + b_l*lH + b_45*AU45 + b_12*AU12);
  prob = inv_logit(mu + b_x*eyex + b_y*eyey);
}


// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  sigma_S ~ cauchy(0,5);
  for (i in 2:N)
    mu[i] ~ normal(mu[i-1], sigma_S);
  for (i in 1:N)
    cat[i] ~ binomial(binom_size[i], prob[i]);
}

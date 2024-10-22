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
  //int<lower=0, upper=1> cat[N];
  vector[N] pn;
  vector[N] pp;
  vector[N] fn;
  vector[N] fp;
  vector[N] eyex;
  vector[N] eyey;
  vector[N] faceP;
  vector[N] faceY;
  vector[N] faceR;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector[N] mu;
  real<lower=0> sigma_S;
  real<lower=0> sigma_O;
  real k1;
  real k2;
  real k3;
  real k4;
}

transformed parameters{
  vector[N] y;
  //p = inv_logit(mu + b_x*eyex + b_y*eyey + b_P*faceP + b_Y*faceY+ b_R*faceR + b_r*rH + b_l*lH + b_45*AU45 + b_12*AU12);
  // p = inv_logit(mu);
  y = mu + k1*pn + k2*pp + k3*fn + k4*fp; 
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  for (i in 2:N)
    mu[i] ~ normal(mu[i-1], sigma_S);
  faceP ~ normal(y, sigma_O);
}

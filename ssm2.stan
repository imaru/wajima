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
  vector[N] facePitch;
  vector[N] faceYaw;
  vector[N] faceRoll;
  vector[N] rightH;
  vector[N] leftH;
  vector[N] AU45;
  vector[N] AU12;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector[N] mu;
  real<lower=0> sigma_S;
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
  vector<lower=0, upper=1>[N] p;
  p = inv_logit(mu + b_x*eyex + b_y*eyey + b_P*facePitch + b_Y*faceYaw + b_R*faceRoll + b_r*rightH + b_l*leftH + b_45*AU45 + b_12*AU12);
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

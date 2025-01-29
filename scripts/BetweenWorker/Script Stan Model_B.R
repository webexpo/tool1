#######################
#
# STAN model object for TOOL2
#
###################

#
#
# V1.00   20 march 2018
#
#
#

#library(rstan)
##model

modelString = "

data{

int <lower=0> N_workers;
int<lower=0> N_observed;
int<lower=0> N_leftcensored;
int<lower=0> N_rightcensored;
int<lower=0> N_intcensored;
real observed_values[N_observed];
real leftcensored_values[N_leftcensored];
real rightcensored_values[N_rightcensored];
real intcensored_left_values[N_intcensored];
real intcensored_right_values[N_intcensored];
int <lower=1> worker_observed[N_observed];
int <lower=1> worker_leftcensored[N_leftcensored];
int <lower=1> worker_rightcensored[N_rightcensored];
int <lower=1> worker_intcensored[N_intcensored];

}

parameters{
real worker_effect[N_workers];  //worker effect
real logsigma_within;
real logsigma_between;
real mu_group;  //group mean
}

transformed parameters {
real <lower=0> sigma_within;
real <lower=0> sigma_between;
sigma_between = exp(logsigma_between);
sigma_within = exp(logsigma_within);
}

model{

// priors

mu_group~uniform(-20,20);

logsigma_within~normal(-0.4106,0.7254381);

logsigma_between~normal(-0.8786,0.7823012);

for (n in 1:N_workers){
worker_effect[n] ~ normal(0, sigma_between); // Random intercepts
}

//likelihood

for (n in 1:N_observed) {

observed_values[n] ~ normal(mu_group+worker_effect[worker_observed[n]],sigma_within);

}


for (n in 1:N_leftcensored) {

target += normal_lcdf(leftcensored_values[n] | mu_group+worker_effect[worker_leftcensored[n]],sigma_within);

}

for (n in 1:N_rightcensored) {

target += normal_lccdf(rightcensored_values[n] | mu_group+worker_effect[worker_rightcensored[n]],sigma_within);

}

for (n in 1:N_intcensored) {

target += log_diff_exp(
normal_lcdf(intcensored_right_values[n] | mu_group+worker_effect[worker_intcensored[n]], sigma_within),
normal_lcdf(intcensored_left_values[n]  | mu_group+worker_effect[worker_intcensored[n]], sigma_within)
);

}

}




" # close quote for modelString


stanmodel.B <- stan_model( model_code=modelString )

saveRDS(stanmodel.B,"C:/jerome/Dropbox/bureau/RStudio/expostats/shinyapps/unified functions/Between-worker/stanmodel.B.RDS")

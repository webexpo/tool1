#######################
#
# STAN model object for TOOL1
#
###################

# V1.00   20 march 2018

#library(rstan)

########STAN censored model multiply censored


modelString = "

data{
int<lower=0> N_observed;
int<lower=0> N_leftcensored;
int<lower=0> N_rightcensored;
int<lower=0> N_intcensored;
real observed_values[N_observed];
real leftcensored_values[N_leftcensored];
real rightcensored_values[N_rightcensored];
real intcensored_left_values[N_intcensored];
real intcensored_right_values[N_intcensored];

}

parameters{
real mu;
real logsigma;
}

transformed parameters {
real <lower=0> sigma;
sigma = exp(logsigma);
}

model{

mu~uniform(-20,20);

logsigma~normal(-0.1744,0.6259421);

observed_values~normal(mu,sigma);

for (n in 1:N_leftcensored) {

target += normal_lcdf(leftcensored_values[n] | mu, sigma);

}

for (n in 1:N_rightcensored) {

target += normal_lccdf(rightcensored_values[n] | mu, sigma);

}

for (n in 1:N_intcensored) {

target += log_diff_exp(normal_lcdf(intcensored_right_values[n] | mu, sigma),normal_lcdf(intcensored_left_values[n] | mu, sigma));

}

}




" # close quote for modelString


stanmodel.seg <- stan_model( model_code=modelString )

saveRDS(stanmodel.seg,"C:/jerome/Dropbox/bureau/RStudio/expostats/shinyapps/expostats functions 2018/Common functions/stanmodel.seg.RDS")

#######################
#
# STAN model object for TOOL3 : determinant functions
#
###################

#
#
# V1.00   20 march 2018
#
#

#library(rstan)
##model

modelString = "

data{

int <lower=0> N_group;
int<lower=0> N_observed;
int<lower=0> N_leftcensored;
int<lower=0> N_rightcensored;
int<lower=0> N_intcensored;
real observed_values[N_observed];
real leftcensored_values[N_leftcensored];
real rightcensored_values[N_rightcensored];
real intcensored_left_values[N_intcensored];
real intcensored_right_values[N_intcensored];
int <lower=1> group_observed[N_observed];
int <lower=1> group_leftcensored[N_leftcensored];
int <lower=1> group_rightcensored[N_rightcensored];
int <lower=1> group_intcensored[N_intcensored];

}

parameters{
real group_mean[N_group];  //group specific mean
real logsigma[N_group];  //group specific SD

}

transformed parameters {
real <lower=0> group_sigma[N_group];
group_sigma = exp(logsigma);
}

model{

// priors

for (n in 1:N_group){

group_mean[n]~uniform(-20,20);

logsigma[n]~normal(-0.1744,0.6259421);

}

//likelihood

for (n in 1:N_observed) {

observed_values[n] ~ normal(group_mean[group_observed[n]],group_sigma[group_observed[n]]);

}


for (n in 1:N_leftcensored) {

target += normal_lcdf(leftcensored_values[n] | group_mean[group_leftcensored[n]],group_sigma[group_leftcensored[n]]);

}

for (n in 1:N_rightcensored) {

target += normal_lccdf(rightcensored_values[n] | group_mean[group_rightcensored[n]],group_sigma[group_rightcensored[n]]);

}

for (n in 1:N_intcensored) {

target += log_diff_exp(
normal_lcdf(intcensored_right_values[n] | group_mean[group_intcensored[n]],group_sigma[group_intcensored[n]]),
normal_lcdf(intcensored_left_values[n]  | group_mean[group_intcensored[n]],group_sigma[group_intcensored[n]])
);

}

}




" # close quote for modelString


stanmodel.D<- stan_model( model_code=modelString )

saveRDS(stanmodel.D,"C:/jerome/Dropbox/bureau/RStudio/expostats/shinyapps/expostats functions 2018/Determinant specific functions/stanmodel.D.RDS")

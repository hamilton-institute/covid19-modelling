# The information and material published herein are provided for general information 
# and educative purposes only. The content does not constitute recommendation of 
# Novartis or any of its affiliates. Neither Novartis nor its affiliates make statements, 
# representations or warranties about the accuracy, reliability, correctness or 
# completeness of the published information and material

# Stan version of Hamilton JAGS SIR model
#
# Brian Buckley July 2020
# -----------------------------------------------------------------------------------

stan_code = '
data {
  // Population size
  int<lower=1> N;
  // Time (days)
  int<lower=0> T;
  int<lower=0> T_max;
  // Assume the number of people infected on day 1 is the same as those transitioning from E to I
  int<lower=0> I_start; 
  // As above but for removed
  int<lower=0> R_start;
  int N_I_R[T];
  int N_S_I[T]; 
}

transformed data {
  int S[T];
  int I[T];
  int R[T];

  // Need a value for S[1] and I[1]
  I[1] = I_start; // Assume the number of people infected on day 1 is the same as those transitioning from E to I
  R[1] = R_start; // As above but for removed
  S[1] = N - I[1] - R[1]; // Left over

  // These are the known time evolution steps
  for(t in 2:T) {
    S[t] = S[t-1] - N_S_I[t-1];
    I[t] = I[t-1] + N_S_I[t-1] - N_I_R[t-1];
    R[t] = N - S[t] - I[t];
  }
}

parameters {
  // Prior on the hyper-parameters
  real<lower=0.3, upper=1> beta;
  real<lower=9, upper=12> gamma_inv;
}

transformed parameters {
  real gamma;
  real<lower=0, upper=1> p_I_R;
  real<lower=0, upper=1> p_S_I;
  real mu_IR;
  real mu_SI;
  real sigma_IR;
  real sigma_SI;
  real R_0;
  gamma = 1/gamma_inv;

  p_S_I = 1 - exp( -beta );
  p_I_R = 1 - exp( -gamma );
  R_0 = (beta/gamma);

  for(t in 1:T) {
    mu_IR = I[t] * p_I_R;
    mu_SI = S[t] * p_S_I;
    sigma_IR = sqrt(fabs(I[t] * p_I_R * (1 - p_I_R)));
    sigma_SI = sqrt(fabs(S[t] * p_S_I * (1 - p_S_I)));
  }
}

model {
  beta ~ uniform(0.3, 1); 
  gamma_inv ~ uniform(6, 12);

  // Likelihood - using the Normal approximation to the Binomial
  for (t in 1:T) {
    N_I_R[t] ~ normal(mu_IR, sigma_IR);
    N_S_I[t] ~ normal(mu_SI, sigma_SI);
  }

}

generated quantities {
  int S_hat[T] = S;
  int I_hat[T] = I;
  int R_hat[T] = R;
}
'

### Fit to simulated data

# Summary values
N = 1000 # Population size
T = 30 # Maximum time steps
t = 1:T

# First the hyper-parameters
gamma_inv = 10
gamma = 1/gamma_inv
beta = 0.6

# Now the probabilities
p_I_R = 1 - exp(-gamma)
p_S_I = 1 - exp(-beta)

# Give an initial values for everthing required.  Note Stan does not accept NA.
N_S_I = N_I_R = S = I = R = rep(0, T)

S[1] = N - 10
I[1] = 10
R[1] = 0

# For Stan we use the normal appromimation to the binomial
N_I_R[1] = as.integer(rnorm(1, I[1]*p_I_R, sqrt(abs(I[1]*p_I_R*(1-p_I_R)))))
N_S_I[1] = as.integer(rnorm(1, S[1]*p_S_I, sqrt(abs(S[1]*p_S_I*(1-p_S_I)))))

# Now can loop through filling in the other values
for (t in 2:T) {
  S[t] <- S[t-1] - N_S_I[t-1]
  I[t] <- I[t-1] + N_S_I[t-1] - N_I_R[t-1]
  R[t] = N - S[t] - I[t] 
  
  # Normal approximation to binomial
  N_I_R[t] = as.integer(rnorm(1, I[t]*p_I_R, sqrt(abs(I[t]*p_I_R*(1-p_I_R)))))
  N_S_I[t] = as.integer(rnorm(1, S[t]*p_S_I, sqrt(abs(S[t]*p_S_I*(1-p_S_I)))))
}

# Create R0
R_0 = (beta/gamma)

# Create a plot of the epidemic
library(ggplot2)
library(tidyr)
tibble(t = 1:T,
       S, I, R) %>%
  pivot_longer(names_to = 'Compartment', values_to = 'People', -t) %>%
  ggplot(aes(x = t, y = People, colour = Compartment)) +
  geom_line()

N_future = 50 # Forecast 100 days into the future
T_max = T + N_future

# normal distribution cannot have 0 sd
N_S_I[N_S_I==0] <-1

# Cannot add the empty cells for forecast
stan_data = list(N = N,
                 T = T,
                 T_max = T_max,
                 N_S_I = N_S_I,
                 N_I_R = N_I_R,
                 I_start = I[1],
                 R_start = R[1]
)

library(rstan)
# Enable parallel running if available:
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

mod = stan_model(model_code = stan_code)
stan_samp = sampling(mod,data = stan_data, iter = 10000, warmup = 2000, thin = 10)

# Diagnostics - see https://mc-stan.org/rstan/reference/stan_plot_diagnostics.html
stan_diag(stan_samp, info = 'sample')
stan_par(stan_samp, par = "beta")
stan_par(stan_samp, par = "gamma_inv")
stan_par(stan_samp, par = "R_0")

# Now plot the number of infected over time:
# ---------------------------------------------------------------------------------
  
S_post <- colMeans(extract(stan_samp, "S_hat")$S_hat)
I_post <- colMeans(extract(stan_samp, "I_hat")$I_hat)
R_post <- colMeans(extract(stan_samp, "R_hat")$R_hat)

tibble(t = 1:T,
       S_post, I_post, R_post) %>%
  pivot_longer(names_to = 'Compartment', values_to = 'People', -t) %>%
  ggplot(aes(x = t, y = People, colour = Compartment)) +
  geom_line()

# Now plot the parameter estimates against their true values
# ---------------------------------------------------------------------------------

beta_post <- extract(stan_samp, "beta")$beta
gamma_inv_post <- extract(stan_samp, "gamma_inv")$gamma_inv
r_0_post <- extract(stan_samp, "R_0")$R_0

tibble(iter = 1:length(beta_post),
       beta = beta_post,
       gamma_inv = gamma_inv_post,
       R0 = r_0_post) %>%
  pivot_longer(names_to = 'Type', values_to = 'Sample',-iter) %>%
  ggplot(aes(x = Sample, fill = Type)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ Type, scales = 'free') +
  geom_vline(data = tibble(iter = rep(1, 3),
                           Sample = c(beta, gamma_inv, R_0),
                           Type = c('beta', 'gamma_inv', 'R0')),
             aes(xintercept = Sample)) +
  theme(legend.position = 'None')
ggsave(file = 'stan_plot_not_good_20200714.pdf')

# Try Shiny Stan
# ---------------------------------------------------------------------------------
library(shinystan)
launch_shinystan(stan_samp)

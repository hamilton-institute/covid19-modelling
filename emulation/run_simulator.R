## Function to run the simulator for the SEIR model for time to extinction

run_simulator = function (R0, E, I, R, num_proc = 3, n_runs = 1000) {
  
  # Required packages
  require(foreach)
  require(doParallel)
  registerDoParallel(num_proc)
  
  # Source in functions
  source('seir_extinct.R')
  source('utils.R')
  
  gamma_E = 6.6 # Best guesses - should total about 14 days
  gamma_I = 7.4
  S_1 = 4.9*10^6 # Population of Ireland
  
  # Starting matrix
  N = matrix(0, nrow = 1, ncol = 4) # a matrix to store number of people in each compartment
  
  # equal mean holding times for E and I compartments (mean(E+I) = 14days)
  mean_holding_times = c(gamma_E, gamma_I)  # mean holding times at compartment E and I
  total_holding_times = sum(mean_holding_times)
  beta = R0 / c(total_holding_times, total_holding_times)
  
  ##### Assign initial conditions
  N[1,1] = S_1
  N[1,2] = E
  N[1,3] = I
  N[1,4] = R
  
  # initialise simulation condition
  t = 0
  dt = 1 # time increment in days
  t_phase = Inf
  
  ##### Run simulation
  times = foreach(i=1:n_runs, .combine=c) %dopar%
    seir_model(t_phase, t, dt, N, mean_holding_times, beta)
  
  # Return output
  out = c(R0, E, I, R, 
          quantile(times, 0.05),
          quantile(times, 0.1),
          quantile(times, 0.25),
          quantile(times, 0.5),
          quantile(times, 0.75),
          quantile(times, 0.9),
          quantile(times, 0.95))
  
  return(out)
  
}

# ans = run_simulator(0.55,347,437,884858)
# ans = run_simulator(3.35512463259511,494,225,847664)

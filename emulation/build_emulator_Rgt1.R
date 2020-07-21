# In this code I will run the SEIR model to predict the sample quantiles, 5%, 10%, 25%, 50%, 75%, 90% and 95% extinction time for an epidemic. 

# NOTE: I'm building different emulators for R0 < 1 and R0 >= 1 as these will have different behaviours

# This will all be based on the following parameters:

# Fixed parameters
# Holding time at E = 6.6
# Holding time at I = 7.4
# Population = 4.9*10^6

# Variable parameters
# R0 - (0.1, 0.9 by 0.1) and (1.0 to 6 by 0.1)
# #E = (1, 500, by = 10)
# #I = (1, 500, by = 10)
# #R = (50k, 1000k by = 50k)

# The plan is:
# 1. Run the model at the extremes
# 2. Fit GP
# 3. Output all the input parameters and the quantiles
# 4. Find the input associated with the biggest variance in the GP
# 5. Run the model again with the new recommend input parameters
# 6. Repeat from 2
# 7. Stop when it's bored


# Set up ------------------------------------------------------------------

# Clear workspace and load in packages
rm(list = ls())
library(GPfit)
library(lhs)

# Source in simulator
source('run_simulator.R')

# Set up fixed parameters
n_runs = 1000 # Total number of runs - doubt we'll ever hit this

# Create initial grid -----------------------------------------------------

# Starting grid is based on latin hypercube sample
R0_range = c(1.1,6)
E_range = I_range = c(1, 500)
R_range = c(5*10^3, 1000*10^3)

n_grid = 20
initial_grid = maximinLHS(20, 4)

# Start run ---------------------------------------------------------------

# Create output file
cat(c("R0", "E", "I", "R", 
      "q5", "q10", "q25", "q50", "q75", "q90", "q95",
      '\n'),
    sep = ',',
    file = 'out_gt1.txt',
    append = FALSE)

for (run in 1:n_runs) {
  print(run)
  
  # Get the current values = get from initial grid first
  if(run <= n_grid) {
    R0 = initial_grid[run, 1]*diff(range(R0_range)) + min(R0_range)
    E = round(initial_grid[run, 2]*diff(range(I_range)) + min(I_range))
    I = round(initial_grid[run, 3]*diff(range(E_range)) + min(E_range))
    R = round(initial_grid[run, 4]*diff(range(R_range)) + min(R_range))
  } else {
    # Stuff to do here if we're not on initial grid
    R0 = new_input[1]
    E = new_input[2]
    I = new_input[3]
    R = new_input[4]
  }
  
  # Run the simulator
  f_x = run_simulator(R0,E,I,R)
  
  # Create holder for everything
  cat(f_x,'\n',
      sep = ',',
      file = 'out_gt1.txt',
      append = TRUE)
  
  # If we're outside of the initial grid build an emulator based on the 50% quantile
  if(run >= n_grid) {
    # Load in the output file
    curr_set = read.csv('out_gt1.txt')
    x_raw = x = curr_set[,1:4] # Need to normalize these to 0,1
    x[,1] = (x_raw[,1] - min(R0_range))/diff(range(R0_range))
    x[,2] = (x_raw[,2] - min(E_range))/diff(range(E_range))
    x[,3] = (x_raw[,3] - min(I_range))/diff(range(I_range))
    x[,4] = (x_raw[,4] - min(R_range))/diff(range(R_range))
    y = curr_set[,'q50']
    GPmodel = GP_fit(x, y)
    
    # Create a big random grid and find the biggest variance
    big_grid = matrix(runif(200*4), ncol = 4)
    pred_vals = predict(GPmodel, big_grid)
    # Find the biggest MSE
    big_mse = which.max(pred_vals$MSE)
    inputs = big_grid[big_mse,]
    new_input = c(
      inputs[1]*diff(range(R0_range)) + min(R0_range),
      round(inputs[2]*diff(range(E_range)) + min(E_range)),
      round(inputs[3]*diff(range(I_range)) + min(I_range)),
      round(inputs[4]*diff(range(R_range)) + min(R_range))
    )
    
  }
  
}

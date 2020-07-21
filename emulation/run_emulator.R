# Run the latest emulator function

run_emulator = function(R0, E, I, R) {
  
  require(GPfit)
  
  # Probably need some error checking for these to make sure they're not outside the range
  
  # Get the ranges of the parameters
  E_range = I_range = c(1, 500)
  R_range = c(5*10^3, 1000*10^3)
  
  # Load in the right emulator
  if(R0 < 1) {
    emu = readRDS("GP_models_lt1.rds")
    R0_range = c(0.1,0.99)
    input = matrix(NA, nrow = 1, ncol = 4)
    input[,1] = (R0 - min(R0_range))/diff(range(R0_range))
    input[,2] = (E - min(E_range))/diff(range(E_range))
    input[,3] = (I - min(I_range))/diff(range(I_range))
    input[,4] = (R - min(R_range))/diff(range(R_range))
  } else if(R0 > 1) {
    emu = readRDS("GP_models_gt1.rds")
    R0_range = c(1,6)
    input = matrix(NA, nrow = 1, ncol = 4)
    input[,1] = (R0 - min(R0_range))/diff(range(R0_range))
    input[,2] = (E - min(E_range))/diff(range(E_range))
    input[,3] = (I - min(I_range))/diff(range(I_range))
    input[,4] = (R - min(R_range))/diff(range(R_range))
  } else {
    # If R0 equal to 1 there's no R0 in the GP
    emu = readRDS("GP_models_eq1.rds")
    input = matrix(NA, nrow = 1, ncol = 3)
    input[,1] = (E - min(E_range))/diff(range(E_range))
    input[,2] = (I - min(I_range))/diff(range(I_range))
    input[,3] = (R - min(R_range))/diff(range(R_range))
  }
    
  
  # Now produce the predictions for each emulator
  # "q5", "q10", "q25", "q50", "q75", "q90", "q95",
  out = rep(NA, 7)
  # Specify the inputs - corrected for their shape
  
  # Now produce the predictions
  for (i in 1:7) out[i] = round(predict(emu[[i]], input)$Y_hat) # rounding to nearest day
  names(out) = c("q5", "q10", "q25", "q50", "q75", "q90", "q95")
  
  # Check for weird values - i.e. not in order
  if(any(diff(order(out))<0)) warning('Some values not in correct order - more emulator runs required')
  
  
  return(out)
}

# source('run_simulator.R')
# truth = run_simulator(0.55,347,437,884858)
# pred = run_emulator(0.55,347,437,884858)

# truth = run_simulator(1.1,25,35,300000)
# pred = run_emulator(1.1,25,35,300000)

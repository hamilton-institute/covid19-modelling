# Fit the emulators for the latest data set

# Clear workspace and load parameters
rm(list = ls())
library(GPfit)
library(BART)

# R0 < 1 ------------------------------------------------------------------

R0_range = c(0.1,0.99)
E_range = I_range = c(1, 500)
R_range = c(5*10^3, 1000*10^3)

curr_set = read.csv('out_lt1.txt')
x_raw = x = curr_set[,1:4] # Need to normalize these to 0,1
x[,1] = (x_raw[,1] - min(R0_range))/diff(range(R0_range))
x[,2] = (x_raw[,2] - min(E_range))/diff(range(E_range))
x[,3] = (x_raw[,3] - min(I_range))/diff(range(I_range))
x[,4] = (x_raw[,4] - min(R_range))/diff(range(R_range))

# Now build a model based on each of the quantiles
# "q5", "q10", "q25", "q50", "q75", "q90", "q95",
# GP_models_lt1 = list(m5 = GP_fit(x, curr_set[,'q5']),
#                      m10 = GP_fit(x, curr_set[,'q10']),
#                      m25 = GP_fit(x, curr_set[,'q25']),
#                      m50 = GP_fit(x, curr_set[,'q50']),
#                      m75 = GP_fit(x, curr_set[,'q75']),
#                      m90 = GP_fit(x, curr_set[,'q90']),
#                      m95 = GP_fit(x, curr_set[,'q95']))
# saveRDS(GP_models_lt1, file = 'GP_models_lt1.rds')

BART_models_lt1 = list(m5 = wbart(x, curr_set[,'q5']),
                       m10 = wbart(x, curr_set[,'q10']),
                       m25 = wbart(x, curr_set[,'q25']),
                       m50 = wbart(x, curr_set[,'q50']),
                       m75 = wbart(x, curr_set[,'q75']),
                       m90 = wbart(x, curr_set[,'q90']),
                       m95 = wbart(x, curr_set[,'q95']))
saveRDS(BART_models_lt1, file = 'BART_models_lt1.rds')

# R0 > 1 ------------------------------------------------------------------

R0_range = c(1,6)
E_range = I_range = c(1, 500)
R_range = c(5*10^3, 1000*10^3)

curr_set = read.csv('out_gt1.txt')
x_raw = x = curr_set[,1:4] # Need to normalize these to 0,1
x[,1] = (x_raw[,1] - min(R0_range))/diff(range(R0_range))
x[,2] = (x_raw[,2] - min(E_range))/diff(range(E_range))
x[,3] = (x_raw[,3] - min(I_range))/diff(range(I_range))
x[,4] = (x_raw[,4] - min(R_range))/diff(range(R_range))

# Now build a model based on each of the quantiles
# "q5", "q10", "q25", "q50", "q75", "q90", "q95",
# GP_models_gt1 = list(m5 = GP_fit(x, curr_set[,'q5']),
#                      m10 = GP_fit(x, curr_set[,'q10']),
#                      m25 = GP_fit(x, curr_set[,'q25']),
#                      m50 = GP_fit(x, curr_set[,'q50']),
#                      m75 = GP_fit(x, curr_set[,'q75']),
#                      m90 = GP_fit(x, curr_set[,'q90']),
#                      m95 = GP_fit(x, curr_set[,'q95']))
# saveRDS(GP_models_gt1, file = 'GP_models_gt1.rds')

BART_models_gt1 = list(m5 = wbart(x, curr_set[,'q5']),
                       m10 = wbart(x, curr_set[,'q10']),
                       m25 = wbart(x, curr_set[,'q25']),
                       m50 = wbart(x, curr_set[,'q50']),
                       m75 = wbart(x, curr_set[,'q75']),
                       m90 = wbart(x, curr_set[,'q90']),
                       m95 = wbart(x, curr_set[,'q95']))
saveRDS(BART_models_gt1, file = 'BART_models_gt1.rds')

# R0 = 1 ------------------------------------------------------------------

R0_range = 1
E_range = I_range = c(1, 500)
R_range = c(5*10^3, 1000*10^3)

curr_set = read.csv('out_eq1.txt')
x_raw = x = curr_set[,1:3] # Need to normalize these to 0,1
x[,1] = (x_raw[,1] - min(E_range))/diff(range(E_range))
x[,2] = (x_raw[,2] - min(I_range))/diff(range(I_range))
x[,3] = (x_raw[,3] - min(R_range))/diff(range(R_range))

# Now build a model based on each of the quantiles
# "q5", "q10", "q25", "q50", "q75", "q90", "q95",
# GP_models_eq1 = list(m5 = GP_fit(x, curr_set[,'q5']),
#                      m10 = GP_fit(x, curr_set[,'q10']),
#                      m25 = GP_fit(x, curr_set[,'q25']),
#                      m50 = GP_fit(x, curr_set[,'q50']),
#                      m75 = GP_fit(x, curr_set[,'q75']),
#                      m90 = GP_fit(x, curr_set[,'q90']),
#                      m95 = GP_fit(x, curr_set[,'q95']))
# saveRDS(GP_models_eq1, file = 'GP_models_eq1.rds')

BART_models_eq1 = list(m5 = wbart(x, curr_set[,'q5']),
                       m10 = wbart(x, curr_set[,'q10']),
                       m25 = wbart(x, curr_set[,'q25']),
                       m50 = wbart(x, curr_set[,'q50']),
                       m75 = wbart(x, curr_set[,'q75']),
                       m90 = wbart(x, curr_set[,'q90']),
                       m95 = wbart(x, curr_set[,'q95']))
saveRDS(BART_models_eq1, file = 'BART_models_eq1.rds')



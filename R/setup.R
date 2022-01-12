FILES        = paste0("R/", c("helper.R", "probit-regression.R"))
BASE_SEED    = 31415L
L2SENS       = c(0.01, 0.05, 0.1)
EPSILON      = seq(0.1, 0.5, 0.1)
DELTA        = seq(0.1, 0.5, 0.1)

# Set TEST variable in `../simulation.R` to control if a test or the full benchmark is run:
if (TEST) {
  REPETITIONS = 10L
} else {
  REPETITIONS  = 1000L
}

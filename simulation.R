TEST = FALSE

source(here::here("R/setup.R"))

SRC_DIR        = here::here("R/")
BATCHTOOLS_DIR = here::here("batchtools/")

## Batchtools
## ===========================================================

library(batchtools)

if (FALSE) unlink("batchtools", recursive = TRUE)

if (dir.exists(BATCHTOOLS_DIR)) {

  loadRegistry(BATCHTOOLS_DIR, writeable = TRUE, work.dir = here::here())
  submitJobs(findNotDone())

} else {
  reg = makeExperimentRegistry(
    file.dir = BATCHTOOLS_DIR,
    packages = c("checkmate", "mlr"),
    source   = FILES,
    seed     = 31415)

  reg$cluster.functions = makeClusterFunctionsMulticore(ncpus = parallel::detectCores() - 1)

  saveRegistry(reg)

  source(paste0(SRC_DIR, "add-experiments.R"))
  submitJobs()
}

if (FALSE) {
  getJobStatus()

  out = reduceResultsList()
  dtmp = do.call(rbind, lapply(out, function(a) do.call(rbind, lapply(a, function(b) b$aucs))))

  library(dplyr)

  dtmp %>%
    group_by(l2sens, epsilon, delta) %>%
    summarize(delta_ci = mean(noise)) %>%
    as.data.frame()
}

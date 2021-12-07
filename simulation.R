source(here::here("R/setup.R"))

SRC_DIR        = here::here("R/")
BATCHTOOLS_DIR = here::here("batchtools/")

## Batchtools
## ===========================================================

library(batchtools)

unlink("batchtools", recursive = TRUE)

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
}

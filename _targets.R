# _targets.R
# Pipeline definition — run tar_make() in the console to execute

library(targets)
library(tarchetypes)

tar_option_set(packages = c("tidyverse", "readxl", "here"))

source("R/utils.R")
source("R/ingest.R")

list(

  # Track the raw file — pipeline reruns if this file changes
  tar_file(raw_abs_employment, "data/raw/61600_DO003_260221.xlsx"),

  # Ingest: read, merge, tidy the ABS employment data
  tar_target(
    emp_sa2_tidy,
    ingest_abs_employment(raw_abs_employment)
  )

  # Future targets slot in here as you build out the project:
  # tar_target(sa2_panel, build_panel(emp_sa2_tidy, ...)),
  # tar_target(sc_results, run_synthetic_control(sa2_panel)),
  # tar_target(welfare_estimates, compute_welfare(sc_results)),
)
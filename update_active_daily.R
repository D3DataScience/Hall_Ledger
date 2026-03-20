# Daily updater for active players, current-season FanGraphs refreshes, and regenerated player pages.
args <- commandArgs(trailingOnly = TRUE)

# Project root for the daily update run.
project_root <- getwd()

# Shared sourced-script runner.
run_script <- function(path) {
  message("")
  message("Running ", path)
  sys.source(path, envir = new.env(parent = globalenv()))
  message("Finished ", path)
}

# Daily update steps for active-player refreshes.
scripts_to_run <- c(
  file.path("Scripts", "data", "build_active_hall_candidates.R"),
  file.path("Scripts", "data", "refresh_current_fangraphs_season.R"),
  file.path("Scripts", "site", "generate_player_pages.R")
)

message("Hall Ledger active update starting from ", project_root)

# Run each daily update script in order.
for (script_path in scripts_to_run) {
  if (!file.exists(script_path)) {
    stop("Required script not found: ", script_path, call. = FALSE)
  }

  run_script(script_path)
}

message("")
message("Hall Ledger active update complete.")

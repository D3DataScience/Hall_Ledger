# Main full-site rebuild entry point for Hall Ledger.
args <- commandArgs(trailingOnly = TRUE)

refresh_photos <- "--refresh-photos" %in% args

# Project root for the rebuild run.
project_root <- getwd()

# Shared sourced-script runner.
run_script <- function(path) {
  message("")
  message("Running ", path)
  sys.source(path, envir = new.env(parent = globalenv()))
  message("Finished ", path)
}

# Core data rebuild steps.
scripts_to_run <- c(
  file.path("Scripts", "data", "hof_data_setup.R"),
  file.path("Scripts", "data", "build_non_hof_milestone_players.R"),
  file.path("Scripts", "data", "build_active_hall_candidates.R"),
  file.path("Scripts", "data", "build_corrective_induction_players.R"),
  file.path("Scripts", "data", "build_site_data_fangraphs_api.R")
)

if (refresh_photos) {
  # Optional photo refresh steps.
  scripts_to_run <- c(
    scripts_to_run,
    file.path("Scripts", "photos", "build_player_photo_manifest.R"),
    file.path("Scripts", "photos", "discover_player_photos_mlb.R"),
    file.path("Scripts", "photos", "download_player_photos_from_manifest.R")
  )
}

# Final page generation step.
scripts_to_run <- c(
  scripts_to_run,
  file.path("Scripts", "site", "generate_player_pages.R")
)

message("Hall Ledger rebuild starting from ", project_root)

# Run each rebuild script in order.
for (script_path in scripts_to_run) {
  if (!file.exists(script_path)) {
    stop("Required script not found: ", script_path, call. = FALSE)
  }

  run_script(script_path)
}

message("")
message("Hall Ledger rebuild complete.")

# One-step active-player update for Hall Ledger.
args <- commandArgs(trailingOnly = TRUE)

project_root <- getwd()

run_script <- function(path) {
  message("")
  message("Running ", path)
  sys.source(path, envir = new.env(parent = globalenv()))
  message("Finished ", path)
}

suppressWarnings({
  Sys.setenv(
    HALL_LEDGER_INCLUDE_CURRENT_SEASON = "1",
    HALL_LEDGER_REFRESH_CURRENT_SEASON = "1"
  )
})

scripts_to_run <- c(
  file.path("Scripts", "data", "build_active_hall_candidates.R"),
  file.path("Scripts", "data", "build_site_data_fangraphs_api.R"),
  file.path("Scripts", "site", "generate_player_pages.R")
)

message("Hall Ledger active daily update starting from ", project_root)

for (script_path in scripts_to_run) {
  if (!file.exists(script_path)) {
    stop("Required script not found: ", script_path, call. = FALSE)
  }

  run_script(script_path)
}

message("")
message("Hall Ledger active daily update complete.")

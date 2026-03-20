# Scheduler-safe wrapper that only runs the active daily update inside a fixed Eastern time window.
project_root <- getwd()

# Eastern-time schedule window for the active update.
target_tz <- "America/New_York"
start_date <- as.Date("2026-03-27")
end_date <- as.Date("2026-11-10")
target_hour <- 23L
target_minute <- 0L
state_path <- file.path("data", "active_daily_window_state.txt")

dir.create("data", showWarnings = FALSE)

# Current time converted into the target schedule zone.
now_et <- as.POSIXlt(Sys.time(), tz = target_tz)
today_et <- as.Date(format(now_et, "%Y-%m-%d", tz = target_tz))
hour_et <- as.integer(format(now_et, "%H", tz = target_tz))
minute_et <- as.integer(format(now_et, "%M", tz = target_tz))

message("Hall Ledger scheduled active update check")
message("Project root: ", project_root)
message("Current Eastern time: ", format(now_et, "%Y-%m-%d %I:%M:%S %p %Z", tz = target_tz))

if (today_et < start_date || today_et > end_date) {
  message("Outside scheduled date window. No update run.")
  quit(save = "no")
}

if ((hour_et < target_hour) || (hour_et == target_hour && minute_et < target_minute)) {
  # Stop early until the nightly run time is reached.
  message("Before the scheduled 11:00 PM Eastern run time. No update run.")
  quit(save = "no")
}

# Read the last successful Eastern-date run.
last_run_date <- if (file.exists(state_path)) {
  trimws(paste(readLines(state_path, warn = FALSE), collapse = ""))
} else {
  ""
}

today_et_chr <- as.character(today_et)

if (identical(last_run_date, today_et_chr)) {
  message("Update already ran for ", today_et_chr, " Eastern. No second run needed.")
  quit(save = "no")
}

# Hand off to the normal daily updater and record the run date.
message("Running update_active_daily.R")
sys.source("update_active_daily.R", envir = new.env(parent = globalenv()))
writeLines(today_et_chr, state_path)

message("Recorded successful run for ", today_et_chr, " Eastern.")

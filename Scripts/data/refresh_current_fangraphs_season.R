# Wrapper that refreshes the current FanGraphs season cache without changing the main builder defaults.
suppressWarnings({
  Sys.setenv(
    HALL_LEDGER_INCLUDE_CURRENT_SEASON = "1",
    HALL_LEDGER_REFRESH_CURRENT_SEASON = "1"
  )
})

sys.source(
  file.path("Scripts", "data", "build_site_data_fangraphs_api.R"),
  envir = new.env(parent = globalenv())
)

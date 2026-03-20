# Hall Ledger player photo manifest builder
# Builds the sitewide photo manifest used for image discovery and downloads.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tibble)
})

dir.create("data", showWarnings = FALSE)

hall_players <- if (file.exists("data/hall_of_fame_lahman_players.csv")) {
  read_csv("data/hall_of_fame_lahman_players.csv", show_col_types = FALSE) %>%
    transmute(playerID, name, source_group = "Official Inductions", bbref_url)
} else {
  tibble(playerID = character(), name = character(), source_group = character(), bbref_url = character())
}

milestone_players <- if (file.exists("data/non_hof_milestone_players.csv")) {
  read_csv("data/non_hof_milestone_players.csv", show_col_types = FALSE) %>%
    transmute(playerID, name, source_group = "Statistical Inductions", bbref_url)
} else {
  tibble(playerID = character(), name = character(), source_group = character(), bbref_url = character())
}

active_players <- if (file.exists("data/active_hall_candidates.csv")) {
  read_csv("data/active_hall_candidates.csv", show_col_types = FALSE) %>%
    transmute(playerID, name, source_group = "Active Inductions", bbref_url)
} else {
  tibble(playerID = character(), name = character(), source_group = character(), bbref_url = character())
}

retired_players <- if (file.exists("data/retired_passed_over_players.csv")) {
  read_csv("data/retired_passed_over_players.csv", show_col_types = FALSE) %>%
    transmute(playerID, name, source_group = "Passed-Over Players", bbref_url)
} else {
  tibble(playerID = character(), name = character(), source_group = character(), bbref_url = character())
}

existing_manifest <- if (file.exists("data/player_photo_manifest.csv")) {
  read_csv("data/player_photo_manifest.csv", show_col_types = FALSE)
} else {
  tibble(
    playerID = character(),
    image_url = character(),
    name = character(),
    source_group = character(),
    bbref_url = character(),
    image_source = character(),
    mlbam_id = character()
  )
}

if (!("image_url" %in% names(existing_manifest))) {
  existing_manifest$image_url <- NA_character_
}
if (!("name" %in% names(existing_manifest))) {
  existing_manifest$name <- NA_character_
}
if (!("source_group" %in% names(existing_manifest))) {
  existing_manifest$source_group <- NA_character_
}
if (!("bbref_url" %in% names(existing_manifest))) {
  existing_manifest$bbref_url <- NA_character_
}
if (!("image_source" %in% names(existing_manifest))) {
  existing_manifest$image_source <- NA_character_
}
if (!("mlbam_id" %in% names(existing_manifest))) {
  existing_manifest$mlbam_id <- NA_character_
}

player_manifest <- bind_rows(
  hall_players,
  milestone_players,
  active_players,
  retired_players
) %>%
  distinct(playerID, .keep_all = TRUE) %>%
  left_join(
    existing_manifest %>%
      select(
        playerID,
        image_url,
        name_existing = name,
        source_group_existing = source_group,
        bbref_url_existing = bbref_url,
        image_source,
        mlbam_id
      ),
    by = "playerID"
  ) %>%
  transmute(
    playerID,
    image_url = image_url,
    name = coalesce(name_existing, name),
    source_group = coalesce(source_group_existing, source_group),
    bbref_url = coalesce(bbref_url_existing, bbref_url),
    image_source = image_source,
    mlbam_id = mlbam_id
  ) %>%
  arrange(name)

write_csv(player_manifest, "data/player_photo_manifest.csv")

message("Wrote data/player_photo_manifest.csv")

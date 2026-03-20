# Hall Ledger MLB photo discovery
# Searches MLB's people API for player headshots and updates the photo manifest.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(httr)
  library(jsonlite)
  library(stringr)
  library(tibble)
})

dir.create("data", showWarnings = FALSE)

manifest_path <- "data/player_photo_manifest.csv"
name_override_path <- "data/player_name_overrides.csv"
mlb_search_api <- "https://statsapi.mlb.com/api/v1/people/search"
mlb_headshot_template <- paste0(
  "https://img.mlbstatic.com/mlb-photos/image/upload/",
  "d_people:generic:headshot:67:current.png/",
  "w_426,q_auto:best/v1/people/%s/headshot/67/current"
)
user_agent_header <- user_agent("HallLedgerPhotoDiscovery/1.0")

if (!file.exists(manifest_path)) {
  stop("Missing data/player_photo_manifest.csv. Run Scripts/build_player_photo_manifest.R first.")
}

manifest <- read_csv(manifest_path, show_col_types = FALSE)

name_overrides <- if (file.exists(name_override_path)) {
  read_csv(name_override_path, show_col_types = FALSE) %>%
    mutate(
      playerID = as.character(playerID),
      fangraphs_name = as.character(fangraphs_name),
      mlb_name = as.character(mlb_name)
    ) %>%
    distinct(playerID, .keep_all = TRUE)
} else {
  tibble(playerID = character(), fangraphs_name = character(), mlb_name = character())
}

required_columns <- c("playerID", "name", "image_url", "source_group")
missing_columns <- setdiff(required_columns, names(manifest))
if (length(missing_columns) > 0) {
  stop("Manifest is missing required columns: ", paste(missing_columns, collapse = ", "))
}

if (!("image_source" %in% names(manifest))) {
  manifest$image_source <- NA_character_
}
if (!("mlbam_id" %in% names(manifest))) {
  manifest$mlbam_id <- NA_character_
}

manifest <- manifest %>%
  mutate(
    image_url = as.character(image_url),
    image_source = as.character(image_source),
    mlbam_id = as.character(mlbam_id)
  )

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

normalize_name <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9 ]", " ") %>%
    str_squish()
}

build_search_names <- function(player_name) {
  base_name <- str_squish(player_name)
  variants <- c(
    base_name,
    str_replace_all(base_name, "\\.", ""),
    str_replace_all(base_name, "'", ""),
    str_replace_all(base_name, "-", " ")
  )

  if (str_detect(base_name, "^Jacob deGrom$")) {
    variants <- c(variants, "Jacob de Grom")
  }

  unique(variants[nzchar(variants)])
}

search_mlb_people <- function(search_name) {
  response <- GET(
    mlb_search_api,
    query = list(names = search_name),
    user_agent_header
  )
  stop_for_status(response)

  parsed <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
  if (!("people" %in% names(parsed)) || length(parsed$people) == 0) {
    return(tibble())
  }

  as_tibble(parsed$people)
}

choose_best_match <- function(candidates, player_name) {
  if (nrow(candidates) == 0) {
    return(NULL)
  }

  player_norm <- normalize_name(player_name)
  sport_value <- if ("sport.name" %in% names(candidates)) {
    candidates[["sport.name"]]
  } else if ("sport" %in% names(candidates) && is.list(candidates[["sport"]])) {
    vapply(candidates[["sport"]], function(x) x$name %||% "", character(1))
  } else {
    rep("", nrow(candidates))
  }
  active_value <- if ("active" %in% names(candidates)) {
    as.character(candidates[["active"]])
  } else {
    rep("", nrow(candidates))
  }

  candidates_ranked <- candidates %>%
    mutate(
      full_name = if ("fullName" %in% names(.)) {
        as.character(fullName)
      } else {
        paste(
          if ("firstName" %in% names(.)) as.character(firstName) else "",
          if ("lastName" %in% names(.)) as.character(lastName) else ""
        ) %>% str_squish()
      },
      candidate_norm = normalize_name(full_name),
      exact_match = candidate_norm == player_norm,
      baseball_match = sport_value %in% c("Baseball", "Major League Baseball"),
      active_match = active_value %in% c("TRUE", "true", "1"),
      score = (exact_match * 100L) + (baseball_match * 10L) + (active_match * 2L)
    ) %>%
    arrange(desc(score), desc(active_match), full_name)

  candidates_ranked[1, , drop = FALSE]
}

discover_mlb_headshot <- function(player_name) {
  search_names <- build_search_names(player_name)

  for (search_name in search_names) {
    candidates <- tryCatch(
      search_mlb_people(search_name),
      error = function(e) tibble()
    )

    best_match <- choose_best_match(candidates, player_name)
    if (is.null(best_match) || nrow(best_match) == 0) {
      next
    }

    mlbam_id <- as.character(best_match$id[[1]])
    if (is.na(mlbam_id) || mlbam_id == "") {
      next
    }

    return(list(
      mlbam_id = mlbam_id,
      image_url = sprintf(mlb_headshot_template, mlbam_id)
    ))
  }

  NULL
}

queue <- manifest %>%
  distinct(playerID, .keep_all = TRUE) %>%
  left_join(name_overrides %>% select(playerID, mlb_name), by = "playerID") %>%
  mutate(search_name = if_else(!is.na(mlb_name) & mlb_name != "", mlb_name, name)) %>%
  mutate(image_source = if_else(is.na(image_source), "", image_source)) %>%
  filter(image_source != "MLB")

if (nrow(queue) == 0) {
  message("No MLB photo discovery work remaining.")
  quit(save = "no")
}

for (i in seq_len(nrow(queue))) {
  row <- queue[i, ]
  player_id <- row$playerID
  player_name <- row$search_name

  message("Finding MLB headshot for ", player_id, " (", i, "/", nrow(queue), ")")

  headshot_info <- tryCatch(
    discover_mlb_headshot(player_name),
    error = function(e) NULL
  )

  if (is.null(headshot_info)) {
    Sys.sleep(0.25)
    next
  }

  manifest <- manifest %>%
    mutate(
      image_url = if_else(playerID == player_id, headshot_info$image_url, image_url),
      image_source = if_else(playerID == player_id, "MLB", image_source),
      mlbam_id = if_else(playerID == player_id, headshot_info$mlbam_id, mlbam_id)
    )

  Sys.sleep(0.25)
}

write_csv(manifest, manifest_path)

message("Updated data/player_photo_manifest.csv with MLB headshot URLs.")

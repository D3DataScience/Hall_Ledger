# Pulls FanGraphs batting and pitching data, caches it by season, and matches it back to Hall Ledger players.
suppressPackageStartupMessages({
  library(jsonlite)
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(purrr)
  library(tidyr)
})

dir.create("data", showWarnings = FALSE)
dir.create("data/fangraphs_cache", showWarnings = FALSE)

current_season <- as.integer(format(Sys.Date(), "%Y"))
include_current_season <- identical(Sys.getenv("HALL_LEDGER_INCLUDE_CURRENT_SEASON"), "1")
refresh_current_season_cache <- identical(Sys.getenv("HALL_LEDGER_REFRESH_CURRENT_SEASON"), "1")
latest_completed_season <- if (include_current_season) current_season else current_season - 1L
refresh_all_fangraphs_cache <- FALSE
fangraphs_chunk_size <- 10L

safe_year <- function(x, fallback) {
  year_value <- suppressWarnings(as.integer(x))
  ifelse(is.na(year_value), fallback, year_value)
}

normalize_name_key <- function(x) {
  x %>%
    iconv(from = "", to = "ASCII//TRANSLIT") %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9 ]", " ") %>%
    str_squish()
}

safe_first <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) NA_character_ else as.character(x[[1]])
}

safe_max_num <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else max(x)
}

name_override_path <- "data/player_name_overrides.csv"
if (!file.exists(name_override_path)) {
  write_csv(
    tibble(
      playerID = c("speaktr01"),
      fangraphs_name = c("Tristram Speaker"),
      fangraphs_playerid = c("1012309"),
      mlb_name = c("Tristram Speaker")
    ),
    name_override_path
  )
}

name_overrides <- read_csv(name_override_path, show_col_types = FALSE) %>%
  {
    if (!("fangraphs_name" %in% names(.))) .$fangraphs_name <- NA_character_
    if (!("fangraphs_playerid" %in% names(.))) .$fangraphs_playerid <- NA_character_
    if (!("mlb_name" %in% names(.))) .$mlb_name <- NA_character_
    .
  } %>%
  mutate(
    playerID = as.character(playerID),
    fangraphs_name = as.character(fangraphs_name),
    fangraphs_playerid = as.character(fangraphs_playerid),
    mlb_name = as.character(mlb_name)
  ) %>%
  distinct(playerID, .keep_all = TRUE)

read_player_source <- function(path, source_label) {
  if (!file.exists(path)) {
    return(tibble())
  }

  read_csv(path, show_col_types = FALSE) %>%
    transmute(
      playerID = as.character(playerID),
      name = as.character(name),
      debut = as.character(debut),
      finalGame = as.character(finalGame),
      primary_position = if ("primary_position" %in% names(.)) as.character(primary_position) else NA_character_,
      source_group = source_label
    )
}

fetch_fg_leaders <- function(stats = c("bat", "pit"), season, season_start = season, pageitems = 5000L) {
  stats <- match.arg(stats)

  url <- paste0(
    "https://www.fangraphs.com/api/leaders/major-league/data?",
    "pos=all",
    "&stats=", stats,
    "&lg=all",
    "&season=", season,
    "&month=0",
    "&season1=", season_start,
    "&qual=0",
    "&ind=0",
    "&pageitems=", pageitems
  )

  tryCatch(
    {
      payload <- fromJSON(url)
      if (!"data" %in% names(payload) || is.null(payload$data)) {
        return(tibble())
      }
      as_tibble(payload$data)
    },
    error = function(e) {
      message("  ", stats, " ", season, " failed: ", conditionMessage(e))
      tibble()
    }
  )
}

cache_path_for <- function(stats = c("bat", "pit"), season) {
  stats <- match.arg(stats)
  file.path("data", "fangraphs_cache", paste0("fangraphs_", stats, "_", season, ".csv"))
}

build_year_chunks <- function(years, chunk_size = 10L) {
  if (length(years) == 0) return(list())
  split(years, ceiling(seq_along(years) / chunk_size))
}

write_chunk_to_year_cache <- function(df, stats = c("bat", "pit"), years) {
  stats <- match.arg(stats)
  normalizer <- if (stats == "bat") normalize_fg_batting else normalize_fg_pitching
  normalized <- normalizer(df)

  for (year_value in years) {
    cache_path <- cache_path_for(stats, year_value)
    year_rows <- normalized %>% filter(season == year_value)
    write_csv(year_rows, cache_path)
  }
}

ensure_fg_cache_columns <- function(df, stats = c("bat", "pit")) {
  stats <- match.arg(stats)

  if (!("fg_playerid" %in% names(df))) {
    df$fg_playerid <- NA_character_
  }

  if (stats == "bat") {
    required_cols <- names(normalize_fg_batting(tibble()))
  } else {
    required_cols <- names(normalize_fg_pitching(tibble()))
  }

  missing_cols <- setdiff(required_cols, names(df))
  for (col_name in missing_cols) {
    df[[col_name]] <- NA
  }

  df[, required_cols]
}

normalize_fg_batting <- function(df) {
  if (nrow(df) == 0) {
    return(
      tibble(
        player_name = character(),
        player_name_key = character(),
        fg_playerid = character(),
        season = integer(),
        age = integer(),
        team = character(),
        games = integer(),
        plate_appearances = integer(),
        at_bats = integer(),
        hits = integer(),
        home_runs = integer(),
        runs_batted_in = integer(),
        runs = integer(),
        stolen_bases = integer(),
        batting_average = double(),
        on_base_percentage = double(),
        slugging_percentage = double(),
        on_base_plus_slugging = double(),
        fwar = double()
      )
    )
  }

  df %>%
    transmute(
      player_name = as.character(PlayerName),
      player_name_key = normalize_name_key(as.character(PlayerName)),
      fg_playerid = if ("playerid" %in% names(.)) as.character(playerid) else NA_character_,
      season = suppressWarnings(as.integer(Season)),
      age = suppressWarnings(as.integer(Age)),
      team = as.character(TeamNameAbb),
      games = suppressWarnings(as.integer(G)),
      plate_appearances = suppressWarnings(as.integer(PA)),
      at_bats = suppressWarnings(as.integer(AB)),
      hits = suppressWarnings(as.integer(H)),
      home_runs = suppressWarnings(as.integer(HR)),
      runs_batted_in = suppressWarnings(as.integer(RBI)),
      runs = suppressWarnings(as.integer(R)),
      stolen_bases = suppressWarnings(as.integer(SB)),
      batting_average = suppressWarnings(as.numeric(AVG)),
      on_base_percentage = suppressWarnings(as.numeric(OBP)),
      slugging_percentage = suppressWarnings(as.numeric(SLG)),
      on_base_plus_slugging = suppressWarnings(as.numeric(OPS)),
      fwar = suppressWarnings(as.numeric(WAR))
    ) %>%
    filter(!is.na(player_name_key), player_name_key != "", !is.na(season)) %>%
    filter(!is.na(plate_appearances), plate_appearances > 0) %>%
    group_by(fg_playerid, player_name_key, season) %>%
    summarise(
      player_name = safe_first(player_name),
      fg_playerid = safe_first(fg_playerid),
      age = suppressWarnings(as.integer(safe_max_num(age))),
      team = safe_first(team),
      games = suppressWarnings(as.integer(safe_max_num(games))),
      plate_appearances = suppressWarnings(as.integer(safe_max_num(plate_appearances))),
      at_bats = suppressWarnings(as.integer(safe_max_num(at_bats))),
      hits = suppressWarnings(as.integer(safe_max_num(hits))),
      home_runs = suppressWarnings(as.integer(safe_max_num(home_runs))),
      runs_batted_in = suppressWarnings(as.integer(safe_max_num(runs_batted_in))),
      runs = suppressWarnings(as.integer(safe_max_num(runs))),
      stolen_bases = suppressWarnings(as.integer(safe_max_num(stolen_bases))),
      batting_average = safe_max_num(batting_average),
      on_base_percentage = safe_max_num(on_base_percentage),
      slugging_percentage = safe_max_num(slugging_percentage),
      on_base_plus_slugging = safe_max_num(on_base_plus_slugging),
      fwar = safe_max_num(fwar),
      .groups = "drop"
    )
}

normalize_fg_pitching <- function(df) {
  if (nrow(df) == 0) {
    return(
      tibble(
        player_name = character(),
        player_name_key = character(),
        fg_playerid = character(),
        season = integer(),
        age = integer(),
        team = character(),
        games = integer(),
        games_started = integer(),
        wins = integer(),
        losses = integer(),
        saves = integer(),
        innings_pitched = character(),
        hits_allowed = integer(),
        walks_allowed = integer(),
        strikeouts = integer(),
        earned_run_average = double(),
        whip = double(),
        fwar = double()
      )
    )
  }

  df %>%
    transmute(
      player_name = as.character(PlayerName),
      player_name_key = normalize_name_key(as.character(PlayerName)),
      fg_playerid = if ("playerid" %in% names(.)) as.character(playerid) else NA_character_,
      season = suppressWarnings(as.integer(Season)),
      age = suppressWarnings(as.integer(Age)),
      team = as.character(TeamNameAbb),
      games = suppressWarnings(as.integer(G)),
      games_started = suppressWarnings(as.integer(GS)),
      wins = suppressWarnings(as.integer(W)),
      losses = suppressWarnings(as.integer(L)),
      saves = suppressWarnings(as.integer(SV)),
      innings_pitched = as.character(IP),
      hits_allowed = suppressWarnings(as.integer(H)),
      walks_allowed = suppressWarnings(as.integer(BB)),
      strikeouts = suppressWarnings(as.integer(SO)),
      earned_run_average = suppressWarnings(as.numeric(ERA)),
      whip = suppressWarnings(as.numeric(WHIP)),
      fwar = suppressWarnings(as.numeric(WAR))
    ) %>%
    filter(!is.na(player_name_key), player_name_key != "", !is.na(season)) %>%
    filter(!is.na(innings_pitched), innings_pitched != "", innings_pitched != "0") %>%
    group_by(fg_playerid, player_name_key, season) %>%
    summarise(
      player_name = safe_first(player_name),
      fg_playerid = safe_first(fg_playerid),
      age = suppressWarnings(as.integer(safe_max_num(age))),
      team = safe_first(team),
      games = suppressWarnings(as.integer(safe_max_num(games))),
      games_started = suppressWarnings(as.integer(safe_max_num(games_started))),
      wins = suppressWarnings(as.integer(safe_max_num(wins))),
      losses = suppressWarnings(as.integer(safe_max_num(losses))),
      saves = suppressWarnings(as.integer(safe_max_num(saves))),
      innings_pitched = safe_first(innings_pitched),
      hits_allowed = suppressWarnings(as.integer(safe_max_num(hits_allowed))),
      walks_allowed = suppressWarnings(as.integer(safe_max_num(walks_allowed))),
      strikeouts = suppressWarnings(as.integer(safe_max_num(strikeouts))),
      earned_run_average = safe_max_num(earned_run_average),
      whip = safe_max_num(whip),
      fwar = safe_max_num(fwar),
      .groups = "drop"
    )
}

player_pool <- bind_rows(
  read_player_source("data/hall_of_fame_lahman_players.csv", "Official Induction"),
  read_player_source("data/non_hof_milestone_players.csv", "Statistical Induction"),
  read_player_source("data/active_hall_candidates.csv", "Active Induction"),
  read_player_source("data/retired_passed_over_players.csv", "Passed-Over Induction")
) %>%
  filter(!is.na(playerID), playerID != "", !is.na(name), name != "") %>%
  distinct(playerID, .keep_all = TRUE) %>%
  left_join(name_overrides %>% select(playerID, fangraphs_name, fangraphs_playerid), by = "playerID") %>%
  mutate(
    debut_year = safe_year(debut, 1871L),
    final_year = pmin(safe_year(finalGame, latest_completed_season), latest_completed_season),
    final_year = if_else(final_year < debut_year, debut_year, final_year),
    player_name_key = normalize_name_key(name),
    fangraphs_playerid = if_else(is.na(fangraphs_playerid), "", fangraphs_playerid),
    fangraphs_name_key = if_else(
      !is.na(fangraphs_name) & fangraphs_name != "",
      normalize_name_key(fangraphs_name),
      player_name_key
    )
  ) %>%
  arrange(name)

years_to_pull <- sort(unique(unlist(map2(player_pool$debut_year, player_pool$final_year, seq.int))))
missing_batting_years <- years_to_pull[
  refresh_all_fangraphs_cache |
    (refresh_current_season_cache & years_to_pull == current_season) |
    !file.exists(vapply(years_to_pull, function(y) cache_path_for("bat", y), character(1)))
]
missing_pitching_years <- years_to_pull[
  refresh_all_fangraphs_cache |
    (refresh_current_season_cache & years_to_pull == current_season) |
    !file.exists(vapply(years_to_pull, function(y) cache_path_for("pit", y), character(1)))
]

message("Building FanGraphs batting season cache for ", length(years_to_pull), " years...")
walk(years_to_pull[!years_to_pull %in% missing_batting_years], function(year_value) {
  message("  batting ", year_value, " (cached)")
})
walk(build_year_chunks(missing_batting_years, fangraphs_chunk_size), function(year_chunk) {
  start_year <- min(year_chunk)
  end_year <- max(year_chunk)
  message("  batting ", start_year, "-", end_year, " (fetch chunk)")

  chunk_rows <- fetch_fg_leaders("bat", season = end_year, season_start = start_year)
  if (nrow(chunk_rows) > 0) {
    write_chunk_to_year_cache(chunk_rows, "bat", year_chunk)
    return(invisible(NULL))
  }

  walk(year_chunk, function(year_value) {
    message("    batting ", year_value, " (fallback)")
    season_rows <- fetch_fg_leaders("bat", season = year_value, season_start = year_value) %>% normalize_fg_batting()
    write_csv(season_rows, cache_path_for("bat", year_value))
  })
})

message("Building FanGraphs pitching season cache for ", length(years_to_pull), " years...")
walk(years_to_pull[!years_to_pull %in% missing_pitching_years], function(year_value) {
  message("  pitching ", year_value, " (cached)")
})
walk(build_year_chunks(missing_pitching_years, fangraphs_chunk_size), function(year_chunk) {
  start_year <- min(year_chunk)
  end_year <- max(year_chunk)
  message("  pitching ", start_year, "-", end_year, " (fetch chunk)")

  chunk_rows <- fetch_fg_leaders("pit", season = end_year, season_start = start_year)
  if (nrow(chunk_rows) > 0) {
    write_chunk_to_year_cache(chunk_rows, "pit", year_chunk)
    return(invisible(NULL))
  }

  walk(year_chunk, function(year_value) {
    message("    pitching ", year_value, " (fallback)")
    season_rows <- fetch_fg_leaders("pit", season = year_value, season_start = year_value) %>% normalize_fg_pitching()
    write_csv(season_rows, cache_path_for("pit", year_value))
  })
})

batting_seasons <- map_dfr(years_to_pull, function(year_value) {
  cache_path <- cache_path_for("bat", year_value)
  if (!file.exists(cache_path)) {
    return(normalize_fg_batting(tibble()))
  }
  read_csv(cache_path, show_col_types = FALSE) %>%
    ensure_fg_cache_columns("bat")
})

pitching_seasons <- map_dfr(years_to_pull, function(year_value) {
  cache_path <- cache_path_for("pit", year_value)
  if (!file.exists(cache_path)) {
    return(normalize_fg_pitching(tibble()))
  }
  read_csv(cache_path, show_col_types = FALSE) %>%
    ensure_fg_cache_columns("pit")
})

player_year_grid <- player_pool %>%
  transmute(
    playerID,
    name,
    primary_position,
    source_group,
    fangraphs_playerid,
    fangraphs_name_key,
    season = map2(debut_year, final_year, seq.int)
  ) %>%
  tidyr::unnest(season)

batting_matches <- bind_rows(
  player_year_grid %>%
    filter(fangraphs_playerid != "") %>%
    left_join(
      batting_seasons,
      by = c("fangraphs_playerid" = "fg_playerid", "season")
    ),
  player_year_grid %>%
    filter(fangraphs_playerid == "") %>%
    left_join(
      batting_seasons,
      by = c("fangraphs_name_key" = "player_name_key", "season")
    )
) %>%
  mutate(match_type = if_else(!is.na(player_name), "matched", "missing")) %>%
  transmute(
    playerID,
    name,
    source_group,
    primary_position,
    season,
    fangraphs_name = player_name,
    team,
    age,
    games,
    plate_appearances,
    at_bats,
    hits,
    home_runs,
    runs_batted_in,
    runs,
    stolen_bases,
    batting_average,
    on_base_percentage,
    slugging_percentage,
    on_base_plus_slugging,
    batting_fwar = fwar,
    batting_match_type = match_type
  )

pitching_matches <- bind_rows(
  player_year_grid %>%
    filter(fangraphs_playerid != "") %>%
    left_join(
      pitching_seasons,
      by = c("fangraphs_playerid" = "fg_playerid", "season")
    ),
  player_year_grid %>%
    filter(fangraphs_playerid == "") %>%
    left_join(
      pitching_seasons,
      by = c("fangraphs_name_key" = "player_name_key", "season")
    )
) %>%
  mutate(match_type = if_else(!is.na(player_name), "matched", "missing")) %>%
  transmute(
    playerID,
    name,
    source_group,
    primary_position,
    season,
    fangraphs_name = player_name,
    team,
    age,
    games,
    games_started,
    wins,
    losses,
    saves,
    innings_pitched,
    hits_allowed,
    walks_allowed,
    strikeouts,
    earned_run_average,
    whip,
    pitching_fwar = fwar,
    pitching_match_type = match_type
  )

career_totals <- player_pool %>%
  select(playerID, name, source_group, primary_position) %>%
  left_join(
    batting_matches %>%
      group_by(playerID) %>%
      summarise(
        batting_seasons_matched = sum(batting_match_type == "matched", na.rm = TRUE),
        batting_games = sum(games, na.rm = TRUE),
        hits = sum(hits, na.rm = TRUE),
        home_runs = sum(home_runs, na.rm = TRUE),
        runs_batted_in = sum(runs_batted_in, na.rm = TRUE),
        stolen_bases = sum(stolen_bases, na.rm = TRUE),
        career_batting_fwar = round(sum(batting_fwar, na.rm = TRUE), 1),
        career_batting_average = if_else(sum(at_bats, na.rm = TRUE) > 0, round(sum(hits, na.rm = TRUE) / sum(at_bats, na.rm = TRUE), 3), NA_real_),
        .groups = "drop"
      ),
    by = "playerID"
  ) %>%
  left_join(
    pitching_matches %>%
      group_by(playerID) %>%
      summarise(
        pitching_seasons_matched = sum(pitching_match_type == "matched", na.rm = TRUE),
        pitching_games = sum(games, na.rm = TRUE),
        wins = sum(wins, na.rm = TRUE),
        losses = sum(losses, na.rm = TRUE),
        saves = sum(saves, na.rm = TRUE),
        strikeouts = sum(strikeouts, na.rm = TRUE),
        career_pitching_fwar = round(sum(pitching_fwar, na.rm = TRUE), 1),
        .groups = "drop"
      ),
    by = "playerID"
  ) %>%
  mutate(
    career_batting_fwar = if_else(is.na(career_batting_fwar), NA_real_, career_batting_fwar),
    career_pitching_fwar = if_else(is.na(career_pitching_fwar), NA_real_, career_pitching_fwar),
    career_total_fwar = round(coalesce(career_batting_fwar, 0) + coalesce(career_pitching_fwar, 0), 1),
    career_total_fwar = if_else(is.na(career_batting_fwar) & is.na(career_pitching_fwar), NA_real_, career_total_fwar)
  ) %>%
  arrange(name)

coverage <- player_pool %>%
  transmute(playerID, name, source_group, debut_year, final_year, expected_seasons = final_year - debut_year + 1L) %>%
  left_join(
    batting_matches %>%
      group_by(playerID) %>%
      summarise(batting_seasons_matched = sum(batting_match_type == "matched", na.rm = TRUE), .groups = "drop"),
    by = "playerID"
  ) %>%
  left_join(
    pitching_matches %>%
      group_by(playerID) %>%
      summarise(pitching_seasons_matched = sum(pitching_match_type == "matched", na.rm = TRUE), .groups = "drop"),
    by = "playerID"
  ) %>%
  mutate(
    batting_seasons_matched = coalesce(batting_seasons_matched, 0L),
    pitching_seasons_matched = coalesce(pitching_seasons_matched, 0L),
    coverage_summary = case_when(
      batting_seasons_matched > 0 & pitching_seasons_matched > 0 ~ "batting_and_pitching",
      batting_seasons_matched > 0 ~ "batting_only",
      pitching_seasons_matched > 0 ~ "pitching_only",
      TRUE ~ "no_match"
    )
  ) %>%
  arrange(name)

write_csv(player_pool, "data/fangraphs_player_pool.csv")
write_csv(batting_matches, "data/fangraphs_batting_seasons.csv")
write_csv(pitching_matches, "data/fangraphs_pitching_seasons.csv")
write_csv(career_totals, "data/fangraphs_player_careers.csv")
write_csv(coverage, "data/fangraphs_player_coverage.csv")

message("")
message("Wrote:")
message("data/fangraphs_player_pool.csv")
message("data/fangraphs_batting_seasons.csv")
message("data/fangraphs_pitching_seasons.csv")
message("data/fangraphs_player_careers.csv")
message("data/fangraphs_player_coverage.csv")
message("data/fangraphs_cache/")

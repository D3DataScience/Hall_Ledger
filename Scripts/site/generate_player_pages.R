# Generates Hall Ledger player pages, directory pages, and the shared search index from built data files.

suppressPackageStartupMessages({
  library(Lahman)
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(jsonlite)
  library(tidyr)
})

dir.create("players", showWarnings = FALSE)
dir.create("milestone-players", showWarnings = FALSE)
dir.create("active-players", showWarnings = FALSE)
dir.create("retired-players", showWarnings = FALSE)
dir.create("data", showWarnings = FALSE)

safe_value <- function(x, fallback = "Unknown") {
  if (length(x) == 0) {
    return(fallback)
  }

  x_chr <- as.character(x)
  x_chr[is.na(x_chr) | x_chr == ""] <- fallback
  x_chr
}

safe_value_one <- function(x, fallback = "Unknown") {
  if (length(x) == 0) {
    return(fallback)
  }

  x_chr <- as.character(x[[1]])
  if (is.na(x_chr) || x_chr == "") fallback else x_chr
}

html_escape <- function(x) {
  x <- safe_value(x, "")
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

format_average_text <- function(x) {
  if (is.na(x)) return("")
  sub("^0", "", sprintf("%.3f", x))
}

format_war_text <- function(x) {
  if (length(x) == 0 || is.na(x)) return("")
  rounded <- round(as.numeric(x), 1)
  if (!is.na(rounded) && abs(rounded) < 0.05) rounded <- 0
  sprintf("%.1f", rounded)
}

sum_display_war <- function(x) {
  values <- suppressWarnings(as.numeric(x))
  values <- values[!is.na(values)]
  if (length(values) == 0) return(NA_real_)
  round(sum(round(values, 1)), 1)
}

format_ip_text <- function(ip) {
  if (is.na(ip) || ip == "") "" else as.character(ip)
}

season_age_text <- function(year_id, birth_year) {
  year_num <- suppressWarnings(as.integer(year_id))
  birth_num <- suppressWarnings(as.integer(birth_year))

  if (is.na(year_num) || is.na(birth_num)) return("")
  as.character(year_num - birth_num)
}

split_pipe_values <- function(x) {
  if (is.na(x) || x == "") return(character())
  values <- str_split(x, "\\s*\\|\\s*")[[1]]
  trimws(values[values != ""])
}

named_split <- function(df, key_col = "playerID") {
  if (nrow(df) == 0) {
    return(list())
  }

  split(df, df[[key_col]])
}

active_player_overrides <- c(
  "scherma01",
  "verlaju01"
)

final_year_overrides <- c(
  "kershcl01" = "2025",
  "cabremi01" = "2023"
)

infer_year_from_timeline <- function(team_timeline, which = c("start", "end")) {
  which <- match.arg(which)
  values <- split_pipe_values(team_timeline)
  if (length(values) == 0) return(NA_character_)

  year_matches <- str_match(values, "\\((\\d{4})-(\\d{4})\\)")
  if (all(is.na(year_matches[, 2])) && all(is.na(year_matches[, 3]))) {
    return(NA_character_)
  }

  if (which == "start") {
    years <- suppressWarnings(as.integer(year_matches[, 2]))
  } else {
    years <- suppressWarnings(as.integer(year_matches[, 3]))
  }

  years <- years[!is.na(years)]
  if (length(years) == 0) return(NA_character_)
  as.character(if (which == "start") min(years) else max(years))
}

is_negro_leagues_player <- function(player) {
  player_id <- safe_value_one(player$playerID, "")
  context <- safe_value_one(player$player_context, "")
  teams_text <- paste(split_pipe_values(player$teams), collapse = " | ")

  isTRUE(str_ends(player_id, "99")) ||
    str_detect(context, regex("Negro|Black baseball", ignore_case = TRUE)) ||
    str_detect(teams_text, regex("Monarchs|Black Sox|American Giants|ABCs|Bacharach|Grays|Elite Giants|Stars|Cuban Stars|Lincoln Giants|Potomacs|Crawfords", ignore_case = TRUE))
}

display_year_value <- function(player, field = c("debut", "finalGame")) {
  field <- match.arg(field)
  player_id <- safe_value_one(player$playerID, "")

  if (field == "finalGame" && (player_id %in% active_player_overrides || player_id %in% active_player_ids)) {
    return("Active")
  }

  if (field == "finalGame" && player_id %in% names(final_year_overrides)) {
    return(unname(final_year_overrides[[player_id]]))
  }

  raw_value <- safe_value_one(player[[field]], "")
  if (raw_value != "") return(raw_value)

  if (!is_negro_leagues_player(player)) return("Unknown")
  infer_year_from_timeline(player$team_timeline, if (field == "debut") "start" else "end")
}

find_player_image <- function(player_id, page_prefix = "..") {
  exts <- c("jpg", "jpeg", "png", "webp")
  for (ext in exts) {
    rel_path <- file.path("images", "players", paste0(player_id, ".", ext))
    if (file.exists(rel_path)) {
      return(paste0(page_prefix, "/images/players/", player_id, ".", ext))
    }
  }
  NA_character_
}

format_innings_pitched <- function(ip_outs) {
  ifelse(is.na(ip_outs), NA_character_, paste0(ip_outs %/% 3, ".", ip_outs %% 3))
}

weighted_mean_safe <- function(x, w) {
  valid <- !is.na(x) & !is.na(w) & w > 0
  if (!any(valid)) return(NA_real_)
  stats::weighted.mean(x[valid], w[valid])
}

two_way_min_career_pa <- 50
two_way_min_career_ip_outs <- 9
fangraphs_batting_path <- "data/fangraphs_batting_seasons.csv"
fangraphs_pitching_path <- "data/fangraphs_pitching_seasons.csv"

if (!file.exists(fangraphs_batting_path) || !file.exists(fangraphs_pitching_path)) {
  stop(
    "FanGraphs season files are required before generating player pages. Run Scripts/data/build_site_data_fangraphs_api.R first.",
    call. = FALSE
  )
}

teams_lookup <- Teams %>% distinct(yearID, lgID, teamID, .keep_all = TRUE)
if ("teamIDBR" %in% names(teams_lookup)) {
  teams_lookup$team_abbrev <- teams_lookup$teamIDBR
} else if ("teamIDretro" %in% names(teams_lookup)) {
  teams_lookup$team_abbrev <- teams_lookup$teamIDretro
} else {
  teams_lookup$team_abbrev <- teams_lookup$teamID
}
teams_lookup <- teams_lookup %>% transmute(yearID, lgID, teamID, team_abbrev)

format_team_card_name <- function(team_name, lgID) {
  if (is.na(team_name) || team_name == "") {
    return(NA_character_)
  }

  if (!(lgID %in% c("AL", "NL"))) {
    return(team_name)
  }

  city_name <- team_name %>%
    str_replace(" of Anaheim$", "") %>%
    str_replace(" (Red Sox|White Sox|Blue Jays)$", "") %>%
    str_replace(" [^ ]+$", "")

  league_label <- if (lgID == "AL") "A.L." else "N.L."
  paste0(city_name, " ", league_label)
}

team_card_timeline <- bind_rows(
  Batting %>% select(playerID, yearID, teamID, lgID),
  Pitching %>% select(playerID, yearID, teamID, lgID),
  Fielding %>% select(playerID, yearID, teamID, lgID)
) %>%
  distinct() %>%
  left_join(Teams %>% transmute(yearID, teamID, lgID, team_name = name), by = c("yearID", "teamID", "lgID")) %>%
  mutate(
    team_name = coalesce(team_name, teamID),
    card_team_name = vapply(seq_len(n()), function(i) format_team_card_name(team_name[[i]], lgID[[i]]), character(1))
  ) %>%
  arrange(playerID, yearID, card_team_name) %>%
  group_by(playerID) %>%
  mutate(
    previous_team = lag(card_team_name),
    previous_year = lag(yearID),
    new_stint = if_else(
      row_number() == 1L |
        is.na(previous_team) |
        card_team_name != previous_team |
        yearID != previous_year + 1L,
      1L,
      0L
    ),
    stint_id = cumsum(new_stint)
  ) %>%
  ungroup() %>%
  group_by(playerID, stint_id, card_team_name) %>%
  summarise(start_year = min(yearID, na.rm = TRUE), end_year = max(yearID, na.rm = TRUE), .groups = "drop") %>%
  arrange(playerID, start_year, end_year, card_team_name) %>%
  mutate(
    stint_text = if_else(
      start_year == end_year,
      as.character(start_year),
      paste0(start_year, "-", end_year)
    )
  ) %>%
  group_by(playerID, card_team_name) %>%
  summarise(
    team_line = paste0(first(card_team_name), ", ", paste(stint_text, collapse = ", ")),
    first_year = min(start_year, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(playerID, first_year, card_team_name) %>%
  group_by(playerID) %>%
  summarise(
    team_card_timeline = paste(team_line, collapse = " | "),
    .groups = "drop"
  )

batting_team_seasons <- Batting %>%
  group_by(playerID, yearID, lgID, teamID) %>%
  summarise(
    games = sum(G, na.rm = TRUE),
    plate_appearances = sum(
      coalesce(AB, 0L) +
        coalesce(BB, 0L) +
        coalesce(HBP, 0L) +
        coalesce(SH, 0L) +
        coalesce(SF, 0L),
      na.rm = TRUE
    ),
    at_bats = sum(AB, na.rm = TRUE),
    runs = sum(R, na.rm = TRUE),
    hits = sum(H, na.rm = TRUE),
    doubles = sum(X2B, na.rm = TRUE),
    triples = sum(X3B, na.rm = TRUE),
    home_runs = sum(HR, na.rm = TRUE),
    runs_batted_in = sum(RBI, na.rm = TRUE),
    stolen_bases = sum(SB, na.rm = TRUE),
    walks = sum(BB, na.rm = TRUE),
    hit_by_pitch = sum(HBP, na.rm = TRUE),
    sacrifice_flies = sum(SF, na.rm = TRUE),
    total_bases = sum(H - X2B - X3B - HR + (2 * X2B) + (3 * X3B) + (4 * HR), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    batting_average = if_else(at_bats > 0, round(hits / at_bats, 3), NA_real_),
    on_base_percentage = if_else((at_bats + walks + hit_by_pitch + sacrifice_flies) > 0, round((hits + walks + hit_by_pitch) / (at_bats + walks + hit_by_pitch + sacrifice_flies), 3), NA_real_),
    slugging_percentage = if_else(at_bats > 0, round(total_bases / at_bats, 3), NA_real_),
    on_base_plus_slugging = on_base_percentage + slugging_percentage
  ) %>%
  left_join(teams_lookup, by = c("yearID", "lgID", "teamID"))

pitching_team_seasons <- Pitching %>%
  group_by(playerID, yearID, lgID, teamID) %>%
  summarise(
    games = sum(G, na.rm = TRUE),
    games_started = sum(GS, na.rm = TRUE),
    wins = sum(W, na.rm = TRUE),
    losses = sum(L, na.rm = TRUE),
    saves = sum(SV, na.rm = TRUE),
    hits_allowed = sum(H, na.rm = TRUE),
    walks_allowed = sum(BB, na.rm = TRUE),
    strikeouts = sum(SO, na.rm = TRUE),
    innings_outs = sum(IPouts, na.rm = TRUE),
    earned_runs = sum(ER, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    earned_run_average = if_else(innings_outs > 0, round((earned_runs * 27) / innings_outs, 2), NA_real_),
    whip = if_else(innings_outs > 0, round(((hits_allowed + walks_allowed) * 3) / innings_outs, 2), NA_real_),
    innings_pitched = format_innings_pitched(innings_outs)
  ) %>%
  left_join(teams_lookup, by = c("yearID", "lgID", "teamID"))

batting_yearly_clubs <- Batting %>%
  group_by(playerID, yearID) %>%
  summarise(
    home_runs = sum(HR, na.rm = TRUE),
    stolen_bases = sum(SB, na.rm = TRUE),
    .groups = "drop"
  )

pitching_yearly_clubs <- Pitching %>%
  group_by(playerID, yearID) %>%
  summarise(
    strikeouts = sum(SO, na.rm = TRUE),
    .groups = "drop"
  )

career_batting_clubs <- batting_team_seasons %>%
  group_by(playerID) %>%
  summarise(
    career_home_runs = sum(home_runs, na.rm = TRUE),
    .groups = "drop"
  )

career_pitching_clubs <- pitching_team_seasons %>%
  group_by(playerID) %>%
  summarise(
    career_strikeouts = sum(strikeouts, na.rm = TRUE),
    .groups = "drop"
  )

raw_club_memberships <- bind_rows(
  career_batting_clubs %>%
    filter(career_home_runs >= 500) %>%
    transmute(playerID, club_label = "500 HR Club", club_code = "500 HR"),
  batting_yearly_clubs %>%
    filter(home_runs >= 50) %>%
    transmute(playerID, club_label = "50 HR Club", club_code = "50 HR"),
  career_pitching_clubs %>%
    filter(career_strikeouts >= 3000) %>%
    transmute(playerID, club_label = "3000 K Club", club_code = "3000 K"),
  pitching_yearly_clubs %>%
    filter(strikeouts >= 300) %>%
    transmute(playerID, club_label = "300 K Club", club_code = "300 K"),
  batting_yearly_clubs %>%
    filter(home_runs >= 30, stolen_bases >= 30) %>%
    transmute(playerID, club_label = "30/30 Club", club_code = "30/30"),
  batting_yearly_clubs %>%
    filter(home_runs >= 40, stolen_bases >= 40) %>%
    transmute(playerID, club_label = "40/40 Club", club_code = "40/40"),
  batting_yearly_clubs %>%
    filter(home_runs >= 50, stolen_bases >= 50) %>%
    transmute(playerID, club_label = "50/50 Club", club_code = "50/50")
) %>%
  distinct(playerID, club_label, club_code) %>%
  arrange(playerID, club_label)

club_memberships <- raw_club_memberships %>%
  group_by(playerID) %>%
  filter(
    !(club_label == "30/30 Club" & any(club_label %in% c("40/40 Club", "50/50 Club"))),
    !(club_label == "40/40 Club" & any(club_label == "50/50 Club"))
  ) %>%
  ungroup() %>%
  arrange(playerID, club_label)

fangraphs_batting <- read_csv(fangraphs_batting_path, show_col_types = FALSE) %>%
  filter(batting_match_type == "matched") %>%
  transmute(
    playerID = as.character(playerID),
    yearID = suppressWarnings(as.integer(season)),
    teamID = as.character(team),
    lgID = "MLB",
    team_abbrev = as.character(team),
    games = suppressWarnings(as.integer(games)),
    plate_appearances = suppressWarnings(as.integer(plate_appearances)),
    at_bats = suppressWarnings(as.integer(at_bats)),
    runs = suppressWarnings(as.integer(runs)),
    hits = suppressWarnings(as.integer(hits)),
    home_runs = suppressWarnings(as.integer(home_runs)),
    runs_batted_in = suppressWarnings(as.integer(runs_batted_in)),
    stolen_bases = suppressWarnings(as.integer(stolen_bases)),
    batting_average = suppressWarnings(as.numeric(batting_average)),
    on_base_percentage = suppressWarnings(as.numeric(on_base_percentage)),
    slugging_percentage = suppressWarnings(as.numeric(slugging_percentage)),
    on_base_plus_slugging = suppressWarnings(as.numeric(on_base_plus_slugging)),
    war = round(suppressWarnings(as.numeric(batting_fwar)), 1)
  ) %>%
  arrange(playerID, yearID, team_abbrev)

fangraphs_pitching <- read_csv(fangraphs_pitching_path, show_col_types = FALSE) %>%
  filter(pitching_match_type == "matched") %>%
  transmute(
    playerID = as.character(playerID),
    yearID = suppressWarnings(as.integer(season)),
    teamID = as.character(team),
    lgID = "MLB",
    team_abbrev = as.character(team),
    games = suppressWarnings(as.integer(games)),
    games_started = suppressWarnings(as.integer(games_started)),
    wins = suppressWarnings(as.integer(wins)),
    losses = suppressWarnings(as.integer(losses)),
    saves = suppressWarnings(as.integer(saves)),
    hits_allowed = suppressWarnings(as.integer(hits_allowed)),
    walks_allowed = suppressWarnings(as.integer(walks_allowed)),
    strikeouts = suppressWarnings(as.integer(strikeouts)),
    innings_pitched = as.character(innings_pitched),
    earned_run_average = suppressWarnings(as.numeric(earned_run_average)),
    whip = suppressWarnings(as.numeric(whip)),
    war = round(suppressWarnings(as.numeric(pitching_fwar)), 1)
  ) %>%
  arrange(playerID, yearID, team_abbrev)

fangraphs_batting_career <- fangraphs_batting %>%
  group_by(playerID) %>%
  summarise(
    total_games = sum(games, na.rm = TRUE),
    total_plate_appearances = sum(plate_appearances, na.rm = TRUE),
    total_at_bats = sum(at_bats, na.rm = TRUE),
    total_runs = sum(runs, na.rm = TRUE),
    total_hits = sum(hits, na.rm = TRUE),
    total_home_runs = sum(home_runs, na.rm = TRUE),
    total_runs_batted_in = sum(runs_batted_in, na.rm = TRUE),
    total_stolen_bases = sum(stolen_bases, na.rm = TRUE),
    career_batting_average = if (sum(at_bats, na.rm = TRUE) > 0) round(sum(hits, na.rm = TRUE) / sum(at_bats, na.rm = TRUE), 3) else NA_real_,
    career_on_base_percentage = round(weighted_mean_safe(on_base_percentage, plate_appearances), 3),
    career_slugging_percentage = round(weighted_mean_safe(slugging_percentage, at_bats), 3),
    total_war = sum_display_war(war),
    .groups = "drop"
  ) %>%
  transmute(
    playerID,
    games = total_games,
    plate_appearances = total_plate_appearances,
    at_bats = total_at_bats,
    runs = total_runs,
    hits = total_hits,
    home_runs = total_home_runs,
    runs_batted_in = total_runs_batted_in,
    stolen_bases = total_stolen_bases,
    batting_average = career_batting_average,
    on_base_percentage = career_on_base_percentage,
    slugging_percentage = career_slugging_percentage,
    on_base_plus_slugging = if_else(
      !is.na(career_on_base_percentage) & !is.na(career_slugging_percentage),
      round(career_on_base_percentage + career_slugging_percentage, 3),
      NA_real_
    ),
    war = total_war
  )

fangraphs_pitching_career <- fangraphs_pitching %>%
  group_by(playerID) %>%
  summarise(
    total_games = sum(games, na.rm = TRUE),
    total_games_started = sum(games_started, na.rm = TRUE),
    total_wins = sum(wins, na.rm = TRUE),
    total_losses = sum(losses, na.rm = TRUE),
    total_saves = sum(saves, na.rm = TRUE),
    total_hits_allowed = sum(hits_allowed, na.rm = TRUE),
    total_walks_allowed = sum(walks_allowed, na.rm = TRUE),
    total_strikeouts = sum(strikeouts, na.rm = TRUE),
    innings_pitched = {
      innings_values <- innings_pitched[!is.na(innings_pitched) & innings_pitched != ""]
      if (length(innings_values) == 0) NA_character_ else as.character(round(sum(suppressWarnings(as.numeric(innings_values)), na.rm = TRUE), 1))
    },
    career_earned_run_average = round(weighted_mean_safe(earned_run_average, games), 2),
    career_whip = round(weighted_mean_safe(whip, games), 2),
    total_war = sum_display_war(war),
    .groups = "drop"
  ) %>%
  transmute(
    playerID,
    games = total_games,
    games_started = total_games_started,
    wins = total_wins,
    losses = total_losses,
    saves = total_saves,
    hits_allowed = total_hits_allowed,
    walks_allowed = total_walks_allowed,
    strikeouts = total_strikeouts,
    innings_pitched,
    earned_run_average = career_earned_run_average,
    whip = career_whip,
    war = total_war
  )

lahman_batting_tot_seasons <- batting_team_seasons %>%
  group_by(playerID, yearID) %>%
  filter(n() > 1) %>%
  summarise(
    lgID = "MLB",
    teamID = "TOT",
    team_abbrev = "TOT",
    games = sum(games, na.rm = TRUE),
    plate_appearances = sum(plate_appearances, na.rm = TRUE),
    at_bats = sum(at_bats, na.rm = TRUE),
    runs = sum(runs, na.rm = TRUE),
    hits = sum(hits, na.rm = TRUE),
    home_runs = sum(home_runs, na.rm = TRUE),
    runs_batted_in = sum(runs_batted_in, na.rm = TRUE),
    stolen_bases = sum(stolen_bases, na.rm = TRUE),
    walks = sum(walks, na.rm = TRUE),
    hit_by_pitch = sum(hit_by_pitch, na.rm = TRUE),
    sacrifice_flies = sum(sacrifice_flies, na.rm = TRUE),
    total_bases = sum(total_bases, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    batting_average = if_else(at_bats > 0, round(hits / at_bats, 3), NA_real_),
    on_base_percentage = if_else((at_bats + walks + hit_by_pitch + sacrifice_flies) > 0, round((hits + walks + hit_by_pitch) / (at_bats + walks + hit_by_pitch + sacrifice_flies), 3), NA_real_),
    slugging_percentage = if_else(at_bats > 0, round(total_bases / at_bats, 3), NA_real_),
    on_base_plus_slugging = on_base_percentage + slugging_percentage,
    war = NA_real_
  )

lahman_batting_seasons <- bind_rows(
  batting_team_seasons %>% mutate(war = NA_real_),
  lahman_batting_tot_seasons
) %>%
  mutate(team_sort = if_else(teamID == "TOT", 0L, 1L)) %>%
  arrange(playerID, yearID, team_sort, team_abbrev, teamID) %>%
  select(-team_sort)

lahman_pitching_tot_seasons <- pitching_team_seasons %>%
  group_by(playerID, yearID) %>%
  filter(n() > 1) %>%
  summarise(
    lgID = "MLB",
    teamID = "TOT",
    team_abbrev = "TOT",
    games = sum(games, na.rm = TRUE),
    games_started = sum(games_started, na.rm = TRUE),
    wins = sum(wins, na.rm = TRUE),
    losses = sum(losses, na.rm = TRUE),
    saves = sum(saves, na.rm = TRUE),
    hits_allowed = sum(hits_allowed, na.rm = TRUE),
    walks_allowed = sum(walks_allowed, na.rm = TRUE),
    strikeouts = sum(strikeouts, na.rm = TRUE),
    innings_outs = sum(innings_outs, na.rm = TRUE),
    earned_runs = sum(earned_runs, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    earned_run_average = if_else(innings_outs > 0, round((earned_runs * 27) / innings_outs, 2), NA_real_),
    whip = if_else(innings_outs > 0, round(((hits_allowed + walks_allowed) * 3) / innings_outs, 2), NA_real_),
    innings_pitched = format_innings_pitched(innings_outs),
    war = NA_real_
  )

lahman_pitching_seasons <- bind_rows(
  pitching_team_seasons %>% mutate(war = NA_real_),
  lahman_pitching_tot_seasons
) %>%
  mutate(team_sort = if_else(teamID == "TOT", 0L, 1L)) %>%
  arrange(playerID, yearID, team_sort, team_abbrev, teamID) %>%
  select(-team_sort)

lahman_batting_career <- batting_team_seasons %>%
  group_by(playerID) %>%
  summarise(
    games = sum(games, na.rm = TRUE),
    plate_appearances = sum(plate_appearances, na.rm = TRUE),
    at_bats = sum(at_bats, na.rm = TRUE),
    runs = sum(runs, na.rm = TRUE),
    hits = sum(hits, na.rm = TRUE),
    home_runs = sum(home_runs, na.rm = TRUE),
    runs_batted_in = sum(runs_batted_in, na.rm = TRUE),
    stolen_bases = sum(stolen_bases, na.rm = TRUE),
    batting_average = if_else(sum(at_bats, na.rm = TRUE) > 0, round(sum(hits, na.rm = TRUE) / sum(at_bats, na.rm = TRUE), 3), NA_real_),
    on_base_percentage = if_else(sum(at_bats + walks + hit_by_pitch + sacrifice_flies, na.rm = TRUE) > 0, round((sum(hits, na.rm = TRUE) + sum(walks, na.rm = TRUE) + sum(hit_by_pitch, na.rm = TRUE)) / sum(at_bats + walks + hit_by_pitch + sacrifice_flies, na.rm = TRUE), 3), NA_real_),
    slugging_percentage = if_else(sum(at_bats, na.rm = TRUE) > 0, round(sum(total_bases, na.rm = TRUE) / sum(at_bats, na.rm = TRUE), 3), NA_real_),
    .groups = "drop"
  ) %>%
  mutate(
    on_base_plus_slugging = if_else(
      !is.na(on_base_percentage) & !is.na(slugging_percentage),
      round(on_base_percentage + slugging_percentage, 3),
      NA_real_
    ),
    war = NA_real_
  )

lahman_pitching_career <- pitching_team_seasons %>%
  group_by(playerID) %>%
  summarise(
    games = sum(games, na.rm = TRUE),
    games_started = sum(games_started, na.rm = TRUE),
    wins = sum(wins, na.rm = TRUE),
    losses = sum(losses, na.rm = TRUE),
    saves = sum(saves, na.rm = TRUE),
    hits_allowed = sum(hits_allowed, na.rm = TRUE),
    walks_allowed = sum(walks_allowed, na.rm = TRUE),
    strikeouts = sum(strikeouts, na.rm = TRUE),
    innings_outs = sum(innings_outs, na.rm = TRUE),
    earned_runs = sum(earned_runs, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  transmute(
    playerID,
    games,
    games_started,
    wins,
    losses,
    saves,
    hits_allowed,
    walks_allowed,
    strikeouts,
    innings_pitched = format_innings_pitched(innings_outs),
    earned_run_average = if_else(innings_outs > 0, round((earned_runs * 27) / innings_outs, 2), NA_real_),
    whip = if_else(innings_outs > 0, round(((hits_allowed + walks_allowed) * 3) / innings_outs, 2), NA_real_),
    war = NA_real_
  )

players_missing_fg_batting <- setdiff(unique(batting_team_seasons$playerID), unique(fangraphs_batting$playerID))
players_missing_fg_pitching <- setdiff(unique(pitching_team_seasons$playerID), unique(fangraphs_pitching$playerID))

batting_seasons <- bind_rows(
  fangraphs_batting,
  lahman_batting_seasons %>% filter(playerID %in% players_missing_fg_batting)
)
pitching_seasons <- bind_rows(
  fangraphs_pitching,
  lahman_pitching_seasons %>% filter(playerID %in% players_missing_fg_pitching)
)
batting_career_rows <- bind_rows(
  fangraphs_batting_career,
  lahman_batting_career %>% filter(playerID %in% players_missing_fg_batting)
)
pitching_career_rows <- bind_rows(
  fangraphs_pitching_career,
  lahman_pitching_career %>% filter(playerID %in% players_missing_fg_pitching)
)

batting_rows_by_player <- named_split(batting_seasons)
pitching_rows_by_player <- named_split(pitching_seasons)
batting_career_by_player <- named_split(batting_career_rows)
pitching_career_by_player <- named_split(pitching_career_rows)

batting_career_totals <- batting_team_seasons %>%
  group_by(playerID) %>%
  summarise(
    career_plate_appearances = sum(plate_appearances, na.rm = TRUE),
    .groups = "drop"
  )

pitching_career_totals <- pitching_team_seasons %>%
  group_by(playerID) %>%
  summarise(
    career_innings_outs = sum(innings_outs, na.rm = TRUE),
    .groups = "drop"
  )

two_way_player_summary <- batting_career_totals %>%
  inner_join(pitching_career_totals, by = "playerID") %>%
  filter(
    career_plate_appearances >= two_way_min_career_pa,
    career_innings_outs >= two_way_min_career_ip_outs
  ) %>%
  mutate(
    career_innings_pitched = format_innings_pitched(career_innings_outs)
  ) %>%
  arrange(playerID)

two_way_override_path <- "data/two_way_overrides.csv"
if (!file.exists(two_way_override_path)) {
  write_csv(
    tibble(
      playerID = c("ruthba01", "wardjo01", "dihigma99", "roganbu99", "dayle99"),
      force_two_way = c(TRUE, TRUE, TRUE, TRUE, TRUE),
      note = c(
        "Babe Ruth manual two-way inclusion",
        "John Montgomery Ward manual two-way inclusion",
        "Martin Dihigo manual two-way inclusion",
        "Bullet Rogan manual two-way inclusion",
        "Leon Day manual two-way inclusion"
      )
    ),
    two_way_override_path
  )
}

two_way_overrides <- read_csv(two_way_override_path, show_col_types = FALSE) %>%
  mutate(
    force_two_way = if_else(is.na(force_two_way), FALSE, force_two_way),
    note = if_else(is.na(note), "", as.character(note))
  ) %>%
  filter(force_two_way) %>%
  distinct(playerID, .keep_all = TRUE)

two_way_overrides <- bind_rows(
  two_way_overrides,
  tibble(
    playerID = "ohtansh01",
    force_two_way = TRUE,
    note = "Shohei Ohtani active two-way inclusion"
  )
) %>%
  distinct(playerID, .keep_all = TRUE)

two_way_player_summary <- two_way_overrides %>%
  transmute(
    playerID,
    career_plate_appearances = NA_real_,
    career_innings_outs = NA_real_,
    career_innings_pitched = NA_character_,
    detection_method = "manual_override",
    detection_note = note
  ) %>%
  arrange(playerID)

hall_players <- read_csv("data/hall_of_fame_lahman_players.csv", show_col_types = FALSE) %>% mutate(status = "Official Induction") %>% arrange(name)
milestone_players <- if (file.exists("data/non_hof_milestone_players.csv")) {
  read_csv("data/non_hof_milestone_players.csv", show_col_types = FALSE) %>% mutate(status = "Statistical Induction") %>% arrange(name)
} else {
  tibble()
}
active_players <- if (file.exists("data/active_hall_candidates.csv")) {
  read_csv("data/active_hall_candidates.csv", show_col_types = FALSE) %>% mutate(status = "Active Induction") %>% arrange(name)
} else {
  tibble()
}
retired_players <- if (file.exists("data/retired_passed_over_players.csv")) {
  read_csv("data/retired_passed_over_players.csv", show_col_types = FALSE) %>% mutate(status = "Corrective Induction") %>% arrange(name)
} else {
  tibble()
}
hall_players <- hall_players %>% left_join(team_card_timeline, by = "playerID")
milestone_players <- milestone_players %>% left_join(team_card_timeline, by = "playerID")
active_players <- active_players %>% left_join(team_card_timeline, by = "playerID")
retired_players <- retired_players %>% left_join(team_card_timeline, by = "playerID")
player_birth_year_lookup <- bind_rows(hall_players, milestone_players, active_players, retired_players) %>%
  transmute(playerID, birthYear = suppressWarnings(as.integer(birthYear))) %>%
  distinct(playerID, .keep_all = TRUE) %>%
  { setNames(.$birthYear, .$playerID) }
active_player_ids <- if (nrow(active_players) > 0) active_players$playerID else character()

active_status_value <- function(df) {
  if ("active_status" %in% names(df)) {
    value <- as.character(df$active_status)
  } else if ("active_case" %in% names(df)) {
    value <- as.character(df$active_case)
  } else if ("projection_case" %in% names(df)) {
    value <- as.character(df$projection_case)
  } else {
    value <- rep("Active Induction", nrow(df))
  }
  
  value[is.na(value) | value == ""] <- "Active Induction"
  value
}

passed_over_status_value <- function(df) {
  if ("passed_over_status" %in% names(df)) {
    value <- as.character(df$passed_over_status)
  } else {
    value <- rep("Passed-Over Player", nrow(df))
  }

  value[is.na(value) | value == ""] <- "Passed-Over Player"
  value
}

include_in_directory_value <- function(df) {
  if ("include_in_directory" %in% names(df)) {
    value <- as.logical(df$include_in_directory)
  } else {
    value <- rep(TRUE, nrow(df))
  }

  value[is.na(value)] <- TRUE
  value
}

build_nav <- function(page_prefix) {
  paste0(
    '<nav class="topbar">',
    '<a class="brand-lockup" href="', page_prefix, '/index.html">',
    '<span class="brand">Hall Ledger</span>',
    '<span class="brand-tagline">A baseball Hall of Fame reference</span>',
    '</a>',
    '<div class="topbar-actions">',
    '<div class="topbar-links">',
    '<a class="topbar-link" href="', page_prefix, '/players/index.html#hall-player-search">Player Directory</a>',
    '<a class="topbar-link" href="', page_prefix, '/players/hall_index.html#hall-player-search">Official Inductions Directory</a>',
    '<a class="topbar-link" href="', page_prefix, '/milestone-players/milestone_index.html">Statistical Inductions</a>',
    '<a class="topbar-link" href="', page_prefix, '/active-players/active_index.html">Active Inductions</a>',
    '<a class="topbar-link" href="', page_prefix, '/retired-players/retired_index.html">Corrective Inductions</a>',
    '</div>',
    '<form class="topbar-search" data-global-player-search aria-label="Find a player anywhere on Hall Ledger">',
    '<input class="topbar-search-input" type="search" placeholder="Find any player" autocomplete="off">',
    '<button class="topbar-search-button" type="submit">Go</button>',
    '<div class="topbar-search-results" hidden></div>',
    '</form></div></nav>'
  )
}

build_image_panel <- function(player, page_prefix) {
  image_path <- find_player_image(player$playerID, page_prefix)
  if (is.na(image_path)) {
    return('<div class="player-image-panel"><div class="player-image-placeholder">Image coming soon</div></div>')
  }

  paste0(
    '<div class="player-image-panel"><a class="player-image-link" href="', html_escape(player$bbref_url), '" target="_blank" rel="noreferrer">',
    '<img class="player-image" src="', html_escape(image_path), '" alt="', html_escape(player$name), '">',
    '</a></div>'
  )
}

build_team_list <- function(team_timeline) {
  teams <- split_pipe_values(team_timeline)
  if (length(teams) == 0) return('<p class="player-side-copy">No team timeline available.</p>')
  list_class <- if (length(teams) >= 9) {
    "detail-list detail-list-compact detail-list-tight"
  } else {
    "detail-list detail-list-compact"
  }
  paste0('<ul class="', list_class, '"><li>', paste(html_escape(teams), collapse = '</li><li>'), '</li></ul>')
}

build_year_note <- function(player, debut_display, final_display) {
  if (!is_negro_leagues_player(player)) return("")

  note_text <- "Years for Negro Leagues and Black baseball players are shown from the available Lahman and team timeline records."
  paste0(
    '<p class="source-note year-footnote">* ',
    html_escape(note_text),
    '</p>'
  )
}

build_overview_cards <- function(player) {
  cards <- c()
  team_values <- split_pipe_values(player$teams)
  if (length(team_values) > 0) {
    cards <- c(cards, paste0('<article class="future-card"><h3>Teams</h3><ul class="detail-list"><li>', paste(html_escape(team_values), collapse = '</li><li>'), '</li></ul></article>'))
  }
  position_values <- split_pipe_values(player$positions)
  if (length(position_values) > 0) {
    cards <- c(cards, paste0('<article class="future-card"><h3>Positions</h3><ul class="detail-list"><li>', paste(html_escape(position_values), collapse = '</li><li>'), '</li></ul></article>'))
  }
  paste(cards, collapse = "")
}

resolve_major_awards <- function(player) {
  player_id <- safe_value_one(player$playerID, "")
  award_overrides <- c(
    "riverma01" = "All-Star Game MVP (1) | Reliever of the Year (5) | World Series MVP (1)",
    "hoffmtr01" = "Reliever of the Year (2)",
    "fingero01" = "Cy Young (1) | MVP (1) | Reliever of the Year (4) | World Series MVP (1)",
    "eckerde01" = "Cy Young (1) | MVP (1) | Reliever of the Year (2)",
    "suttebr01" = "Cy Young (1) | Reliever of the Year (4)",
    "smithle02" = "Reliever of the Year (3)",
    "wagnebi02" = "Reliever of the Year (2)",
    "gossari01" = "Reliever of the Year (1)",
    "smoltjo01" = "Cy Young (1) | Reliever of the Year (2) | Silver Slugger (1)",
    "rodrifr03" = "Reliever of the Year (4)",
    "nathajo01" = "Reliever of the Year (1)",
    "francjo01" = "Reliever of the Year (2)",
    "britzza01" = "Reliever of the Year (2)",
    "chapmar01" = "Reliever of the Year (2)",
    "kimbrcr01" = "Reliever of the Year (3)"
  )

  if (player_id %in% names(award_overrides)) {
    return(unname(award_overrides[[player_id]]))
  }

  current_value <- safe_value_one(player$major_awards, "")

  if (current_value != "") {
    return(current_value)
  }

  ""
}

normalize_major_awards_display <- function(major_awards_text) {
  award_values <- split_pipe_values(major_awards_text)
  if (length(award_values) == 0) {
    return("")
  }

  reliever_total <- 0
  other_awards <- c()

  for (award_text in award_values) {
    award_count_match <- str_match(award_text, "\\((\\d+)\\)$")
    award_count <- suppressWarnings(as.integer(award_count_match[, 2]))
    if (is.na(award_count)) {
      award_count <- 1L
    }

    award_label <- str_trim(str_replace(award_text, "\\s*\\(\\d+\\)$", ""))
    award_label_lower <- str_to_lower(award_label)

    if (str_detect(award_label_lower, "reliever of the year|rolaids relief")) {
      reliever_total <- reliever_total + award_count
    } else {
      other_awards <- c(other_awards, award_text)
    }
  }

  if (reliever_total > 0) {
    other_awards <- c(other_awards, paste0("Reliever of the Year (", reliever_total, ")"))
  }

  paste(other_awards, collapse = " | ")
}

build_header_honors <- function(player) {
  cards <- c()
  major_awards_text <- normalize_major_awards_display(resolve_major_awards(player))

  if (!is.na(player$all_star_selections) && player$all_star_selections > 0) {
    cards <- c(cards, paste0(
      '<article class="header-honor-card header-honor-card-count">',
      '<p class="header-honor-label">All-Star Selections</p>',
      '<div class="all-star-count-row">',
      '<p class="header-honor-value">', html_escape(player$all_star_selections), '</p>',
      build_all_star_chip_grid(player$all_star_selections),
      '</div>',
      '</article>'
    ))
  }

  if (major_awards_text != "") {
    cards <- c(cards, paste0(
      '<article class="header-honor-card">',
      '<p class="header-honor-label">Major Awards</p>',
      '<ul class="header-honor-list"><li>',
      paste(vapply(split_pipe_values(major_awards_text), build_honor_item, character(1)), collapse = '</li><li>'),
      '</li></ul></article>'
    ))
  }

  if (!is.na(player$title_awards) && player$title_awards != "") {
    cards <- c(cards, paste0(
      '<article class="header-honor-card">',
      '<p class="header-honor-label">Titles And Leaders</p>',
      '<ul class="header-honor-list"><li>',
      paste(vapply(split_pipe_values(player$title_awards), build_honor_item, character(1)), collapse = '</li><li>'),
      '</li></ul></article>'
    ))
  }

  if (length(cards) == 0) return("")

  paste0(
    '<div class="player-honors-band">',
    '<div class="player-honors-heading">',
    '<p class="player-card-label">Honors</p>',
    '<p class="player-side-copy">Awards, titles, and All-Star honors at a glance.</p>',
    '</div>',
    '<div class="player-honors-grid">',
    paste(cards, collapse = ""),
    '</div></div>'
  )
}

build_honor_icon <- function(honor_text) {
  honor_lower <- str_to_lower(honor_text)

  if (str_detect(honor_lower, "world series mvp")) return("WSMVP")
  if (str_detect(honor_lower, "all-star game mvp")) return("ASMVP")
  if (str_detect(honor_lower, "mvp")) return("MVP")
  if (str_detect(honor_lower, "cy young")) return("CY")
  if (str_detect(honor_lower, "rookie")) return("ROY")
  if (str_detect(honor_lower, "gold glove")) return("GG")
  if (str_detect(honor_lower, "silver slugger")) return("SS")
  if (str_detect(honor_lower, "all-star")) return("AS")
  if (str_detect(honor_lower, "reliever|rolaids relief")) return("RY")
  if (str_detect(honor_lower, "batting title")) return("BA")
  if (str_detect(honor_lower, "home run")) return("HR")
  if (str_detect(honor_lower, "rbi")) return("RBI")
  if (str_detect(honor_lower, "era")) return("ERA")
  if (str_detect(honor_lower, "wins")) return("W")
  if (str_detect(honor_lower, "strikeout")) return("SO")
  if (str_detect(honor_lower, "stolen base")) return("SB")

  "AW"
}

build_honor_item <- function(honor_text) {
  paste0(
    '<span class="honor-chip">',
    '<span class="honor-chip-icon">', html_escape(build_honor_icon(honor_text)), '</span>',
    '<span class="honor-chip-text">', html_escape(honor_text), '</span>',
    '</span>'
  )
}

build_all_star_chip_grid <- function(all_star_count) {
  if (is.na(all_star_count) || all_star_count <= 0) return("")

  chips <- paste(
    rep('<span class="all-star-mini-chip" aria-hidden="true">&#9733;</span>', all_star_count),
    collapse = ""
  )

  paste0('<div class="all-star-chip-grid">', chips, '</div>')
}

build_club_chip <- function(club_label, club_code) {
  paste0(
    '<span class="club-chip">',
    '<span class="club-chip-title">', html_escape(club_label), '</span>',
    '</span>'
  )
}

build_player_club_chips <- function(player) {
  player_id <- safe_value_one(player$playerID, "")
  clubs <- club_memberships %>% filter(playerID == player_id)

  if (nrow(clubs) == 0) return("")

  chips_html <- vapply(
    seq_len(nrow(clubs)),
    function(i) build_club_chip(clubs$club_label[[i]], clubs$club_code[[i]]),
    character(1)
  )

  paste0(
    '<div class="player-club-block">',
    '<div class="player-club-chip-grid">',
    paste(chips_html, collapse = ""),
    '</div></div>'
  )
}

build_player_career_war <- function(player, player_id) {
  batting_row <- batting_career_by_player[[player_id]]
  pitching_row <- pitching_career_by_player[[player_id]]
  primary_position <- safe_value_one(player$primary_position, "Unknown")

  batting_war <- if (!is.null(batting_row) && "war" %in% names(batting_row) && nrow(batting_row) > 0) {
    suppressWarnings(as.numeric(batting_row$war[[1]]))
  } else {
    NA_real_
  }
  pitching_war <- if (!is.null(pitching_row) && "war" %in% names(pitching_row) && nrow(pitching_row) > 0) {
    suppressWarnings(as.numeric(pitching_row$war[[1]]))
  } else {
    NA_real_
  }

  if (is.na(batting_war) && is.na(pitching_war)) {
    return("")
  }

  total_war <- if (is_two_way_player(player_id)) {
    sum_display_war(c(batting_war, pitching_war))
  } else if (primary_position == "P" && !is.na(pitching_war)) {
    pitching_war
  } else if (!is.na(batting_war)) {
    batting_war
  } else {
    pitching_war
  }

  paste0(
    '<div class="player-side-stat">',
    '<p class="player-card-label">Career fWAR</p>',
    '<p class="player-side-stat-value">', format_war_text(total_war), '</p>',
    '</div>'
  )
}

build_player_meta_line <- function(player) {
  position_text <- html_escape(player_role_label(player))

  paste0(
    '<div class="player-meta-line player-meta-line-single">',
    '<span class="player-meta-item"><span class="player-meta-label">Primary position</span><span class="player-meta-value">', position_text, '</span></span>',
    '</div>'
  )
}

build_retirement_note <- function(player) {
  player_id <- safe_value_one(player$playerID, "")
  final_year_display <- display_year_value(player, "finalGame")
  final_year <- suppressWarnings(as.integer(final_year_display))
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  is_active <- player_id %in% active_player_overrides || identical(final_year_display, "Active")
  if (is.na(final_year) || is_active || (current_year - final_year) > 5) return("")
  paste0('<p class="source-note recent-retirement-note">', html_escape(player$name), ' retired recently and is expected to appear on a future Hall of Fame ballot.</p>')
}

build_passed_over_status_note <- function(player) {
  note_text <- safe_value_one(player$passed_over_note, "")
  if (note_text == "") return("")

  note_sentences <- str_split(note_text, "(?<=\\.)\\s+", simplify = FALSE)[[1]]
  keep_sentences <- note_sentences[
    str_detect(note_sentences, regex("BBWAA Hall of Fame ballot|current ballot|recently retired", ignore_case = TRUE))
  ]

  if (length(keep_sentences) == 0) {
    return("")
  }

  paste0(
    '<p class="source-note recent-retirement-note">',
    html_escape(str_squish(paste(keep_sentences, collapse = " "))),
    '</p>'
  )
}

is_two_way_player <- function(player_id) {
  player_id %in% two_way_player_summary$playerID
}

player_role_label <- function(player) {
  player_id <- safe_value_one(player$playerID, "")

  if (player_id == "ohtansh01") {
    return("P / DH")
  }

  if (is_two_way_player(player_id)) {
    primary_pos <- safe_value_one(player$primary_position, "Unknown")
    all_positions <- split_pipe_values(safe_value_one(player$positions, ""))
    non_pitch_positions <- all_positions[all_positions != "P" & all_positions != ""]

    display_pos <- if (!is.na(primary_pos) && primary_pos != "" && primary_pos != "P") {
      primary_pos
    } else if (length(non_pitch_positions) > 0) {
      non_pitch_positions[[1]]
    } else {
      "P"
    }

    if (display_pos == "P") {
      return("P")
    }

    return(paste0(display_pos, " / P"))
  }
  safe_value_one(player$primary_position, "Unknown")
}

build_batting_rows <- function(player_id) {
  rows <- batting_rows_by_player[[player_id]]
  if (is.null(rows) || nrow(rows) == 0) return("")
  player_birth_year <- player_birth_year_lookup[[player_id]]

  year_totals <- table(rows$yearID)
  has_tot_group <- as.integer(year_totals[as.character(rows$yearID)]) > 1

  html_rows <- vapply(seq_len(nrow(rows)), function(i) {
    row <- rows[i, ]
    grouped_year <- has_tot_group[[i]]
    row_class <- if (!grouped_year) "" else if (row$teamID == "TOT") "season-total-row" else "season-detail-row"
    row_role <- if (!grouped_year) "" else if (row$teamID == "TOT") "total" else "detail"
    row_group <- if (!grouped_year) "" else paste0(' data-group="bat-', player_id, '-', row$yearID, '" data-group-role="', row_role, '"')
    age_text <- season_age_text(row$yearID, player_birth_year)
    paste0(
      '<tr', if (row_class != "") paste0(' class="', row_class, '"') else "", row_group, '>',
      '<td>', row$yearID, '</td><td>', html_escape(row$team_abbrev), '</td><td>', age_text, '</td><td>', row$games, '</td><td>', row$plate_appearances, '</td><td>', row$at_bats, '</td><td>', row$hits, '</td><td>', row$home_runs, '</td><td>', row$runs_batted_in, '</td><td>', row$runs, '</td><td>', row$stolen_bases, '</td><td>', format_average_text(row$batting_average), '</td><td>', format_average_text(row$on_base_percentage), '</td><td>', format_average_text(row$slugging_percentage), '</td><td>', format_average_text(row$on_base_plus_slugging), '</td><td>', format_war_text(row$war), '</td></tr>'
    )
  }, character(1))
  paste(html_rows, collapse = "")
}

build_pitching_rows <- function(player_id) {
  rows <- pitching_rows_by_player[[player_id]]
  if (is.null(rows) || nrow(rows) == 0) return("")
  player_birth_year <- player_birth_year_lookup[[player_id]]

  year_totals <- table(rows$yearID)
  has_tot_group <- as.integer(year_totals[as.character(rows$yearID)]) > 1

  html_rows <- vapply(seq_len(nrow(rows)), function(i) {
    row <- rows[i, ]
    grouped_year <- has_tot_group[[i]]
    row_class <- if (!grouped_year) "" else if (row$teamID == "TOT") "season-total-row" else "season-detail-row"
    row_role <- if (!grouped_year) "" else if (row$teamID == "TOT") "total" else "detail"
    row_group <- if (!grouped_year) "" else paste0(' data-group="pitch-', player_id, '-', row$yearID, '" data-group-role="', row_role, '"')
    age_text <- season_age_text(row$yearID, player_birth_year)
    paste0(
      '<tr', if (row_class != "") paste0(' class="', row_class, '"') else "", row_group, '>',
      '<td>', row$yearID, '</td><td>', html_escape(row$team_abbrev), '</td><td>', age_text, '</td><td>', row$games, '</td><td>', row$games_started, '</td><td>', row$wins, '</td><td>', row$losses, '</td><td>', row$saves, '</td><td>', format_ip_text(row$innings_pitched), '</td><td>', row$hits_allowed, '</td><td>', row$walks_allowed, '</td><td>', row$strikeouts, '</td><td>', ifelse(is.na(row$earned_run_average), "", sprintf("%.2f", row$earned_run_average)), '</td><td>', ifelse(is.na(row$whip), "", sprintf("%.2f", row$whip)), '</td><td>', format_war_text(row$war), '</td></tr>'
    )
  }, character(1))
  paste(html_rows, collapse = "")
}

build_batting_career_footer <- function(player_id) {
  row <- batting_career_by_player[[player_id]]
  if (is.null(row) || nrow(row) == 0) return("")

  paste0(
    '<tfoot><tr class="career-stats-row">',
    '<td colspan="3"><strong>Totals</strong></td><td>', row$games, '</td><td>', row$plate_appearances, '</td><td>', row$at_bats, '</td><td>', row$hits, '</td><td>', row$home_runs, '</td><td>', row$runs_batted_in, '</td><td>', row$runs, '</td><td>', row$stolen_bases, '</td><td>', format_average_text(row$batting_average), '</td><td>', format_average_text(row$on_base_percentage), '</td><td>', format_average_text(row$slugging_percentage), '</td><td>', format_average_text(row$on_base_plus_slugging), '</td><td>', format_war_text(row$war), '</td>',
    '</tr></tfoot>'
  )
}

build_pitching_career_footer <- function(player_id) {
  row <- pitching_career_by_player[[player_id]]
  if (is.null(row) || nrow(row) == 0) return("")

  paste0(
    '<tfoot><tr class="career-stats-row">',
    '<td colspan="3"><strong>Totals</strong></td><td>', row$games, '</td><td>', row$games_started, '</td><td>', row$wins, '</td><td>', row$losses, '</td><td>', row$saves, '</td><td>', format_ip_text(row$innings_pitched), '</td><td>', row$hits_allowed, '</td><td>', row$walks_allowed, '</td><td>', row$strikeouts, '</td><td>', ifelse(is.na(row$earned_run_average), "", sprintf("%.2f", row$earned_run_average)), '</td><td>', ifelse(is.na(row$whip), "", sprintf("%.2f", row$whip)), '</td><td>', format_war_text(row$war), '</td>',
    '</tr></tfoot>'
  )
}

build_stats_section <- function(player) {
  player_id <- safe_value_one(player$playerID, "")
  primary_position <- safe_value_one(player$primary_position, "Unknown")
  batting_data <- batting_rows_by_player[[player_id]]
  pitching_data <- pitching_rows_by_player[[player_id]]
  batting_rows <- build_batting_rows(player_id)
  pitching_rows <- build_pitching_rows(player_id)
  batting_footer <- build_batting_career_footer(player_id)
  pitching_footer <- build_pitching_career_footer(player_id)
  batting_note <- 'FanGraphs batting data presented by season and team.'
  pitching_note <- 'FanGraphs pitching data presented by season and team.'
  batting_section <- if (nzchar(batting_rows)) {
    paste0('<section class="section"><div class="section-heading"><div><p class="eyebrow">Season Stats</p><h2>Year-by-year batting stats</h2></div><p class="section-copy">', batting_note, '</p></div><div class="table-shell"><table class="data-table season-stats-table sortable-table"><thead><tr><th data-sort-type="number">Year</th><th data-sort-type="text">Team</th><th data-sort-type="number">Age</th><th data-sort-type="number">G</th><th data-sort-type="number">PA</th><th data-sort-type="number">AB</th><th data-sort-type="number">H</th><th data-sort-type="number">HR</th><th data-sort-type="number">RBI</th><th data-sort-type="number">R</th><th data-sort-type="number">SB</th><th data-sort-type="number">BA</th><th data-sort-type="number">OBP</th><th data-sort-type="number">SLG</th><th data-sort-type="number">OPS</th><th data-sort-type="number">fWAR</th></tr></thead><tbody>', batting_rows, '</tbody>', batting_footer, '</table></div></section>')
  } else {
    ""
  }
  pitching_section <- if (nzchar(pitching_rows)) {
    paste0('<section class="section"><div class="section-heading"><div><p class="eyebrow">Season Stats</p><h2>Year-by-year pitching stats</h2></div><p class="section-copy">', pitching_note, '</p></div><div class="table-shell"><table class="data-table season-stats-table sortable-table"><thead><tr><th data-sort-type="number">Year</th><th data-sort-type="text">Team</th><th data-sort-type="number">Age</th><th data-sort-type="number">G</th><th data-sort-type="number">GS</th><th data-sort-type="number">W</th><th data-sort-type="number">L</th><th data-sort-type="number">SV</th><th data-sort-type="number">IP</th><th data-sort-type="number">H</th><th data-sort-type="number">BB</th><th data-sort-type="number">SO</th><th data-sort-type="number">ERA</th><th data-sort-type="number">WHIP</th><th data-sort-type="number">fWAR</th></tr></thead><tbody>', pitching_rows, '</tbody>', pitching_footer, '</table></div></section>')
  } else {
    ""
  }

  if (is_two_way_player(player_id)) {
    return(paste0(batting_section, pitching_section))
  }
  if (primary_position == "P" && nzchar(pitching_section)) {
    return(pitching_section)
  }
  if (nzchar(batting_section)) {
    return(batting_section)
  }
  if (nzchar(pitching_section)) {
    return(pitching_section)
  }
  ""
}

build_page_html <- function(player, page_prefix, side_copy, recent_note = "") {
  header_honors_html <- build_header_honors(player)
  stats_section_html <- build_stats_section(player)
  debut_display <- display_year_value(player, "debut")
  final_display <- display_year_value(player, "finalGame")
  year_marker <- if (is_negro_leagues_player(player)) "*" else ""
  year_note_html <- build_year_note(player, debut_display, final_display)
  player_meta_line_html <- build_player_meta_line(player)
  career_war_html <- build_player_career_war(player, safe_value_one(player$playerID, ""))
  side_copy_html <- if (safe_value_one(side_copy, "") != "") {
    paste0('<p class="player-side-copy">', html_escape(side_copy), '</p>')
  } else {
    ""
  }
  paste0(
    '<!DOCTYPE html><html lang="en"><head><meta charset="UTF-8"><meta name="viewport" content="width=device-width, initial-scale=1.0"><title>', html_escape(player$name), ' | Hall Ledger</title><link rel="stylesheet" href="', page_prefix, '/styles.css"><script defer src="', page_prefix, '/js/table-sort.js"></script><script defer src="', page_prefix, '/js/player-index.js"></script><script defer src="', page_prefix, '/js/player-search.js"></script><script defer src="', page_prefix, '/js/all-star-layout.js"></script><script defer src="', page_prefix, '/js/team-plaque-layout.js"></script></head><body><div class="page-shell"><header class="hero">',
    build_nav(page_prefix),
    '<div class="player-card-header">',
    build_image_panel(player, page_prefix),
    '<div class="player-card-main"><h1>', html_escape(player$name), '</h1><p class="player-years">Active years: ', html_escape(debut_display), year_marker, ' to ', html_escape(final_display), year_marker, '</p>', year_note_html, player_meta_line_html, '<div class="player-team-block"><p class="player-card-label">Teams</p>', build_team_list(if ("team_card_timeline" %in% names(player)) player$team_card_timeline else player$team_timeline), '</div></div>',
    '<aside class="player-card-side">', side_copy_html, build_player_club_chips(player), career_war_html, '</aside></div>',
    header_honors_html,
    recent_note,
    '</header><main>',
    stats_section_html,
    '</main></div></body></html>'
  )
}

write_official_page <- function(player) {
  file_name <- file.path("players", paste0(player$playerID, ".html"))
  writeLines(build_page_html(player, "..", "Official Induction"), file_name)
}

write_milestone_page <- function(player) {
  file_name <- file.path("milestone-players", paste0(player$playerID, ".html"))
  writeLines(build_page_html(player, "..", "Statistical Induction"), file_name)
}

write_active_page <- function(player) {
  file_name <- file.path("active-players", paste0(player$playerID, ".html"))
  writeLines(
    build_page_html(
      player,
      "..",
      "Active Induction"
    ),
    file_name
  )
}

write_retired_page <- function(player) {
  file_name <- file.path("retired-players", paste0(player$playerID, ".html"))
  writeLines(
    build_page_html(
      player,
      "..",
      "Corrective Induction",
      build_passed_over_status_note(player)
    ),
    file_name
  )
}

hall_rows <- hall_players %>%
  mutate(
    negro_player = str_ends(playerID, "99") |
      str_detect(coalesce(teams, ""), regex("Monarchs|Black Sox|American Giants|ABCs|Bacharach|Grays|Elite Giants|Stars|Cuban Stars|Lincoln Giants|Potomacs|Crawfords", ignore_case = TRUE)),
    inferred_debut = vapply(team_timeline, infer_year_from_timeline, character(1), which = "start"),
    inferred_final = vapply(team_timeline, infer_year_from_timeline, character(1), which = "end"),
    primary_position_display = if_else(is.na(primary_position) | as.character(primary_position) == "", "Unknown", as.character(primary_position)),
    debut_display = if_else(
      is.na(debut) | as.character(debut) == "",
      if_else(negro_player, coalesce(inferred_debut, "Unknown"), "Unknown"),
      as.character(debut)
    ),
    final_game_display = if_else(
      is.na(finalGame) | as.character(finalGame) == "",
      if_else(negro_player, coalesce(inferred_final, "Unknown"), "Unknown"),
      as.character(finalGame)
    )
  ) %>%
  transmute(
      sort_name = name,
      row_html = paste0(
        '<tr><td><a href="', playerID, '.html">', html_escape(name), '</a></td>',
        '<td><span class="status-pill status-official">Official Induction</span></td>',
        '<td>Class of ', induction_year, '</td>',
        '<td>', html_escape(primary_position_display), '</td>',
        '<td>', html_escape(debut_display), '</td>',
        '<td>', html_escape(final_game_display), '</td></tr>'
      )
  )

hall_directory_rows <- hall_rows %>%
  arrange(sort_name) %>%
  pull(row_html)

milestone_rows <- milestone_players %>%
  mutate(
    milestone_display = if_else(is.na(qualifying_milestones), "", as.character(qualifying_milestones)),
    primary_position_display = if_else(is.na(primary_position) | as.character(primary_position) == "", "Unknown", as.character(primary_position)),
    debut_display = if_else(is.na(debut) | as.character(debut) == "", "Unknown", as.character(debut)),
    final_game_display = case_when(
      playerID %in% active_player_overrides ~ "Active",
      playerID %in% names(final_year_overrides) ~ unname(final_year_overrides[playerID]),
      is.na(finalGame) | as.character(finalGame) == "" ~ "Unknown",
      TRUE ~ as.character(finalGame)
    )
  ) %>%
  transmute(
      sort_name = name,
      row_html = paste0(
        '<tr><td><a href="../milestone-players/', playerID, '.html">', html_escape(name), '</a></td>',
        '<td><span class="status-pill status-ledger">Statistical Induction</span></td>',
        '<td>Hall Ledger Class of ', html_escape(if_else(is.na(milestone_class_year), "2026", as.character(milestone_class_year))), '</td>',
        '<td>', html_escape(primary_position_display), '</td>',
        '<td>', html_escape(debut_display), '</td>',
        '<td>', html_escape(final_game_display), '</td></tr>'
      )
  )

milestone_directory_rows <- milestone_players %>%
  mutate(
    milestone_display = if_else(is.na(qualifying_milestones), "", as.character(qualifying_milestones)),
    primary_position_display = if_else(is.na(primary_position) | as.character(primary_position) == "", "Unknown", as.character(primary_position)),
    debut_display = if_else(is.na(debut) | as.character(debut) == "", "Unknown", as.character(debut)),
    final_game_display = case_when(
      playerID %in% active_player_overrides ~ "Active",
      playerID %in% names(final_year_overrides) ~ unname(final_year_overrides[playerID]),
      is.na(finalGame) | as.character(finalGame) == "" ~ "Unknown",
      TRUE ~ as.character(finalGame)
    )
  ) %>%
  transmute(
      sort_name = name,
      row_html = paste0(
        '<tr><td><a href="', playerID, '.html">', html_escape(name), '</a></td>',
        '<td><span class="status-pill status-ledger">Statistical Induction</span></td>',
        '<td>Hall Ledger Class of ', html_escape(if_else(is.na(milestone_class_year), "2026", as.character(milestone_class_year))), '</td>',
        '<td>', html_escape(primary_position_display), '</td>',
        '<td>', html_escape(debut_display), '</td>',
        '<td>', html_escape(final_game_display), '</td></tr>'
      )
  ) %>%
  arrange(sort_name)

active_directory_rows <- if (nrow(active_players) > 0) {
  active_players %>%
    mutate(
      case_display = active_status_value(cur_data()),
      primary_position_display = if_else(is.na(primary_position) | as.character(primary_position) == "", "Unknown", as.character(primary_position)),
      debut_display = if_else(is.na(debut) | as.character(debut) == "", "Unknown", as.character(debut)),
      final_game_display = "Active"
    ) %>%
    transmute(
      sort_name = name,
      row_html = paste0(
        '<tr><td><a href="', playerID, '.html">', html_escape(name), '</a></td>',
        '<td><span class="status-pill status-active">Active Induction</span></td>',
        '<td>Hall Ledger Class of 2026</td>',
        '<td>', html_escape(primary_position_display), '</td>',
        '<td>', html_escape(debut_display), '</td>',
        '<td>', html_escape(final_game_display), '</td></tr>'
      )
    ) %>%
    arrange(sort_name)
} else {
  tibble(sort_name = character(), row_html = character())
}

active_rows <- if (nrow(active_players) > 0) {
  active_players %>%
    mutate(
      case_display = active_status_value(cur_data()),
      primary_position_display = if_else(is.na(primary_position) | as.character(primary_position) == "", "Unknown", as.character(primary_position)),
      debut_display = if_else(is.na(debut) | as.character(debut) == "", "Unknown", as.character(debut)),
      final_game_display = "Active"
    ) %>%
    transmute(
      sort_name = name,
      row_html = paste0(
        '<tr><td><a href="../active-players/', playerID, '.html">', html_escape(name), '</a></td>',
        '<td><span class="status-pill status-active">Active Induction</span></td>',
        '<td>Hall Ledger Class of 2026</td>',
        '<td>', html_escape(primary_position_display), '</td>',
        '<td>', html_escape(debut_display), '</td>',
        '<td>', html_escape(final_game_display), '</td></tr>'
      )
    )
} else {
  tibble(sort_name = character(), row_html = character())
}

retired_directory_rows <- if (nrow(retired_players) > 0) {
  retired_players %>%
    filter(include_in_directory_value(cur_data())) %>%
    mutate(
      case_display = passed_over_status_value(cur_data()),
      primary_position_display = if_else(is.na(primary_position) | as.character(primary_position) == "", "Unknown", as.character(primary_position)),
      debut_display = if_else(is.na(debut) | as.character(debut) == "", "Unknown", as.character(debut)),
      final_game_display = if_else(is.na(finalGame) | as.character(finalGame) == "", "Unknown", as.character(finalGame))
    ) %>%
    transmute(
      sort_name = name,
      row_html = paste0(
        '<tr><td><a href="', playerID, '.html">', html_escape(name), '</a></td>',
        '<td><span class="status-pill status-passed-over">Corrective Induction</span></td>',
        '<td>', html_escape(if_else(is.na(passed_over_class_year), "Manual Review", paste0("Hall Ledger Class of ", as.character(passed_over_class_year)))), '</td>',
        '<td>', html_escape(primary_position_display), '</td>',
        '<td>', html_escape(debut_display), '</td>',
        '<td>', html_escape(final_game_display), '</td></tr>'
      )
    ) %>%
    arrange(sort_name)
} else {
  tibble(sort_name = character(), row_html = character())
}

retired_rows <- if (nrow(retired_players) > 0) {
  retired_players %>%
    filter(include_in_directory_value(cur_data())) %>%
    mutate(
      case_display = passed_over_status_value(cur_data()),
      primary_position_display = if_else(is.na(primary_position) | as.character(primary_position) == "", "Unknown", as.character(primary_position)),
      debut_display = if_else(is.na(debut) | as.character(debut) == "", "Unknown", as.character(debut)),
      final_game_display = if_else(is.na(finalGame) | as.character(finalGame) == "", "Unknown", as.character(finalGame))
    ) %>%
    transmute(
      sort_name = name,
      row_html = paste0(
        '<tr><td><a href="../retired-players/', playerID, '.html">', html_escape(name), '</a></td>',
        '<td><span class="status-pill status-passed-over">Corrective Induction</span></td>',
        '<td>', html_escape(if_else(is.na(passed_over_class_year), "Manual Review", paste0("Hall Ledger Class of ", as.character(passed_over_class_year)))), '</td>',
        '<td>', html_escape(primary_position_display), '</td>',
        '<td>', html_escape(debut_display), '</td>',
        '<td>', html_escape(final_game_display), '</td></tr>'
      )
    )
} else {
  tibble(sort_name = character(), row_html = character())
}

directory_rows <- bind_rows(hall_rows, milestone_rows, active_rows, retired_rows) %>%
  arrange(sort_name) %>%
  pull(row_html)

hall_directory_html <- paste0(
  '<!DOCTYPE html><html lang="en"><head><meta charset="UTF-8"><meta name="viewport" content="width=device-width, initial-scale=1.0"><title>Official Inductions Directory | Hall Ledger</title><link rel="stylesheet" href="../styles.css"><script defer src="../js/table-sort.js"></script><script defer src="../js/player-index.js"></script><script defer src="../js/player-search.js"></script></head><body><div class="page-shell"><header class="hero">', build_nav(".."),
  '<div class="hero-copy"><p class="eyebrow">Official Inductions</p><h1>Official Inductions Directory</h1><p class="lede">Every officially inducted player page currently included on Hall Ledger.</p><p class="section-copy">This directory includes only players who have been officially inducted into the National Baseball Hall of Fame.</p></div></header><main><section class="section"><div class="table-shell"><table class="data-table"><thead><tr><th>Player</th><th>Status</th><th>Class</th><th>Primary Position</th><th>Debut</th><th>Final Year</th></tr></thead><tbody>',
  paste(hall_directory_rows, collapse = "\n"),
  '</tbody></table></div></section></main></div></body></html>'
)
writeLines(hall_directory_html, "players/hall_index.html")

directory_html <- paste0(
  '<!DOCTYPE html><html lang="en"><head><meta charset="UTF-8"><meta name="viewport" content="width=device-width, initial-scale=1.0"><title>Player Directory | Hall Ledger</title><link rel="stylesheet" href="../styles.css"><script defer src="../js/table-sort.js"></script><script defer src="../js/player-index.js"></script><script defer src="../js/player-search.js"></script></head><body><div class="page-shell"><header class="hero">', build_nav(".."),
  '<div class="hero-copy"><p class="eyebrow">Hall Ledger Directory</p><h1>Player Directory</h1><p class="lede">A searchable directory of every player currently included on Hall Ledger.</p><p class="section-copy">Official, statistical, active, and corrective inductions are listed together here for a complete sitewide directory.</p></div></header><main><section class="section"><div class="table-shell"><table class="data-table" id="hall-player-table"><thead><tr><th>Player</th><th>Status</th><th>Class</th><th>Primary Position</th><th>Debut</th><th>Final Year</th></tr></thead><tbody>',
  paste(directory_rows, collapse = "\n"),
  '</tbody></table></div></section></main></div></body></html>'
)
writeLines(directory_html, "players/index.html")

milestone_directory_html <- paste0(
  '<!DOCTYPE html><html lang="en"><head><meta charset="UTF-8"><meta name="viewport" content="width=device-width, initial-scale=1.0"><title>Statistical Inductions | Hall Ledger</title><link rel="stylesheet" href="../styles.css"><script defer src="../js/table-sort.js"></script><script defer src="../js/player-index.js"></script><script defer src="../js/player-search.js"></script></head><body><div class="page-shell"><header class="hero">', build_nav(".."),
  '<div class="hero-copy"><p class="eyebrow">Statistical Inductions</p><h1>Statistical Inductions Directory</h1><p class="lede">Players included on Hall Ledger through the statistical induction lane.</p><p class="section-copy">These players are part of Hall Ledger through the site&#39;s statistical standards rather than official Hall of Fame election.</p></div></header><main><section class="section"><div class="table-shell"><table class="data-table"><thead><tr><th>Player</th><th>Status</th><th>Class</th><th>Primary Position</th><th>Debut</th><th>Final Year</th></tr></thead><tbody>',
  paste(milestone_directory_rows$row_html, collapse = "\n"),
  '</tbody></table></div></section></main></div></body></html>'
)
writeLines(milestone_directory_html, "milestone-players/milestone_index.html")

active_directory_html <- paste0(
  '<!DOCTYPE html><html lang="en"><head><meta charset="UTF-8"><meta name="viewport" content="width=device-width, initial-scale=1.0"><title>Active Inductions | Hall Ledger</title><link rel="stylesheet" href="../styles.css"><script defer src="../js/table-sort.js"></script><script defer src="../js/player-index.js"></script><script defer src="../js/player-search.js"></script></head><body><div class="page-shell"><header class="hero">', build_nav(".."),
  '<div class="hero-copy"><p class="eyebrow">Active Inductions</p><h1>Active Inductions Directory</h1><p class="lede">Current players who already meet Hall Ledger standards.</p><p class="section-copy">These pages recognize active players whose careers already warrant inclusion on Hall Ledger.</p></div></header><main><section class="section"><div class="table-shell"><table class="data-table"><thead><tr><th>Player</th><th>Status</th><th>Class</th><th>Primary Position</th><th>Debut</th><th>Current Status</th></tr></thead><tbody>',
  paste(active_directory_rows$row_html, collapse = "\n"),
  '</tbody></table></div></section></main></div></body></html>'
)
writeLines(active_directory_html, "active-players/active_index.html")

retired_directory_html <- paste0(
  '<!DOCTYPE html><html lang="en"><head><meta charset="UTF-8"><meta name="viewport" content="width=device-width, initial-scale=1.0"><title>Corrective Inductions | Hall Ledger</title><link rel="stylesheet" href="../styles.css"><script defer src="../js/table-sort.js"></script><script defer src="../js/player-index.js"></script><script defer src="../js/player-search.js"></script></head><body><div class="page-shell"><header class="hero">', build_nav(".."),
  '<div class="hero-copy"><p class="eyebrow">Corrective Inductions</p><h1>Corrective Inductions Directory</h1><p class="lede">Retired players included through Hall Ledger&#39;s corrective induction lane.</p><p class="section-copy">These pages recognize retired players who are included on Hall Ledger outside the official, statistical, and active lanes.</p></div></header><main><section class="section"><div class="table-shell"><table class="data-table"><thead><tr><th>Player</th><th>Status</th><th>Class</th><th>Primary Position</th><th>Debut</th><th>Final Year</th></tr></thead><tbody>',
  paste(retired_directory_rows$row_html, collapse = "\n"),
  '</tbody></table></div></section></main></div></body></html>'
)
writeLines(retired_directory_html, "retired-players/retired_index.html")

search_index <- bind_rows(
  hall_players %>% transmute(name, playerID, status = "Official Induction", note = paste0("Inducted ", induction_year), path = paste0("players/", playerID, ".html")),
  milestone_players %>% transmute(name, playerID, status = "Statistical Induction", note = qualifying_milestones, path = paste0("milestone-players/", playerID, ".html")),
  active_players %>% transmute(
    name,
    playerID,
    status = "Active Induction",
    note = active_status_value(cur_data()),
    path = paste0("active-players/", playerID, ".html")
  ),
  retired_players %>% transmute(
    name,
    playerID,
    status = "Corrective Induction",
    note = passed_over_status_value(cur_data()),
    path = paste0("retired-players/", playerID, ".html")
  )
) %>% arrange(name)
writeLines(paste0("window.HALL_LEDGER_PLAYERS = ", toJSON(search_index, auto_unbox = TRUE, dataframe = "rows", na = "null"), ";"), "js/player-index.js")

two_way_player_list <- bind_rows(hall_players, milestone_players, active_players, retired_players) %>%
  distinct(playerID, name, status) %>%
  inner_join(two_way_player_summary, by = "playerID") %>%
  arrange(name)
write_csv(two_way_player_list, "data/two_way_players.csv")

invisible(lapply(split(hall_players, seq_len(nrow(hall_players))), write_official_page))
if (nrow(milestone_players) > 0) invisible(lapply(split(milestone_players, seq_len(nrow(milestone_players))), write_milestone_page))
if (nrow(active_players) > 0) invisible(lapply(split(active_players, seq_len(nrow(active_players))), write_active_page))
if (nrow(retired_players) > 0) invisible(lapply(split(retired_players, seq_len(nrow(retired_players))), write_retired_page))

message("Finished building all player pages.")

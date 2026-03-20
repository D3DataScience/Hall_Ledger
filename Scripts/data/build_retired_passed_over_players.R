# Builds the corrective-induction player dataset for retired players added outside the automatic lanes.
suppressPackageStartupMessages({
  library(Lahman)
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
})

dir.create("data", showWarnings = FALSE)

safe_first <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) NA_character_ else as.character(x[[1]])
}

format_innings_pitched <- function(ip_outs) {
  ifelse(is.na(ip_outs), NA_character_, paste0(ip_outs %/% 3, ".", ip_outs %% 3))
}

format_average <- function(num, den, digits = 3) {
  ifelse(den > 0, round(num / den, digits), NA_real_)
}

seed_path <- "data/retired_passed_over_players_seed.csv"
if (!file.exists(seed_path)) {
  write_csv(
    tibble(
      playerID = character(),
      name = character(),
      passed_over_status = character(),
      passed_over_note = character(),
      passed_over_class_year = integer(),
      include_in_directory = logical()
    ),
    seed_path
  )
}

retired_seed <- read_csv(seed_path, show_col_types = FALSE) %>%
  {
    if (!("passed_over_status" %in% names(.))) .$passed_over_status <- NA_character_
    if (!("passed_over_note" %in% names(.))) .$passed_over_note <- NA_character_
    if (!("passed_over_class_year" %in% names(.))) .$passed_over_class_year <- NA_integer_
    if (!("include_in_directory" %in% names(.))) .$include_in_directory <- TRUE
    .
  } %>%
  mutate(
    playerID = as.character(playerID),
    name = as.character(name),
    passed_over_status = coalesce(as.character(passed_over_status), "Passed-Over Player"),
    passed_over_note = coalesce(as.character(passed_over_note), ""),
    passed_over_class_year = suppressWarnings(as.integer(passed_over_class_year)),
    include_in_directory = if_else(is.na(include_in_directory), TRUE, as.logical(include_in_directory))
  ) %>%
  filter(!is.na(playerID), playerID != "") %>%
  distinct(playerID, .keep_all = TRUE)

final_year_overrides <- c(
  "greinza01" = "2023"
)

players_people <- People %>%
  transmute(
    playerID,
    name_people = paste(nameFirst, nameLast),
    birthYear,
    debut = if_else(is.na(debut), NA_character_, substr(as.character(debut), 1, 4)),
    finalGame = if_else(is.na(finalGame), NA_character_, substr(as.character(finalGame), 1, 4)),
    bbref_url = paste0("https://www.baseball-reference.com/players/", substr(playerID, 1, 1), "/", playerID, ".shtml")
  ) %>%
  mutate(
    name_people = if_else(playerID == "griffke02", "Ken Griffey Jr.", name_people)
  )

people_lookup <- players_people %>%
  rename(metadata_playerID = playerID)

people_name_lookup <- people_lookup %>%
  transmute(seed_name = name_people, metadata_playerID) %>%
  distinct(seed_name, .keep_all = TRUE)

batting_totals <- Batting %>%
  group_by(playerID) %>%
  summarise(
    batting_games = sum(G, na.rm = TRUE),
    hits = sum(H, na.rm = TRUE),
    home_runs = sum(HR, na.rm = TRUE),
    runs_batted_in = sum(RBI, na.rm = TRUE),
    stolen_bases = sum(SB, na.rm = TRUE),
    batting_average = format_average(sum(H, na.rm = TRUE), sum(AB, na.rm = TRUE)),
    .groups = "drop"
  )

pitching_totals <- Pitching %>%
  group_by(playerID) %>%
  summarise(
    pitching_games = sum(G, na.rm = TRUE),
    strikeouts = sum(SO, na.rm = TRUE),
    wins = sum(W, na.rm = TRUE),
    losses = sum(L, na.rm = TRUE),
    saves = sum(SV, na.rm = TRUE),
    innings_outs = sum(IPouts, na.rm = TRUE),
    earned_runs = sum(ER, na.rm = TRUE),
    earned_run_average = if_else(innings_outs > 0, round((earned_runs * 27) / innings_outs, 2), NA_real_),
    innings_pitched = format_innings_pitched(innings_outs),
    .groups = "drop"
  ) %>%
  select(-innings_outs, -earned_runs)

position_lookup <- Fielding %>%
  filter(!is.na(POS), POS != "") %>%
  group_by(playerID, POS) %>%
  summarise(games = sum(G, na.rm = TRUE), .groups = "drop")

primary_positions <- position_lookup %>%
  arrange(playerID, desc(games), POS) %>%
  group_by(playerID) %>%
  summarise(primary_position = safe_first(POS), .groups = "drop")

all_positions <- position_lookup %>%
  arrange(playerID, desc(games), POS) %>%
  group_by(playerID) %>%
  summarise(positions = paste(POS, collapse = " | "), .groups = "drop")

team_names <- Teams %>% transmute(yearID, teamID, lgID, team_name = name)

team_timeline <- bind_rows(
  Batting %>% select(playerID, yearID, teamID, lgID),
  Pitching %>% select(playerID, yearID, teamID, lgID),
  Fielding %>% select(playerID, yearID, teamID, lgID)
) %>%
  distinct() %>%
  left_join(team_names, by = c("yearID", "teamID", "lgID")) %>%
  mutate(team_name = coalesce(team_name, teamID)) %>%
  group_by(playerID, team_name) %>%
  summarise(start_year = min(yearID, na.rm = TRUE), end_year = max(yearID, na.rm = TRUE), .groups = "drop") %>%
  arrange(playerID, start_year, team_name)

team_summary <- team_timeline %>%
  group_by(playerID) %>%
  summarise(
    teams = paste(team_name, collapse = " | "),
    team_timeline = paste0(team_name, " (", start_year, "-", end_year, ")", collapse = " | "),
    .groups = "drop"
  )

award_standardizer <- function(award_id) {
  case_when(
    award_id %in% c("Most Valuable Player", "MVP") ~ "MVP",
    award_id == "Cy Young Award" ~ "Cy Young",
    award_id == "Rookie of the Year Award" ~ "Rookie of the Year",
    award_id == "Gold Glove" ~ "Gold Glove",
    award_id == "Silver Slugger" ~ "Silver Slugger",
    award_id == "World Series MVP" ~ "World Series MVP",
    award_id == "All-Star Game MVP" ~ "All-Star Game MVP",
    str_detect(award_id, regex("Mariano Rivera", ignore_case = TRUE)) ~ "Mariano Rivera Reliever of the Year",
    str_detect(award_id, regex("Trevor Hoffman", ignore_case = TRUE)) ~ "Trevor Hoffman Reliever of the Year",
    str_detect(award_id, regex("Reliever of the Year", ignore_case = TRUE)) ~ "Reliever of the Year",
    str_detect(award_id, regex("^Rolaids Relief", ignore_case = TRUE)) ~ "Rolaids Relief Award",
    TRUE ~ NA_character_
  )
}

title_award_map <- c(
  "AL Batting Champion" = "Batting Title",
  "NL Batting Champion" = "Batting Title",
  "AL Home Run Champion" = "Home Run Title",
  "NL Home Run Champion" = "Home Run Title",
  "AL RBI Champion" = "RBI Title",
  "NL RBI Champion" = "RBI Title",
  "AL ERA Champion" = "ERA Title",
  "NL ERA Champion" = "ERA Title",
  "AL Wins Leader" = "Wins Leader",
  "NL Wins Leader" = "Wins Leader",
  "AL Strikeout Champion" = "Strikeout Title",
  "NL Strikeout Champion" = "Strikeout Title",
  "AL Stolen Base Champion" = "Stolen Base Title",
  "NL Stolen Base Champion" = "Stolen Base Title"
)

major_awards <- AwardsPlayers %>%
  mutate(award_name = award_standardizer(awardID)) %>%
  filter(!is.na(award_name)) %>%
  count(playerID, award_name, name = "award_count") %>%
  group_by(playerID) %>%
  summarise(major_awards = paste0(award_name, " (", award_count, ")", collapse = " | "), .groups = "drop")

all_star_counts <- AllstarFull %>%
  distinct(playerID, yearID) %>%
  count(playerID, name = "all_star_selections")

title_awards <- AwardsPlayers %>%
  filter(awardID %in% names(title_award_map)) %>%
  mutate(title_name = unname(title_award_map[awardID])) %>%
  count(playerID, title_name, name = "award_count") %>%
  group_by(playerID) %>%
  summarise(title_awards = paste0(title_name, " (", award_count, ")", collapse = " | "), .groups = "drop")

existing_player_ids <- c()
for (path in c(
  "data/hall_of_fame_lahman_players.csv",
  "data/non_hof_milestone_players.csv",
  "data/active_hall_candidates.csv"
)) {
  if (file.exists(path)) {
    existing_player_ids <- c(existing_player_ids, read_csv(path, show_col_types = FALSE)$playerID)
  }
}
existing_player_ids <- unique(existing_player_ids)

retired_players <- retired_seed %>%
  filter(!playerID %in% existing_player_ids) %>%
  left_join(
    people_lookup %>%
      transmute(playerID = metadata_playerID, direct_name_people = name_people),
    by = "playerID"
  ) %>%
  left_join(people_name_lookup, by = c("name" = "seed_name")) %>%
  mutate(
    metadata_playerID = coalesce(
      if_else(!is.na(direct_name_people), playerID, NA_character_),
      metadata_playerID
    )
  ) %>%
  left_join(
    people_lookup %>%
      select(metadata_playerID, name_people, birthYear, debut, finalGame, bbref_url),
    by = "metadata_playerID"
  ) %>%
  mutate(name = coalesce(name, name_people)) %>%
  select(-direct_name_people, -name_people) %>%
  left_join(primary_positions %>% rename(metadata_playerID = playerID), by = "metadata_playerID") %>%
  left_join(all_positions %>% rename(metadata_playerID = playerID), by = "metadata_playerID") %>%
  left_join(team_summary %>% rename(metadata_playerID = playerID), by = "metadata_playerID") %>%
  left_join(batting_totals %>% rename(metadata_playerID = playerID), by = "metadata_playerID") %>%
  left_join(pitching_totals %>% rename(metadata_playerID = playerID), by = "metadata_playerID") %>%
  left_join(major_awards %>% rename(metadata_playerID = playerID), by = "metadata_playerID") %>%
  left_join(all_star_counts %>% rename(metadata_playerID = playerID), by = "metadata_playerID") %>%
  left_join(title_awards %>% rename(metadata_playerID = playerID), by = "metadata_playerID") %>%
  mutate(
    finalGame = coalesce(finalGame, unname(final_year_overrides[playerID])),
    player_context = "Major League Baseball",
    year_note = "Years taken from Lahman player data. Passed-over status is a manual Hall Ledger designation."
  ) %>%
  select(
    playerID, name, birthYear, debut, finalGame, bbref_url, player_context, year_note,
    primary_position, positions, teams, team_timeline, batting_games, hits, home_runs,
    runs_batted_in, stolen_bases, batting_average, pitching_games, strikeouts, wins,
    losses, saves, innings_pitched, earned_run_average, passed_over_status,
    passed_over_note, passed_over_class_year, include_in_directory, major_awards, all_star_selections, title_awards
  ) %>%
  arrange(name)

write_csv(retired_players, "data/retired_passed_over_players.csv")

message("Wrote data/retired_passed_over_players.csv")

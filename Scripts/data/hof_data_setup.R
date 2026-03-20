# Hall Ledger official Hall data builder

# Builds the official Hall of Fame player dataset and supporting Hall Ledger metadata from Lahman.
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

players_people <- People %>%
  transmute(
    playerID,
    name = paste(nameFirst, nameLast),
    birthYear,
    debut = if_else(is.na(debut), NA_character_, substr(as.character(debut), 1, 4)),
    finalGame = if_else(is.na(finalGame), NA_character_, substr(as.character(finalGame), 1, 4)),
    bbref_url = paste0(
      "https://www.baseball-reference.com/players/",
      substr(playerID, 1, 1),
      "/",
      playerID,
      ".shtml"
    )
  ) %>%
  mutate(
    name = if_else(playerID == "griffke02", "Ken Griffey Jr.", name)
  )

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
    wins = sum(W, na.rm = TRUE),
    losses = sum(L, na.rm = TRUE),
    saves = sum(SV, na.rm = TRUE),
    strikeouts = sum(SO, na.rm = TRUE),
    innings_outs = sum(IPouts, na.rm = TRUE),
    earned_runs = sum(ER, na.rm = TRUE),
    earned_run_average = if_else(
      innings_outs > 0,
      round((earned_runs * 27) / innings_outs, 2),
      NA_real_
    ),
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

team_names <- Teams %>%
  transmute(yearID, teamID, lgID, team_name = name)

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
    award_id %in% c("TSN Major League Player of the Year", "TSN Pitcher of the Year", "TSN Fireman of the Year") ~ NA_character_,
    award_id %in% c("Player of the Week", "Player of the Month", "Baseball America All-Star", "TSN All-Star") ~ NA_character_,
    TRUE ~ award_id
  )
}

major_award_ids <- c("MVP", "Cy Young", "Rookie of the Year", "Gold Glove", "Silver Slugger", "World Series MVP", "All-Star Game MVP", "Mariano Rivera Reliever of the Year", "Trevor Hoffman Reliever of the Year", "Reliever of the Year", "Rolaids Relief Award")

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
  filter(!is.na(award_name), award_name %in% major_award_ids) %>%
  count(playerID, award_name, name = "award_count") %>%
  arrange(playerID, award_name) %>%
  group_by(playerID) %>%
  summarise(major_awards = paste0(award_name, " (", award_count, ")", collapse = " | "), .groups = "drop")

title_awards <- AwardsPlayers %>%
  filter(awardID %in% names(title_award_map)) %>%
  mutate(title_name = unname(title_award_map[awardID])) %>%
  count(playerID, title_name, name = "award_count") %>%
  arrange(playerID, title_name) %>%
  group_by(playerID) %>%
  summarise(title_awards = paste0(title_name, " (", award_count, ")", collapse = " | "), .groups = "drop")

all_star_counts <- AllstarFull %>%
  distinct(playerID, yearID) %>%
  count(playerID, name = "all_star_selections")

hall_players <- HallOfFame %>%
  filter(inducted == "Y", category == "Player") %>%
  group_by(playerID) %>%
  summarise(induction_year = min(yearID, na.rm = TRUE), .groups = "drop") %>%
  left_join(players_people, by = "playerID") %>%
  left_join(primary_positions, by = "playerID") %>%
  left_join(all_positions, by = "playerID") %>%
  left_join(team_summary, by = "playerID") %>%
  left_join(batting_totals, by = "playerID") %>%
  left_join(pitching_totals, by = "playerID") %>%
  left_join(major_awards, by = "playerID") %>%
  left_join(all_star_counts, by = "playerID") %>%
  left_join(title_awards, by = "playerID") %>%
  mutate(player_context = "Major League Baseball", year_note = "Years taken from Lahman player data.") %>%
  select(induction_year, playerID, name, birthYear, debut, finalGame, bbref_url, player_context, year_note, primary_position, positions, teams, team_timeline, batting_games, hits, home_runs, runs_batted_in, stolen_bases, batting_average, pitching_games, wins, losses, saves, strikeouts, innings_pitched, earned_run_average, major_awards, all_star_selections, title_awards) %>%
  arrange(name)

recent_official <- tribble(
  ~year, ~category, ~name,
  2026, "Player", "Carlos Beltran",
  2026, "Player", "Andruw Jones",
  2026, "Player", "Jeff Kent",
  2025, "Player", "Ichiro Suzuki",
  2025, "Player", "CC Sabathia",
  2025, "Player", "Billy Wagner",
  2025, "Player", "Dick Allen",
  2025, "Player", "Dave Parker",
  2024, "Player", "Adrian Beltre",
  2024, "Player", "Joe Mauer",
  2024, "Player", "Todd Helton"
) %>%
  left_join(players_people, by = "name") %>%
  left_join(primary_positions, by = "playerID") %>%
  left_join(all_positions, by = "playerID") %>%
  left_join(team_summary, by = "playerID") %>%
  left_join(batting_totals, by = "playerID") %>%
  left_join(pitching_totals, by = "playerID") %>%
  left_join(major_awards, by = "playerID") %>%
  left_join(all_star_counts, by = "playerID") %>%
  left_join(title_awards, by = "playerID") %>%
  mutate(final_game = finalGame, source = "recent_official_class") %>%
  select(year, category, name, playerID, birthYear, debut, final_game, bbref_url, primary_position, positions, teams, team_timeline, batting_games, hits, home_runs, runs_batted_in, stolen_bases, batting_average, pitching_games, wins, losses, saves, strikeouts, innings_pitched, earned_run_average, major_awards, all_star_selections, title_awards, source) %>%
  arrange(desc(year), name)

recent_player_rows <- recent_official %>%
  filter(category == "Player", !playerID %in% hall_players$playerID) %>%
  transmute(induction_year = year, playerID, name, birthYear, debut, finalGame = final_game, bbref_url, player_context = "Major League Baseball", year_note = "Years taken from Lahman player data.", primary_position, positions, teams, team_timeline, batting_games, hits, home_runs, runs_batted_in, stolen_bases, batting_average, pitching_games, wins, losses, saves, strikeouts, innings_pitched, earned_run_average, major_awards, all_star_selections, title_awards)

hall_players_master <- bind_rows(hall_players, recent_player_rows) %>%
  filter(playerID != "grantfr99") %>%
  arrange(name)

write_csv(hall_players_master, "data/hall_of_fame_lahman_players.csv")
write_csv(recent_official, "data/official_hof.csv")

message("Wrote data/hall_of_fame_lahman_players.csv and data/official_hof.csv")

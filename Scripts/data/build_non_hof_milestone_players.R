# Hall Ledger non-HOF milestone player builder

# Builds the statistical-induction player dataset from Hall Ledger milestone rules and Lahman data.
suppressPackageStartupMessages({
  library(Lahman)
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(purrr)
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
    bbref_url = paste0("https://www.baseball-reference.com/players/", substr(playerID, 1, 1), "/", playerID, ".shtml")
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

batting_yearly <- Batting %>%
  group_by(playerID, yearID) %>%
  summarise(
    season_hits = sum(H, na.rm = TRUE),
    season_home_runs = sum(HR, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(playerID, yearID) %>%
  group_by(playerID) %>%
  mutate(
    career_hits = cumsum(season_hits),
    career_home_runs = cumsum(season_home_runs)
  ) %>%
  ungroup()

pitching_yearly <- Pitching %>%
  group_by(playerID, yearID) %>%
  summarise(
    season_strikeouts = sum(SO, na.rm = TRUE),
    season_wins = sum(W, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(playerID, yearID) %>%
  group_by(playerID) %>%
  mutate(
    career_strikeouts = cumsum(season_strikeouts),
    career_wins = cumsum(season_wins)
  ) %>%
  ungroup()

first_home_run_500_year <- batting_yearly %>%
  filter(career_home_runs >= 500) %>%
  group_by(playerID) %>%
  summarise(first_500_hr_year = min(yearID, na.rm = TRUE), .groups = "drop")

first_hit_3000_year <- batting_yearly %>%
  filter(career_hits >= 3000) %>%
  group_by(playerID) %>%
  summarise(first_3000_hit_year = min(yearID, na.rm = TRUE), .groups = "drop")

first_strikeout_3000_year <- pitching_yearly %>%
  filter(career_strikeouts >= 3000) %>%
  group_by(playerID) %>%
  summarise(first_3000_so_year = min(yearID, na.rm = TRUE), .groups = "drop")

first_win_300_year <- pitching_yearly %>%
  filter(career_wins >= 300) %>%
  group_by(playerID) %>%
  summarise(first_300_win_year = min(yearID, na.rm = TRUE), .groups = "drop")

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

hall_player_ids <- HallOfFame %>%
  filter(inducted == "Y", category == "Player") %>%
  distinct(playerID) %>%
  pull(playerID)

milestone_players <- players_people %>%
  left_join(batting_totals, by = "playerID") %>%
  left_join(pitching_totals, by = "playerID") %>%
  left_join(primary_positions, by = "playerID") %>%
  left_join(all_positions, by = "playerID") %>%
  left_join(team_summary, by = "playerID") %>%
  left_join(major_awards, by = "playerID") %>%
  left_join(all_star_counts, by = "playerID") %>%
  left_join(title_awards, by = "playerID") %>%
  left_join(first_home_run_500_year, by = "playerID") %>%
  left_join(first_hit_3000_year, by = "playerID") %>%
  left_join(first_strikeout_3000_year, by = "playerID") %>%
  left_join(first_win_300_year, by = "playerID") %>%
  filter(!playerID %in% hall_player_ids) %>%
  mutate(
    qualifying_milestones = pmap_chr(
      list(home_runs, hits, strikeouts, wins),
      function(home_runs, hits, strikeouts, wins) {
        milestones <- c()
        if (!is.na(home_runs) && home_runs >= 500) milestones <- c(milestones, "500+ HR")
        if (!is.na(hits) && hits >= 3000) milestones <- c(milestones, "3000+ Hits")
        if (!is.na(strikeouts) && strikeouts >= 3000) milestones <- c(milestones, "3000+ SO")
        if (!is.na(wins) && wins >= 300) milestones <- c(milestones, "300+ Wins")
        paste(milestones, collapse = " | ")
      }
    ),
    milestone_class_year = pmap_int(
      list(first_500_hr_year, first_3000_hit_year, first_3000_so_year, first_300_win_year),
      function(first_500_hr_year, first_3000_hit_year, first_3000_so_year, first_300_win_year) {
        years <- c(first_500_hr_year, first_3000_hit_year, first_3000_so_year, first_300_win_year)
        years <- years[!is.na(years)]
        if (length(years) == 0) {
          return(NA_integer_)
        }
        min(years)
      }
    ),
    player_context = "Major League Baseball",
    year_note = "Years taken from Lahman player data."
  ) %>%
  filter(qualifying_milestones != "") %>%
  select(playerID, name, birthYear, debut, finalGame, bbref_url, player_context, year_note, primary_position, positions, teams, team_timeline, batting_games, hits, home_runs, runs_batted_in, stolen_bases, batting_average, pitching_games, strikeouts, wins, losses, saves, innings_pitched, earned_run_average, qualifying_milestones, milestone_class_year, major_awards, all_star_selections, title_awards) %>%
  arrange(name)

write_csv(milestone_players, "data/non_hof_milestone_players.csv")

message("Wrote data/non_hof_milestone_players.csv")

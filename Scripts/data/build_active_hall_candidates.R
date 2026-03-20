# Builds the active-induction player dataset from Lahman data plus Hall Ledger seed choices.

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

seed_path <- "data/active_hall_candidates_seed.csv"
if (!file.exists(seed_path)) {
  write_csv(
    tibble(
      playerID = c(
        "ohtansh01", "sotoju01", "troutmi01", "freemfr01", "bettsmo01",
        "judgeaa01", "lindofr01", "altuvjo01", "ramirjo01", "machama01",
        "harpebr03", "arenano01", "stantmi03", "seageco01", "goldspa01",
        "salech01", "degroja01", "colege01", "chapmar01", "janseke01"
      ),
      name = c(
        "Shohei Ohtani", "Juan Soto", "Mike Trout", "Freddie Freeman", "Mookie Betts",
        "Aaron Judge", "Francisco Lindor", "Jose Altuve", "Jose Ramirez", "Manny Machado",
        "Bryce Harper", "Nolan Arenado", "Giancarlo Stanton", "Corey Seager", "Paul Goldschmidt",
        "Chris Sale", "Jacob deGrom", "Gerrit Cole", "Aroldis Chapman", "Kenley Jansen"
      ),
      active_status = c(
        "Active Hall Player", "Active Hall Player", "Active Hall Player", "Active Hall Player", "Active Hall Player",
        "Active Hall Player", "Active Hall Player", "Active Hall Player", "Active Hall Player", "Active Hall Player",
        "Active Hall Player", "Active Hall Player", "Active Hall Player", "Active Hall Player", "Active Hall Player",
        "Active Hall Player", "Active Hall Player", "Active Hall Player", "Active Hall Player", "Active Hall Player"
      ),
      active_note = c(
        "A historic two-way peak and multiple MVPs already make him a Hall Ledger Hall of Famer.",
        "An elite age-curve and on-base profile already put him on a Hall Ledger Hall of Fame level.",
        "An MVP peak and all-around value already built a Hall Ledger Hall-level resume.",
        "Long-term star production and consistency already place him on Hall Ledger.",
        "MVP-level peak plus all-around value already make him one of the clearest active Hall Ledger cases.",
        "Historic power seasons and MVP-level peak already make him a major Hall Ledger case.",
        "Elite two-way shortstop value already gives him a Hall Ledger Hall-level resume.",
        "An MVP second-base resume with star-level October visibility already puts him on Hall Ledger.",
        "Sustained elite production at third base already gives him a Hall Ledger Hall case.",
        "Long-run star value and a premium-position resume already put him on Hall Ledger.",
        "Two MVPs and a star-level peak already make him a Hall Ledger Hall of Famer.",
        "A historic Gold Glove stack at third base already anchors a Hall Ledger Hall case.",
        "His MVP peak and power resume already put him in the Hall Ledger Hall conversation.",
        "His October resume, star shortstop peak, and championship-level production already make him an active Hall Ledger case.",
        "His MVP season and long-run first-base production already make him a Hall Ledger Hall case.",
        "His Cy Young peak and strikeout-heavy left-handed resume already make him a Hall Ledger Hall case.",
        "His Cy Young peak and dominant run prevention already make him a Hall Ledger Hall case.",
        "His Cy Young resume and long-run ace value already put him on Hall Ledger.",
        "A historic relief peak and long-run closer value already make him a Hall Ledger Hall case.",
        "An elite closer resume with sustained save totals already makes him a Hall Ledger Hall case."
      )
    ),
    seed_path
  )
}

active_seed <- read_csv(seed_path, show_col_types = FALSE) %>%
  filter(!playerID %in% c("verlaju01", "scherma01")) %>%
  mutate(
    active_status = coalesce(
      if ("active_status" %in% names(.)) as.character(active_status) else NA_character_,
      if ("active_case" %in% names(.)) as.character(active_case) else NA_character_,
      "Active Hall Player"
    )
  ) %>%
  distinct(playerID, .keep_all = TRUE)

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

active_players <- active_seed %>%
  left_join(players_people, by = "playerID") %>%
  mutate(name = coalesce(name, name_people)) %>%
  select(-name_people) %>%
  left_join(primary_positions, by = "playerID") %>%
  left_join(all_positions, by = "playerID") %>%
  left_join(team_summary, by = "playerID") %>%
  left_join(batting_totals, by = "playerID") %>%
  left_join(pitching_totals, by = "playerID") %>%
  left_join(major_awards, by = "playerID") %>%
  left_join(all_star_counts, by = "playerID") %>%
  left_join(title_awards, by = "playerID") %>%
  mutate(
    major_awards = case_when(
      playerID == "chapmar01" ~ "Mariano Rivera Reliever of the Year (2)",
      playerID == "kimbrcr01" ~ "Mariano Rivera Reliever of the Year (1) | Trevor Hoffman Reliever of the Year (1) | Rolaids Relief Award (1)",
      TRUE ~ major_awards
    ),
    player_context = "Major League Baseball",
    year_note = "Years taken from Lahman player data. Active-player status is a Hall Ledger snapshot as of March 16, 2026.",
    finalGame = NA_character_
  ) %>%
  select(
    playerID, name, birthYear, debut, finalGame, bbref_url, player_context, year_note,
    primary_position, positions, teams, team_timeline, batting_games, hits, home_runs,
    runs_batted_in, stolen_bases, batting_average, pitching_games, strikeouts, wins,
    losses, saves, innings_pitched, earned_run_average, active_note,
    active_status,
    major_awards, all_star_selections, title_awards
  ) %>%
  arrange(name)

write_csv(active_players, "data/active_hall_candidates.csv")

message("Wrote data/active_hall_candidates.csv")

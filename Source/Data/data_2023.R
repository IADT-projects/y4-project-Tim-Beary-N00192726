library(tidyverse)
library(tidyr)
library(dplyr)
library(worldfootballR)




team_standard <- fb_big5_advanced_season_stats(
  season_end_year = 2023,
  stat_type = "standard",
  team_or_player = "team"
)

team_standard_filtered <- team_standard |>
  filter(Team_or_Opponent == "team")

write.csv(team_standard_filtered, "Utils/team_data_2023.csv")


player_standard <- fb_big5_advanced_season_stats(
  season_end_year = 2023,
  stat_type = "standard",
  team_or_player = "player"
)

player_standard$Age <- as.numeric(gsub("-.*", "", player_standard$Age))


player_standard$index <- 1:nrow(player_standard)

player_standard_filtered <- player_standard |>
  select(
    index,
    Squad,
    Comp,
    Player,
    Age,
    Url,
    Nation,
    Pos,
    Starts_Playing,
    MP_Playing,
    Min_Playing,
    Gls,
    Ast,
    G_minus_PK,
    npxG_Expected,
    xAG_Expected,
    PrgC_Progression,
    PrgP_Progression,
    PrgR_Progression
  )



player_shooting <- fb_big5_advanced_season_stats(
  season_end_year = 2023,
  stat_type = "shooting",
  team_or_player = "player"
)

player_shooting$index <- 1:nrow(player_shooting)

player_shooting_filtered <- player_shooting |>
  select(
    index,
    Sh_Standard,
    SoT_Standard,
    G_per_Sh_Standard,
    G_per_SoT_Standard,
    npxG_per_Sh_Expected,
    
  )

player_passing <- fb_big5_advanced_season_stats(
  season_end_year = 2023,
  stat_type = "passing",
  team_or_player = "player"
)

player_passing$index <- 1:nrow(player_passing)

player_passing_filtered <- player_passing |>
  select(index, Att_Total, Cmp_percent_Total, xA,  Final_Third, PPA, CrsPA)

player_gsca <- fb_big5_advanced_season_stats(
  season_end_year = 2023,
  stat_type = "gca",
  team_or_player = "player"
)

player_gsca$index <- 1:nrow(player_gsca)

player_gsca_filtered <- player_gsca |>
  select(index, SCA_SCA, GCA_GCA)

player_defense <- fb_big5_advanced_season_stats(
  season_end_year = 2023,
  stat_type = "defense",
  team_or_player = "player"
)

player_defense$index <- 1:nrow(player_defense)

player_defense_filtered <- player_defense |>
  select(
    index,
    TklW_Tackles,
    Blocks_Blocks,
    Int,
    `Tkl+Int`,
    Clr,
    Tkl_Challenges,
    Tkl_percent_Challenges
  )

player_possession <- fb_big5_advanced_season_stats(
  season_end_year = 2023,
  stat_type = "possession",
  team_or_player = "player"
)

player_possession$index <- 1:nrow(player_possession)

player_possession_filtered <- player_possession |>
  select(
    index,
    Touches_Touches,
    `Att 3rd_Touches`,
    Att_Take,
    Succ_Take,
    Succ_percent_Take,
    Carries_Carries,
    Final_Third_Carries,
    CPA_Carries
  )

player_misc <- fb_big5_advanced_season_stats(
  season_end_year = 2023,
  stat_type = "misc",
  team_or_player = "player"
)

player_misc$index <- 1:nrow(player_possession)

player_misc_filtered <- player_misc |>
  select(index,  Fld, Crs, Won_percent_Aerial)


player_combined_df = list(
  player_standard_filtered,
  player_shooting_filtered,
  player_passing_filtered,
  player_gsca_filtered,
  player_defense_filtered,
  player_possession_filtered,
  player_misc_filtered
)

player_combined_df <- player_combined_df |>
  reduce(right_join, by = 'index')



player_combined_df <- player_combined_df |>
  rename(Name = Player)
transfermarkt_players <- player_dictionary_mapping()
transfermarkt_players <- transfermarkt_players |>
  rename(Name = PlayerFBref)

player_positions = list(transfermarkt_players, player_combined_df)
player_combined_df <- player_positions |>
  reduce(right_join, by = 'Name')

player_combined_df <- subset(player_combined_df, select = -Pos)
player_combined_df <- subset(player_combined_df, select = -UrlFBref)
player_combined_df <-
  subset(player_combined_df, select = -UrlTmarkt)
player_combined_df <- player_combined_df |>
  rename(Position = TmPos)

player_combined_df <-
  subset(player_combined_df,!duplicated(player_combined_df$Name))

player_combined_df$Position <-
  ifelse(
    player_combined_df$Position == "Left-Back" |
      player_combined_df$Position == "Right-Back",
    "Full-Back",
    player_combined_df$Position
  )
player_combined_df$Position <-
  ifelse(
    player_combined_df$Position == "Central Midfield" |
      player_combined_df$Position == "Defensive Midfield",
    "Midfield",
    player_combined_df$Position
  )
player_combined_df$Position <-
  ifelse(
    player_combined_df$Position == "Centre-Forward" |
      player_combined_df$Position == "Second Striker",
    "Forward",
    player_combined_df$Position
  )
player_combined_df$Position <-
  ifelse(
    player_combined_df$Position == "Left Winger" |
      player_combined_df$Position == "Right Winger",
    "Winger",
    player_combined_df$Position
  )
player_combined_df$Position <-
  ifelse(
    player_combined_df$Position == "Left Midfield" |
      player_combined_df$Position == "Right Midfield",
    "Winger",
    player_combined_df$Position
  )

player_data_2023 <- player_combined_df

write.csv(player_data_2023, "Utils/player_data_2023.csv")

players_per90_2023 <- player_combined_df |>
  mutate_at(vars(12:50), ~ (. / Min_Playing) * 90) |>
  mutate_at(vars(12:50), round, 2)

write.csv(players_per90_2023, "Utils/players_per90_2023.csv")


forwards_stats <- players_per90_2023 |>
  filter(Position == "Forward") |>
  filter(Min_Playing >= 450) |>
  gather(Statistic, Value,-Name,-Position)

forwards_percentiles <- players_per90_2023 |>
  filter(Position == "Forward") |>
  filter(Min_Playing >= 450) |>
  mutate(across(where(is.numeric), ~ round(cume_dist(.), 2))) |>
  gather(Statistic, percentile,-Name,-Position)

forwards_stats$index <- 1:nrow(forwards_stats)
forwards_percentiles$index <- 1:nrow(forwards_percentiles)

forwards_scouting_reports <-
  merge(forwards_stats, forwards_percentiles[, c("percentile", "index")], by = "index")

forwards_scouting_reports <-
  filter(
    forwards_scouting_reports,
    Statistic != "index",
    Statistic != "Age",
    Statistic != "Squad",
    Statistic != "Comp",
    Statistic != "Nation",
    Statistic != "Url"
  )

midfielders_stats <- players_per90_2023 |>
  filter(Position == "Midfield") |>
  filter(Min_Playing >= 450) |>
  gather(Statistic, Value,-Name,-Position)

midfielders_percentiles <- players_per90_2023 |>
  filter(Position == "Midfield") |>
  filter(Min_Playing >= 450) |>
  mutate(across(where(is.numeric), ~ round(cume_dist(.), 2))) |>
  gather(Statistic, percentile,-Name,-Position)

midfielders_stats$index <- 1:nrow(midfielders_stats)
midfielders_percentiles$index <- 1:nrow(midfielders_percentiles)

midfielders_scouting_reports <-
  merge(midfielders_stats, midfielders_percentiles[, c("percentile", "index")], by = "index")

midfielders_scouting_reports <-
  filter(
    midfielders_scouting_reports,
    Statistic != "index",
    Statistic != "Age",
    Statistic != "Squad",
    Statistic != "Comp",
    Statistic != "Nation",
    Statistic != "Url"
  )

centrebacks_stats <- players_per90_2023 |>
  filter(Position == "Centre-Back") |>
  filter(Min_Playing >= 450) |>
  gather(Statistic, Value,-Name,-Position)

centrebacks_percentiles <- players_per90_2023 |>
  filter(Position == "Centre-Back") |>
  filter(Min_Playing >= 450) |>
  mutate(across(where(is.numeric), ~ round(cume_dist(.), 2))) |>
  gather(Statistic, percentile,-Name,-Position)

centrebacks_stats$index <- 1:nrow(centrebacks_stats)
centrebacks_percentiles$index <- 1:nrow(centrebacks_percentiles)

centrebacks_scouting_reports <-
  merge(centrebacks_stats, centrebacks_percentiles[, c("percentile", "index")], by = "index")

centrebacks_scouting_reports <-
  filter(
    centrebacks_scouting_reports,
    Statistic != "index",
    Statistic != "Age",
    Statistic != "Squad",
    Statistic != "Comp",
    Statistic != "Nation",
    Statistic != "Url"
  )

fullbacks_stats <- players_per90_2023 |>
  filter(Position == "Full-Back") |>
  filter(Min_Playing >= 450) |>
  gather(Statistic, Value,-Name,-Position)

fullbacks_percentiles <- players_per90_2023 |>
  filter(Position == "Full-Back") |>
  filter(Min_Playing >= 450) |>
  mutate(across(where(is.numeric), ~ round(cume_dist(.), 2))) |>
  gather(Statistic, percentile,-Name,-Position)

fullbacks_stats$index <- 1:nrow(fullbacks_stats)
fullbacks_percentiles$index <- 1:nrow(fullbacks_percentiles)

fullbacks_scouting_reports <-
  merge(fullbacks_stats, fullbacks_percentiles[, c("percentile", "index")], by = "index")

fullbacks_scouting_reports <-
  filter(
    fullbacks_scouting_reports,
    Statistic != "index",
    Statistic != "Age",
    Statistic != "Squad",
    Statistic != "Comp",
    Statistic != "Nation",
    Statistic != "Url"
  )

wingers_attmids_stats <- players_per90_2023 |>
  filter(Position == "Winger" | Position == "Attacking Midfield") |>
  filter(Min_Playing >= 450) |>
  gather(Statistic, Value,-Name,-Position)

wingers_attmids_percentiles <- players_per90_2023 |>
  filter(Position == "Winger" | Position == "Attacking Midfield") |>
  filter(Min_Playing >= 450) |>
  mutate(across(where(is.numeric), ~ round(cume_dist(.), 2))) |>
  gather(Statistic, percentile,-Name,-Position)

wingers_attmids_stats$index <- 1:nrow(wingers_attmids_stats)
wingers_attmids_percentiles$index <-
  1:nrow(wingers_attmids_percentiles)

wingers_attmids_scouting_reports <-
  merge(wingers_attmids_stats, wingers_attmids_percentiles[, c("percentile", "index")], by = "index")

wingers_attmids_scouting_reports <-
  filter(
    wingers_attmids_scouting_reports,
    Statistic != "index",
    Statistic != "Age",
    Statistic != "Squad",
    Statistic != "Comp",
    Statistic != "Nation",
    Statistic != "Url"
  )

complete_scouting_reports <-
  rbind(
    forwards_scouting_reports,
    wingers_attmids_scouting_reports,
    midfielders_scouting_reports,
    centrebacks_scouting_reports,
    fullbacks_scouting_reports
  )

complete_scouting_reports$percentile <-
  as.numeric(complete_scouting_reports$percentile)
complete_scouting_reports$Value <-
  as.numeric(complete_scouting_reports$Value)
class(complete_scouting_reports$Value)


complete_scouting_reports$percentile <-
  complete_scouting_reports$percentile * 100

scouting_reports_2023 <- complete_scouting_reports

write.csv(scouting_reports_2023, "Utils/scouting_reports_2023.csv")


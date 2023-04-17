library(tidyverse)
library(tidyr)
library(dplyr)
library(worldfootballR)




team_standard <- load_fb_big5_advanced_season_stats(
  season_end_year = 2018:2022,
  stat_type = "standard",
  team_or_player = "team"
)

team_standard_filtered <- team_standard |>
  filter(Team_or_Opponent == "team")

write.csv(team_standard_filtered, "Utils/team_data_historic.csv")


# historic_player_data <- fb_big5_advanced_season_stats(
#   season_end_year = 2018:2022,
#   stat_type = "standard",
#   team_or_player = "player"
# )
# 
# write.csv(historic_player_data, "Utils/historic_player_data.csv")


player_standard <- load_fb_big5_advanced_season_stats(
  season_end_year = 2018:2022,
  stat_type = "standard",
  team_or_player = "player"
)

player_standard$Age <- as.numeric(gsub("-.*", "", player_standard$Age))


player_standard$index <- 1:nrow(player_standard)

player_standard_filtered <- player_standard |>
  select(
    index,
    Season_End_Year,
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



player_shooting <- load_fb_big5_advanced_season_stats(
  season_end_year = 2018:2022,
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

player_passing <- load_fb_big5_advanced_season_stats(
  season_end_year = 2018:2022,
  stat_type = "passing",
  team_or_player = "player"
)

player_passing$index <- 1:nrow(player_passing)

player_passing_filtered <- player_passing |>
  select(index, Att_Total, Cmp_percent_Total, xA,  Final_Third, PPA, CrsPA)

player_gsca <- load_fb_big5_advanced_season_stats(
  season_end_year = 2018:2022,
  stat_type = "gca",
  team_or_player = "player"
)

player_gsca$index <- 1:nrow(player_gsca)

player_gsca_filtered <- player_gsca |>
  select(index, SCA_SCA, GCA_GCA)

player_defense <- load_fb_big5_advanced_season_stats(
  season_end_year = 2018:2022,
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

player_possession <- load_fb_big5_advanced_season_stats(
  season_end_year = 2018:2022,
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

player_misc <- load_fb_big5_advanced_season_stats(
  season_end_year = 2018:2022,
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

player_data_historic <- player_combined_df

write.csv(player_data_historic, "Utils/player_data_historic.csv")

players_per90_historic <- player_combined_df |>
  mutate_at(vars(13:50), ~ (. / Min_Playing) * 90) |>
  mutate_at(vars(13:50), round, 2)

write.csv(players_per90_historic, "Utils/players_per90_historic.csv")

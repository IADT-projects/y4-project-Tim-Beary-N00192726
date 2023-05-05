library(tidyverse)
library(tidyr)
library(dplyr)
library(worldfootballR)




player_standard <- load_fb_big5_advanced_season_stats(
  season_end_year = 2018:2023,
  stat_type = "standard",
  team_or_player = "player"
)

player_standard$Age <-
  as.numeric(gsub("-.*", "", player_standard$Age))


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
    xAG_Expected
  )



player_shooting <- load_fb_big5_advanced_season_stats(
  season_end_year = 2018:2023,
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
  season_end_year = 2018:2023,
  stat_type = "passing",
  team_or_player = "player"
)

player_passing$index <- 1:nrow(player_passing)

player_passing_filtered <- player_passing |>
  select(index, Cmp_Total, Att_Total, Cmp_percent_Total, PrgP, PrgDist_Total, xA, KP,  Final_Third, PPA, CrsPA)

player_gsca <- load_fb_big5_advanced_season_stats(
  season_end_year = 2018:2023,
  stat_type = "gca",
  team_or_player = "player"
)

player_gsca$index <- 1:nrow(player_gsca)

player_gsca_filtered <- player_gsca |>
  select(index, SCA_SCA, GCA_GCA)

player_defense <- load_fb_big5_advanced_season_stats(
  season_end_year = 2018:2023,
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
  season_end_year = 2018:2023,
  stat_type = "possession",
  team_or_player = "player"
)

player_possession$index <- 1:nrow(player_possession)

player_possession_filtered <- player_possession |>
  select(
    index,
    Touches_Touches,
    `Att 3rd_Touches`,
    `Att Pen_Touches`,
    Att_Take,
    Succ_Take,
    Succ_percent_Take,
    Carries_Carries,
    PrgC_Carries,
    PrgDist_Carries,
    Final_Third_Carries,
    CPA_Carries,
    PrgR_Receiving
  )

player_misc <- load_fb_big5_advanced_season_stats(
  season_end_year = 2018:2023,
  stat_type = "misc",
  team_or_player = "player"
)

player_misc$index <- 1:nrow(player_possession)

player_misc_filtered <- player_misc |>
  select(index, Fls,  Fld, Crs, Recov, Won_percent_Aerial)


player_combined_df = list(
  player_standard_filtered,
  player_shooting_filtered,
  player_possession_filtered,
  player_passing_filtered,
  player_gsca_filtered,
  player_defense_filtered,
  player_misc_filtered
)

player_combined_df <- player_combined_df |>
  reduce(right_join, by = 'index')



player_combined_df <- player_combined_df |>
  rename(Name = Player) |>
  rename(UrlFBref = Url)
transfermarkt_players <- player_dictionary_mapping()
transfermarkt_players <- transfermarkt_players |>
  select(-PlayerFBref)

player_positions = list(transfermarkt_players, player_combined_df)
player_combined_df <- player_positions |>
  reduce(right_join, by = 'UrlFBref')

player_combined_df <- subset(player_combined_df, select = -Pos)
player_combined_df <-
  subset(player_combined_df, select = -UrlTmarkt)
player_combined_df <- player_combined_df |>
  rename(Position = TmPos) |>
  rename(Url = UrlFBref)


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
    "Midfielder",
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



player_combined_df <- player_combined_df |>
  rename(Starts = Starts_Playing) |>
  rename(`Matches Played` = MP_Playing) |>
  rename(`Minutes Played` = Min_Playing) |>
  rename(Goals = Gls) |>
  rename(Assists = Ast) |>
  rename(`Goals minus Penalties` = G_minus_PK) |>
  rename(`Non-Penalty xG` = npxG_Expected) |>
  rename(xAG = xAG_Expected) |>
  rename(`Total Shots` = Sh_Standard) |>
  rename(`Shots on Target` = SoT_Standard) |>
  rename(`Goals per Shot` = G_per_Sh_Standard) |>
  rename(`Goals per Shot on Target` = G_per_SoT_Standard) |>
  rename(`Non-Penalty xG per Shot` = npxG_per_Sh_Expected) |>
  rename(`Passes Completed` = Cmp_Total) |>
  rename(`Passes Attempted` = Att_Total) |>
  rename(`Pass Completion %` = Cmp_percent_Total) |>
  rename(`Progressive Passes` = PrgP) |>
  rename(`Progressive Passing Distance` = PrgDist_Total) |>
  rename(xA = xA) |>
  rename(`Key Passes` = KP) |>
  rename(`Passes into Final Third` = Final_Third) |>
  rename(`Passes into Penalty Area` = PPA) |>
  rename(`Crosses into Penalty Area` = CrsPA) |>
  rename(`Shot Creating Actions` = SCA_SCA) |>
  rename(`Goal Creating Actions` = GCA_GCA) |>
  rename(`Tackles Won` = TklW_Tackles) |>
  rename(`Blocks` = Blocks_Blocks) |>
  rename(`Interceptions` = Int) |>
  rename(`Tackles + Int` = `Tkl+Int`) |>
  rename(`Clearances` = Clr) |>
  rename(`Dribblers Tackled` = Tkl_Challenges) |>
  rename(`Dribblers Tackled %` = Tkl_percent_Challenges) |>
  rename(`Touches` = Touches_Touches) |>
  rename(`Attacking 3rd Touches` = `Att 3rd_Touches`) |>
  rename(`Touches in Penalty Area` = `Att Pen_Touches`) |>
  rename(`Dribbles Attempted` = Att_Take) |>
  rename(`Successful Dribbles` = Succ_Take) |>
  rename(`Successful Dribbles %` = Succ_percent_Take) |>
  rename(`Carries` = Carries_Carries) |>
  rename(`Progressive Carries` = PrgC_Carries) |>
  rename(`Progressive Carrying Distance` = PrgDist_Carries) |>
  rename(`Carries into Final Third` = Final_Third_Carries) |>
  rename(`Carries into Penalty Area` = CPA_Carries) |>
  rename(`Progressive Passes Recieved` = PrgR_Receiving) |>
  rename(`Fouls Commited` = Fls) |>
  rename(`Fouls Drawn` = Fld) |>
  rename(`Crosses` = Crs) |>
  rename(`Ball Recoveries` = Recov) |>
  rename(`Aerials Won %` = Won_percent_Aerial)


player_data_historic <- player_combined_df

write.csv(player_data_historic, "Data/player_data_historic.csv")

players_per90_historic <- player_combined_df |>
  mutate_at(vars(13:58, -`Successful Dribbles %`, -`Pass Completion %`, -`Dribblers Tackled %`, -`Aerials Won %`), ~ (. / `Minutes Played`) * 90) |>
  mutate_at(vars(13:58, -`Successful Dribbles %`, -`Pass Completion %`, -`Dribblers Tackled %`, -`Aerials Won %`), round, 2)

write.csv(players_per90_historic, "Data/players_per90_historic.csv")

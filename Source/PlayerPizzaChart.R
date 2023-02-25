library(shiny)
library(tidyverse)
library(ggplot2)
library(worldfootballR)
library(ggshakeR)
library(tidyr)
library(geomtextpath)
library(dplyr)



player_standard <- fb_big5_advanced_season_stats(
  season_end_year = 2023, stat_type = "standard", team_or_player = "player"
)

player_standard$index <- 1:nrow(player_standard)

player_standard_filtered <- player_standard |>
  select(index, Squad, Comp, Player, Url, Nation, Pos, Starts_Playing, Min_Playing, Gls, Ast, Gls_Per, Ast_Per, npxG_Expected, xAG_Expected, npxG_Per, xAG_Per, PrgC_Progression, PrgP_Progression, PrgR_Progression)

player_shooting <- load_fb_big5_advanced_season_stats(
  season_end_year = 2023, stat_type = "shooting", team_or_player = "player"
)

player_shooting$index <- 1:nrow(player_shooting)

player_shooting_filtered <- player_shooting |>
  select(index, Sh_Standard,SoT_Standard, Sh_per_90_Standard, SoT_per_90_Standard, npxG_Expected)

player_passing <- load_fb_big5_advanced_season_stats(
  season_end_year = 2023, stat_type = "passing", team_or_player = "player"
)

player_passing$index <- 1:nrow(player_passing)

player_passing_filtered <- player_passing |>
  select(index,  Cmp_percent_Total, Final_Third, PPA)

player_gsca <- load_fb_big5_advanced_season_stats(
  season_end_year = 2023, stat_type = "gca", team_or_player = "player"
)

player_gsca$index <- 1:nrow(player_gsca)

player_gsca_filtered <- player_gsca |>
  select(index, SCA_SCA, SCA90_SCA, GCA_GCA, GCA90_GCA)

player_defense <- load_fb_big5_advanced_season_stats(
  season_end_year = 2023, stat_type = "defense", team_or_player = "player"
)

player_defense$index <- 1:nrow(player_defense)

player_defense_filtered <- player_defense |>
  select(index, TklW_Tackles, Blocks_Blocks, Int, `Tkl+Int`,Clr, Tkl_Challenges, Tkl_percent_Challenges)

player_possession <- load_fb_big5_advanced_season_stats(
  season_end_year = 2023, stat_type = "possession", team_or_player = "player"
)

player_possession$index <- 1:nrow(player_possession)

player_possession_filtered <- player_possession |>
  select(index, `Mid 3rd_Touches`,`Att 3rd_Touches`, Att_Take, Succ_percent_Take, Final_Third_Carries, CPA_Carries)

player_misc <- load_fb_big5_advanced_season_stats(
  season_end_year = 2023, stat_type = "misc", team_or_player = "player"
)

player_misc$index <- 1:nrow(player_possession)

player_misc_filtered <- player_misc |>
  select(index,  Fld, Crs, Won_percent_Aerial)


player_combined_df = list(player_standard_filtered, player_shooting_filtered, player_passing_filtered, player_gsca_filtered, player_defense_filtered, player_possession_filtered, player_misc_filtered)

player_combined_df <- player_combined_df |>
  reduce(right_join, by='index')


player_combined_df <- player_combined_df |>
  rename(Name = Player)
transfermarkt_players <- player_dictionary_mapping()
transfermarkt_players <- transfermarkt_players |>
  rename(Name = PlayerFBref)

player_positions = list(transfermarkt_players, player_combined_df)
player_combined_df <- player_positions |>
  reduce(right_join, by='Name')

player_combined_df <- subset(player_combined_df, select = -Pos)
player_combined_df <- subset(player_combined_df, select = -UrlFBref)
player_combined_df <- subset(player_combined_df, select = -UrlTmarkt)
player_combined_df <- player_combined_df |>
  rename(Position = TmPos)

player_combined_df <- subset(player_combined_df, !duplicated(player_combined_df$Name))

player_combined_df$Position <- ifelse(player_combined_df$Position == "Left-Back" | player_combined_df$Position == "Right-Back", "Full-Back", player_combined_df$Position)
player_combined_df$Position <- ifelse(player_combined_df$Position == "Central Midfield" | player_combined_df$Position == "Defensive Midfield", "Midfield", player_combined_df$Position)
player_combined_df$Position <- ifelse(player_combined_df$Position == "Centre-Forward" | player_combined_df$Position == "Second Striker", "Forward", player_combined_df$Position)
player_combined_df$Position <- ifelse(player_combined_df$Position == "Left Winger" | player_combined_df$Position == "Right Winger", "Winger", player_combined_df$Position)
player_combined_df$Position <- ifelse(player_combined_df$Position == "Left Midfield" | player_combined_df$Position == "Right Midfield", "Winger", player_combined_df$Position)



forwards_stats <- player_combined_df |>
  filter(Position == "Forward") |>
  filter(Min_Playing >= 450) |>
  gather(Statistic, Value, -Name)

forwards_percentiles <- player_combined_df |>
  filter(Position == "Forward") |>
  filter(Min_Playing >= 450) |>
  mutate(across(where(is.numeric), ~ round(((cume_dist(.) + percent_rank(.)) / 2) * 100))) |>
  gather(Statistic, percentile, -Name)

  forwards_stats$index <- 1:nrow(forwards_stats)
  forwards_percentiles$index <- 1:nrow(forwards_percentiles)



forwards_scouting_reports <- merge(forwards_stats, forwards_percentiles[, c("percentile", "index")], by = "index")

forwards_scouting_reports <- filter(forwards_scouting_reports, Statistic != "index", Statistic != "Squad", Statistic != "Comp", Statistic != "Nation", Statistic != "Url", Statistic != "Position")


ui <- fluidPage(
  titlePanel("Player Scouting Report"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("player", "Select Player:",
                  unique(forwards_scouting_reports$Name),
                  selected = NULL),
      actionButton("submit", "Submit")
    ),
    
    mainPanel(
      plotOutput("radarChart")
    )
  )
)


server <- function(input, output) {
  
  
  observeEvent(input$submit, {
    
    selected_player <- forwards_scouting_reports |>
      filter(Name == input$player)
    selected_player <- selected_player[c(3,4,5,6,7,8,9,10,11,12,13,14),]
    selected_player$index <- 1:12
    selected_player <- selected_player |>
      mutate(type = case_when(
        index %in% 1:4 ~ "Attacking",
        index %in% 5:8 ~ "Possession",
        index %in% 9:12 ~ "Misc"
      ))
    selected_player$type <- factor(selected_player$type, levels = c("Attacking", "Possession", "Misc"))
    
    output$radarChart <- renderPlot({
      
      color1 <- "red"
        color2 <- "blue"
          color3 <- "orange"
            ggplot(data = selected_player, aes(x = reorder(Statistic, index), y = percentile, label= Value)) +
              # add the bar/pizza slices that are colored
              geom_bar(data = selected_player, width = 1,
                       color = "oldlace",
                       stat = "identity") +
              # wrap bar chart as around polar center
              coord_curvedpolar() +
              # add the background behind each bar (alpha at .5 for slight transparency so the bars standout)
              # geom_bar(aes(y=100, fill=type), stat="identity", width=1, alpha=0.5) +
              # add & customize line that border whole pizza
              geom_hline(yintercept = seq(0, 100, by = 100),
                         color = "oldlace",
                         linewidth = 1) +
              # add & customize lines between each pizza slice
              geom_vline(xintercept = seq(.5, 12, by = 1),
                         color = "oldlace",
                         linewidth = .5) +
              # add percentile labels (labels are fill by bar colors) - option 1
              #geom_label(aes(label=value, fill=type), color = "white", size=2.5, fontface="bold", family = "Comic Sans MS", show.legend = FALSE) +
              # add percentile labels (labels are choice of fill and color) - option 2
              geom_label(color = "gray20", fill = "oldlace", size=2.5, fontface="bold", family = "Comic Sans MS", show.legend = FALSE) +
              # manually set the colors of bars (3 here for each group of stats (scoring, possession, defending))
              # scale_fill_manual(values=c(color1, color2, color3)) +
              # theme manipulation to customize plot (play around with these!)
              theme(legend.position = "top",
                    legend.direction = "horizontal",
                    legend.background = element_rect(fill = "oldlace", color="oldlace"),
                    legend.title = element_blank(),
                    legend.text = element_text(colour = "gray20", family = "Comic Sans MS", face = "bold"),
                    legend.key.size = unit(.5, "cm"),
                    legend.box.spacing = unit(0, "mm"),
                    plot.title = element_text(hjust = .5, colour = "gray20", face = "bold", size = 16, family = "Comic Sans MS"),
                    plot.subtitle = element_text(hjust = .5, colour = "gray20", size = 8, family = "Comic Sans MS"),
                    plot.background = element_rect(fill = "oldlace", color="oldlace"),
                    panel.background = element_rect(fill = "oldlace", color="oldlace"),
                    panel.grid = element_blank(),
                    axis.text.y = element_blank(),
                    axis.ticks = element_blank(),
                    axis.title = element_blank(),
                    axis.text.x = element_text(face = "bold", size = 5.5, family = "Comic Sans MS")) 
    })
  })
  
  
  
}
shinyApp(ui = ui, server = server)

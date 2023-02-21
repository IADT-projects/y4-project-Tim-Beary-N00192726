devtools::install_github("JaseZiv/worldfootballR")

library(shiny)
library(tidyverse)
library(ggplot2)
library(worldfootballR)
library(ggshakeR)
library(tidyr)
library(geomtextpath)


player_standard <- load_fb_big5_advanced_season_stats(
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
  select(index, Player, Sh_Standard,SoT_Standard, Sh_per_90_Standard, SoT_per_90_Standard, npxG_Expected)

player_passing <- load_fb_big5_advanced_season_stats(
  season_end_year = 2023, stat_type = "passing", team_or_player = "player"
)

player_passing$index <- 1:nrow(player_passing)

player_passing_filtered <- player_passing |>
  select(index, Player, Cmp_percent_Total, Final_Third, PPA)

player_gsca <- load_fb_big5_advanced_season_stats(
  season_end_year = 2023, stat_type = "gca", team_or_player = "player"
)

player_gsca$index <- 1:nrow(player_gsca)

player_gsca_filtered <- player_gsca |>
  select(index, Player, SCA_SCA, SCA90_SCA, GCA_GCA, GCA90_GCA)

player_defense <- load_fb_big5_advanced_season_stats(
  season_end_year = 2023, stat_type = "defense", team_or_player = "player"
)

player_defense$index <- 1:nrow(player_defense)

player_defense_filtered <- player_defense |>
  select(index, Player, TklW_Tackles, Blocks_Blocks, Int, `Tkl+Int`,Clr, Tkl_Challenges, Tkl_percent_Challenges)

player_possession <- load_fb_big5_advanced_season_stats(
  season_end_year = 2023, stat_type = "possession", team_or_player = "player"
)

player_possession$index <- 1:nrow(player_possession)

player_possession_filtered <- player_possession |>
  select(index, Player, `Mid 3rd_Touches`,`Att 3rd_Touches`, Att_Take, Succ_percent_Take, Final_Third_Carries, CPA_Carries)

player_misc <- load_fb_big5_advanced_season_stats(
  season_end_year = 2023, stat_type = "misc", team_or_player = "player"
)

player_misc$index <- 1:nrow(player_possession)

player_misc_filtered <- player_misc |>
  select(index, Player, Fld, Crs, Won_percent_Aerial)


df_list = list(player_standard_filtered, player_shooting_filtered, player_passing_filtered, player_gsca_filtered, player_defense_filtered, player_possession_filtered, player_misc_filtered) 

player_combined_df <- df_list |>
  reduce(right_join, by='index')

# Define UI for application that draws a histogram
ui <- navbarPage("Football Application",
                 tabPanel(
                   "Players",
                   
                   titlePanel("Football Players Scatter Plot"),
                   
                   sidebarLayout(
                     sidebarPanel(
                       selectInput("Comp", "Select League:",
                                   unique(player_combined_df$Comp),
                                   selected = NULL),
                       selectInput("x", "Select x axis:",
                                   c(colnames(player_combined_df)),
                                   selected = NULL),
                       selectInput("y", "Select y axis:",
                                   c(colnames(player_combined_df)),
                                   selected = NULL)
                     ),
                     
                     mainPanel(
                       plotOutput("scatterPlotPlayers")
                     )
                   )
                 )
)

server <- function(input, output) {
  
  
  selected_comp_players <- reactive({
    player_combined_df[player_combined_df$Comp == input$Comp, ]
  })
  
  output$scatterPlotPlayers <- renderPlot({
    
    ggplot(selected_comp_players(), aes_string(x = input$x, y = input$y)) +
      geom_point() +
      geom_text(aes(label = selected_comp_players()$Player.x), hjust = 0, vjust = 0, nudge_x = 0.3, nudge_y = 0.3) +
      ggtitle("Football Scatter Plot") +
      xlab(input$x) +
      ylab(input$y)
  })
}

shinyApp(ui = ui, server = server)
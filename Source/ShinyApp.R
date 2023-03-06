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
  select(index, Squad, Comp, Player, Url, Nation, Pos, Starts_Playing, MP_Playing, Min_Playing, Gls, Ast,G_minus_PK, npxG_Expected, xAG_Expected, PrgC_Progression, PrgP_Progression, PrgR_Progression)

player_shooting <- fb_big5_advanced_season_stats(
  season_end_year = 2023, stat_type = "shooting", team_or_player = "player"
)

player_shooting$index <- 1:nrow(player_shooting)

player_shooting_filtered <- player_shooting |>
  select(index, Sh_Standard,SoT_Standard, G_per_Sh_Standard, G_per_SoT_Standard, npxG_per_Sh_Expected, )

player_passing <- fb_big5_advanced_season_stats(
  season_end_year = 2023, stat_type = "passing", team_or_player = "player"
)

player_passing$index <- 1:nrow(player_passing)

player_passing_filtered <- player_passing |>
  select(index,Att_Total, Cmp_percent_Total, xA,  Final_Third, PPA, CrsPA)

player_gsca <- fb_big5_advanced_season_stats(
  season_end_year = 2023, stat_type = "gca", team_or_player = "player"
)

player_gsca$index <- 1:nrow(player_gsca)

player_gsca_filtered <- player_gsca |>
  select(index, SCA_SCA, GCA_GCA)

player_defense <- fb_big5_advanced_season_stats(
  season_end_year = 2023, stat_type = "defense", team_or_player = "player"
)

player_defense$index <- 1:nrow(player_defense)

player_defense_filtered <- player_defense |>
  select(index, TklW_Tackles, Blocks_Blocks, Int, `Tkl+Int`,Clr, Tkl_Challenges, Tkl_percent_Challenges)

player_possession <- fb_big5_advanced_season_stats(
  season_end_year = 2023, stat_type = "possession", team_or_player = "player"
)

player_possession$index <- 1:nrow(player_possession)

player_possession_filtered <- player_possession |>
  select(index, Touches_Touches,`Att 3rd_Touches`, Att_Take, Succ_Take, Succ_percent_Take,Carries_Carries, Final_Third_Carries, CPA_Carries)

player_misc <- fb_big5_advanced_season_stats(
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

players_per90 <- player_combined_df |>
  mutate_at(vars(11:49), ~( . / Min_Playing) * 90 ) |>
  mutate_at(vars(11:49), round, 2)

forwards_stats <- players_per90 |>
  filter(Position == "Forward") |>
  filter(Min_Playing >= 450) |>
  gather(Statistic, Value, -Name, -Position)

forwards_percentiles <- players_per90 |>
  filter(Position == "Forward") |>
  filter(Min_Playing >= 450) |>
  mutate(across(where(is.numeric), ~ round(cume_dist(.), 2))) |>
  gather(Statistic, percentile, -Name, -Position)

forwards_stats$index <- 1:nrow(forwards_stats)
forwards_percentiles$index <- 1:nrow(forwards_percentiles)

forwards_scouting_reports <- merge(forwards_stats, forwards_percentiles[, c("percentile", "index")], by = "index")

forwards_scouting_reports <- filter(forwards_scouting_reports, Statistic != "index", Statistic != "Squad", Statistic != "Comp", Statistic != "Nation", Statistic != "Url")

midfielders_stats <- players_per90 |>
  filter(Position == "Midfield") |>
  filter(Min_Playing >= 450) |>
  gather(Statistic, Value, -Name, -Position)

midfielders_percentiles <- players_per90 |>
  filter(Position == "Midfield") |>
  filter(Min_Playing >= 450) |>
  mutate(across(where(is.numeric), ~ round(cume_dist(.), 2))) |>
  gather(Statistic, percentile, -Name, -Position)

midfielders_stats$index <- 1:nrow(midfielders_stats)
midfielders_percentiles$index <- 1:nrow(midfielders_percentiles)

midfielders_scouting_reports <- merge(midfielders_stats, midfielders_percentiles[, c("percentile", "index")], by = "index")

midfielders_scouting_reports <- filter(midfielders_scouting_reports, Statistic != "index", Statistic != "Squad", Statistic != "Comp", Statistic != "Nation", Statistic != "Url")

centrebacks_stats <- players_per90 |>
  filter(Position == "Centre-Back") |>
  filter(Min_Playing >= 450) |>
  gather(Statistic, Value, -Name, -Position)

centrebacks_percentiles <- players_per90 |>
  filter(Position == "Centre-Back") |>
  filter(Min_Playing >= 450) |>
  mutate(across(where(is.numeric), ~ round(cume_dist(.), 2))) |>
  gather(Statistic, percentile, -Name, -Position)

centrebacks_stats$index <- 1:nrow(centrebacks_stats)
centrebacks_percentiles$index <- 1:nrow(centrebacks_percentiles)

centrebacks_scouting_reports <- merge(centrebacks_stats, centrebacks_percentiles[, c("percentile", "index")], by = "index")

centrebacks_scouting_reports <- filter(centrebacks_scouting_reports, Statistic != "index", Statistic != "Squad", Statistic != "Comp", Statistic != "Nation", Statistic != "Url")

fullbacks_stats <- players_per90 |>
  filter(Position == "Full-Back") |>
  filter(Min_Playing >= 450) |>
  gather(Statistic, Value, -Name, -Position)

fullbacks_percentiles <- players_per90 |>
  filter(Position == "Full-Back") |>
  filter(Min_Playing >= 450) |>
  mutate(across(where(is.numeric), ~ round(cume_dist(.), 2))) |>
  gather(Statistic, percentile, -Name, -Position)

fullbacks_stats$index <- 1:nrow(fullbacks_stats)
fullbacks_percentiles$index <- 1:nrow(fullbacks_percentiles)

fullbacks_scouting_reports <- merge(fullbacks_stats, fullbacks_percentiles[, c("percentile", "index")], by = "index")

fullbacks_scouting_reports <- filter(fullbacks_scouting_reports, Statistic != "index", Statistic != "Squad", Statistic != "Comp", Statistic != "Nation", Statistic != "Url")

wingers_attmids_stats <- players_per90 |>
  filter(Position == "Winger" | Position == "Attacking Midfield") |>
  filter(Min_Playing >= 450) |>
  gather(Statistic, Value, -Name, -Position)

wingers_attmids_percentiles <- players_per90 |>
  filter(Position == "Winger" | Position == "Attacking Midfield") |>
  filter(Min_Playing >= 450) |>
  mutate(across(where(is.numeric), ~ round(cume_dist(.), 2))) |>
  gather(Statistic, percentile, -Name, -Position)

wingers_attmids_stats$index <- 1:nrow(wingers_attmids_stats)
wingers_attmids_percentiles$index <- 1:nrow(wingers_attmids_percentiles)

wingers_attmids_scouting_reports <- merge(wingers_attmids_stats, wingers_attmids_percentiles[, c("percentile", "index")], by = "index")

wingers_attmids_scouting_reports <- filter(wingers_attmids_scouting_reports, Statistic != "index", Statistic != "Squad", Statistic != "Comp", Statistic != "Nation", Statistic != "Url")

complete_scouting_reports <- rbind(forwards_scouting_reports, wingers_attmids_scouting_reports, midfielders_scouting_reports, centrebacks_scouting_reports, fullbacks_scouting_reports)

complete_scouting_reports$percentile <- as.numeric(complete_scouting_reports$percentile)
complete_scouting_reports$Value <- as.numeric(complete_scouting_reports$Value)
class(complete_scouting_reports$Value)


complete_scouting_reports$percentile <- complete_scouting_reports$percentile * 100

# Define UI for application that draws a histogram
ui <- navbarPage("Football Application",
                 tabPanel(
                   "Player/Team Scatter Plots",
                   
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
                                   selected = NULL),
                       actionButton("submit", "Submit")
                     ),
                     
                     mainPanel(
                       plotlyOutput("scatterPlotPlayers")
                     )
                   )
                   
                 ), 
                 tabPanel("Player Pizza Chart", 
                          
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("player", "Select Player:",
                                          unique(complete_scouting_reports$Name),
                                          selected = NULL),
                              actionButton("submit1", "Submit")
                            ),
                            
                            mainPanel(
                              plotOutput("radarChart")
                            )
                          ))
)

server <- function(input, output) {
  
  
  selected_comp_players <- reactive({
    player_combined_df[player_combined_df$Comp == input$Comp, ]
  })
  
  observeEvent(input$submit, {
  
  output$scatterPlotPlayers <- renderPlotly({
    
    
    
    p <- ggplot(selected_comp_players(), aes_string(x = input$x, y = input$y, text = "Name")) +
      geom_point(aes(color=Position)) +
      ggtitle("Football Scatter Plot") +
      xlab(input$x) +
      ylab(input$y)
    
    ggplotly(p, tooltip = c("text","x", "y"))
    
  })
  
  })
  
  observeEvent(input$submit1, {
    
    selected_player <- complete_scouting_reports |>
      filter(Name == input$player)
    
    selected_player <- selected_player[c(6,7,16,8,9,10,21,18,42,28,32,31),]
    selected_player$index <- 1:12
    selected_player <- selected_player |>
      mutate(type = case_when(
        index %in% 1:4 ~ "Attacking",
        index %in% 5:8 ~ "Possession",
        index %in% 9:12 ~ "Misc"
      ))
    selected_player$type <- factor(selected_player$type, levels = c("Attacking", "Possession", "Misc"))
    
    output$radarChart <- renderPlot(
      width = 700,
      height = 700,
      
      {
        
        color1 <- "red"
          color2 <- "blue"
            color3 <- "orange"
              ggplot(data = selected_player, aes(x = reorder(Statistic, index), y = percentile, label= percentile, fill = type)) +
                # add the bar/pizza slices that are colored
                geom_bar(data = selected_player, width = 1,
                         stat = "identity") +
                scale_y_continuous(limits = c(0, 100)) +
                # wrap bar chart as around polar center
                coord_curvedpolar() +
                # add the background behind each bar (alpha at .5 for slight transparency so the bars standout)
                # geom_bar(aes(y=100, fill=type), stat="identity", width=1, alpha=0.5) +
                # add & customize line that border whole pizza
                geom_hline(yintercept = seq(0, 100, by = 100),
                           linewidth = 1) +
                # # add & customize lines between each pizza slice
                geom_vline(xintercept = seq(.5, 12, by = 1),
                           linewidth = .5) +
                # add percentile labels (labels are fill by bar colors) - option 1
                #geom_label(aes(label=value, fill=type), color = "white", size=2.5, fontface="bold", family = "Comic Sans MS", show.legend = FALSE) +
                # add percentile labels (labels are choice of fill and color) - option 2
                geom_label(color = "gray20", fill = "oldlace", size=2.5, fontface="bold", family = "Comic Sans MS", show.legend = FALSE) +
                # manually set the colors of bars (3 here for each group of stats (scoring, possession, defending))
                scale_fill_manual(values=c(color1, color2, color3)) +
                # theme manipulation to customize plot (play around with these!)
                theme(legend.position = "top",
                      legend.direction = "horizontal",
                      legend.title = element_blank(),
                      legend.text = element_text(colour = "gray20", family = "Comic Sans MS", face = "bold"),
                      legend.key.size = unit(.5, "cm"),
                      legend.box.spacing = unit(0, "mm"),
                      plot.title = element_text(hjust = .5, colour = "gray20", face = "bold", size = 16, family = "Comic Sans MS"),
                      plot.subtitle = element_text(hjust = .5, colour = "gray20", size = 8, family = "Comic Sans MS"),
                      panel.grid = element_blank(),
                      axis.text.y = element_blank(),
                      axis.ticks = element_blank(),
                      axis.title = element_blank(),
                      axis.text.x = element_text(face = "bold", size = 10, family = "Comic Sans MS")) +
                labs(title = selected_player$Name[1],
                     subtitle = "Tim Beary // 2022/2023 Season // Data from Fbref via: worldfootballR", x = NULL, y = NULL)
      })
  })
  
}

shinyApp(ui = ui, server = server)
library(shiny)
library(tidyverse)
library(ggplot2)
library(worldfootballR)
library(ggshakeR)
library(tidyr)
library(geomtextpath)
library(dplyr)
library(plotly)
library(DT)
library(bslib)
library(shinyBS)
library(shinycssloaders)
library(shinyWidgets)

player_data_2023 <- read.csv("Utils/player_data_2023.csv")
scouting_reports_2023 <- read.csv("Utils/scouting_reports_2023.csv")
team_data_2023 <- read.csv("Utils/team_data_2023.csv")



ui <- navbarPage(
  theme = bs_theme(version = 4, bootswatch = "sandstone"),
  "Football Application",
  tabPanel(
    "Player Scatter Plots",
    
    titlePanel("Football Players Scatter Plot"),
    
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "Positions",
          "Select Positions:",
          c(player_data_2023$Position),
          selected = c(scouting_reports_2023$Position),
          multiple = TRUE,
        ),
        selectInput(
          "Comp",
          "Select League:",
          c(player_data_2023$Comp),
          selected = c(player_data_2023$Comp),
          multiple = TRUE
        ),
        selectInput("x", "Select x axis:",
                    c(colnames(player_data_2023)),
                    selected = NULL),
        selectInput("y", "Select y axis:",
                    c(colnames(player_data_2023)),
                    selected = NULL),
        radioButtons("radio", "Stat Type:",
                     choices = c("Totals", "Per 90s")),
        sliderInput(
          inputId = "slider",
          label = "Minutes",
          min = 1,
          max = max(player_data_2023$Min_Playing),
          value = c(450, max(player_data_2023$Min_Playing))
        ),
        sliderInput(
          inputId = "age",
          label = "Age",
          min = min(player_data_2023$Age),
          max = max(player_data_2023$Age),
          value = c(min(player_data_2023$Age), max(player_data_2023$Age))
        ),
        selectInput("highlight",
                    "Select a player to hightlight: (in red)",
                    choices = NULL),
        actionButton("submit", "Submit")
      ),
      
      mainPanel(
        tabPanel("playerPlot", plotlyOutput("scatterPlotPlayers")),
        tabPanel("dataTable", DT::dataTableOutput('dt1')),
      ),
      
    )
    
  ),
  tabPanel(
    "Team Scatter Plots",
    
    titlePanel("Football Teams Scatter Plot"),
    
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "Comp1",
          "Select League:",
          unique(team_data_2023$Comp),
          selected = NULL
        ),
        selectInput("x1", "Select x axis:",
                    c(colnames(
                      team_data_2023
                    )),
                    selected = NULL),
        selectInput("y1", "Select y axis:",
                    c(colnames(
                      team_data_2023
                    )),
                    selected = NULL),
        actionButton("submit2", "Submit")
      ),
      
      mainPanel(tabPanel(
        "teamPlot", plotlyOutput("scatterPlotTeams")
      ), )
    )
    
  ),
  
  tabPanel("Player Pizza Chart",
           sidebarLayout(
             sidebarPanel(
               selectInput(
                 "player",
                 "Select Player:",
                 unique(scouting_reports_2023$Name),
                 selected = NULL
               ),
               checkboxInput(
                 inputId = "multi_select",
                 label = "Compare Players",
                 value = FALSE
               ),
               conditionalPanel(
                 condition = "input.multi_select == true",
                 selectInput(
                   "player_comparison",
                   "Select Player to compare:",
                   unique(scouting_reports_2023$Name),
                   selected = NULL
                 ),
               ),
               actionButton("submit1", "Submit"),
             ),
             mainPanel(splitLayout(
               plotOutput("radarChart", click = "chart_click"),
               plotOutput("comparisonChart", click = "comp_click")
             ))
           ))
)

server <- function(input, output, session) {
  selected_comp_players <- reactive({
    if (input$radio == "Totals") {
      if (is.null(input$Comp) & is.null(input$Positions)) {
        player_data_2023 |>
          filter(
            Min_Playing >= input$slider[1] &
              Min_Playing <= input$slider[2] &
              Age >= input$age[1] &
              Age <= input$age[2]
          )
      } else if (is.null(input$Comp)) {
        player_data_2023[player_data_2023$Position %in% input$Positions,] |>
          filter(
            Min_Playing >= input$slider[1] &
              Min_Playing <= input$slider[2] &
              Age >= input$age[1] &
              Age <= input$age[2]
          )
      } else if (is.null(input$Positions)) {
        player_data_2023[player_data_2023$Comp %in% input$Comp,] |>
          filter(
            Min_Playing >= input$slider[1] &
              Min_Playing <= input$slider[2] &
              Age >= input$age[1] &
              Age <= input$age[2]
          )
      } else {
        player_data_2023[player_data_2023$Comp %in% input$Comp &
                             player_data_2023$Position %in% input$Positions,] |>
          filter(
            Min_Playing >= input$slider[1] &
              Min_Playing <= input$slider[2] &
              Age >= input$age[1] &
              Age <= input$age[2]
          )
      }
    } else {
      if (is.null(input$Comp) & is.null(input$Positions)) {
        players_per90 |>
          filter(
            Min_Playing >= input$slider[1] &
              Min_Playing <= input$slider[2] &
              Age >= input$age[1] &
              Age <= input$age[2]
          )
      } else if (is.null(input$Comp)) {
        players_per90[players_per90$Position %in% input$Positions,] |>
          filter(
            Min_Playing >= input$slider[1] &
              Min_Playing <= input$slider[2] &
              Age >= input$age[1] &
              Age <= input$age[2]
          )
      } else if (is.null(input$Positions)) {
        players_per90[players_per90$Comp %in% input$Comp,] |>
          filter(
            Min_Playing >= input$slider[1] &
              Min_Playing <= input$slider[2] &
              Age >= input$age[1] &
              Age <= input$age[2]
          )
      } else {
        players_per90[players_per90$Comp %in% input$Comp &
                        players_per90$Position %in% input$Positions,] |>
          filter(
            Min_Playing >= input$slider[1] &
              Min_Playing <= input$slider[2] &
              Age >= input$age[1] &
              Age <= input$age[2]
          )
      }
    }
  })
  
  
  
  
  
  
  selected_comp_teams <- reactive({
    team_data_2023[team_data_2023$Comp == input$Comp1, ]
  })
  
  highlight_player <- reactive({
    selected_comp_players() |>
      filter(Name == input$highlight)
  })
  
  observe({
    updateSelectInput(session,
                      "highlight",
                      choices = unique(selected_comp_players()$Name))
  })
  
  observeEvent(input$chart_click, {
    showModal(modalDialog(
      plotOutput("pizzaChartLarge", width = "100%", height = "100%"),
      easyClose = TRUE,
      tags$style(
        type = "text/css",
        ".modal-content { width: 90vw !important; height: 90vh; }"
      ),
      tags$style(
        type = "text/css",
        ".modal-dialog { width: 90vw !important; height: 90vh; margin: 5vw; }"
      ),
    ))
  })
  
  observeEvent(input$comp_click, {
    showModal(modalDialog(
      plotOutput(
        "comparisonChartLarge",
        width = "100%",
        height = "100%"
      ),
      easyClose = TRUE,
      tags$style(
        type = "text/css",
        ".modal-content { width: 90vw !important; height: 90vh; }"
      ),
      tags$style(
        type = "text/css",
        ".modal-dialog { width: 90vw !important; height: 90vh; margin: 5vw; }"
      ),
    ))
  })
  
  observeEvent(input$submit, {
    output$scatterPlotPlayers <- renderPlotly({
      p <-
        ggplot(selected_comp_players(),
               aes_string(
                 x = input$x,
                 y = input$y,
                 text = "Name"
               )) +
        geom_point() +
        geom_point(data = highlight_player(),
                   color = "red",
                   size = 3) +
        ggtitle("Football Scatter Plot") +
        xlab(input$x) +
        ylab(input$y)
      
      ggplotly(p, tooltip = c("text", "x", "y"))
      
    })
    output$dt1 = DT::renderDataTable({
      selected_comp_players()
    })
  })
  
  observeEvent(input$submit2, {
    output$scatterPlotTeams <- renderPlotly({
      p <-
        ggplot(selected_comp_teams(),
               aes_string(
                 x = input$x1,
                 y = input$y1,
                 text = "Squad"
               )) +
        geom_point() +
        geom_text(
          aes(label = selected_comp_teams()$Squad),
          hjust = 0,
          vjust = 0,
          nudge_x = 0.3,
          nudge_y = 0.3
        ) +
        ggtitle("Football Scatter Plot") +
        xlab(input$x1) +
        ylab(input$y1)
      
      ggplotly(p, tooltip = c("text", "x", "y"))
      
    })
    
  })
  
  observeEvent(input$submit1, {
    selected_player <- scouting_reports_2023 |>
      filter(Name == input$player)
    
    
    compare_player <- scouting_reports_2023 |>
      filter(Name == input$player_comparison)
    
    selected_player <-
      selected_player[c(6, 7, 16, 8, 9, 10, 21, 18, 42, 28, 32, 31),]
    selected_player$index <- 1:12
    selected_player <- selected_player |>
      mutate(
        type = case_when(
          index %in% 1:4 ~ "Attacking",
          index %in% 5:8 ~ "Possession",
          index %in% 9:12 ~ "Misc"
        )
      )
    selected_player$type <-
      factor(selected_player$type,
             levels = c("Attacking", "Possession", "Misc"))
    
    compare_player <-
      compare_player[c(6, 7, 16, 8, 9, 10, 21, 18, 42, 28, 32, 31),]
    compare_player$index <- 1:12
    compare_player <- compare_player |>
      mutate(
        type = case_when(
          index %in% 1:4 ~ "Attacking",
          index %in% 5:8 ~ "Possession",
          index %in% 9:12 ~ "Misc"
        )
      )
    compare_player$type <-
      factor(compare_player$type,
             levels = c("Attacking", "Possession", "Misc"))
    
    output$radarChart <- renderPlot({
      color1 <- "red"
      color2 <- "blue"
      color3 <- "orange"
      ggplot(data = selected_player,
             aes(
               x = reorder(Statistic, index),
               y = percentile,
               label = percentile,
               fill = type
             )) +
        # add the bar/pizza slices that are colored
        geom_bar(data = selected_player,
                 width = 1,
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
        geom_label(
          color = "gray20",
          fill = "oldlace",
          size = 2.5,
          fontface = "bold",
          family = "Comic Sans MS",
          show.legend = FALSE
        ) +
        # manually set the colors of bars (3 here for each group of stats (scoring, possession, defending))
        scale_fill_manual(values = c(color1, color2, color3)) +
        # theme manipulation to customize plot (play around with these!)
        theme(
          legend.position = "top",
          legend.direction = "horizontal",
          legend.title = element_blank(),
          legend.text = element_text(
            colour = "gray20",
            family = "Comic Sans MS",
            face = "bold"
          ),
          legend.key.size = unit(.5, "cm"),
          legend.box.spacing = unit(0, "mm"),
          plot.title = element_text(
            hjust = .5,
            colour = "gray20",
            face = "bold",
            size = 16,
            family = "Comic Sans MS"
          ),
          plot.subtitle = element_text(
            hjust = .5,
            colour = "gray20",
            size = 8,
            family = "Comic Sans MS"
          ),
          panel.grid = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          axis.text.x = element_text(
            face = "bold",
            size = 10,
            family = "Comic Sans MS"
          )
        ) +
        labs(
          title = selected_player$Name[1],
          subtitle = "Tim Beary // 2022/2023 Season // Data from Fbref via: worldfootballR",
          x = NULL,
          y = NULL
        )
    })
    
    output$pizzaChartLarge <- renderPlot({
      color1 <- "red"
      color2 <- "blue"
      color3 <- "orange"
      ggplot(data = selected_player,
             aes(
               x = reorder(Statistic, index),
               y = percentile,
               label = percentile,
               fill = type
             )) +
        # add the bar/pizza slices that are colored
        geom_bar(data = selected_player,
                 width = 1,
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
        geom_label(
          color = "gray20",
          fill = "oldlace",
          size = 2.5,
          fontface = "bold",
          family = "Comic Sans MS",
          show.legend = FALSE
        ) +
        # manually set the colors of bars (3 here for each group of stats (scoring, possession, defending))
        scale_fill_manual(values = c(color1, color2, color3)) +
        # theme manipulation to customize plot (play around with these!)
        theme(
          legend.position = "top",
          legend.direction = "horizontal",
          legend.title = element_blank(),
          legend.text = element_text(
            colour = "gray20",
            family = "Comic Sans MS",
            face = "bold"
          ),
          legend.key.size = unit(.5, "cm"),
          legend.box.spacing = unit(0, "mm"),
          plot.title = element_text(
            hjust = .5,
            colour = "gray20",
            face = "bold",
            size = 16,
            family = "Comic Sans MS"
          ),
          plot.subtitle = element_text(
            hjust = .5,
            colour = "gray20",
            size = 8,
            family = "Comic Sans MS"
          ),
          panel.grid = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          axis.text.x = element_text(
            face = "bold",
            size = 10,
            family = "Comic Sans MS"
          ),
        ) +
        labs(
          title = selected_player$Name[1],
          subtitle = "Tim Beary // 2022/2023 Season // Data from Fbref via: worldfootballR",
          x = NULL,
          y = NULL
        )
    })
    
    if (input$multi_select && !is.null(input$player_comparison)) {
      output$comparisonChart <- renderPlot({
        color1 <- "red"
        color2 <- "blue"
        color3 <- "orange"
        ggplot(data = compare_player,
               aes(
                 x = reorder(Statistic, index),
                 y = percentile,
                 label = percentile,
                 fill = type
               )) +
          # add the bar/pizza slices that are colored
          geom_bar(data = compare_player,
                   width = 1,
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
          geom_label(
            color = "gray20",
            fill = "oldlace",
            size = 2.5,
            fontface = "bold",
            family = "Comic Sans MS",
            show.legend = FALSE
          ) +
          # manually set the colors of bars (3 here for each group of stats (scoring, possession, defending))
          scale_fill_manual(values = c(color1, color2, color3)) +
          # theme manipulation to customize plot (play around with these!)
          theme(
            legend.position = "top",
            legend.direction = "horizontal",
            legend.title = element_blank(),
            legend.text = element_text(
              colour = "gray20",
              family = "Comic Sans MS",
              face = "bold"
            ),
            legend.key.size = unit(.5, "cm"),
            legend.box.spacing = unit(0, "mm"),
            plot.title = element_text(
              hjust = .5,
              colour = "gray20",
              face = "bold",
              size = 16,
              family = "Comic Sans MS"
            ),
            plot.subtitle = element_text(
              hjust = .5,
              colour = "gray20",
              size = 8,
              family = "Comic Sans MS"
            ),
            panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            axis.text.x = element_text(
              face = "bold",
              size = 10,
              family = "Comic Sans MS"
            )
          ) +
          labs(
            title = compare_player$Name[1],
            subtitle = "Tim Beary // 2022/2023 Season // Data from Fbref via: worldfootballR",
            x = NULL,
            y = NULL
          )
      })
      
      output$comparisonChartLarge <- renderPlot({
        color1 <- "red"
        color2 <- "blue"
        color3 <- "orange"
        ggplot(data = compare_player,
               aes(
                 x = reorder(Statistic, index),
                 y = percentile,
                 label = percentile,
                 fill = type
               )) +
          # add the bar/pizza slices that are colored
          geom_bar(data = compare_player,
                   width = 1,
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
          geom_label(
            color = "gray20",
            fill = "oldlace",
            size = 2.5,
            fontface = "bold",
            family = "Comic Sans MS",
            show.legend = FALSE
          ) +
          # manually set the colors of bars (3 here for each group of stats (scoring, possession, defending))
          scale_fill_manual(values = c(color1, color2, color3)) +
          # theme manipulation to customize plot (play around with these!)
          theme(
            legend.position = "top",
            legend.direction = "horizontal",
            legend.title = element_blank(),
            legend.text = element_text(
              colour = "gray20",
              family = "Comic Sans MS",
              face = "bold"
            ),
            legend.key.size = unit(.5, "cm"),
            legend.box.spacing = unit(0, "mm"),
            plot.title = element_text(
              hjust = .5,
              colour = "gray20",
              face = "bold",
              size = 16,
              family = "Comic Sans MS"
            ),
            plot.subtitle = element_text(
              hjust = .5,
              colour = "gray20",
              size = 8,
              family = "Comic Sans MS"
            ),
            panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            axis.text.x = element_text(
              face = "bold",
              size = 10,
              family = "Comic Sans MS"
            )
          ) +
          labs(
            title = compare_player$Name[1],
            subtitle = "Tim Beary // 2022/2023 Season // Data from Fbref via: worldfootballR",
            x = NULL,
            y = NULL
          )
      })
    }
    
    
  })
  
}

shinyApp(ui = ui, server = server)

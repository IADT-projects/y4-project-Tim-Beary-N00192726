library(shiny)
library(tidyverse)
library(ggplot2)
library(worldfootballR)
library(tidyr)
library(geomtextpath)
library(dplyr)
library(plotly)
library(DT)
library(bslib)
library(shinyBS)
library(shinyWidgets)

player_data_2023 <- read.csv("Data/player_data_2023.csv")
player_per90_2023 <- read.csv("Data/players_per90_2023.csv")
scouting_reports_2023 <- read.csv("Data/scouting_reports_2023.csv")
team_data_2023 <- read.csv("Data/team_data_2023.csv")
player_data_historic <- read.csv("Data/player_data_historic.csv")
player_per90_historic <- read.csv("Data/players_per90_historic.csv")




ui <- navbarPage(
  theme = bs_theme(version = 5, bootswatch = "lux"),
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
        verticalLayout(plotlyOutput("scatterPlotPlayers"), DTOutput('dt1'))
      ),
      
    )
    
  ),
  tabPanel(
    "Historic Player Scatter Plots",
    
    titlePanel("Football Historic Player Scatter Plot"),
    
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "Season",
          "Select Seasons:",
          c(player_data_historic$Season_End_Year),
          selected = c(player_data_historic$Season_End_Year),
          multiple = TRUE,
        ),
        selectInput(
          "Positions2",
          "Select Positions:",
          c(scouting_reports_2023$Position),
          selected = c(scouting_reports_2023$Position),
          multiple = TRUE,
        ),
        selectInput(
          "Comp2",
          "Select League:",
          c(player_data_historic$Comp),
          selected = c(player_data_historic$Comp),
          multiple = TRUE
        ),
        selectInput("x2", "Select x axis:",
                    c(colnames(player_data_historic)),
                    selected = NULL),
        selectInput("y2", "Select y axis:",
                    c(colnames(player_data_historic)),
                    selected = NULL),
        radioButtons("radio2", "Stat Type:",
                     choices = c("Totals", "Per 90s")),
        sliderInput(
          inputId = "slider2",
          label = "Minutes",
          min = 1,
          max = max(player_data_historic$Min_Playing),
          value = c(450, max(player_data_historic$Min_Playing))
        ),
        selectInput("highlight1",
                    "Select a player to hightlight: (in red)",
                    choices = NULL),
        actionButton("submit_historic", "Submit")
      ),
      mainPanel(
        verticalLayout(plotlyOutput("scatterPlotHistoricPlayers"), DTOutput('dt2'))
      ),
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
             mainPanel((
               plotOutput("radarChart",height = "80vh")
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
        player_per90_2023 |>
          filter(
            Min_Playing >= input$slider[1] &
              Min_Playing <= input$slider[2] &
              Age >= input$age[1] &
              Age <= input$age[2]
          )
      } else if (is.null(input$Comp)) {
        player_per90_2023[player_per90_2023$Position %in% input$Positions,] |>
          filter(
            Min_Playing >= input$slider[1] &
              Min_Playing <= input$slider[2] &
              Age >= input$age[1] &
              Age <= input$age[2]
          )
      } else if (is.null(input$Positions)) {
        player_per90_2023[player_per90_2023$Comp %in% input$Comp,] |>
          filter(
            Min_Playing >= input$slider[1] &
              Min_Playing <= input$slider[2] &
              Age >= input$age[1] &
              Age <= input$age[2]
          )
      } else {
        player_per90_2023[player_per90_2023$Comp %in% input$Comp &
                        player_per90_2023$Position %in% input$Positions,] |>
          filter(
            Min_Playing >= input$slider[1] &
              Min_Playing <= input$slider[2] &
              Age >= input$age[1] &
              Age <= input$age[2]
          )
      }
    }
  })
  
  reactive_historic_players <- reactive({
  if (input$radio2 == "Totals") {
    if (is.null(input$Comp2) & is.null(input$Positions2) & is.null(input$Season)) {
      player_data_historic |>
        filter(Min_Playing >= input$slider2[1] &
                 Min_Playing <= input$slider2[2])
    } else if (is.null(input$Comp2) & is.null(input$Season)) {
      player_data_historic[player_data_historic$Position %in% input$Positions2, ] |>
        filter(Min_Playing >= input$slider2[1] &
                 Min_Playing <= input$slider2[2])
    } else if (is.null(input$Positions2) & is.null(input$Season)) {
      player_data_historic[player_data_historic$Comp %in% input$Comp2, ] |>
        filter(Min_Playing >= input$slider2[1] &
                 Min_Playing <= input$slider2[2])
    } else if (is.null(input$Season)) {
      player_data_historic[player_data_historic$Comp %in% input$Comp2 &
                             player_data_historic$Position %in% input$Positions2, ] |>
        filter(Min_Playing >= input$slider2[1] &
                 Min_Playing <= input$slider2[2])
    } else {
      player_data_historic[player_data_historic$Comp %in% input$Comp2 &
                             player_data_historic$Position %in% input$Positions2 &
                             player_data_historic$Season_End_Year %in%  input$Season, ] |>
        filter(Min_Playing >= input$slider2[1] &
                 Min_Playing <= input$slider2[2])
    }
  } else {
    if (is.null(input$Comp2) & is.null(input$Positions2) & is.null(input$Season)) {
      player_per90_historic |>
        filter(Min_Playing >= input$slider2[1] &
                 Min_Playing <= input$slider2[2])
    } else if (is.null(input$Comp2) & is.null(input$Season)) {
      player_per90_historic[player_per90_historic$Position %in% input$Positions2, ] |>
        filter(Min_Playing >= input$slider2[1] &
                 Min_Playing <= input$slider2[2])
    } else if (is.null(input$Positions2) & is.null(input$Season)) {
      player_per90_historic[player_per90_historic$Comp %in% input$Comp2, ] |>
        filter(Min_Playing >= input$slider2[1] &
                 Min_Playing <= input$slider2[2])
    } else if (is.null(input$Season)) {
      player_per90_historic[player_per90_historic$Comp %in% input$Comp2 &
                                player_per90_historic$Position %in% input$Positions2, ] |>
        filter(Min_Playing >= input$slider2[1] &
                 Min_Playing <= input$slider2[2])
    } else {
      player_per90_historic[player_per90_historic$Comp %in% input$Comp2 &
                                player_per90_historic$Position %in% input$Positions2 &
                                player_per90_historic$Season_End_Year %in%  input$Season, ] |>
        filter(Min_Playing >= input$slider2[1] &
                 Min_Playing <= input$slider2[2])
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
  
  highlight_historic_player <- reactive({
    reactive_historic_players() |>
      filter(Name == input$highlight1)
  })
  
  observe({
    updateSelectInput(session,
                      "highlight",
                      choices = unique(selected_comp_players()$Name))
  })
  
  observe({
    updateSelectInput(session,
                      "highlight1",
                      choices = unique(reactive_historic_players()$Name))
  })
  
  dataTablePlayers <- reactive({
    selected_comp_players() |>
      select(
        Name,
        Position,
        Comp,
        Squad,
        input$x,
        input$y
      )
  })
  
  dataTableHistoricPlayers <- reactive({
    reactive_historic_players() |>
      select(
        Name,
        Position,
        Comp,
        Season_End_Year,
        Squad,
        input$x2,
        input$y2
      )
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
    output$dt1 = renderDataTable({
      dataTablePlayers()
    })
  })
  
  observeEvent(input$submit_historic, {
    output$scatterPlotHistoricPlayers <- renderPlotly({
      p <-
        ggplot(reactive_historic_players(),
               aes_string(
                 x = input$x2,
                 y = input$y2,
                 text = "Name",
                 text1 = 'Season_End_Year'
               )) +
        geom_point() +
        geom_point(data = highlight_historic_player(),
                   color = "red",
                   size = 3) +
        ggtitle("Football Scatter Plot") +
        xlab(input$x2) +
        ylab(input$y2)
      
      ggplotly(p, tooltip = c("text", "text1", "x", "y"))
      
    })
    output$dt2 = renderDataTable({
      dataTableHistoricPlayers()
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
    
    if (input$multi_select && !is.null(input$player_comparison)) {
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
          geom_bar(data = selected_player,
                   stat = "identity",
                   width = 1,
          ) +
          geom_bar(data = compare_player, stat = "identity", width = 1, alpha = 0, color = "black", size = 2) +
          scale_y_continuous(limits = c(0, 100)) +
          coord_curvedpolar() +
          geom_hline(yintercept = seq(0, 100, by = 100),
                     linewidth = 1) +
          geom_vline(xintercept = seq(.5, 12, by = 1),
                     linewidth = .5) +

          geom_label(
            color = "gray20",
            fill = "oldlace",
            size = 2.5,
            fontface = "bold",
            family = "Comic Sans MS",
            show.legend = FALSE
          ) +
          scale_fill_manual(values = c(color1, color2, color3)) +
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
              size = 16,
              family = "Comic Sans MS"
            ),
            plot.subtitle = element_text(
              hjust = .5,
              colour = "black",
              face = "bold",
              size = 16,
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
            subtitle = compare_player$Name[1],
            x = NULL,
            y = NULL
          )
      })
      
    } else {

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
        geom_bar(data = selected_player,
                 width = 1,
                 stat = "identity") +
        scale_y_continuous(limits = c(0, 100)) +
        coord_curvedpolar() +
        geom_hline(yintercept = seq(0, 100, by = 100),
                   linewidth = 1) +
        geom_vline(xintercept = seq(.5, 12, by = 1),
                   linewidth = .5) +
        geom_label(
          color = "gray20",
          fill = "oldlace",
          size = 2.5,
          fontface = "bold",
          family = "Comic Sans MS",
          show.legend = FALSE
        ) +
        scale_fill_manual(values = c(color1, color2, color3)) +
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
            colour = "black",
            size = 16,
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
          x = NULL,
          y = NULL
        )
    })
    }
})
}
shinyApp(ui = ui, server = server)

library(shiny)
library(tidyverse)
library(worldfootballR)
library(plotly)
library(DT)
library(bslib)
library(geomtextpath)
library(rsconnect)
library(stringr)

player_data_2023 <- read.csv("Data/player_data_2023.csv")
players_per90_2023 <- read.csv("Data/players_per90_2023.csv")
scouting_reports_2023 <- read.csv("Data/scouting_reports_2023.csv")
player_data_historic <- read.csv("Data/player_data_historic.csv")
players_per90_historic <- read.csv("Data/players_per90_historic.csv")


ui <- navbarPage(
  theme = bs_theme(version = 4, bootswatch = "litera"),
  "Football Data Scouting Application",
  tabPanel(
    "Player Scatter Plots",
    sidebarLayout(
      sidebarPanel(
        selectizeInput(
          "Positions",
          "Select Positions:",
          choices = unique(scouting_reports_2023$Position),
          selected = c(scouting_reports_2023$Position),
          multiple = TRUE,
          options = list(plugins = list('remove_button'))
        ),
        selectizeInput(
          "Comp",
          "Select League:",
          choices = unique(player_data_2023$Comp),
          selected = c(player_data_2023$Comp),
          multiple = TRUE,
          options = list(plugins = list('remove_button'))
          
        ),
        selectInput("x", "Select x axis:",
                    c(colnames(player_data_2023[12:58])),
                    selected = ""),
        selectInput("y", "Select y axis:",
                    c(colnames(player_data_2023[12:58])),
                    selected = ""),
        radioButtons("radio", "Stat Type:",
                     choices = c("Totals", "Per 90s")),
        sliderInput(
          inputId = "slider",
          label = "Minutes",
          min = 1,
          max = max(player_data_2023$Minutes.Played),
          value = c(450, max(player_data_2023$Minutes.Played))
        ),
        sliderInput(
          inputId = "age",
          label = "Age",
          min = min(player_data_2023$Age),
          max = max(player_data_2023$Age),
          value = c(min(player_data_2023$Age), max(player_data_2023$Age))
        ),
        selectizeInput(
          "highlight",
          "Search a player to hightlight: (in red)",
          choices = NULL,
          selected = "",
          multiple = TRUE,
          options = list(plugins = list('remove_button'))
        ),
        actionButton("submit", "Submit")
      ),
      
      mainPanel(verticalLayout(
        plotlyOutput("scatterPlotPlayers"), DTOutput('dt1')
      )),
    )
  ),
  tabPanel(
    "Historic Player Scatter Plots",
    sidebarLayout(
      sidebarPanel(
        selectizeInput(
          "Season",
          "Select Seasons:",
          choices = unique(c(player_data_historic$Season_End_Year)),
          selected = c(player_data_historic$Season_End_Year),
          multiple = TRUE,
          options = list(plugins = list('remove_button'))
        ),
        selectizeInput(
          "Positions2",
          "Select Positions:",
          choices = unique(scouting_reports_2023$Position),
          selected = c(player_data_historic$Position),
          multiple = TRUE,
          options = list(plugins = list('remove_button'))
        ),
        selectizeInput(
          "Comp2",
          "Select League:",
          choices = unique(player_data_historic$Comp),
          selected = c(player_data_historic$Comp),
          multiple = TRUE,
          options = list(plugins = list('remove_button'))
        ),
        selectInput("x2", "Select x axis:",
                    c(colnames(player_data_historic[13:58])),
                    
                    selected = NULL),
        selectInput("y2", "Select y axis:",
                    c(colnames(player_data_historic[13:58])),
                    
                    selected = NULL),
        radioButtons("radio2", "Stat Type:",
                     choices = c("Totals", "Per 90s")),
        sliderInput(
          inputId = "slider2",
          label = "Minutes",
          min = 1,
          max = max(player_data_historic$Minutes.Played),
          value = c(450, max(player_data_historic$Minutes.Played))
        ),
        sliderInput(
          inputId = "historic_age_slider",
          label = "Age",
          min = 14,
          max = 43,
          value = c(14,43)
        ),
        selectizeInput(
          "highlight1",
          "Search a player to hightlight: (in red)",
          choices = NULL,
          selected = "",
          multiple = TRUE,
          options = list(plugins = list('remove_button'))
        ),
        actionButton("submit_historic", "Submit")
      ),
      mainPanel(verticalLayout(
        plotlyOutput("scatterPlotHistoricPlayers"), DTOutput('dt2')
      )),
    )
  ),
  
  tabPanel("Player Pizza Chart",
           sidebarLayout(
             sidebarPanel(
               selectizeInput(
                 "pizzaComp",
                 "Select League:",
                 choices = unique(player_data_2023$Comp),
                 selected = c(player_data_2023$Comp),
                 multiple = TRUE,
                 options = list(plugins = list('remove_button'))
               ),
               selectizeInput(
                 "pizzaPosition",
                 "Select Position:",
                 choices = NULL,
                 selected = "",
               ),
               selectizeInput(
                 "player",
                 "Search Player:",
                 choices = NULL,
                 selected = "",
               ),
               checkboxInput(
                 inputId = "multi_select",
                 label = "Compare Players",
                 value = FALSE
               ),
               conditionalPanel(
                 condition = "input.multi_select == true",
                 selectizeInput(
                   "player_comparison",
                   "Search Player to compare:",
                   choices = NULL,
                   selected = ""
                 ),
               ),
               actionButton("submit1", "Submit"),
             ),
             mainPanel((plotOutput(
               "radarChart", height = "80vh"
             )))
           ))
)

server <- function(input, output, session) {
  
  
  reactive_player_data <- reactive({
    if (input$radio == "Totals") {
      if (is.null(input$Comp) & is.null(input$Positions)) {
        player_data_2023 |>
          filter(
            Minutes.Played >= input$slider[1] &
              Minutes.Played <= input$slider[2] &
              Age >= input$age[1] &
              Age <= input$age[2]
          )
      } else if (is.null(input$Comp)) {
        player_data_2023[player_data_2023$Position %in% input$Positions, ] |>
          filter(
            Minutes.Played >= input$slider[1] &
              Minutes.Played <= input$slider[2] &
              Age >= input$age[1] &
              Age <= input$age[2]
          )
      } else if (is.null(input$Positions)) {
        player_data_2023[player_data_2023$Comp %in% input$Comp, ] |>
          filter(
            Minutes.Played >= input$slider[1] &
              Minutes.Played <= input$slider[2] &
              Age >= input$age[1] &
              Age <= input$age[2]
          )
      } else {
        player_data_2023[player_data_2023$Comp %in% input$Comp &
                           player_data_2023$Position %in% input$Positions, ] |>
          filter(
            Minutes.Played >= input$slider[1] &
              Minutes.Played <= input$slider[2] &
              Age >= input$age[1] &
              Age <= input$age[2]
          )
      }
    } else {
      if (is.null(input$Comp) & is.null(input$Positions)) {
        players_per90_2023 |>
          filter(
            Minutes.Played >= input$slider[1] &
              Minutes.Played <= input$slider[2] &
              Age >= input$age[1] &
              Age <= input$age[2]
          )
      } else if (is.null(input$Comp)) {
        players_per90_2023[players_per90_2023$Position %in% input$Positions, ] |>
          filter(
            Minutes.Played >= input$slider[1] &
              Minutes.Played <= input$slider[2] &
              Age >= input$age[1] &
              Age <= input$age[2]
          )
      } else if (is.null(input$Positions)) {
        players_per90_2023[players_per90_2023$Comp %in% input$Comp, ] |>
          filter(
            Minutes.Played >= input$slider[1] &
              Minutes.Played <= input$slider[2] &
              Age >= input$age[1] &
              Age <= input$age[2]
          )
      } else {
        players_per90_2023[players_per90_2023$Comp %in% input$Comp &
                            players_per90_2023$Position %in% input$Positions, ] |>
          filter(
            Minutes.Played >= input$slider[1] &
              Minutes.Played <= input$slider[2] &
              Age >= input$age[1] &
              Age <= input$age[2]
          )
      }
    }
  })
  
  reactive_historic_players <- reactive({
    if (input$radio2 == "Totals") {
      if (is.null(input$Comp2) &
          is.null(input$Positions2) & is.null(input$Season)) {
        player_data_historic |>
          filter(Minutes.Played >= input$slider2[1] &
                   Minutes.Played <= input$slider2[2] &
                   Age >= input$historic_age_slider[1] &
                   Age <= input$historic_age_slider[2])
      } else if (is.null(input$Comp2) & is.null(input$Season)) {
        player_data_historic[player_data_historic$Position %in% input$Positions2,] |>
          filter(Minutes.Played >= input$slider2[1] &
                   Minutes.Played <= input$slider2[2] &
                   Age >= input$historic_age_slider[1] &
                   Age <= input$historic_age_slider[2])
      } else if (is.null(input$Positions2) & is.null(input$Season)) {
        player_data_historic[player_data_historic$Comp %in% input$Comp2,] |>
          filter(Minutes.Played >= input$slider2[1] &
                   Minutes.Played <= input$slider2[2] &
                   Age >= input$historic_age_slider[1] &
                   Age <= input$historic_age_slider[2])
      } else if (is.null(input$Season)) {
        player_data_historic[player_data_historic$Comp %in% input$Comp2 &
                               player_data_historic$Position %in% input$Positions2,] |>
          filter(Minutes.Played >= input$slider2[1] &
                   Minutes.Played <= input$slider2[2] &
                   Age >= input$historic_age_slider[1] &
                   Age <= input$historic_age_slider[2])
      } else {
        player_data_historic[player_data_historic$Comp %in% input$Comp2 &
                               player_data_historic$Position %in% input$Positions2 &
                               player_data_historic$Season_End_Year %in%  input$Season,] |>
          filter(Minutes.Played >= input$slider2[1] &
                   Minutes.Played <= input$slider2[2] & 
                   Age >= input$historic_age_slider[1] &
                   Age <= input$historic_age_slider[2])
      }
    } else {
      if (is.null(input$Comp2) &
          is.null(input$Positions2) & is.null(input$Season)) {
        players_per90_historic |>
          filter(Minutes.Played >= input$slider2[1] &
                   Minutes.Played <= input$slider2[2] & 
                   Age >= input$historic_age_slider[1] &
                   Age <= input$historic_age_slider[2])
      } else if (is.null(input$Comp2) & is.null(input$Season)) {
        players_per90_historic[players_per90_historic$Position %in% input$Positions2,] |>
          filter(Minutes.Played >= input$slider2[1] &
                   Minutes.Played <= input$slider2[2] & 
                   Age >= input$historic_age_slider[1] &
                   Age <= input$historic_age_slider[2])
      } else if (is.null(input$Positions2) & is.null(input$Season)) {
        players_per90_historic[players_per90_historic$Comp %in% input$Comp2,] |>
          filter(Minutes.Played >= input$slider2[1] &
                   Minutes.Played <= input$slider2[2] &
                   Age >= input$historic_age_slider[1] &
                   Age <= input$historic_age_slider[2])
      } else if (is.null(input$Season)) {
        players_per90_historic[players_per90_historic$Comp %in% input$Comp2 &
                                players_per90_historic$Position %in% input$Positions2,] |>
          filter(Minutes.Played >= input$slider2[1] &
                   Minutes.Played <= input$slider2[2] &
                   Age >= input$historic_age_slider[1] &
                   Age <= input$historic_age_slider[2])
      } else {
        players_per90_historic[players_per90_historic$Comp %in% input$Comp2 &
                                players_per90_historic$Position %in% input$Positions2 &
                                players_per90_historic$Season_End_Year %in%  input$Season,] |>
          filter(Minutes.Played >= input$slider2[1] &
                   Minutes.Played <= input$slider2[2] &
                   Age >= input$historic_age_slider[1] &
                   Age <= input$historic_age_slider[2])
      }
    }
  })
  
  
  highlight_player <- reactive({
    reactive_player_data()[reactive_player_data()$Name %in% input$highlight,]
  })
  
  highlight_historic_player <- reactive({
      reactive_historic_players()[reactive_historic_players()$Name %in% input$highlight1,]
  })
  
  observe({
    updateSelectizeInput(
      session,
      "highlight",
      choices = unique(reactive_player_data()$Name),
      selected = "",
      server = TRUE
    )
  })
  
  observe({
    updateSelectizeInput(
      session,
      "highlight1",
      choices = unique(reactive_historic_players()$Name),
      selected = "",
      server = TRUE
    )
  })
  
  
  dataTablePlayers <- reactive({
    reactive_player_data() |>
      select(Name,
             Position,
             Comp,
             Squad,
             Minutes.Played,
             input$x,
             input$y)
  })
  
  dataTableHistoricPlayers <- reactive({
    reactive_historic_players() |>
      select(Name,
             Position,
             Comp,
             Season_End_Year,
             Squad,
             Minutes.Played,
             input$x2,
             input$y2)
  })
  
  
  observeEvent(input$submit, {
    output$scatterPlotPlayers <- renderPlotly({
      p <-
        ggplot(reactive_player_data(),
               aes_string(
                 x = input$x,
                 y = input$y,
                 text = "Name"
               )) +
        geom_point() +
        geom_vline(xintercept = mean(reactive_player_data()[,input$x], na.rm = TRUE), linetype = "dashed") +
        geom_hline(yintercept = mean(reactive_player_data()[,input$y], na.rm = TRUE), linetype = "dashed") +
        geom_point(data = highlight_player(),
                   color = "#ff6d00",
                   size = 5) +
        # geom_text(
        #   data = highlight_player(),
        #   aes(label = highlight_player()$Name),
        #   color = "#ff6d00",
        #   nudge_y = 0.90,
        # )  +
        ggtitle("Football Scatter Plot") +
        xlab(input$x) +
        ylab(input$y)
      
      ggplotly(p, tooltip = c("text", "x", "y"))
      
    })
    output$dt1 = renderDataTable(
      dataTablePlayers(),
      # extensions = "Buttons",
      # options = list(
      #   buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      #   dom = 'Bfrtip'
      # )
    )
  })
  
  observeEvent(input$submit_historic, {
    output$scatterPlotHistoricPlayers <- renderPlotly({
      p <-
        ggplot(
          reactive_historic_players(),
          aes_string(
            x = input$x2,
            y = input$y2,
            name = "Name",
            year = 'Season_End_Year',
            team = "Squad"
          )
        ) +
        geom_point() +
        geom_vline(xintercept = mean(reactive_historic_players()[,input$x2], na.rm = TRUE), linetype = "dashed") +
        geom_hline(yintercept = mean(reactive_historic_players()[,input$y2], na.rm = TRUE), linetype = "dashed") +
        geom_point(data = highlight_historic_player(),
                   color = "#ff6d00",
                   size = 5) +
        ggtitle("Football Scatter Plot") +
        xlab(input$x2) +
        ylab(input$y2)
      
      ggplotly(p, tooltip = c("name", "year", "team", "x", "y"))
      
    })
    output$dt2 = renderDataTable({
      dataTableHistoricPlayers()
    })
  })
  
  
  
  selected_comp <- reactive({
    scouting_reports_2023[scouting_reports_2023$Comp %in% input$pizzaComp,]
  })
  
  
  updateSelectizeInput(session,
                       "pizzaPosition",
                       choices = unique(scouting_reports_2023$Position),
                       selected = "",
                       server = TRUE)
  
  selected_player_position <- reactive({
    selected_comp()[selected_comp()$Position %in% input$pizzaPosition,]
  })
  
  
  observe({
    updateSelectizeInput(session,
                      "player",
                      choices = selected_player_position()$Name,
                      selected = "",
                      server = TRUE)
    updateSelectizeInput(session,
                      "player_comparison",
                      choices = selected_player_position()$Name,
                      selected = "",
                      server = TRUE)
  })
  

  
  observeEvent(input$submit1, {
    
    
    validate(
      need(input$pizzaPosition, "Please select a Position"),
      need(input$player, "Please select a Player"),
      if (input$multi_select) {
        need(input$player_comparison, "Please select a Player")
      }
    )

    selected_player <- reactive({
      selected_player_position() |>
        filter(Name == input$player)
    })
    
      if (selected_player()$Position[1] == "Forward") {
        selected_player <- selected_player()[c(4, 5, 7, 10, 15, 16, 21, 24, 29, 32, 34, 36, 41, 46, 49), ]
        selected_player$index <- 1:15
        selected_player <- selected_player |>
          mutate(
            type = case_when(
              index %in% 1:4 ~ "Attacking",
              index %in% 5:8 ~ "Possession",
              index %in% 9:12 ~ "Passing",
              index %in% 13:15 ~ "Misc"
            )
          )
        selected_player$type <-
          factor(selected_player$type,
                 levels = c("Attacking", "Possession", "Passing", "Misc"))
      } else if (selected_player()$Position[1] == "Midfielder") {
        selected_player <- selected_player()[c(7, 8, 15, 18, 20, 21, 23, 29, 32, 33, 26, 38, 40, 48, 44), ]
        selected_player$index <- 1:15
        selected_player <- selected_player |>
          mutate(
            type = case_when(
              index %in% 1:3 ~ "Attacking",
              index %in% 4:7 ~ "Possession",
              index %in% 8:11 ~ "Passing",
              index %in% 12:15 ~ "Defending"
            )
          )
        selected_player$type <-
          factor(selected_player$type,
                 levels = c("Attacking", "Possession", "Passing", "Defending"))
      } else if (selected_player()$Position[1] == "Full-Back") {
        selected_player <- selected_player()[c(7, 8, 15, 18, 21, 23, 24, 26, 29, 33, 35, 38, 40, 44, 48), ]
        selected_player$index <- 1:15
        selected_player <- selected_player |>
          mutate(
            type = case_when(
              index %in% 1:3 ~ "Attacking",
              index %in% 4:7 ~ "Possession",
              index %in% 8:11 ~ "Passing",
              index %in% 12:15 ~ "Defending"
            )
          )
        selected_player$type <-
          factor(selected_player$type,
                 levels = c("Attacking", "Possession", "Passing", "Defending"))
      } else if (selected_player()$Position[1] == "Centre-Back") {
        selected_player <- selected_player()[c(7, 9, 49, 18, 21, 22, 19, 26, 28, 29, 30, 38, 40, 44, 39), ]
        selected_player$index <- 1:15
        selected_player <- selected_player |>
          mutate(
            type = case_when(
              index %in% 1:3 ~ "Attacking",
              index %in% 4:7 ~ "Possession",
              index %in% 8:11 ~ "Passing",
              index %in% 12:15 ~ "Defending"
            )
          )
        selected_player$type <-
          factor(selected_player$type,
                 levels = c("Attacking", "Possession", "Passing", "Defending"))
      } else {
        selected_player <- selected_player()[c(4, 5, 7, 8, 15, 16, 18, 24, 26, 31, 32, 36, 41, 46, 49), ]
        selected_player$index <- 1:15
        selected_player <- selected_player |>
          mutate(
            type = case_when(
              index %in% 1:4 ~ "Attacking",
              index %in% 5:8 ~ "Possession",
              index %in% 9:12 ~ "Passing",
              index %in% 13:15 ~ "Misc"
            )
          )
        selected_player$type <-
          factor(selected_player$type,
                 levels = c("Attacking", "Possession", "Passing", "Misc"))
      }
    
    if (input$multi_select) {
    compare_player <- scouting_reports_2023 |>
      filter(Name == input$player_comparison)
    
    if (compare_player$Position[1] == "Forward") {
      compare_player <- compare_player[c(4, 5, 7, 10, 15, 16, 21, 24, 29, 32, 34, 36, 41, 46, 49), ]
      compare_player$index <- 1:15
      compare_player <- compare_player |>
        mutate(
          type = case_when(
            index %in% 1:4 ~ "Attacking",
            index %in% 5:8 ~ "Possession",
            index %in% 9:12 ~ "Passing",
            index %in% 13:15 ~ "Misc"
          )
        )
      compare_player$type <-
        factor(compare_player$type,
               levels = c("Attacking", "Possession", "Passing", "Misc"))
    } else if (compare_player$Position[1] == "Midfielder") {
      compare_player <- compare_player[c(7, 8, 15, 18, 20, 21, 23, 29, 32, 33, 26, 38, 40, 48, 44), ]
      compare_player$index <- 1:15
      compare_player <- compare_player |>
        mutate(
          type = case_when(
            index %in% 1:3 ~ "Attacking",
            index %in% 4:7 ~ "Possession",
            index %in% 8:11 ~ "Passing",
            index %in% 12:15 ~ "Defending"
          )
        )
      compare_player$type <-
        factor(compare_player$type,
               levels = c("Attacking", "Possession", "Passing", "Defending"))
    } else if (compare_player$Position[1] == "Full-Back") {
      compare_player <- compare_player[c(7, 8, 15, 18, 21, 23, 24, 26, 29, 33, 35, 38, 40, 44, 48), ]
      compare_player$index <- 1:15
      compare_player <- compare_player |>
        mutate(
          type = case_when(
            index %in% 1:3 ~ "Attacking",
            index %in% 4:7 ~ "Possession",
            index %in% 8:11 ~ "Passing",
            index %in% 12:15 ~ "Defending"
          )
        )
      compare_player$type <-
        factor(compare_player$type,
               levels = c("Attacking", "Possession", "Passing", "Defending"))
    } else if (compare_player$Position[1] == "Centre-Back") {
      compare_player <- compare_player[c(7, 9, 49, 18, 21, 22, 19, 26, 28, 29, 30, 38, 40, 44, 39), ]
      compare_player$index <- 1:15
      compare_player <- compare_player |>
        mutate(
          type = case_when(
            index %in% 1:3 ~ "Attacking",
            index %in% 4:7 ~ "Possession",
            index %in% 8:11 ~ "Passing",
            index %in% 12:15 ~ "Defending"
          )
        )
      compare_player$type <-
        factor(compare_player$type,
               levels = c("Attacking", "Possession", "Passing", "Defending"))
    } else {
      compare_player <- compare_player[c(4, 5, 7, 8, 15, 16, 18, 24, 26, 31, 32, 36, 41, 46, 49), ]
      compare_player$index <- 1:15
      compare_player <- compare_player |>
        mutate(
          type = case_when(
            index %in% 1:4 ~ "Attacking",
            index %in% 5:8 ~ "Possession",
            index %in% 9:12 ~ "Passing",
            index %in% 13:15 ~ "Misc"
          )
        )
      compare_player$type <-
        factor(compare_player$type,
               levels = c("Attacking", "Possession", "Passing", "Misc"))
    }
    } else {
      NULL
    }
    
    if (input$multi_select && !is.null(input$player_comparison)) {
      output$radarChart <- renderPlot({
        color1 <- "#008a71"
        color2 <- "#0362cc"
        color3 <- "#ffa602"
        color4 <- "#ff6d00"
        ggplot(data = selected_player,
               aes(
                 x = reorder(Statistic, index),
                 y = percentile,
                 label = percentile,
                 fill = type
               )) +
          geom_bar(data = selected_player,
                   stat = "identity",
                   width = 1,) +
          geom_bar(
            data = compare_player,
            stat = "identity",
            width = 1,
            alpha = 0,
            color = "black",
            size = 2,
            show.legend = FALSE
          ) +
          scale_y_continuous(limits = c(0, 100)) +
          scale_x_discrete(labels = function(x) str_wrap(x, width = 18)) +
          coord_curvedpolar() +
          geom_hline(yintercept = seq(0, 100, by = 100),
                     linewidth = 1) +
          geom_vline(xintercept = seq(.5, 15, by = 1),
                     linewidth = .5) +
          
          geom_label(
            color = "gray20",
            fill = "oldlace",
            size = 4,
            fontface = "bold",
            show.legend = FALSE
          ) +
          scale_fill_manual(values = c(color1, color2, color3, color4)) +
          theme(
            plot.title = element_text(
              hjust = .5,
              colour = "black",
              size = 16,
              face = "bold"
            ),
            plot.subtitle = element_text(
              hjust = .5,
              colour = "black",
              size = 16
            ),
            panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            axis.text.x = element_text(
              face = "plain",
              size = 12
            ),
            panel.background = element_rect(fill = "white")
          ) +
          labs(
            title = paste0(selected_player$Name[1], " vs ", compare_player$Name[1]),
            subtitle = paste0("Percentile Rank vs ", selected_player$Position[1],"s", " | 2022/2023 Season"),
            caption = "Data from FBref via worldfootballR | https://fbref.com/en/",
            x = NULL,
            y = NULL
          )
      })
      
    } else {
      output$radarChart <- renderPlot({
        color1 <- "#008a71"
        color2 <- "#0362cc"
        color3 <- "#ffa602"
        color4 <- "#ff6d00"
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
          scale_x_discrete(labels = function(x) str_wrap(x, width = 9)) +
          coord_polar() +
          geom_hline(yintercept = seq(0, 100, by = 100),
                     linewidth = 0.5) +
          geom_vline(xintercept = seq(.5, 15, by = 1),
                     linewidth = .2) +
          geom_label(
            color = "gray20",
            fill = "oldlace",
            size = 4,
            fontface = "bold",
            show.legend = FALSE
          ) +
          scale_fill_manual(values = c(color1, color2, color3, color4)) +
          theme(
            plot.title = element_text(
              hjust = .5,
              colour = "black",
              face = "bold",
              size = 16
            ),
            plot.subtitle = element_text(
              hjust = .5,
              colour = "black",
              size = 16
            ),
            panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            axis.text.x = element_text(
              face = "plain",
              size = 12,
            ),
            panel.background = element_rect(fill = "white")
          ) +
          labs(title = paste0(selected_player$Name[1]),
               subtitle = paste0("Percentile Rank vs ", selected_player$Position[1],"s", " | 2022/2023 Season"),
               caption = "Data from FBref via worldfootballR | https://fbref.com/en/",
               x = NULL,
               y = NULL)
      })
    }
  })
}

shinyApp(ui = ui, server = server)

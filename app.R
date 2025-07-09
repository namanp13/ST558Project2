library(shiny)
library(httr)
library(jsonlite)
library(tibble)
library(purrr)
library(dplyr)

source("functions.R")

# Prepare team choices
teams_data <- get_teams(api_key)
teams_choices <- reactive({
  data <- get_teams(api_key)
  setNames(data$id, data$full_name)
})

ui <- fluidPage(
  titlePanel("NBA Stats Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("query_type", "Select Query Type", choices = c(
        "Search Players by Name",
        "List All Teams",
        "Filter Teams by Conference",
        "Games for Team on Date",
        "All Games on Date",
        "Games for Team over Date Range"
      )),
      uiOutput("query_inputs")
    ),
    mainPanel(
      tableOutput("api_data"),
      downloadButton("download_data", "Download CSV")
    )
  )
)

server <- function(input, output, session) {
  output$query_inputs <- renderUI({
    switch(input$query_type,
           "Search Players by Name" = textInput("player_name", "Enter Player Name", ""),
           "List All Teams" = NULL,
           "Filter Teams by Conference" = selectInput("conference", "Select Conference", c("East", "West")),
           "Games for Team on Date" = tagList(
             selectInput("team_id", "Select Team", choices = teams_choices()),
             dateInput("game_date", "Select Date")
           ),
           "All Games on Date" = dateInput("game_date_all", "Select Date"),
           "Games for Team over Date Range" = tagList(
             selectInput("team_id_range", "Select Team", choices = teams_choices),
             dateRangeInput("game_date_range", "Select Date Range")
           )
    )
  })
  
  queried_data <- reactive({
    req(input$query_type)
    switch(input$query_type,
           "Search Players by Name" = get_player(input$player_name, api_key),
           "List All Teams" = get_teams(api_key),
           "Filter Teams by Conference" = {
             teams <- get_teams(api_key)
             teams %>% filter(conference == input$conference)
           },
           "Games for Team on Date" = get_games(input$team_id, input$game_date, api_key),
           "All Games on Date" = get_games(NULL, input$game_date_all, api_key),
           "Games for Team over Date Range" = {
             dates <- input$game_date_range
             get_games(input$team_id_range, dates, api_key)
           }
    )
  })
  
  output$api_data <- renderTable({
    queried_data()
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("nba_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(queried_data(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)


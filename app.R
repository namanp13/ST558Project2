library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(purrr)
library(jsonlite)
library(httr)

#Couple of notes: I used some different syntax: .data[[input$var]]. The reason for that is because I realized I can't write aes(x = input$x_var) because that would make it think that input$x_var is the column itself. All .data[[input$x_var]] does is it tells ggplot to look at the column of the dataframe. I also read somewhere that this is better for more interactive apps. I also used req(input$x_var) because I was getting an error where input$x_var was NULL. I looked it up online and found that req() was the solution to it (I think at shiny.posit.co). 

api_key <- "1f08e5ca-ec1f-4a50-805b-203ad159024c"

get_player <- function(name, api_key) {
  url <- paste0("https://api.balldontlie.io/v1/players?search=", name)
  resp <- GET(url, add_headers(Authorization = api_key))
  content(resp, as = "text", encoding = "UTF-8") |>
    fromJSON() |>
    pluck("data") |>
    tibble::as_tibble()
}

get_teams <- function(api_key) {
  url <- "https://api.balldontlie.io/v1/teams"
  resp <- GET(url, add_headers(Authorization = api_key))
  content(resp, as = "text", encoding = "UTF-8") |>
    fromJSON() |>
    pluck("data") |>
    tibble::as_tibble()
}

teams_data <- get_teams(api_key)

ui <- navbarPage(
  "NBA Stats Explorer",
  
  tabPanel("About",
           fluidPage(h2("About this App"), p("This app allows you to explore NBA team and player data. There's different information about players regarding their teams, names, cities, height, weight, and so on. It comes from the ", a("balldontlie API", href = "https://www.balldontlie.io/"), "."), p("Tabs:"), tags$ul(tags$li(strong("Data Download:"), " Allows you to query players or teams. It also allows you to subset data or download it."),tags$li(strong("Data Exploration:"), " Allows you to summarize and visualize NBA data with different plots and tables.")),img(src = "nba_logo.png", height = "300px"))
  ),
  
  tabPanel("Data Download", sidebarLayout(sidebarPanel(textInput("player_name", "Search Player Name (leave blank for teams):"),actionButton("query", "Query Data"),hr(), checkboxGroupInput("columns", "Include columns:", choices = NULL),
downloadButton("download", "Download CSV")),
  
  mainPanel(DTOutput("data_table")))),
  
  tabPanel("Data Exploration",sidebarLayout(sidebarPanel(selectInput("dataset", "Select dataset:", choices = c("Teams", "Players")),uiOutput("xvar_ui"), uiOutput("facet_ui"), selectInput("plot_type", "Plot Type:", choices = c("Bar", "Heatmap"))),
  
  mainPanel(tableOutput("summary_table"),plotOutput("explore_plot")))))

server <- function(input, output, session) {
  
  queried_data <- reactiveVal(teams_data)
  
  observeEvent(input$query, {
    if (input$player_name != "") {
      queried_data(get_player(input$player_name, api_key))
    } else {
      queried_data(teams_data)
    }
    
    updateCheckboxGroupInput(session, "columns", choices = names(queried_data()),selected = names(queried_data()))})
  
  output$data_table <- renderDT({
    req(queried_data())
    queried_data()[, input$columns, drop = FALSE]
  })
  
  output$download <- downloadHandler(
    filename = function() { "nba_data.csv" }, content = function(file) {
      write.csv(queried_data()[, input$columns, drop = FALSE], file, row.names = FALSE)
    }
  )
  
  output$xvar_ui <- renderUI({
    req(input$dataset)
    choices <- if (input$dataset == "Teams") names(teams_data) else names(queried_data())
    selectInput("xvar", "X Variable:", choices = choices)
  })
  
  output$facet_ui <- renderUI({
    req(input$dataset)
    choices <- if (input$dataset == "Teams") names(teams_data) else names(queried_data())
    selectInput("facet", "Facet Variable:", choices = c("None", choices))
  })
  
  output$summary_table <- renderTable({
    data <- if (input$dataset == "Teams") teams_data else queried_data()
    req(input$xvar)
    data %>% count(.data[[input$xvar]])
  })
  
  output$explore_plot <- renderPlot({
    data <- if (input$dataset == "Teams") teams_data else queried_data()
    req(input$xvar)
    
    if (input$plot_type == "Bar") {
      p <- ggplot(data, aes(x = .data[[input$xvar]])) +
        geom_bar(fill = "blue") +
        labs(title = paste("Bar Plot of", input$xvar), x = input$xvar, y = "Count")
    } else {
      heatmap_data <- data %>%
        count(.data[[input$xvar]], .data[[input$facet]])
      p <- ggplot(heatmap_data, aes(x = .data[[input$xvar]], y = .data[[input$facet]], fill = n)) +
        geom_tile() +
        labs(title = paste("Heatmap of", input$xvar, "vs", input$facet), x = input$xvar, y = input$facet, fill = "Count")}
    
    if (input$facet != "None" && input$plot_type == "Bar") {
      p <- p + facet_wrap(as.formula(paste("~", input$facet)))}
    p})}

shinyApp(ui, server)


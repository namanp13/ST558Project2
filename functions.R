api_key <- "1f08e5ca-ec1f-4a50-805b-203ad159024c"

get_player <- function(name, api_key) {
  url <- paste0("https://api.balldontlie.io/v1/players?search=", name)
  response <- httr::GET(url, httr::add_headers(Authorization = api_key))
  
  httr::content(response, as = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON() |>
    purrr::pluck("data") |>
    tibble::as_tibble()
}


get_teams <- function(api_key) {
  url2 <- "https://api.balldontlie.io/v1/teams"
  response2 <- httr::GET(url2, httr::add_headers(Authorization = api_key))
  
  httr::content(response2, as = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON() |>
    purrr::pluck("data") |>
    tibble::as_tibble()
}

get_games <- function(id, date, api_key) {
  url3 <- paste0("https://api.balldontlie.io/v1/games?team_ids[]=", id, "&dates[]=", date)
  response3 <- httr::GET(url3, httr::add_headers(Authorization = api_key))
  
  httr::content(response3, as = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON() |>
    purrr::pluck("data") |>
    tibble::as_tibble()
}

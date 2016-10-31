library(leaflet)
library(shiny)
library(shinyBS)
library(shinydashboard)
library(RMySQL)

# Define the fields we want to save from the form
fields <- c("name", "r_num_years")
fieldsRoute <- c("location", "destination")
fieldsFactors <-
  c(
    "route_id",
    "time",
    "day",
    "cars",
    "lanes",
    "damage",
    "zones",
    "events",
    "weather",
    "distance"
  )

shinyServer(function(input, output, session) {
  killDbConnections()

  # Whenever a field is filled, aggregate all form data
  formData <- reactive({
    data <- sapply(fields, function(x)
      input[[x]])
    data
  })

  formDataRoute <- reactive({
    data <- sapply(fieldsRoute, function(x)
      input[[x]])
    data
  })

  formDataFactors <- reactive({
    data <- sapply(fieldsFactors, function(x)
      input[[x]])
    data
  })

  # When the Submit button is clicked, save the form data
  observeEvent(input$submit, {
    saveData(formData())
  })

  observeEvent(input$saveRoute, {
    saveRouteData(formDataRoute())
    toggleModal(session, "modalRoute", toggle = "close")
    createAlert(
      session,
      "SuccessRouteAlert",
      content = "Route is successfully added",
      dismiss = TRUE,
      append = TRUE
    )
    updateSelectInput(session,
                      "routeId",
                      choices = loadDataRouteId()$route_id)
  })

  observeEvent(input$saveFactors, {
    saveFactorsData(formDataFactors())
    toggleModal(session, "modalFactors", toggle = "close")
    createAlert(
      session,
      "SuccessFactorsAlert",
      content = "Factors is successfully added",
      dismiss = TRUE,
      append = TRUE
    )
  })

  # Show the previous responses
  # (update with current response when Submit is clicked)
  output$responses <- DT::renderDataTable({
    input$submit
    loadData()
  })

  # Show the previous responses
  output$factors <- DT::renderDataTable({
    loadDataFactors(input$routeId)
  })

  # Show data from loadData to selectInput
  observe({
    updateSelectInput(session,
                      "routeId",
                      choices = loadDataRouteId()$route_id)
  })

  output$locationDestinationData <- renderText({
    paste(
      loadDataRoute(input$routeId)$location,
      "->",
      loadDataRoute(input$routeId)$destination
    )
  })

  observe({
    c_num <- input$routeId
    # Change the value
    updateNumericInput(session, "route_id", value = c_num)

  })
})

options(mysql = list(
  "host" = "127.0.0.1",
  "port" = 3306,
  "user" = "root",
  "password" = ""
))

#' To connect to a MySQL database
#'
#' @param host
#' @param port
#' @param dbname
#' @param user
#' @param password
#' @export
dbConnectFunc <-
  function() {
    dbConnect(
      RMySQL::MySQL(),
      dbname = databaseNavify,
      host = options()$mysql$host,
      port = options()$mysql$port,
      user = options()$mysql$user,
      password = options()$mysql$password
    )
  }

# Create a MySQL database.
databaseName <- "myshinydatabase"
databaseNavify <- "navifactors"
# Table that will store the responses.
table <- "responses"
tableRoute <- "route"
tableFactors <- "factors"

#' Save function that simply stores responses in MySQL.
#'
#' @param data
#' @examples
#' saveData(data)
#' @export
saveData <- function(data) {
  db <-
    RMySQL::dbConnect(
      RMySQL::MySQL(),
      dbname = databaseNavify,
      host = options()$mysql$host,
      port = options()$mysql$port,
      user = options()$mysql$user,
      password = options()$mysql$password
    )
  # Construct the update query by looping over the data fields
  query <- sprintf(
    "INSERT INTO %s (%s) VALUES ('%s')",
    table,
    paste(names(data), collapse = ", "),
    paste(data, collapse = "', '")
  )
  # Submit the update query and disconnect
  DBI::dbGetQuery(db, query)
  RMySQL::dbDisconnect(db)
}

#' Save function that stores route in database.
#'
#' @param location
#' @param destination
#' saveRouteData(data)
saveRouteData <- function(data) {
  db <-
    RMySQL::dbConnect(
      RMySQL::MySQL(),
      dbname = databaseNavify,
      host = options()$mysql$host,
      port = options()$mysql$port,
      user = options()$mysql$user,
      password = options()$mysql$password
    )
  # Construct the update query by looping over the data fields
  query <- sprintf(
    "INSERT INTO %s (%s) VALUES ('%s')",
    tableRoute,
    paste(names(data), collapse = ", "),
    paste(data, collapse = "', '")
  )
  # Submit the update query and disconnect
  DBI::dbGetQuery(db, query)
  RMySQL::dbDisconnect(db)
}

#' Save function that stores factors forr a specific route in database.
#'
#' @param location
#' @param destination
#' saveFactorsData(data)
saveFactorsData <- function(data) {
  db <-
    RMySQL::dbConnect(
      RMySQL::MySQL(),
      dbname = databaseNavify,
      host = options()$mysql$host,
      port = options()$mysql$port,
      user = options()$mysql$user,
      password = options()$mysql$password
    )
  # Construct the update query by looping over the data fields
  query <- sprintf(
    "INSERT INTO %s (%s) VALUES ('%s')",
    tableFactors,
    paste(names(data), collapse = ", "),
    paste(data, collapse = "', '")
  )
  # Submit the update query and disconnect
  DBI::dbGetQuery(db, query)
  RMySQL::dbDisconnect(db)
}

#' Retrieve function that simply load responses from MySQL.
#'
#' @export
loadData <- function() {
  db <-
    RMySQL::dbConnect(
      RMySQL::MySQL(),
      dbname = databaseNavify,
      host = options()$mysql$host,
      port = options()$mysql$port,
      user = options()$mysql$user,
      password = options()$mysql$password
    )
  # Construct the fetching query
  query <- sprintf("SELECT * FROM %s", tableRoute)
  # Submit the fetch query and disconnect
  data <- DBI::dbGetQuery(db, query)
  RMySQL::dbDisconnect(db)
  data
}

#' Retrieve function that simply load route from MySQL.
#'
#' @export
loadDataRouteId <- function() {
  db <-
    RMySQL::dbConnect(
      RMySQL::MySQL(),
      dbname = databaseNavify,
      host = options()$mysql$host,
      port = options()$mysql$port,
      user = options()$mysql$user,
      password = options()$mysql$password
    )
  # Construct the fetching query
  query <- sprintf("SELECT * FROM %s", tableRoute)
  # Submit the fetch query and disconnect
  data <- DBI::dbGetQuery(db, query)
  RMySQL::dbDisconnect(db)
  data
}

#' Retrieve function that simply load route from MySQL.
#'
#' @export
loadDataRoute <- function(id) {
  db <-
    RMySQL::dbConnect(
      RMySQL::MySQL(),
      dbname = databaseNavify,
      host = options()$mysql$host,
      port = options()$mysql$port,
      user = options()$mysql$user,
      password = options()$mysql$password
    )
  # Construct the fetching query
  query <-
    sprintf("SELECT * FROM %s WHERE route_id = %s", tableRoute, id)
  # Submit the fetch query and disconnect
  data <- DBI::dbGetQuery(db, query)
  RMySQL::dbDisconnect(db)
  data
}

#' Load function that stores factors for route in database.
#'
#' @param time
#' @param ...
#' loadDataFactors(id)
loadDataFactors <- function(id) {
  db <-
    RMySQL::dbConnect(
      RMySQL::MySQL(),
      dbname = databaseNavify,
      host = options()$mysql$host,
      port = options()$mysql$port,
      user = options()$mysql$user,
      password = options()$mysql$password
    )
  # Construct the fetching query
  query <-
    sprintf("SELECT * FROM %s WHERE route_id = %s", tableFactors, id)
  # Submit the fetch query and disconnect
  data <- DBI::dbGetQuery(db, query)
  RMySQL::dbDisconnect(db)
  data
}

#' function kill all open connections at once
#'
killDbConnections <- function () {
  all_cons <- RMySQL::dbListConnections(RMySQL::MySQL())
  print(all_cons)
  for (con in all_cons)
    +  RMySQL::dbDisconnect(con)
  print(paste(length(all_cons), " connections killed."))
}

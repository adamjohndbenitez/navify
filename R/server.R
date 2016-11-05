require(magrittr)

streets <-
  read.csv(
    file = "C:\\Users\\Adam\\Documents\\CIT-U\\Masters of Computer Science\\Capstone - Thesis\\Navify - Traffixer\\Navify\\street.csv",
    header = TRUE,
    sep = ",",
    stringsAsFactors = FALSE
  )

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

shiny::shinyServer(function(input, output, session) {

  # Show data from loadData to selectInput
  observe({
    updateSelectInput(session,
                      "routeId",
                      choices = loadDataRouteId()$route_id)
  })

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
    removeModal(session = getDefaultReactiveDomain())
    updateSelectInput(session,
                      "routeId",
                      choices = loadDataRouteId()$route_id)
  })

  observeEvent(input$saveFactors, {
    saveFactorsData(formDataFactors())
    removeModal(session = getDefaultReactiveDomain())
    # Show the previous responses
    output$factors <- DT::renderDataTable({
      loadDataFactors(input$routeId)
    })
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

  output$locationDestinationData <- renderText({
    paste(
      loadDataRoute(input$routeId)$location,
      "->",
      loadDataRoute(input$routeId)$destination
    )
  })

  output$googleMapId = renderPlot({
    map <- ggmap::get_map(location = "philippines", zoom = 7, maptype = "terrain")
    plot(map)
  })

  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)

  output$mymap <- leaflet::renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addTiles() %>%  # Add default OpenStreetMap map tiles
      leaflet::setView(lng=123.89071, lat=10.31672, zoom = 15)
  })

  observeEvent(input$createRoute, {
    showModal(modalDialog(
      title = "Create Route",
      footer = tagList(
        actionButton("saveRoute", "saveRoute"),
        modalButton("Cancel")
      ),
      size = "m",
      easyClose = FALSE,
      shiny::fluidRow(shiny::column(
        width = 12,
        shiny::column(
          width = 6,
          # Select location
          shiny::selectInput(
            inputId = "location",
            label = "Select Location",
            choices = streets$name
          )
        ),
        shiny::column(
          width = 6,
          # Select destination
          shiny::selectInput(
            inputId = "destination",
            label = "Select Destination",
            choices = streets$name
          )
        )
      ))
    ))
  })

  observeEvent(input$createFactors, {
    # get the value of the routeId from selected routId to modal create factors
    observe({
      modal_routeId <- input$routeId
      # Change the value
      updateNumericInput(session, "route_id", value = modal_routeId)

    })
    showModal(modalDialog(
      title = "Create Factors",
      footer = tagList(
        actionButton("saveFactors", "saveFactors"),
        modalButton("Cancel")
      ),
      size = "l",
      easyClose = FALSE,
      # modal to create factors
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::column(width = 6, shiny::textOutput("locationText")),
          shiny::column(width = 6, shiny::textOutput("DestinationText"))
        ),
        shiny::column(
          width = 12,
          shiny::column(
            width = 3,
            shiny::selectInput(
              inputId = "time",
              label = "Time:",
              choices = list(
                "6 AM" = 6,
                "7 AM" = 7,
                "8 AM" = 8,
                "9 AM" = 9,
                "10 AM" = 10,
                "11 AM" = 11,
                "12 PM" = 12,
                "1 PM" = 13,
                "2 PM" = 14,
                "3 PM" = 15,
                "4 PM" = 16,
                "5 PM" = 17,
                "6 PM" = 18,
                "7 PM" = 19,
                "8 PM" = 20,
                "9 PM" = 21,
                "10 PM" = 22,
                "11 PM" = 23,
                "12 AM" = 24,
                "1 AM" = 1,
                "2 AM" = 2,
                "3 AM" = 3,
                "4 AM" = 4,
                "5 AM" = 5
              ),
              selected = 1
            ),
            selectize = TRUE
          ),
          shiny::column(
            width = 3,
            shiny::selectInput(
              inputId = "day",
              label = "Day:",
              choices = list(
                "Monday" = 1,
                "Tuesday" = 2,
                "Wednesday" = 3,
                "Thursday" = 4,
                "Friday" = 5,
                "Saturday" = 6,
                "Sunday" = 7
              ),
              selected = 1
            )
          ),
          shiny::column(
            width = 3,
            shiny::numericInput(
              inputId = "cars",
              label = "Cars:",
              value = 200,
              min = 1,
              max = 1000
            )
          ),
          shiny::column(
            width = 3,
            shiny::numericInput(
              inputId = "lanes",
              label = "Lane:",
              value = 3,
              min = 1,
              max = 3
            )
          )
        ),
        shiny::column(
          width = 12,
          shiny::column(
            width = 3,
            shiny::selectInput(
              inputId = "damage",
              label = "Damage:",
              choices = list(
                "None" = 0,
                "Patholes" = 1,
                "Muddy Road" = 2,
                "Construction" = 3
              ),
              selected = 1
            )
          ),
          shiny::column(
            width = 3,
            shiny::selectInput(
              inputId = "zones",
              label = "Zone:",
              choices = list(
                "None" = 0,
                "Schools" = 1,
                "Mall" = 2,
                "Call Centers" = 3
              ),
              selected = 1
            )
          ),
          shiny::column(
            width = 3,
            shiny::numericInput(
              inputId = "distance",
              label = "Distance:",
              value = 200,
              min = 1,
              max = 1000
            )
          ),
          shiny::column(
            width = 3,
            shiny::selectInput(
              inputId = "weather",
              label = "Weather:",
              choices = list(
                "Sunny" = 0,
                "Windy" = 1,
                "Heavy Rain" = 2,
                "Storm" = 3
              ),
              selected = 1
            )
          )
        ),
        shiny::column(
          width = 12,
          shiny::column(
            width = 3,
            shiny::selectInput(
              inputId = "events",
              label = "Events:",
              choices = list(
                "None" = 0,
                "Sinulog" = 1,
                "Fiesta" = 2,
                "Ironman" = 3
              ),
              selected = 1
            )
          ),
          shiny::column(
            width = 3,
            shiny::numericInput(
              "route_id",
              "Route Id:",
              min = 1,
              max = 20,
              value = 15
            )
          )
        )
      )
    ))
  })
})

options(mysql = list(
  "host" = "127.0.0.1",
  "port" = 3306,
  "user" = "root",
  "password" = ""
))

# If you modify global options() or graphics par(), save the old values and reset when you’re done
old <- options(stringsAsFactors = FALSE)
on.exit(options(old), add = TRUE)

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
    sprintf("SELECT * FROM %s WHERE route_id = '%s'", tableRoute, id)
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
    sprintf("SELECT * FROM %s WHERE route_id = '%s'", tableFactors, id)
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

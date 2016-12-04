shiny::shinyServer(function(input, output, session) {
  base::source("R/saveData.R")
  base::source("R/loadData.R")
  base::source("R/loadDataById.R")
  base::source("R/loadDataWhereCond.R")
  base::source("R/createRouteModal.R")
  base::source("R/createFactorsModal.R")

  observe({
    # Show data from loadData to selectInput
    updateSelectInput(session,
                      "routeId",
                      choices = loadDataRoute()$route_id)
  })
  observe({
    # Show data from loadData selecting street table to selectInput
    updateSelectInput(session,
                      "locationSearchId",
                      choices = loadDataStreets()$street_name)
  })
  observe({
  updateSelectInput(session,
                    "destinationSearchId",
                    choices = loadDataStreets()$street_name)
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

  observeEvent(input$saveRoute, {
    shiny::callModule(module = saveData, id = "saveData", formDataRoute(), "routes")
    removeModal(session = getDefaultReactiveDomain())
    updateSelectInput(session,
                      "routeId",
                      choices = loadDataRoute()$route_id)
  })

  observeEvent(input$saveFactors, {
    shiny::callModule(module = saveData, id = "saveData", formDataFactors(), "factors")
    removeModal(session = getDefaultReactiveDomain())
    # Show the previous responses
    output$factors <- DT::renderDataTable({
      loadDataFactors(input$routeId)
    })
  })

  # Show the previous responses
  output$factors <- DT::renderDataTable({
    loadDataFactors(input$routeId)
  })

  observeEvent(input$incidents, {
    dataObject <- jsonlite::fromJSON("http://dev.virtualearth.net/REST/v1/Traffic/Incidents/45.219,-122.325,47.610,-122.107?key=AnpFWQs6cZEkl2G9VXn5WKKb-_FqEKF_nCe3G4C_6tRsxJ9BH1Dd0XL_hmTGU7Ro")
    dataJson <- jsonlite::toJSON(dataObject, pretty = TRUE)
    print(dataObject$resourceSet$resources[[1]])
    print(dataJson)
  })

  output$locationDestinationData <- renderText({
    paste(
      loadDataRouteById(input$routeId)$location,
      "->",
      loadDataRouteById(input$routeId)$destination
    )
  })

  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)

  # output$mymap <- leaflet::renderLeaflet({
  #   leaflet::leaflet() %>%
  #     leaflet::addTiles() %>%  # Add default OpenStreetMap map tiles
  #     leaflet::setView(lng=123.89071, lat=10.31672, zoom = 15)
  # })

  observeEvent(input$createRoute, {
    shiny::callModule(module = createRouteModal, id = "createRouteModal", loadDataStreets()$street_name)
  })

  observeEvent(input$createFactors, {
    shiny::callModule(module = createFactorsModal, id = "createFactorsModal")
    shiny::observe({
      modal_routeId <- input$routeId
      # Change the value
      updateNumericInput(session = session, inputId = "route_id", value = modal_routeId)

    })
  })

  # initial map, showing all streets
  output$DFSmap <- leaflet::renderLeaflet({
    leaflet::leaflet(data = loadDataStreets()) %>%
      leaflet::addTiles() %>%
      leaflet::setView(lng = 123.8854, lat = 10.3157, zoom = 12) %>%
      leaflet::addMarkers(lng = ~longitude, lat = ~latitude, popup = as.character(loadDataStreets()$street_name), clusterOptions = leaflet::markerClusterOptions())
  })
  observeEvent(input$showMap, {
    output$DFSmap <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        leaflet::addTiles() %>%  # Add default OpenStreetMap map tiles
        leaflet::addMarkers(lng = loadDataByStreetName(input$locationSearchId)$longitude,
                            lat = loadDataByStreetName(input$locationSearchId)$latitude,
                            popup = input$locationSearchId) %>%
        leaflet::addMarkers(lng = loadDataByStreetName(input$destinationSearchId)$longitude,
                            lat = loadDataByStreetName(input$destinationSearchId)$latitude,
                            popup = input$destinationSearchId)
    })
  })


})

loadDataRoute <- function() {
  tableRoute <- "routes"
  data <- shiny::callModule(module = loadData, id = "loadData", tableRoute)
}


#' Retrieve function that simply load route from MySQL.
#'
#' @export
loadDataRouteById <- function(id) {
  tableRoute <- "routes"
  data <- shiny::callModule(module = loadDataById, id = "loadDataById", id, tableRoute)
}

#' Retrieve data from factors with given id for route in database.
#'
#' @param tableFactors table name for factors.
#' @param id route id from saving factors.
#' loadDataFactors(id)
loadDataFactors <- function(id) {
  tableFactors <- "factors"
  data <- shiny::callModule(module = loadDataById, id = "loadDataById", id, tableFactors)
}

#' Retrieve data from streets from MySQL.
#'
#' @export
# load data from streets
loadDataStreets <- function() {
  tableStreets <- "streets"
  data <- shiny::callModule(module = loadData, id = "loadData", tableStreets)
  data
}

#' Retrieve data from streets with condition from MySQL.
#'
#' @export
# load data from streets
loadDataByStreetName <- function(name) {
  tableStreets <- "streets"
  data <- shiny::callModule(module = loadDataWhereCond, id = "loadDataWhereCond", name, tableStreets)
  data
}



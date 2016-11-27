shiny::shinyServer(function(input, output, session) {
  base::source("R/saveData.R")
  base::source("R/loadData.R")
  base::source("R/loadDataById.R")
  base::source("R/createRouteModal.R")
  base::source("R/createFactorsModal.R")

  observe({
    # Show data from loadData to selectInput
    updateSelectInput(session,
                      "routeId",
                      choices = loadDataRouteId()$route_id)
    # Show data from loadData selecting street table to selectInput
    updateSelectInput(session,
                      "locationSearchId",
                      choices = loadDataStreets()$street_name)
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
                      choices = loadDataRouteId()$route_id)
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
      loadDataRoute(input$routeId)$location,
      "->",
      loadDataRoute(input$routeId)$destination
    )
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
    shiny::callModule(module = createRouteModal, id = "createRouteModal", loadDataStreets)
  })

  observeEvent(input$createFactors, {
    shiny::callModule(module = createFactorsModal, id = "createFactorsModal")
    shiny::observe({
      modal_routeId <- input$routeId
      # Change the value
      updateNumericInput(session = session, inputId = "route_id", value = modal_routeId)

    })
  })

  output$DFSmap <- leaflet::renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addTiles() %>%  # Add default OpenStreetMap map tiles
      leaflet::addMarkers(lng=123.847311, lat=10.271590, popup="CSR") %>%
      leaflet::addMarkers(lng=123.870742, lat=10.289747, popup="F.Llamas_CSR") %>%
      # leaflet::addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R") %>%
      leaflet::addPolylines(lng=c(123.847311, 123.870742), lat=c(10.271590, 10.289747), stroke = TRUE, color = "black", weight = 5, opacity = 0.7, fill = FALSE, fillColor = "black", fillOpacity = 0.5, dashArray = NULL, smoothFactor = 1, noClip = TRUE, popup = "pull something")
  })
})

loadDataRouteId <- function() {
  tableRoute <- "routes"
  data <- shiny::callModule(module = loadData, id = "loadData", tableRoute)
}


#' Retrieve function that simply load route from MySQL.
#'
#' @export
loadDataRoute <- function(id) {
  tableRoute <- "routes"
  data <- shiny::callModule(module = loadDataById, id = "loadDataById", id, tableRoute)
}

#' Load function that stores factors for route in database.
#'
#' @param tableFactors table name for factors.
#' @param id route id from saving factors.
#' loadDataFactors(id)
loadDataFactors <- function(id) {
  tableFactors <- "factors"
  data <- shiny::callModule(module = loadDataById, id = "loadDataById", id, tableFactors)
}

#' Retrieve function that simply load streets from MySQL.
#'
#' @export
# load data from streets
loadDataStreets <- function() {
  tableStreets <- "streets"
  data <- shiny::callModule(module = loadData, id = "loadData", tableStreets)
  data
}

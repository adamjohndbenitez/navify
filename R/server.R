shiny::shinyServer(function(input, output, session) {
  base::source("R/saveData.R")
  base::source("R/loadData.R")
  base::source("R/loadDataById.R")
  base::source("R/createRouteModal.R")
  base::source("R/createFactorsModal.R")

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
    shiny::callModule(module = saveData, id = "saveData", formData(), "responses")
  })

  observeEvent(input$saveRoute, {
    shiny::callModule(module = saveData, id = "saveData", formDataRoute(), "route")
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
  # (update with current response when Submit is clicked)
  output$responses <- DT::renderDataTable({
    input$submit
    loadData()
    tableRoute <- "route"
    shiny::callModule(module = loadData, id = "loadData", tableRoute)
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
    shiny::callModule(module = createRouteModal, id = "createRouteModal", streetsData)
  })

  observeEvent(input$createFactors, {
    shiny::callModule(module = createFactorsModal, id = "createFactorsModal")
  })
})

loadDataRouteId <- function() {
  tableRoute <- "route"
  data <- shiny::callModule(module = loadData, id = "loadData", tableRoute)
}


#' Retrieve function that simply load route from MySQL.
#'
#' @export
loadDataRoute <- function(id) {
  tableRoute <- "route"
  data <- shiny::callModule(module = loadDataById, id = "loadDataById", id, tableRoute)
}

#' Load function that stores factors for route in database.
#'
#' @param time
#' @param ...
#' loadDataFactors(id)
loadDataFactors <- function(id) {
  tableFactors <- "factors"
  data <- shiny::callModule(module = loadDataById, id = "loadDataById", id, tableFactors)
}

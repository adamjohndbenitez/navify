shiny::shinyServer(function(input, output, session) {
  base::source("R/saveData.R")
  base::source("R/loadData.R")
  base::source("R/loadDataById.R")
  base::source("R/loadDataWhereCond.R")
  base::source("R/createRouteModal.R")
  base::source("R/createFactorsModal.R")
  base::source("R/possiblePathsModal.R")

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

  dfsEnv$edges <- loadDataEdges()
  # initial map, showing all streets
  output$DFSmap <- leaflet::renderLeaflet({
    leaflet::leaflet(data = loadDataStreets()) %>%
      leaflet::addTiles() %>%
      leaflet::setView(lng = 123.8854, lat = 10.3157, zoom = 12) %>%
      leaflet::addMarkers(lng = ~longitude, lat = ~latitude, popup = as.character(loadDataStreets()$street_name), clusterOptions = leaflet::markerClusterOptions())
  })
  observeEvent(input$showMap, {
    # initialize variables for depth first search
      dfsEnv$vis <- hash::hash()
      dfsEnv$path <- c()
      dfsEnv$allPaths <- list()
      street_nodes <- 34
      for (i in 1:street_nodes) {
        hash::.set(dfsEnv$vis, keys=i, values=FALSE)
      }
      dfs(loadDataByStreetName(input$locationSearchId)$street_id,
          loadDataByStreetName(input$destinationSearchId)$street_id)
      print(dfsEnv$allPaths)

    output$DFSmap <- leaflet::renderLeaflet({
      m <- leaflet::leaflet()
      m <- leaflet::addTiles(m)
      m <- leaflet::addMarkers(map = m, lng = loadDataByStreetName(input$locationSearchId)$longitude,
                            lat = loadDataByStreetName(input$locationSearchId)$latitude,
                            popup = input$locationSearchId)
      m <- leaflet::addMarkers(map = m, lng = loadDataByStreetName(input$destinationSearchId)$longitude,
                            lat = loadDataByStreetName(input$destinationSearchId)$latitude,
                            popup = input$destinationSearchId)
      for (j in 1:length(dfsEnv$allPaths)) {
        for (i in 1:length(dfsEnv$allPaths[[j]])) {
          if (!gtools::invalid(loadDataByStreetId(dfsEnv$allPaths[[j]][i+1]))) {
            m <- leaflet::addPolylines(map = m, lng=c(round(loadDataByStreetId(dfsEnv$allPaths[[j]][i])$longitude, digits = 6),
                                        round(loadDataByStreetId(dfsEnv$allPaths[[j]][i+1])$longitude, digits = 6)),
                                  lat=c(round(loadDataByStreetId(dfsEnv$allPaths[[j]][i])$latitude, digits = 6),
                                        round(loadDataByStreetId(dfsEnv$allPaths[[j]][i+1])$latitude,  digits = 6)), stroke = TRUE, color = "black", weight = 5, opacity = 0.7, fill = FALSE, fillColor = "black", fillOpacity = 0.5, dashArray = NULL, smoothFactor = 1, noClip = TRUE, popup = "pull something")
          }
        }
      }
      m
    })
  })

  # observeEvent(input$possiblePaths, {
  #   # initialize variables for depth first search
  #   dfsEnv$vis <- hash::hash()
  #   dfsEnv$path <- c()
  #   dfsEnv$allPaths <- list()
  #   street_nodes <- 34
  #   for (i in 1:street_nodes) {
  #     hash::.set(dfsEnv$vis, keys=i, values=FALSE)
  #   }
  #   dfs(loadDataByStreetName(input$locationSearchId)$street_id,
  #       loadDataByStreetName(input$destinationSearchId)$street_id)
  #   print(dfsEnv$allPaths)
  #   column_name <- c()
  #   for (i in 1:length(dfsEnv$allPaths)) {
  #     column_name <- append(x = column_name, values = toString(x = ))
  #   }
  #   names(dfsEnv$allPaths) <- column_name
  #   shiny::callModule(module = possiblePathsModal, id = "possiblePathsModal", names(dfsEnv$allPaths))
  # })

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

#' Retrieve data from streets with street_name from MySQL.
#'
#' @param name name of the street.
#' @export
# load data from streets
loadDataByStreetName <- function(name) {
  tableStreets <- "streets"
  columnName <- "street_name"
  data <- shiny::callModule(module = loadDataWhereCond, id = "loadDataWhereCond", name, tableStreets, columnName)
  data
}

#' Retrieve data from streets with street_id from MySQL.
#'
#' @param name name of the street.
#' @export
# load data from streets
loadDataByStreetId <- function(name) {
  tableStreets <- "streets"
  columnName <- "street_id"
  data <- shiny::callModule(module = loadDataWhereCond, id = "loadDataWhereCond", name, tableStreets, columnName)
  data
}

#' Retrieve data from streets from MySQL.
#'
#' @export
# load data from streets
loadDataEdges <- function() {
  tableEdges <- "edges"
  data <- shiny::callModule(module = loadData, id = "loadData", tableEdges)
  data
}

dfs <- function (loc, des) {
  result <- c()

  if (loc == des) {
    pathSize = length(dfsEnv$path)
    for (i in 0:pathSize) {
      result <- append(x = result, values = dfsEnv$path[i])
    }
    result <- append(x = result, values = des)

    lastVectorOfAll <- (length(dfsEnv$allPaths) + 1)
    dfsEnv$allPaths[[lastVectorOfAll]] <- result
    return()
  }

  if (hash::values(dfsEnv$vis, loc)) {
    return()
  }

  hash::values(dfsEnv$vis, keys=loc) <- TRUE

  for (i in 1:length(dfsEnv$edges$edge_id)) {
    first <- dfsEnv$edges$start_vertex[i]
    second <- dfsEnv$edges$end_vertex[i]
    if (first == loc) {
      dfsEnv$path <- append(x = dfsEnv$path, values = first)
      dfs(second, des)
      # lastElem <- length(dfsEnv$path)
      # dfsEnv$path[[lastElem]] <- NULL
      dfsEnv$path <- head(dfsEnv$path, -1)
    }
  }
}

shiny::shinyServer(function(input, output, session) {
  base::source("R/createRouteModal.R")
  base::source("R/createFactorsModal.R")
  base::source("R/deleteDataFactors.R")
  base::source("R/loadData.R")
  base::source("R/loadDataWhereCond.R")
  base::source("R/possiblePathsModal.R")
  base::source("R/saveData.R")
  base::source("R/saveImportedFactors.R")

  shiny::observe({
    updateSelectInput(session = session, inputId = "routeName", choices = loadDataStreets()$street_name)
  })

  shiny::observe({
    updateSelectInput(session = session, inputId = "locationSearchId", choices = loadDataStreets()$street_name)
  })

  shiny::observe({
    updateSelectInput(session = session, inputId = "destinationSearchId", choices = loadDataStreets()$street_name)
  })

  formDataRoute <- shiny::reactive({
    data <- sapply(X = fieldsRoute, FUN = function(x) input[[x]])
    data
  })

  formDataFactors <- shiny::reactive({
    data <- sapply(X = fieldsFactors, FUN = function(x) input[[x]])
    data
  })

  shiny::observeEvent(input$saveFactors, {
    tableName <- "factors"
    shiny::callModule(module = saveData, id = "saveData", formDataFactors(), tableName)
    shiny::removeModal(session = getDefaultReactiveDomain())
    output$factors <- DT::renderDataTable({
      loadDataFactors(input$routeName)
    })
  })

  output$factors <- DT::renderDataTable({
    loadDataFactors(input$routeName)
  }, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))

  shiny::observeEvent(input$createRoute, {
    shiny::callModule(module = createRouteModal, id = "createRouteModal", loadDataStreets()$street_name)
  })

  shiny::observeEvent(input$createFactors, {
    shiny::callModule(module = createFactorsModal, id = "createFactorsModal")
    shiny::observe({
      modal_routeId <- loadDataByStreetName(input$routeName)$street_id
      shiny::updateNumericInput(session = session, inputId = "street_id", value = modal_routeId)

    })
  })

  dfsEnv$edges <- loadDataEdges()

  output$DFSmap <- leaflet::renderLeaflet({
    leaflet::leaflet(data = loadDataStreets()) %>%
    leaflet::addTiles() %>%
    leaflet::setView(lng = 123.8854, lat = 10.3157, zoom = 12) %>%
    leaflet::clearShapes() %>%
    leaflet::addMarkers(lng = ~longitude, lat = ~latitude, popup = as.character(loadDataStreets()$street_name), clusterOptions = leaflet::markerClusterOptions())
  })

  shiny::observeEvent(input$possiblePaths, {
    dfsEnv$vis <- hash::hash()
    dfsEnv$path <- c()
    dfsEnv$allPaths <- list()
    street_nodes <- 34
    for (i in 1:street_nodes) {
      hash::.set(dfsEnv$vis, keys=i, values=FALSE)
    }

    dfs(loadDataByStreetName(input$locationSearchId)$street_id,loadDataByStreetName(input$destinationSearchId)$street_id)

    print(dfsEnv$allPaths)

    street_names <- c()
    for (i in 1:length(dfsEnv$allPaths)) {
      streets <- ""
      for (id in dfsEnv$allPaths[[i]]) {
        streets <- paste(streets, values = loadDataByStreetId(id)$street_name, sep = " ==>> ")
      }
      street_names <- append(x = street_names, values = streets)
    }

    shiny::callModule(module = possiblePathsModal, id = "possiblePathsModal")

    shiny::observe({
      shiny::updateSelectInput(session = session, inputId = "possiblePathsId", choices = street_names)
    })

    shiny::observeEvent(input$showMap, {
      street_ids <- c()
      splitTokens <- strsplit(x = input$possiblePathsId, split = " ==>> ")
      splitTokens[[1]] <- tail(x = splitTokens[[1]], n = -1)

      for (street in splitTokens[[1]]) {
        street_ids <- append(x = street_ids, values = loadDataByStreetName(street)$street_id)
      }

      curves <- openxlsx::readWorkbook(xlsxFile = "curves.xlsx")

      output$DFSmap <- leaflet::renderLeaflet({
        mapData <- leaflet::leaflet()
        mapData <- leaflet::addTiles(mapData)
        mapData <- leaflet::clearShapes(mapData)
        mapData <- leaflet::addMarkers(map = mapData, lng = loadDataByStreetName(input$locationSearchId)$longitude, lat = loadDataByStreetName(input$locationSearchId)$latitude, popup = input$locationSearchId)
        mapData <- leaflet::addMarkers(map = mapData, lng = loadDataByStreetName(input$destinationSearchId)$longitude, lat = loadDataByStreetName(input$destinationSearchId)$latitude, popup = input$destinationSearchId)

        for (i in 1:nrow(curves)) {
          for (j in 1:length(street_ids)) {
            if (!gtools::invalid(street_ids[j + 1])) {
              if (((curves[i, 2] == street_ids[j]) & (curves[i, 3] == street_ids[j + 1])) | ((curves[i, 2] == street_ids[j + 1]) & (curves[i, 3] == street_ids[j]))) {
                latlons <- strsplit(x = curves[i, 4], split = ":")
                unlist_latlons <- unlist(latlons)
                latlon <- strsplit(x = unlist_latlons, split = ",")
                withProgress(expr = {
                  for (k in 1:length(latlon)) {
                    incProgress(amount = 1/length(latlon))
                    Sys.sleep(0.25)
                    if (!gtools::invalid(latlon[k + 1])) {
                      mapData <- leaflet::addPolylines(map = mapData, lng = c(round(as.double(latlon[[k]][2]), digits = 6), round(as.double(latlon[[k + 1]][2]), digits = 6)), lat = c(round(as.double(latlon[[k]][1]), digits = 6), round(as.double(latlon[[k + 1]][1]), digits = 6)), stroke = TRUE, color = "black", weight = 5, opacity = 0.7, fill = FALSE, fillColor = "black", fillOpacity = 0.5, dashArray = NULL, smoothFactor = 1, noClip = TRUE, popup = input$possiblePathsId)
                    }
                  }
                }, value = 0, message = "Rendering Paths: ", detail = "Wait for a while...")
              }
            }
          }
        }

        mapData
      })
    })
  })

  shiny::observeEvent(input$importExcelFactorsId, {
    inFile <- input$excelFileUploadId

    if (is.null(inFile))
      return(NULL)

    sheetNameFile <- openxlsx::getSheetNames(file = inFile$datapath)

    # caution: no validation
    if (length(sheetNameFile) >= 1) {
      shiny::withProgress(expr = {
        for (s in sheetNameFile) {
          shiny::incProgress(amount = 1/length(sheetNameFile), detail = s)
          base::Sys.sleep(0.25)
          dataFactors <- openxlsx::readWorkbook(xlsxFile = inFile$datapath, sheet = s)
          shiny::callModule(module = saveImportedFactors, id = "saveImportedFactors", dataFactors, "factors")
        }
      }, value = 0, message = "Progessing: ")
    }

    output$factors <- DT::renderDataTable({
      loadDataFactors(input$routeName)
    }, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  })

  shiny::observeEvent(input$clearAllDataId, {
    shiny::callModule(module = deleteDataFactors, id = "deleteDataFactors", "factors")
    output$factors <- DT::renderDataTable({
      loadDataFactors(input$routeName)
    }, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  })
})

#' Retrieve data from streets from MySQL.
#'
#' @param tableStreets table name for streets.
#' @export
# load data from streets
loadDataStreets <- function() {
  data <- shiny::callModule(module = loadData, id = "loadData", "streets")
}

#' Retrieve data from streets from MySQL.
#'
#' @param tableEdges table name for edges.
#' @export
# load data from streets
loadDataEdges <- function() {
  data <- shiny::callModule(module = loadData, id = "loadData", "edges")
  data
}

#' Retrieve data from streets with street_id from MySQL.
#'
#' @param name name of the street.
#' @param tableStreets table name for streets.
#' @param columnName column name for streets.
#' @export
# load data from streets
loadDataByStreetId <- function(value) {
  data <- shiny::callModule(module = loadDataWhereCond, id = "loadDataWhereCond", "streets", "street_id", value)
  data
}

#' Retrieve data from streets with street_name from MySQL.
#'
#' @param name name of the street.
#' @param tableStreets table name for streets.
#' @export
# load data from streets
loadDataByStreetName <- function(value) {
  data <- shiny::callModule(module = loadDataWhereCond, id = "loadDataWhereCond", "streets", "street_name", value)
  data
}

#' Retrieve data from factors with given id for route in database.
#'
#' @param tableFactors table name for factors.
#' @param id route id from saving factors.
#' loadDataFactors(id)
loadDataFactors <- function(name) {
  value <- loadDataByStreetName(name)$street_id
  data <- shiny::callModule(module = loadDataWhereCond, id = "loadDataWhereCond", "factors", "street_id", value)
  data
}

#' Produce possible paths on the given location and destination.
#'
#' @param loc is the selected location by user.
#' @param des is the selected destination by user.
#' @export
#' dfs(location, destination)
dfs <- function (loc, des) {
  result <- c()

  if (loc == des) {
    pathSize = base::length(dfsEnv$path)
    for (i in 0:pathSize) {
      result <- base::append(x = result, values = dfsEnv$path[i])
    }
    result <- base::append(x = result, values = des)

    lastVectorOfAll <- (base::length(dfsEnv$allPaths) + 1)
    dfsEnv$allPaths[[lastVectorOfAll]] <- result
    return()
  }

  if (hash::values(dfsEnv$vis, loc)) {
    return()
  }

  hash::values(dfsEnv$vis, keys=loc) <- TRUE

  for (i in 1:base::length(dfsEnv$edges$edge_id)) {
    first <- dfsEnv$edges$start_vertex[i]
    second <- dfsEnv$edges$end_vertex[i]
    if (first == loc) {
      dfsEnv$path <- base::append(x = dfsEnv$path, values = first)
      dfs(second, des)
      dfsEnv$path <- utils::head(dfsEnv$path, -1)
    }
  }
}

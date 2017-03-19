shiny::shinyServer(function(input, output, session) {
  base::source("R/createRouteModal.R")
  base::source("R/createFactorsModal.R")
  base::source("R/deleteDataFactors.R")
  base::source("R/efficientPathModal.R")
  base::source("R/loadData.R")
  base::source("R/loadDataWhereCond.R")
  base::source("R/loadDataFactorsWhereStreetIdMonthDay.R")
  base::source("R/noPossiblePathsModal.R")
  base::source("R/possiblePathsModal.R")
  base::source("R/saveData.R")
  base::source("R/saveImportedFactors.R")
  base::source("R/thereIsAPossiblePathsModal.R")

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
      loadDataFactorsByStreetId(input$routeName)
    }, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  })

  output$factors <- DT::renderDataTable({
    loadDataFactorsByStreetId(input$routeName)
  }, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))

  shiny::observeEvent(input$createRoute, {
    shiny::callModule(module = createRouteModal, id = "createRouteModal", loadDataStreets()$street_name)
  })

  shiny::observeEvent(input$createFactors, {
    shiny::callModule(module = createFactorsModal, id = "createFactorsModal")
    shiny::observe({
      modal_routeId <- loadDataStreetsByStreetName(input$routeName)$street_id
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

    dfs(loadDataStreetsByStreetName(input$locationSearchId)$street_id,loadDataStreetsByStreetName(input$destinationSearchId)$street_id)

    street_names <- c()
    for (i in 1:length(dfsEnv$allPaths)) {
      streets <- ""
      for (id in dfsEnv$allPaths[[i]]) {
        streets <- paste(streets, values = loadDataStreetsByStreetId(id)$street_name, sep = " ==>> ")
      }
      street_names <- append(x = street_names, values = streets)
    }

    shiny::callModule(module = possiblePathsModal, id = "possiblePathsModal", street_names)
  })

  shiny::observeEvent(input$showMap, {
    street_ids <- c()
    splitTokens <- strsplit(x = input$possiblePathsId, split = " ==>> ")
    splitTokens[[1]] <- tail(x = splitTokens[[1]], n = -1)

    for (street in splitTokens[[1]]) {
      street_ids <- append(x = street_ids, values = loadDataStreetsByStreetName(street)$street_id)
    }

    curves <- openxlsx::readWorkbook(xlsxFile = "curves.xlsx")

    output$DFSmap <- leaflet::renderLeaflet({
      mapData <- leaflet::leaflet()
      mapData <- leaflet::addTiles(mapData)
      mapData <- leaflet::clearShapes(mapData)
      mapData <- leaflet::addMarkers(map = mapData, lng = loadDataStreetsByStreetName(input$locationSearchId)$longitude, lat = loadDataStreetsByStreetName(input$locationSearchId)$latitude, popup = input$locationSearchId)
      mapData <- leaflet::addMarkers(map = mapData, lng = loadDataStreetsByStreetName(input$destinationSearchId)$longitude, lat = loadDataStreetsByStreetName(input$destinationSearchId)$latitude, popup = input$destinationSearchId)

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
              }, style = "old", value = 0, message = "Rendering Paths: ", detail = "Wait for a while...")
            }
          }
        }
      }

      output$PredictionTab <- shinydashboard::renderMenu({
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem(text = "Prediction", icon = shiny::icon(name = "eye", class = "fa-1x", lib = "font-awesome"),
            shiny::actionButton(inputId = "predictEfficientPathId", label = "Efficient Path", icon = shiny::icon(name = "question-circle", class = "fa-1x", lib = "font-awesome"), width = "100%")
          )
        )
      })

      mapData
    })

      shiny::removeModal(session = getDefaultReactiveDomain())
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
      }, style = "old", value = 0, message = "Progessing: ")
    }

    output$factors <- DT::renderDataTable(expr = {
      loadDataFactorsByStreetId(input$routeName)
    }, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  })

  shiny::observeEvent(input$clearAllDataId, {
    shiny::callModule(module = deleteDataFactors, id = "deleteDataFactors", "factors")
    output$factors <- DT::renderDataTable({
      loadDataFactorsByStreetId(input$routeName)
    }, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  })

  shiny::observeEvent(input$visualPossiblePathsId, {
    if (is.null(dfsEnv$allPaths)) {
      shiny::callModule(module = noPossiblePathsModal, id = "noPossiblePathsModal")
    } else {
      street_names <- c()
      for (i in 1:length(dfsEnv$allPaths)) {
        streets <- ""
        for (id in dfsEnv$allPaths[[i]]) {
          streets <- paste(streets, values = loadDataStreetsByStreetId(id)$street_name, sep = " ==>> ")
        }
        street_names <- append(x = street_names, values = streets)
      }
      shiny::callModule(module = thereIsAPossiblePathsModal, id = "thereIsAPossiblePathsModal", street_names)

    }
  })

  shiny::observeEvent(input$predictEfficientPathId, {
    if (is.null(dfsEnv$allPaths)) {
      shiny::callModule(module = noPossiblePathsModal, id = "noPossiblePathsModal")
    } else {
      currentDate <- strsplit(x = as.character(Sys.Date()), split = "-")
      currentMonth <- currentDate[[1]][2]
      currentDay <- currentDate[[1]][3]

      PathsToPredict <- list()
      trainingFactors <- data.frame()
      for (path in dfsEnv$allPaths) {
        for (id in path) {
          trainingFactors <- rbind(trainingFactors, loadDataFactorsByStreetIdMonthDay(id, as.numeric(currentMonth), as.numeric(currentDay)))
        }

        for (i in 1:length(trainingFactors$lanes)) {
          if (trainingFactors$lanes[i] == 3) {
            trainingFactors$lanes[i] <- 1
          } else if (trainingFactors$lanes[i] == 1) {
            trainingFactors$lanes[i] <- 3
          }
        }

        trainingFactorsMatrix <- as.matrix(x = trainingFactors)

        trainingFactorsMatrix <- cbind(trainingFactorsMatrix, 0)
        colnames(trainingFactorsMatrix)[7] <- "binary"

        # PathsToPredict <- list(PathsToPredict, trainingFactorsMatrix)
        lastPathsToPredict <- (base::length(PathsToPredict) + 1)

        PathsToPredict[[lastPathsToPredict]] <- trainingFactorsMatrix
      }

      newList <- list()
      for (i in 1:length(PathsToPredict)) {
        nn1 <- neuralnet::neuralnet(formula = binary~day+month+vehicles+lanes+zones+events,
          data = PathsToPredict[[i]], hidden = 2, learningrate = 0.01,
          algorithm = "backprop", err.fct = "sse", linear.output = FALSE)
        lastnewList <- (base::length(newList) + 1)
        # pred <- neuralnet::prediction(x = nn1)
        # print(pred)
        newList[[lastnewList]] <- nn1
      }
      # print(newList[[1]]$net.result[[1]])
      # print(sum(newList[[1]]$net.result[[1]]))
      max <- 0.000000000000
      index <- 0
      for (i in 1:length(newList)) {
        nnResult <- sum(newList[[i]]$net.result[[1]])
        # print(sum(newList[[i]]$net.result[[1]]))
        if (nnResult > max) {
          max <- nnResult
          index <- i
        }
      }

      # print(index)

      streets <- ""
      street_names <- c()
      for (id in dfsEnv$allPaths[[index]]) {
        streets <- paste(streets, values = loadDataStreetsByStreetId(id)$street_name, sep = " ==>> ")
      }
      street_names <- append(x = street_names, values = streets)

      output$text1 <- renderText({
        street_names
      })

      shiny::callModule(module = efficientPathModal, id = "efficientPathModal")
    }
  })

  shiny::observeEvent(input$showPlot, {
    street_ids <- c()
    splitTokens <- strsplit(x = input$plotPossiblePathId, split = " ==>> ")
    splitTokens[[1]] <- tail(x = splitTokens[[1]], n = -1)
    sampleFactors <- data.frame()
    for (street in splitTokens[[1]]) {
      street_ids <- append(x = street_ids, values = loadDataStreetsByStreetName(street)$street_id)
      sampleFactors <- rbind(sampleFactors, loadDataFactorsByStreetId(street))
    }

    currentDate <- strsplit(x = as.character(Sys.Date()), split = "-")
    currentMonth <- currentDate[[1]][2]
    currentDay <- currentDate[[1]][3]

    trainingFactors <- data.frame()
    for (id in street_ids) {
      trainingFactors <- rbind(trainingFactors, loadDataFactorsByStreetIdMonthDay(id, as.numeric(currentMonth), as.numeric(currentDay)))
    }
    sampleFactorsMatrix <- as.matrix(x = sampleFactors)
    trainingFactorsMatrix <- as.matrix(x = trainingFactors)


    # print(trainingFactorsMatrix)
    trainingFactorsMatrix <- cbind(trainingFactorsMatrix, 0)
    colnames(trainingFactorsMatrix)[7] <- "binary"
    nn <- neuralnet::neuralnet(formula = binary~day+month+vehicles+lanes+zones+events,
      data = trainingFactorsMatrix, hidden = 2, learningrate = 0.01,
      algorithm = "backprop", err.fct = "sse", linear.output = FALSE)
    # print(trainingFactorsMatrix)
    # print(nn)
    plot(nn)

    output$plotVehicles <- shiny::renderPlot(expr = {
      neuralnet::gwplot(nn, selected.covariate="vehicles")
    })

    output$plotLanes <- shiny::renderPlot(expr = {
      neuralnet::gwplot(nn, selected.covariate="lanes")
    })

    output$plotZones <- shiny::renderPlot(expr = {
      neuralnet::gwplot(nn, selected.covariate="zones")
    })

    output$plotEvents <- shiny::renderPlot(expr = {
      neuralnet::gwplot(nn, selected.covariate="events")
    })

    shiny::removeModal(session = getDefaultReactiveDomain())
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

#' Retrieve data from streets from MySQL.
#'
#' @param tableEdges table name for edges.
#' @export
# load data from streets
loadDataFactors <- function() {
  data <- shiny::callModule(module = loadData, id = "loadData", "factors")
  data
}

#' Retrieve data from streets with street_id from MySQL.
#'
#' @param name name of the street.
#' @param tableStreets table name for streets.
#' @param columnName column name for streets.
#' @export
# load data from streets
loadDataStreetsByStreetId <- function(value) {
  data <- shiny::callModule(module = loadDataWhereCond, id = "loadDataWhereCond", "streets", "street_id", value)
  data
}

#' Retrieve data from streets with street_name from MySQL.
#'
#' @param name name of the street.
#' @param tableStreets table name for streets.
#' @export
# load data from streets
loadDataStreetsByStreetName <- function(value) {
  data <- shiny::callModule(module = loadDataWhereCond, id = "loadDataWhereCond", "streets", "street_name", value)
  data
}

#' Retrieve data from factors with given street id for street in database.
#'
#' @param tableFactors table name for factors.
#' @param id street id from saving factors.
#' loadDataFactorsByStreetId(id)
loadDataFactorsByStreetId <- function(name) {
  value <- loadDataStreetsByStreetName(name)$street_id
  data <- shiny::callModule(module = loadDataWhereCond, id = "loadDataWhereCond", "factors", "street_id", value)
  data
}

#' Retrieve data from factors with given street id, month, day in database.
#'
#' @param tableFactors table name for factors.
#' @param id street id from saving factors.
#' @param month month of factors.
#' @param day day of factors
#' loadDataFactorsByStreetIdMonthDay(id, month, day)
loadDataFactorsByStreetIdMonthDay <- function(id, month, day) {
  data <- shiny::callModule(module = loadDataFactorsWhereStreetIdMonthDay, id = "loadDataFactorsWhereStreetIdMonthDay", "factors", "street_id", "month", "day", id, month, day)
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

#' write function that takes in the data frame, learning rate - eta,
#' and number of epochs - n.iter and updates the weight factor.
#' At this stage, I am only conserned with the final weight and
#' the number of epochs required for the weight to converge
#'
#' @param x is data set.
#' @param y is binary labels.
#' @param eta is learning rate.
#' @param niter is number of iterations(epoch).
#'
#' @export
#' perceptron(x, y, eta, niter)
perceptron <- function(x, y, eta, niter) {

  # initialize weight vector
  weight <- rep(0, dim(x)[2] + 1)
  errors <- rep(0, niter)


  # loop over number of epochs niter
  for (jj in 1:niter) {

    # loop through training data set
    for (ii in 1:length(y)) {

      # Predict binary label using Heaviside activation
      # function
      z <- sum(weight[2:length(weight)] *
                 as.numeric(x[ii, ])) + weight[1]
      if(z < 0) {
        ypred <- -1
      } else {
        ypred <- 1
      }

      # Change weight - the formula doesn't do anything
      # if the predicted value is correct
      weightdiff <- eta * (y[ii] - ypred) *
        c(1, as.numeric(x[ii, ]))
      weight <- weight + weightdiff

      # Update error function
      if ((y[ii] - ypred) != 0.0) {
        errors[jj] <- errors[jj] + 1
      }

    }
  }

  # weight to decide between the two species
  # print(weight)
  # return(errors)
  print(errors)
  return(weight)
}

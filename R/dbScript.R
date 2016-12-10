#' Creates a default database and tables that are needed for Navify in mysql.
#'
#' @examples
#' dbScript()
#' @export
options(mysql = list(
  "host" = "127.0.0.1",
  "port" = 3306,
  "user" = "root",
  "password" = ""
))

# Select database navifactors
SelectDatabase <- function() {
  db <- RMySQL::dbConnect(RMySQL::MySQL(), host = options()$mysql$host,
                          port = options()$mysql$port, user = options()$mysql$user,
                          password = options()$mysql$password)
  query <- sprintf(
    "SHOW SCHEMAS LIKE 'navifactors';",
    databaseName
  )
  res <- DBI::dbSendQuery(db, query)
  DBI::dbFetch(res, n = -1)
  RMySQL::dbDisconnect(db)
}
SelectDatabase()

# Create database navifactors
databaseName <- "navifactors"
createDatabase <- function() {
  db <- RMySQL::dbConnect(RMySQL::MySQL(), host = options()$mysql$host,
                  port = options()$mysql$port, user = options()$mysql$user,
                  password = options()$mysql$password)
  query <- sprintf(
    "CREATE DATABASE IF NOT EXISTS %s;",
    databaseName
  )
  DBI::dbGetQuery(db, query)
  query <- sprintf(
    "USE %s;",
    databaseName
  )
  DBI::dbGetQuery(db, query)
  RMySQL::dbDisconnect(db)
}
createDatabase()

# Create table streets
createTableStreets <- function() {
  db <- RMySQL::dbConnect(RMySQL::MySQL(), dbname = databaseName, host = options()$mysql$host,
                          port = options()$mysql$port, user = options()$mysql$user,
                          password = options()$mysql$password)
  query <- sprintf("CREATE TABLE %s.streets (street_id INT AUTO_INCREMENT PRIMARY KEY,
                   street_name VARCHAR(100), longitude DOUBLE, latitude DOUBLE);",
                   databaseName)
  DBI::dbGetQuery(db, query)
  RMySQL::dbDisconnect(db)
}
createTableStreets()

# Create table edges
createTableEdges <- function() {
  db <- RMySQL::dbConnect(RMySQL::MySQL(), dbname = databaseName, host = options()$mysql$host,
                          port = options()$mysql$port, user = options()$mysql$user,
                          password = options()$mysql$password)
  query <- sprintf("CREATE TABLE %s.edges (edge_id INT AUTO_INCREMENT PRIMARY KEY,
                   start_vertex INT, end_vertex INT);",
                   databaseName)
  DBI::dbGetQuery(db, query)
  RMySQL::dbDisconnect(db)
}
createTableEdges()

# Create table edges
createTableRoutes<- function() {
  db <- RMySQL::dbConnect(RMySQL::MySQL(), dbname = databaseName, host = options()$mysql$host,
                          port = options()$mysql$port, user = options()$mysql$user,
                          password = options()$mysql$password)
  query <- sprintf("CREATE TABLE %s.routes (route_id INT AUTO_INCREMENT PRIMARY KEY,
                   location VARCHAR(100), destination VARCHAR(100));",
                   databaseName)
  DBI::dbGetQuery(db, query)
  RMySQL::dbDisconnect(db)
}
createTableRoutes()

# Create table factors
createTableFactors<- function() {
  db <- RMySQL::dbConnect(RMySQL::MySQL(), dbname = databaseName, host = options()$mysql$host,
                          port = options()$mysql$port, user = options()$mysql$user,
                          password = options()$mysql$password)
  query <- sprintf("CREATE TABLE %s.factors (factors_id INT AUTO_INCREMENT PRIMARY KEY,
                   route_id INT, time INT, day INT, cars INT, lanes INT, zones INT, events INT,
                   weather INT, distance INT, FOREIGN KEY (route_id) REFERENCES routes(route_id));",
                   databaseName)
  DBI::dbGetQuery(db, query)
  RMySQL::dbDisconnect(db)
}
createTableFactors()

workspace <- 'C:\\Users\\Adam\\Documents\\CIT-U\\Masters of Computer Science\\Capstone - Thesis\\Navify - Traffixer\\Navify\\'
setwd(workspace)
# Inserting data in table streets
insertStreets <- function() {
  streets <- openxlsx::readWorkbook(xlsxFile = "streets.xlsx")
  db <- RMySQL::dbConnect(RMySQL::MySQL(), dbname = databaseName, host = options()$mysql$host,
                          port = options()$mysql$port, user = options()$mysql$user,
                          password = options()$mysql$password)
  for (i in 1:nrow(streets)) {
    query <- sprintf("INSERT INTO navifactors.streets (street_name, longitude, latitude)
                     VALUES ('%s', %f, %f);",
                     streets[i, 2],
                     streets[i, 3],
                     streets[i, 4])
    DBI::dbGetQuery(db, query)
  }
  RMySQL::dbDisconnect(db)
}
insertStreets()

# Inserting data in table edges
insertEdges <- function() {
  edges <- openxlsx::readWorkbook(xlsxFile = "edges.xlsx")
  db <- RMySQL::dbConnect(RMySQL::MySQL(), dbname = databaseName, host = options()$mysql$host,
                          port = options()$mysql$port, user = options()$mysql$user,
                          password = options()$mysql$password)
  for (i in 1:nrow(edges)) {
    query <- sprintf("INSERT INTO navifactors.edges (start_vertex, end_vertex)
                     VALUES (%d, %d);",
                     edges[i, 2],
                     edges[i, 3])
    DBI::dbGetQuery(db, query)
  }
  RMySQL::dbDisconnect(db)
}
insertEdges()

# Select data from table streets
selectStreets <- function() {
  db <- RMySQL::dbConnect(RMySQL::MySQL(), dbname = databaseName, host = options()$mysql$host,
                          port = options()$mysql$port, user = options()$mysql$user,
                          password = options()$mysql$password)
  query <- sprintf("SELECT * FROM navifactors.streets")
  data <- DBI::dbGetQuery(db, query)
  RMySQL::dbDisconnect(db)
  data$street_name
}
selectStreets()

# Select data with condition from table streets
selectStreetsByName <- function() {
  tableStreets <- "streets"
  cond <- "CSR"
  db <- RMySQL::dbConnect(RMySQL::MySQL(), dbname = databaseName, host = options()$mysql$host,
                          port = options()$mysql$port, user = options()$mysql$user,
                          password = options()$mysql$password)

  # Construct the fetching query
  query <-
    sprintf("SELECT * FROM %s WHERE street_name = '%s'", tableStreets, cond)
  # Submit the fetch query and disconnect
  data <- DBI::dbGetQuery(db, query)
  RMySQL::dbDisconnect(db)
  data$latitude
}
as.double(selectStreetsByName())

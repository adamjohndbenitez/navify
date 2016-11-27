base::source("R/databaseConnection.R")
#' Retrieve function that simply load route with condition from MySQL.
#'
#' @export
loadDataWhereCond <- function(input, output, session, name, table) {
  db <- shiny::callModule(databaseConnection, "databaseConnection")
  # Construct the fetching query
  query <-
    sprintf("SELECT * FROM %s WHERE street_name = \"%s\"", table, name)
  # Submit the fetch query and disconnect
  data <- DBI::dbGetQuery(db, query)
  RMySQL::dbDisconnect(db)
  data
}

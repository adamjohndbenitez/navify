base::source("R/databaseConnection.R")
#' Retrieve function that simply load route from MySQL.
#'
#' @export
loadDataById <- function(input, output, session, id, table) {
  db <- shiny::callModule(databaseConnection, "databaseConnection")

  # Construct the fetching query
  query <-
    sprintf("SELECT * FROM %s WHERE street_id = '%s'", table, id)
  # Submit the fetch query and disconnect
  data <- DBI::dbGetQuery(db, query)
  RMySQL::dbDisconnect(db)
  data
}

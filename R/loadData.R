base::source("R/databaseConnection.R")
#' Retrieve function that simply load responses from MySQL.
#'
#' @export
loadData <- function(input, output, session, table) {
  db <- shiny::callModule(databaseConnection, "databaseConnection")

  # Construct the fetching query
  query <- sprintf("SELECT * FROM %s", table)
  # Submit the fetch query and disconnect
  data <- DBI::dbGetQuery(db, query)
  RMySQL::dbDisconnect(db)
  data
}

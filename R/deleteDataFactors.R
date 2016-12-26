base::source("R/databaseConnection.R")
#' Delete function that simply clear all factors from MySQL.
#'
#' @export
deleteDataFactors <- function(input, output, session, table) {
  dbConnect <- shiny::callModule(module = databaseConnection, id = "databaseConnection")
  query <- sprintf("TRUNCATE TABLE navifactors.%s", table)
  data <- DBI::dbGetQuery(conn = dbConnect, statement = query)
  RMySQL::dbDisconnect(conn = dbConnect)
  data
}

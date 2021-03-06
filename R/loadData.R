base::source("R/databaseConnection.R")
#' Retrieve function that simply load streets from MySQL.
#'
#' @param table is the name of the table.
#' @export
#' loadData(tableName)
loadData <- function(input, output, session, table) {
  dbConnect <- shiny::callModule(module = databaseConnection, id = "databaseConnection")
  query <- sprintf("SELECT * FROM %s", table)
  data <- DBI::dbGetQuery(conn = dbConnect, statement = query)
  RMySQL::dbDisconnect(conn = dbConnect)
  data
}

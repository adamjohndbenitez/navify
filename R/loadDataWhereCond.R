base::source("R/databaseConnection.R")
#' Retrieve function that simply load route with condition from MySQL.
#'
#' @param table is the name of the table.
#' @param column is the name of the column of the condition.
#' @param value is the value of the specified column.
#' @export
#' loadDataWhereCond(tableName, columnName, rowValue)
loadDataWhereCond <- function(input, output, session, tableName, columnName, rowValue) {
  dbConnect <- shiny::callModule(module = databaseConnection, id = "databaseConnection")
  query <- sprintf("SELECT * FROM %s WHERE %s = '%s'", tableName, columnName, rowValue)
  data <- DBI::dbGetQuery(conn = dbConnect, statement = query)
  RMySQL::dbDisconnect(conn = dbConnect)
  data
}

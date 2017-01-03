base::source("R/databaseConnection.R")
#' Retrieve function that load factors with street id, month, day from MySQL.
#'
#' @param table is the name of the table.
#' @param column is the name of the column of the condition.
#' @param value is the value of the specified column.
#' @export
#' loadDataWhereCond(tableName, columnName, rowValue)
loadDataFactorsWhereStreetIdMonthDay <- function(input, output, session, tableName, columnStreetId, columnMonth, columnDay, rowStreetId, rowMonth, rowDay) {
  dbConnect <- shiny::callModule(module = databaseConnection, id = "databaseConnection")
  query <- sprintf("SELECT day, month, vehicles, lanes, zones, events FROM %s WHERE %s = %s AND %s = %s AND %s = %s", tableName, columnStreetId, rowStreetId, columnMonth, rowMonth, columnDay, rowDay)
  data <- DBI::dbGetQuery(conn = dbConnect, statement = query)
  RMySQL::dbDisconnect(conn = dbConnect)
  data
}

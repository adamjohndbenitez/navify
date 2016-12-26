base::source("R/databaseConnection.R")
#' Save function that simply stores data in MySQL.
#'
#' @param data inputs from user.
#' @export
#' saveData(dataForm(), tableName)
saveData <- function(input, output, session, rowValue, tableName) {
  dbConnect <- shiny::callModule(module = databaseConnection, id = "databaseConnection")
  query <- sprintf("INSERT INTO %s (%s) VALUES ('%s')", tableName, paste(names(rowValue), collapse = ", "), paste(rowValue, collapse = "', '"))
  DBI::dbGetQuery(conn = dbConnect, statement = query)
  RMySQL::dbDisconnect(conn = dbConnect)
}

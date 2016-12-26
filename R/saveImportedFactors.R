base::source("R/databaseConnection.R")
#' Save function that stores factors from imported excel in MySQL.
#'
#' @param data inputs from user.
#' @examples
#' saveData(data)
#' @export
saveImportedFactors <- function(input, output, session, rowValue, tableName) {
  dbConnect <- shiny::callModule(module = databaseConnection, id = "databaseConnection")
  for (r in 1:nrow(rowValue)) {
    query <- sprintf("INSERT INTO %s (%s) VALUES ('%s')", tableName, paste(names(rowValue), collapse = ", "), paste(rowValue[r,], collapse = "', '"))
    DBI::dbGetQuery(conn = dbConnect, statement = query)
  }
  DBI::dbGetQuery(conn = dbConnect, statement = query)
  RMySQL::dbDisconnect(conn = dbConnect)
}

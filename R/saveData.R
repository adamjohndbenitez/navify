base::source("R/databaseConnection.R")
#' Save function that simply stores responses in MySQL.
#'
#' @param data inputs from user.
#' @examples
#' saveData(data)
#' @export
saveData <- function(input, output, session, data, table) {
  db <- shiny::callModule(databaseConnection, "databaseConnection")

  # Construct the update query by looping over the data fields
  query <- sprintf(
    "INSERT INTO %s (%s) VALUES ('%s')",
    table,
    paste(names(data), collapse = ", "),
    paste(data, collapse = "', '")
  )
  # Submit the update query and disconnect
  DBI::dbGetQuery(db, query)
  RMySQL::dbDisconnect(db)
}

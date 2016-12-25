base::source("R/databaseConnection.R")
#' Save function that stores factors in MySQL.
#'
#' @param data inputs from user.
#' @examples
#' saveData(data)
#' @export
saveImportedFactors <- function(input, output, session, data, table) {
  db <- shiny::callModule(databaseConnection, "databaseConnection")

  # Construct the update query by looping over the data fields
  for (r in 1:nrow(data)) {
    query <- sprintf(
      "INSERT INTO %s (%s) VALUES ('%s')",
      table,
      paste(names(data), collapse = ", "),
      paste(data[r,], collapse = "', '")
    )
    # Submit the update query and disconnect
    DBI::dbGetQuery(db, query)
  }

  # Submit the update query and disconnect
  DBI::dbGetQuery(db, query)
  RMySQL::dbDisconnect(db)
}

#' To connect to a MySQL database
#'
#' @param host
#' @param port
#' @param dbname
#' @param user
#' @param password
#' @export
databaseConnection <- function(input, output, session) {
  databaseNavify <- "navifactors"
  RMySQL::dbConnect(
    RMySQL::MySQL(),
    dbname = databaseNavify,
    host = options()$mysql$host,
    port = options()$mysql$port,
    user = options()$mysql$user,
    password = options()$mysql$password
  )
}


#' To connect to a MySQL database
#'
#' @param host Where the server is running. The default value is \emph{localhost}.
#' @param port A port number to use for the connection, for connections made using \strong{TCP/IP}.
#' @param dbname database name
#' @param user A mysql account grant privileges to manipulate table
#' @param password for security purposes.
#' @export
databaseConnection <- function(input, output, session) {
  RMySQL::dbConnect(
    RMySQL::MySQL(),
    dbname = databaseNavify,
    host = options()$mysql$host,
    port = options()$mysql$port,
    user = options()$mysql$user,
    password = options()$mysql$password
  )
}


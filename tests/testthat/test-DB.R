#' Type console to test package:
#' devtools::test(pkg =
#' "CIT-U/Masters of Computer Science/Capstone - Thesis/Navify - Traffixer/Navify/tests/testthat")
#'
testthat::context("DB connection")

testthat::test_that("Test DB Disconnection", {
  db <-
    RMySQL::dbConnect(
      RMySQL::MySQL(),
      dbname = databaseNavify,
      host = options()$mysql$host,
      port = options()$mysql$port,
      user = options()$mysql$user,
      password = options()$mysql$password
    )
  testthat::expect_true(object = RMySQL::dbDisconnect(db), info = "create database to make test successful.")
})

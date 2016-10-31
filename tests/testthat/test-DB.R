#' Type console to test package:
#' devtools::test(pkg =
#' "CIT-U/Masters of Computer Science/Capstone - Thesis/Navify - Traffixer/Navify/tests/testthat")
#'
context("DB connection")

test_that("Test DB Disconnection", {
  db <-
    dbConnect(
      MySQL(),
      dbname = databaseNavify,
      host = options()$mysql$host,
      port = options()$mysql$port,
      user = options()$mysql$user,
      password = options()$mysql$password
    )
  expect_true(dbDisconnect(db), TRUE)
})

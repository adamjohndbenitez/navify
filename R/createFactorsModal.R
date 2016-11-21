createFactorsModal <- function(input, output, session) {
    # get the value of the routeId from selected routId to modal create factors
    showModal(modalDialog(
      title = "Create Factors",
      footer = tagList(
        actionButton("saveFactors", "saveFactors"),
        modalButton("Cancel")
      ),
      size = "l",
      easyClose = FALSE,
      # modal to create factors
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::column(width = 6, shiny::textOutput("locationText")),
          shiny::column(width = 6, shiny::textOutput("DestinationText"))
        ),
        shiny::column(
          width = 12,
          shiny::column(
            width = 3,
            shiny::selectInput(
              inputId = "time",
              label = "Time:",
              choices = list(
                "6 AM" = 6,
                "7 AM" = 7,
                "8 AM" = 8,
                "9 AM" = 9,
                "10 AM" = 10,
                "11 AM" = 11,
                "12 PM" = 12,
                "1 PM" = 13,
                "2 PM" = 14,
                "3 PM" = 15,
                "4 PM" = 16,
                "5 PM" = 17,
                "6 PM" = 18,
                "7 PM" = 19,
                "8 PM" = 20,
                "9 PM" = 21,
                "10 PM" = 22,
                "11 PM" = 23,
                "12 AM" = 24,
                "1 AM" = 1,
                "2 AM" = 2,
                "3 AM" = 3,
                "4 AM" = 4,
                "5 AM" = 5
              ),
              selected = 1
            ),
            selectize = TRUE
          ),
          shiny::column(
            width = 3,
            shiny::selectInput(
              inputId = "day",
              label = "Day:",
              choices = list(
                "Monday" = 1,
                "Tuesday" = 2,
                "Wednesday" = 3,
                "Thursday" = 4,
                "Friday" = 5,
                "Saturday" = 6,
                "Sunday" = 7
              ),
              selected = 1
            )
          ),
          shiny::column(
            width = 3,
            shiny::numericInput(
              inputId = "cars",
              label = "Cars:",
              value = 200,
              min = 1,
              max = 1000
            )
          ),
          shiny::column(
            width = 3,
            shiny::numericInput(
              inputId = "lanes",
              label = "Lane:",
              value = 3,
              min = 1,
              max = 3
            )
          )
        ),
        shiny::column(
          width = 12,
          shiny::column(
            width = 3,
            shiny::selectInput(
              inputId = "damage",
              label = "Damage:",
              choices = list(
                "None" = 0,
                "Patholes" = 1,
                "Muddy Road" = 2,
                "Construction" = 3
              ),
              selected = 1
            )
          ),
          shiny::column(
            width = 3,
            shiny::selectInput(
              inputId = "zones",
              label = "Zone:",
              choices = list(
                "None" = 0,
                "Schools" = 1,
                "Mall" = 2,
                "Call Centers" = 3
              ),
              selected = 1
            )
          ),
          shiny::column(
            width = 3,
            shiny::numericInput(
              inputId = "distance",
              label = "Distance:",
              value = 200,
              min = 1,
              max = 1000
            )
          ),
          shiny::column(
            width = 3,
            shiny::selectInput(
              inputId = "weather",
              label = "Weather:",
              choices = list(
                "Sunny" = 0,
                "Windy" = 1,
                "Heavy Rain" = 2,
                "Storm" = 3
              ),
              selected = 1
            )
          )
        ),
        shiny::column(
          width = 12,
          shiny::column(
            width = 3,
            shiny::selectInput(
              inputId = "events",
              label = "Events:",
              choices = list(
                "None" = 0,
                "Sinulog" = 1,
                "Fiesta" = 2,
                "Ironman" = 3
              ),
              selected = 1
            )
          ),
          shiny::column(
            width = 3,
            shiny::numericInput(inputId = "route_id",
              label = "Route Id:",
              min = 1,
              max = 20,
              value = 15
            )
          )
        )
      )
    ))
}

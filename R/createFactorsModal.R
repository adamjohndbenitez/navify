createFactorsModal <- function(input, output, session) {
  shiny::showModal(
    shiny::modalDialog(title =
      shiny::tagList(shiny::icon(name = "plus-square-o", class = "fa-1x", lib = "font-awesome"), "Create Factors"),
      footer = shiny::tagList(
        actionButton(inputId = "saveFactors", label = "Save Factors", icon = shiny::icon(name = "floppy-o", class = "fa-1x", lib = "font-awesome")),
        modalButton(label = "Cancel", icon = shiny::icon(name = "ban", class = "fa-1x", lib = "font-awesome"))
      ), size = "l", easyClose = FALSE,
      shiny::fluidRow(
        shiny::column(width = 12,
          shiny::column(width = 4,
            shinyjs::disabled(
              shiny::textInput(inputId = "street_id", label = "Route Id:", width = "100%", value = "")
            )
          )
        ),
        shiny::column(width = 12,
          shiny::column(width = 4,
            shiny::selectInput(inputId = "month", label = "Month:",
              choices = list(
                "January" = 1,
                "February" = 2,
                "March" = 3,
                "April" = 4,
                "May" = 5,
                "June" = 6,
                "July" = 7,
                "June" = 8,
                "September" = 9,
                "October" = 10,
                "November" = 11,
                "December" = 12
              ), selected = 1, selectize = TRUE
            )
          ),
          shiny::column(width = 4,
            shiny::selectInput(inputId = "day", label = "Day:", choices = 1:31, selected = 1)
          ),
          shiny::column(width = 4,
            shiny::numericInput(inputId = "vehicles", label = "Vehicles:", value = 200, min = 1, max = 1000)
          )
        ),
        shiny::column(width = 12,
          shiny::column(width = 4,
            shiny::selectInput(inputId = "lanes", label = "Lanes:",
              choices = list(
                "1 lane" = 1,
                "2 lanes" = 2,
                "3 lanes" = 3
              ), selected = 1, selectize = TRUE
            )
          ),
          shiny::column(width = 4,
            shiny::selectInput(inputId = "zones", label = "Zone:",
              choices = list(
                "None" = 0,
                "Schools" = 1,
                "Malls" = 2,
                "Schools & Malls" = 3
              ), selected = 0
            )
          ),
          shiny::column(width = 4,
            shiny::selectInput(inputId = "events", label = "Events:",
              choices = list(
                "None" = 0,
                "Fun Run" = 1,
                "Novena Mass" = 2,
                "Grand Parade" = 3
              ), selected = 0
            )
          )
        )
      )
    )
  )
}

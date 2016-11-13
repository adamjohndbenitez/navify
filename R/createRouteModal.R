createRouteModal <- function(input, output, session, streetsData) {
  shiny::showModal(shiny::modalDialog(
    title = "Create Route",
    footer = shiny::tagList(
      shiny::actionButton("saveRoute", "saveRoute"),
      shiny::modalButton("Cancel")
    ),
    size = "m",
    easyClose = FALSE,
    shiny::fluidRow(shiny::column(
      width = 12,
      shiny::column(
        width = 6,
        # Select location
        shiny::selectInput(
          inputId = "location",
          label = "Select Location",
          choices = streetsData()$name
        )
      ),
      shiny::column(
        width = 6,
        # Select destination
        shiny::selectInput(
          inputId = "destination",
          label = "Select Destination",
          choices = streetsData()$name
        )
      )
    ))
  ))
}

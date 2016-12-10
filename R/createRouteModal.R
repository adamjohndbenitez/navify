createRouteModal <- function(input, output, session, loadDataStreets) {
  shiny::showModal(shiny::modalDialog(
    title = "Create Route",
    footer = shiny::tagList(
      shiny::actionButton(inputId = "saveRoute", label = "Save Route"),
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
          choices = loadDataStreets
        )
      ),
      shiny::column(
        width = 6,
        # Select destination
        shiny::selectInput(
          inputId = "destination",
          label = "Select Destination",
          choices = loadDataStreets
        )
      )
    ))
  ))
}

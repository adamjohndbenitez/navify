possiblePathsModal <- function(input, output, session, street_names) {
  shiny::observe({
    shiny::updateSelectInput(session = session, inputId = "possiblePathsId", choices = "")
  })
  shiny::showModal(
    shiny::modalDialog(title = "Possible Paths",
      footer = shiny::tagList(
        shiny::actionButton(inputId = "showMap", label = "Show in Map"),
        shiny::modalButton("Close")
      ), size = "l", easyClose = FALSE,
      shiny::fluidRow(
        shiny::column(width = 12,
          shiny::column(width = 12,
            shiny::selectInput(inputId = "possiblePathsId", label = "Possible Paths:", choices = street_names, width = "100%")
          )
        )
      )
    )
  )
}

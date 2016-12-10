possiblePathsModal <- function(input, output, session, paths) {
  observe({
    # Show data from loadData selecting street table to selectInput
    updateSelectInput(session = session,
                      inputId = "possiblePathsId",
                      choices = paths)
  })
  shiny::showModal(shiny::modalDialog(
    title = "Possible Paths",
    footer = shiny::tagList(
      shiny::actionButton(inputId = "showMap", label = "Show Map"),
      shiny::modalButton("Cancel")
    ),
    size = "s",
    easyClose = FALSE,
    shiny::fluidRow(shiny::column(width = 12,
      shiny::column(width = 7,
        shiny::selectInput(
          inputId = "possiblePathsId",
          label = "Possible Paths:",
          ""
        )
      )
    ))
  ))
  print(paths)
}

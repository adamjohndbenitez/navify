noPossiblePathsModal <- function(input, output, session) {
  shiny::showModal(
    shiny::modalDialog(title = shiny::tagList(shiny::icon(name = "exclamation-triangle", class = "fa-1x", lib = "font-awesome"), "Warning"),
      footer = shiny::modalButton("Close"), size = "s", easyClose = FALSE,
      shiny::fluidRow(
        shiny::column(width = 12,
          shiny::helpText("No possible paths available."),
          shiny::helpText(
            shiny::em("Please select from first tab.")
          )
        )
      )
    )
  )
}

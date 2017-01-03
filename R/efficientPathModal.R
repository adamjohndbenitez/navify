efficientPathModal <- function(input, output, session, street) {
  shiny::showModal(
    shiny::modalDialog(title = "Possible Paths",
      footer = shiny::tagList(
        shiny::modalButton("Close")
      ), size = "l", easyClose = FALSE,
      shiny::fluidRow(
        shiny::column(width = 12,
          shiny::textOutput(outputId = "text1")
        )
      )
    )
  )
}

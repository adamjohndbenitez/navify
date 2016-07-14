
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)

streets <- read.csv(file = "C:\\Users\\Adam\\Documents\\CIT-U\\Masters of Computer Science\\Capstone - Thesis\\Navify - Traffixer\\Navify\\street.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

shinyUI(fluidPage(
  dashboardPage(
    dashboardHeader(title = "Navify"),
    dashboardSidebar(
      # Select location
      selectInput(inputId = "locationId", label = "Select Location",
                  choices = streets$name),

      # Select destination
      selectInput(inputId = "destinationId", label = "Select Destination",
                  choices = streets$name)
    ),
    dashboardBody()
  )
))

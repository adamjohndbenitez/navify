
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
                  choices = streets$name),

      sidebarMenu(
        # Menu Bar for data factors
        menuItem("DataFactors", tabName = "dataFactors", icon = icon("road")),
        # Samplw Menu Bar for mysql
        menuItem("MySQLExample", tabName = "mySQLExample", icon = icon("stop"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "dataFactors",
          fluidRow(
            h2("WIP")
          )
        ),
        tabItem(tabName = "mySQLExample",
          fluidRow(
            DT::dataTableOutput("responses", width = 300), tags$hr(),
            textInput("name", "Name", ""),
            #checkboxInput("used_shiny", "I've built a Shiny app in R before", FALSE),
            sliderInput("r_num_years", "Number of years using R", 0, 25, 2, ticks = FALSE),
            actionButton("submit", "Submit")
          )
        )
      )
    )
  )
))

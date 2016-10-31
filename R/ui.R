# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(leaflet)
library(shiny)
library(shinyBS)
library(shinydashboard)
library(RMySQL)

streets <-
  read.csv(
    file = "C:\\Users\\Adam\\Documents\\CIT-U\\Masters of Computer Science\\Capstone - Thesis\\Navify - Traffixer\\Navify\\street.csv",
    header = TRUE,
    sep = ",",
    stringsAsFactors = FALSE
  )

shiny::shinyUI(fluidPage(
  shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "Navify"),
    shinydashboard::dashboardSidebar(
      # Select location
      selectInput(
        inputId = "locationSearchId",
        label = "Select Location",
        choices = streets$name
      ),

      # Select destination
      selectInput(
        inputId = "destinationSearchId",
        label = "Select Destination",
        choices = streets$name
      ),

      shinydashboard::sidebarMenu(
        # Menu Bar for data factors
        shinydashboard::menuItem("DataFactors", tabName = "dataFactors", icon = icon(name = "database", class = "fa-1x", lib = "font-awesome")),
        # Samplw Menu Bar for mysql
        shinydashboard::menuItem("MySQLExample", tabName = "mySQLExample", icon = icon("stop"))
      )
    ),
    shinydashboard::dashboardBody(shinydashboard::tabItems(
      shinydashboard::tabItem(
        tabName = "dataFactors",
        fluidRow(
          column(width = 12, h2("Traffic Data Management")),
          column(
            width = 12,
            column(
              width = 2,
              # Select location and destination to load factors in data table.
              shiny::selectInput(inputId = "routeId",
                          label = "Select Route",
                          "")
            ),
            column(width = 3,
                   wellPanel(
                     shiny::actionButton(
                       inputId = "createFactors",
                       label = "Create Factors",
                       icon = icon(name = "car", class = "fa-1x", lib = "font-awesome")
                     )
                   )),
            column(width = 5,
                   wellPanel(
                     p("Click link to add route to DB: "),
                     shiny::actionLink(
                       inputId = "createRoute",
                       label = "Create Route",
                       icon = icon(name = "road", class = "fa-1x", lib = "font-awesome")
                     )
                   ))
          ),
          column(
            width = 5,
            verbatimTextOutput(outputId = "locationDestinationData")
          )
        ),
        shinyBS::bsModal(
          id = "modalRoute",
          title = actionButton(
            inputId = "saveRoute",
            label = "Save Route",
            icon = icon(name = "floppy-o", class = "fa-1x", lib = "font-awesome")
          ),
          trigger = "createRoute",
          size = "normal",
          fluidRow(column(
            width = 12,
            column(
              width = 6,
              # Select location
              selectInput(
                inputId = "location",
                label = "Select Location",
                choices = streets$name
              )
            ),
            column(
              width = 6,
              # Select destination
              selectInput(
                inputId = "destination",
                label = "Select Destination",
                choices = streets$name
              )
            )
          ))
        ),
        shinyBS::bsAlert(anchorId = "SuccessRouteAlert"),
        shinyBS::bsAlert(anchorId = "SuccessFactorsAlert"),
        shinyBS::bsModal(
          id = "modalFactors",
          title = actionButton(
            inputId = "saveFactors",
            label = "Save Factors",
            icon = icon(name = "floppy-o", class = "fa-1x", lib = "font-awesome")
          ),
          trigger = "createFactors",
          size = "large",
          fluidRow(
            column(
              width = 12,
              column(width = 6, textOutput("locationText")),
              column(width = 6, textOutput("DestinationText"))
            ),
            column(
              width = 12,
              column(
                width = 3,
                selectInput(
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
              column(
                width = 3,
                selectInput(
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
              column(
                width = 3,
                numericInput(
                  inputId = "cars",
                  label = "Cars:",
                  value = 200,
                  min = 1,
                  max = 1000
                )
              ),
              column(
                width = 3,
                numericInput(
                  inputId = "lanes",
                  label = "Lane:",
                  value = 3,
                  min = 1,
                  max = 3
                )
              )
            ),
            column(
              width = 12,
              column(
                width = 3,
                selectInput(
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
              column(
                width = 3,
                selectInput(
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
              column(
                width = 3,
                numericInput(
                  inputId = "distance",
                  label = "Distance:",
                  value = 200,
                  min = 1,
                  max = 1000
                )
              ),
              column(
                width = 3,
                selectInput(
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
            column(
              width = 12,
              column(
                width = 3,
                selectInput(
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
              column(
                width = 3,
                numericInput(
                  "route_id",
                  "Route Id:",
                  min = 1,
                  max = 20,
                  value = 15
                )
              )
            )
          )
        ),
        tags$hr(),
        fluidRow(column(
          width = 12, DT::dataTableOutput("factors", width = 300)
        ))
      ),
      shinydashboard::tabItem(
        tabName = "mySQLExample",
        sidebarPanel(
          # Inputs excluded for brevity
          fluidRow(
            DT::dataTableOutput("responses", width = 300),
            tags$hr(),
            textInput("name", "Name", ""),
            #checkboxInput("used_shiny", "I've built a Shiny app in R before", FALSE),
            sliderInput("r_num_years", "Number of years using R", 0, 25, 2, ticks = FALSE),
            actionButton("submit", "Submit")
          )
        ),
        shiny::mainPanel(
        )
      )
    ))
  )
))

setwd('C:\\Users\\Adam\\Documents\\CIT-U\\Masters of Computer Science\\Capstone - Thesis\\Navify - Traffixer\\Navify\\')
old <- setwd(tempdir())
on.exit(setwd(old), add = TRUE)

streets <-
  read.csv(
    file = "C:\\Users\\Adam\\Documents\\CIT-U\\Masters of Computer Science\\Capstone - Thesis\\Navify - Traffixer\\Navify\\street.csv",
    header = TRUE,
    sep = ",",
    stringsAsFactors = FALSE
  )

shiny::shinyUI(shiny::fluidPage(
  shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "Navify"),
    shinydashboard::dashboardSidebar(
      # Select location
      shiny::selectInput(
        inputId = "locationSearchId",
        label = "Select Location",
        choices = streets$name
      ),

      # Select destination
      shiny::selectInput(
        inputId = "destinationSearchId",
        label = "Select Destination",
        choices = streets$name
      ),

      shinydashboard::sidebarMenu(
        # Menu Bar for data factors
        shinydashboard::menuItem("Traffic Data Management", tabName = "dataFactors", icon = shiny::icon(name = "database", class = "fa-1x", lib = "font-awesome")),
        # Samplw Menu Bar for mysql
        shinydashboard::menuItem("MySQLExample", tabName = "mySQLExample", icon = shiny::icon("stop")),
        # Google Maps
        shinydashboard::menuItem("Google Map", tabName = "googleMap", icon = shiny::icon("map")),
        # Google Maps
        shinydashboard::menuItem("Leaflet Map", tabName = "leafletMap", icon = shiny::icon("map")),
        # Using jsonlite
        shinydashboard::menuItem("Using Jsonlite", tabName = "jsonlitePackage")
      )
    ),
    shinydashboard::dashboardBody(shinydashboard::tabItems(
      shinydashboard::tabItem(
        tabName = "dataFactors",
        shiny::fluidRow(
          shinydashboard::tabBox(id = "tabSetSelectId", selected = "Factors", title = "Create", width = "6", height = "250px", side = "right",
            shiny::tabPanel("Factors",
                     shiny::column(width = 12,
                            shiny::column(width = 6,
                     # Select location and destination to load factors in data table.
                     shiny::selectInput(inputId = "routeId", label = "Select Route", "")),
                      shiny::column(width = 6,
                     shiny::wellPanel(
                       shiny::actionButton(
                         inputId = "createFactors", label = "Create Factors", icon = shiny::icon(name = "car", class = "fa-1x", lib = "font-awesome"))
                     ))),
                     shiny::column(width = 12,
                     shiny::verbatimTextOutput(outputId = "locationDestinationData")
                     )),
            shiny::tabPanel("Route",
                     shiny::wellPanel(
                       shiny::tags$p("Click link to add route to DB: "),
                       shiny::actionLink(inputId = "createRoute", label = "Create Route", icon = shiny::icon(name = "road", class = "fa-1x", lib = "font-awesome"))
                     ))
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title = "Data Factors", status = "warning", width = 11,
          shiny::column(width = 12, DT::dataTableOutput("factors", width = 300))
        ))
      ),
      shinydashboard::tabItem(
        tabName = "mySQLExample",
        shiny::sidebarPanel(
          # Inputs excluded for brevity
          shiny::fluidRow(
            DT::dataTableOutput("responses", width = "100%", height = "auto"),
            shiny::tags$hr(),
            shiny::textInput("name", "Name", ""),
            #checkboxInput("used_shiny", "I've built a Shiny app in R before", FALSE),
            shiny::sliderInput("r_num_years", "Number of years using R", 0, 25, 2, ticks = FALSE),
            shiny::actionButton("submit", "Submit")
          )
        ),
        shiny::mainPanel(
        )
      ),
      shinydashboard::tabItem(
        tabName = "googleMap",
        shiny::fluidRow(
            shiny::plotOutput("googleMapId", width = 700, height = 700)
        )
      ),
      shinydashboard::tabItem(
        tabName = "leafletMap",
        shiny::fluidRow(
          leaflet::leafletOutput("mymap"),
          shiny::tags$p(),
          shiny::actionButton("recalc", "New points")
        )
      ),
      shinydashboard::tabItem(
        tabName = "jsonlitePackage",
        shiny::actionButton("incidents", "IncidentReports")
      )
    ))
  )
))

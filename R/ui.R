shiny::shinyUI(shiny::fluidPage(
  shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "Navify"),
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        # Leaflet
        shinydashboard::menuItem("DFS", tabName = "dfs", icon = shiny::icon(name = "map", class = "fa-1x", lib = "font-awesome")),
        # Menu Bar for data factors
        shinydashboard::menuItem("Traffic Data Management", tabName = "dataFactors", icon = shiny::icon(name = "database", class = "fa-1x", lib = "font-awesome")),
        # Google Maps
        shinydashboard::menuItem("Leaflet Map", tabName = "leafletMap", icon = shiny::icon(name = "map", class = "fa-1x", lib = "font-awesome")),
        # Using jsonlite
        shinydashboard::menuItem("Using Jsonlite", tabName = "jsonlitePackage", icon = shiny::icon(name = "table", class = "fa-1x", lib = "font-awesome"))
      )
    ),
    shinydashboard::dashboardBody(shinydashboard::tabItems(
      shinydashboard::tabItem(
        tabName = "dfs",
        shiny::fluidRow(
          shinydashboard::box(title = "Select location & destination", status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
                              shiny::column(width = 12,
                                            shiny::column(width = 5,
                                            # Select location
                                            shiny::selectInput(
                                              inputId = "locationSearchId",
                                              label = "Select Location",
                                              ""
                                            )),

                                            shiny::column(width = 5,
                                            # Select destination
                                            shiny::selectInput(
                                              inputId = "destinationSearchId",
                                              label = "Select Destination",
                                              ""
                                            )),
                                            shiny::column(width = 2,
                                                          shiny::wellPanel(
                                                            shiny::actionButton(
                                                              inputId = "possiblePaths", label = "Show Paths", icon = shiny::icon(name = "code-fork", class = "fa-1x", lib = "font-awesome"))
                                                          )
                                            )
                                            )
                              )
        ),
        shiny::fluidRow(
          shinydashboard::box(width = 12,
            leaflet::leafletOutput(outputId = "DFSmap", width = "100%", height = "400px")
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(width = 12, p("datatables for possible path"))
          )
      ),
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

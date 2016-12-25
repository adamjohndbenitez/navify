shiny::shinyUI(
  shiny::fluidPage(
    shinydashboard::dashboardPage(
      shinydashboard::dashboardHeader(title = "Navify"),
      shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem(text = "Possible Paths", tabName = "dfsTab", icon = shiny::icon(name = "map-o", class = "fa-1x", lib = "font-awesome")),
          shinydashboard::menuItem(text = "Traffic Data Management", tabName = "factorsTab", icon = shiny::icon(name = "database", class = "fa-1x", lib = "font-awesome"))

        )
      ),
      shinydashboard::dashboardBody(
        shinydashboard::tabItems(
          shinydashboard::tabItem(tabName = "dfsTab",
            shiny::fluidRow(
              shinydashboard::box(title = "Select location & destination", status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
                shiny::column(width = 12,
                  shiny::column(width = 5,
                    shiny::selectInput(inputId = "locationSearchId", label = "Select Location", choices = "")
                  ),
                  shiny::column(width = 5,
                    shiny::selectInput(inputId = "destinationSearchId", label = "Select Destination", choices = "")
                  ),
                  shiny::column(width = 2,
                    shiny::wellPanel(
                      shiny::actionButton(inputId = "possiblePaths", label = "Show Paths", icon = shiny::icon(name = "code-fork", class = "fa-1x", lib = "font-awesome"))
                    )
                  )
                )
              )
            ),
            shiny::fluidRow(
              shinydashboard::box(width = 12,
                leaflet::leafletOutput(outputId = "DFSmap", width = "100%", height = "400px")
              )
            )
          ),
          shinydashboard::tabItem(tabName = "factorsTab",
            shiny::fluidRow(
              shinydashboard::tabBox(id = "tabSetSelectId", selected = "Factors", title = "Create", width = 6, height = "150px", side = "right",
                shiny::tabPanel(title = "Factors", icon = shiny::icon(name = "cubes", class = "fa-1x", lib = "font-awesome"),
                  shiny::column(width = 12,
                    shiny::column(width = 6,
                      shiny::selectInput(inputId = "routeName", label = "Select Route", "")
                    ),
                    shiny::column(width = 6,
                      shiny::wellPanel(
                        shiny::actionButton(inputId = "createFactors", label = "Create Factors", icon = shiny::icon(name = "car", class = "fa-1x", lib = "font-awesome"))
                      )
                    )
                  )
                ),
                shiny::tabPanel(title = "Import", icon = shiny::icon(name = "table", class = "fa-1x", lib = "font-awesome"),
                  shiny::column(width = 12,
                    shiny::column(width = 6,
                      shiny::fileInput(inputId = "excelFileUploadId", label = "Choose Excel File", accept = c("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", ".xlsx"), width = "100%")
                    ),
                    shiny::column(width = 6,
                      shiny::wellPanel(
                        shiny::actionButton(inputId = "importExcelFactorsId", label = "Import Excel", icon = shiny::icon(name = "file-excel-o", class = "fa-1x", lib = "font-awesome"))
                      )
                    )
                  )
                )
              )
            ),
            shiny::fluidRow(
              shinydashboard::box(title = "Data Factors", status = "warning", width = 11,
                shiny::column(width = 12, DT::dataTableOutput("factors", width = 550))
              )
            )
          )
        )
      )
    )
  )
)

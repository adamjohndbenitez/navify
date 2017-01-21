shiny::shinyUI(
  shiny::fluidPage(
    shinyjs::useShinyjs(),
    shinydashboard::dashboardPage(skin = "green",
      header = shinydashboard::dashboardHeader(title = "Navify"),
      sidebar = shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem(text = "Possible Paths", tabName = "dfsTab", icon = shiny::icon(name = "map-o", class = "fa-1x", lib = "font-awesome"), badgeLabel = "new", badgeColor = "red"),
          shinydashboard::menuItem(text = "Data Management", tabName = "factorsTab", icon = shiny::icon(name = "database", class = "fa-1x", lib = "font-awesome")),
          shinydashboard::menuItem(text = "Traffic Analysis", tabName = "analysisTab", icon = shiny::icon(name = "pie-chart", class = "fa-1x", lib = "font-awesome"))
        ),
        shinydashboard::sidebarMenuOutput(outputId = "PredictionTab")
      ),
      body = shinydashboard::dashboardBody(
        shinydashboard::tabItems(
          shinydashboard::tabItem(tabName = "dfsTab",
            shiny::fluidRow(
              shinydashboard::box(title = "Select location & destination", status = "success", solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
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
              shinydashboard::box(width = 12, status = "success",
                leaflet::leafletOutput(outputId = "DFSmap", width = "100%", height = "400px")
              )
            )
          ),
          shinydashboard::tabItem(tabName = "factorsTab",
            shiny::fluidRow(
              shinydashboard::tabBox(id = "tabBoxId", selected = "Factors", title = "Create", width = 6, height = "150px", side = "right",
                shiny::tabPanel(title = "Factors Tab", value = "Factors", icon = shiny::icon(name = "cubes", class = "fa-1x", lib = "font-awesome"),
                  shiny::column(width = 12,
                    shiny::column(width = 6,
                      shiny::selectInput(inputId = "routeName", label = "Select Route", "")
                    ),
                    shiny::column(width = 6,
                      shiny::wellPanel(
                        shiny::actionButton(inputId = "createFactors", label = "Add Factors", icon = shiny::icon(name = "car", class = "fa-1x", lib = "font-awesome"))
                      )
                    )
                  )
                ),
                shiny::tabPanel(title = "Import Tab", value = "Import", icon = shiny::icon(name = "table", class = "fa-1x", lib = "font-awesome"),
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
              ),
              shinydashboard::box(title = "Delete", status = "danger", solidHeader = TRUE,  width = 6, height = "150px", side = "right",
                shiny::column(width = 12,
                  shiny::column(width = 6,
                    shiny::br(),
                    shiny::helpText(
                      shiny::em("Clear all data from factors: ")
                    )
                  ),
                  shiny::column(width = 6,
                    shiny::wellPanel(
                      shiny::actionButton(inputId = "clearAllDataId", label = "Clear Factors", icon = shiny::icon(name = "eraser", class = "fa-1x", lib = "font-awesome"))
                    )
                  )
                )
              )
            ),
            shiny::fluidRow(
              shinydashboard::box(title = "Data Factors", status = "success", width = 12,
                shiny::column(width = 12, DT::dataTableOutput("factors", width = 1000))
              )
            )
          ),
          shinydashboard::tabItem(tabName = "analysisTab",
            shiny::fluidRow(
              shinydashboard::tabBox(id = "tabBoxId", selected = "Visualize", title = "Traffic Analysis", width = 12, height = "150px", side = "right",
                shiny::tabPanel(title = "Visualization Tab", value = "Visualize", icon = shiny::icon(name = "line-chart", class = "fa-1x", lib = "font-awesome"),
                  shiny::column(width = 12,
                    shiny::column(width = 6,
                      shiny::br(),
                      shiny::helpText(
                        shiny::helpText("Click \"Visualize Data\" button to plot data on graph: ")
                      )
                    ),
                    shiny::column(width = 6,
                      shiny::wellPanel(
                        shiny::actionButton(inputId = "visualPossiblePathsId", label = "Visualize Data", icon = shiny::icon(name = "bar-chart", class = "fa-1x", lib = "font-awesome"))
                      )
                    )
                  )
                ),
                shiny::tabPanel(title = "Prediction Tab", value = "Predict", icon = shiny::icon(name = "eye", class = "fa-1x", lib = "font-awesome"),
                  shiny::column(width = 12,
                    shiny::column(width = 6,
                      shiny::br(),
                      shiny::helpText(
                        shiny::em("Predicted Efficient Path: ")
                      )
                    )
                    # shiny::column(width = 6,
                    #   shiny::wellPanel(
                    #     shiny::actionButton(inputId = "predictEfficientPathId", label = "Predict", icon = shiny::icon(name = "question-circle", class = "fa-1x", lib = "font-awesome"))
                    #   )
                    # )
                  )
                )
              )
            ),
            shiny::fluidRow(
              # shinydashboard::box(width = 12,
              #   shiny::plotOutput(outputId = "somId", width = "100%", height = "370px")
              # )
              shinydashboard::box(
                shiny::plotOutput(outputId = "plotVehicles")
              ),
              shinydashboard::box(
                shiny::plotOutput(outputId = "plotLanes")
              )
            ),
            shiny::fluidRow(
              # shinydashboard::box(width = 12,
              #   shiny::plotOutput(outputId = "somId", width = "100%", height = "370px")
              # )
              shinydashboard::box(
                shiny::plotOutput(outputId = "plotZones")
              ),
              shinydashboard::box(
                shiny::plotOutput(outputId = "plotEvents")
              )
            )
          )
        )
      )
    )
  )
)


# Angus Watters
# Shiny UI app
# 12/31/2020

# Source helper functions -----
source('./helpers.R')

# Initalize data
covid19  <-  read_covid19()
today    <-  today_centroids(counties, covid19)
basemap  <-  basemap(today)


ui <- dashboardPage(
  dashboardHeader(title = "COVID-19 Dashboard"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      column(width = 7,
             autocomplete_input("auto", "Search for a County:",
                                value = "",
                                max_options = 5,
                                structure(today$fips, names = today$name)),
             box(width = NULL, solidHeader = TRUE,
                 leafletOutput("covidMap", height = 650)),
             box(width = NULL, title = "Statistics",
                 solidHeader = TRUE,
                 DTOutput('covidTable')),
             autocomplete_input("auto", "Search for a County:"
      )
    ),
      column(width = 4,
             box(width = NULL, status = "primary",
                 title = "TOTAL CASES/DEATHS",
                 solidHeader = TRUE,
                 dygraphOutput('covidGraph')),
             box(width = NULL, status = "success",
                 title = "NEW CASES",
                 solidHeader = TRUE,
                 plotlyOutput("covidPlotly")),
             # infoBox("Total cases", icon = icon("credit-card"), fill = TRUE),
             valueBoxOutput("totalCases"),
             valueBoxOutput("totalDeaths"),
             valueBoxOutput("deathRate")
      )
    )
  )
)

server <- function(input, output, session) {
  # Global variables initialized
  FIP <- today$fips[which.max(today$cases)]
  v   <- reactiveValues(msg = "")

  # output$Source <- renderMenu({
  #     menuItem("Source", icon = icon("file-code-o"),
  #              href = "https://anguswg-ucsb.github.io/geog176A.html")
  # })
  output$covidMap     <- renderLeaflet({ basemap })
  output$covidGraph = renderDygraph({ make_graph(covid19, FIP) })
  output$covidTable = renderDT({ make_table2(today, FIP) })
  output$covidPlotly = renderPlotly({ make_graph2(covid19, FIP) })
  output$totalCases <- renderValueBox({
    valueBox(
      paste0((cases_info(covid19)[2])),
      subtitle = "CASES",
      icon = icon("user"),
      color = "yellow") })
  output$totalDeaths <- renderValueBox({
    valueBox(
      paste0((cases_info(covid19)[3])),
      subtitle = "DEATHS",
      icon = icon("skull"),
      color = "red") })
  output$deathRate <- renderValueBox({
    valueBox(
      paste0((death_info(covid19)[5])),
      subtitle = "DEATH RATE",
      icon = icon("heart"),
      color = "black") })
  observeEvent(input$covidMap_marker_mouseover, {
    txt = filter(today, fips == input$covidMap_marker_mouseover$id)
    v$msg <- paste0("Mouse is over: ", txt$name, " (", txt$cases, " cases)")
  })

  observeEvent(input$covidMap_marker_mouseout, {
    v$msg <- "Mouse is over: "
  })

  observeEvent(input$covidMap_marker_click, {
    FIP <<- input$covidMap_marker_click$id
    output$covidGraph = renderDygraph({ make_graph(covid19, FIP) })
    leafletProxy('covidMap') %>% zoom_to_county(counties, FIP)
    output$covidTable = renderDT({ make_table2(today, FIP) })
    output$covidPlotly = renderPlotly({ make_graph2(covid19, FIP) })

  })

  observe(
    if(input$auto == ""){
      NULL
    } else {
      FIP <<- input$auto
      leafletProxy("covidMap") %>% zoom_to_county(counties, FIP)
      output$covidChart <- renderDygraph({ make_graph(covid19, FIP) })
      output$covidTable <- renderDT({ make_table2(today, FIP) })
      output$covidPlotly = renderPlotly({ make_graph2(covid19, FIP) })
    })


}

shinyApp( ui, server )
# # User interface ----
# ui <- fluidPage(
#   theme =  shinytheme("paper"),
#   titlePanel(h1("COVID-19 Dashboard",
#                 style='background-color:#464646;color:#add8e6;padding-left: 15px;'),
#              windowTitle = "COVID19-dashboard"),
#   titlePanel('Angus Watters'),
#
#   # Sidebar layout output definitions ----
#   sidebarPanel(
#     autocomplete_input("auto", "Search for a County:",
#                        value = "",
#                        max_options = 5,
#                        structure(today$fips, names = today$name)),
#     # Output: Message ----
#     textOutput("covidMessage", container = h4),
#     # Output: Table -----
#     DTOutput('covidTable')
#   ),
#
#   # Main panel for displaying outputs ----
#   mainPanel(
#     # Output: Map ----
#     leafletOutput('covidMap'),
#
#     dygraphOutput('covidGraph')
#
#     # plotlyOutput("covidPlotly")
#
#   )
# )

ui <- dashboardPage(
  dashboardHeader(title = "COVID-19 Dashboard"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      column(width = 7,
             box(width = NULL, solidHeader = TRUE,
                 leafletOutput("covidMap", height = 650)),
             box(width = NULL, title = "Statistics",
                 solidHeader = TRUE,
                 DTOutput('covidTable')),
              autocomplete_input("auto", "Search for a County:",
                                    value = "",
                                    max_options = 5,
                                    structure(today$fips, names = today$name))
             ),

       column(width = 4,
            box(width = NULL, status = "success",
                title = "Total cases and deaths",
                solidHeader = TRUE,
                dygraphOutput('covidGraph')),
            box(width = NULL, status = "info",
                title = "New cases",
                solidHeader = TRUE,
                plotlyOutput("covidPlotly"))
                    )
    )
  )
)


server <- function(input, output, session) {
  # Global variables initialized
  FIP <- today$fips[which.max(today$cases)]
  v   <- reactiveValues(msg = "")

  # output$Source <- renderMenu({
  #     menuItem("Source", icon = icon("file-code-o"),
  #              href = "https://anguswg-ucsb.github.io/geog176A.html")
  # })
  output$covidMap     <- renderLeaflet({ basemap })
  output$covidGraph = renderDygraph({ make_graph(covid19, FIP) })
  output$covidTable = renderDT({ make_table2(today, FIP) })
  output$covidPlotly = renderPlotly({ make_graph2(covid19, FIP) })
  observeEvent(input$covidMap_marker_mouseover, {
    txt = filter(today, fips == input$covidMap_marker_mouseover$id)
    v$msg <- paste0("Mouse is over: ", txt$name, " (", txt$cases, " cases)")
  })

  observeEvent(input$covidMap_marker_mouseout, {
    v$msg <- "Mouse is over: "
  })

  observeEvent(input$covidMap_marker_click, {
    FIP <<- input$covidMap_marker_click$id
    output$covidGraph = renderDygraph({ make_graph(covid19, FIP) })
    leafletProxy('covidMap') %>% zoom_to_county(counties, FIP)
    output$covidTable = renderDT({ make_table2(today, FIP) })
    output$covidPlotly = renderPlotly({ make_graph2(covid19, FIP) })

  })

  observe(
    if(input$auto == ""){
      NULL
    } else {
      FIP <<- input$auto
      leafletProxy("covidMap") %>% zoom_to_county(counties, FIP)
      output$covidChart <- renderDygraph({ make_graph(covid19, FIP) })
      output$covidTable <- renderDT({ make_table2(today, FIP) })
      output$covidPlotly = renderPlotly({ make_graph2(covid19, FIP) })
    })


}

shinyApp( ui, server )
      # autocomplete_input("auto", "Search for a County:",
      #                        value = "",
      #                        max_options = 5,
      #                        structure(today$fips, names = today$name)),
      # fillPage(
      #   tags$style(type = "text/css", "html, body {width:100%; height:100%}"),
      #   leafletOutput("covidMap", width = "100%", height = "100%")
      #
      # ),
#       div(class="outer",
#           tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
#           leafletOutput("covidMap", width = "100%", height = "100%")),
#       absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
#                     draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
#                     width = 330, height = "auto"),
#
#       autocomplete_input("auto", "Search for a County:",
#                          value = "",
#                          max_options = 5,
#                          structure(today$fips, names = today$name)),
#       # box(leafletOutput('covidMap'), height=1000),
#       box(title = "Total cases and deaths", solidHeader = TRUE, dygraphOutput('covidGraph')),
#       box(DTOutput('covidTable')),
#       box(plotlyOutput("covidPlotly"))
#       )
#   )
# )

server <- function(input, output, session) {
  # Global variables initialized ----
  FIP <- today$fips[which.max(today$cases)]
  v   <- reactiveValues(msg = "")

  # Leaflet Map ----
  # # ---- must be rendered as leaflet ----
  # output$Source <- renderMenu({
  #     menuItem("Source", icon = icon("file-code-o"),
  #              href = "https://anguswg-ucsb.github.io/geog176A.html")
  # })
  output$covidMap     <- renderLeaflet({ basemap })
  output$covidGraph = renderDygraph({ make_graph(covid19, FIP) })
  output$covidTable = renderDT({ make_table2(today, FIP) })
  output$covidPlotly = renderPlotly({ make_graph2(covid19, FIP) })
    observeEvent(input$covidMap_marker_mouseover, {
      txt = filter(today, fips == input$covidMap_marker_mouseover$id)
      v$msg <- paste0("Mouse is over: ", txt$name, " (", txt$cases, " cases)")
    })

    observeEvent(input$covidMap_marker_mouseout, {
      v$msg <- "Mouse is over: "
    })

    observeEvent(input$covidMap_marker_click, {
      FIP <<- input$covidMap_marker_click$id
      output$covidGraph = renderDygraph({ make_graph(covid19, FIP) })
      leafletProxy('covidMap') %>% zoom_to_county(counties, FIP)
      output$covidTable = renderDT({ make_table2(today, FIP) })
      output$covidPlotly = renderPlotly({ make_graph2(covid19, FIP) })

    })

    observe(
        if(input$auto == ""){
          NULL
        } else {
          FIP <<- input$auto
          leafletProxy("covidMap") %>% zoom_to_county(counties, FIP)
          output$covidChart <- renderDygraph({ make_graph(covid19, FIP) })
          output$covidTable <- renderDT({ make_table2(today, FIP) })
          output$covidPlotly = renderPlotly({ make_graph2(covid19, FIP) })
        })


}

shinyApp( ui, server )

# Server logic ----
# server <- function(input, output, session) {
#   # Global variables initialized ----
#   FIP <- today$fips[which.max(today$cases)]
#   v   <- reactiveValues(msg = "")
#
#   # Leaflet Map ----
#   # ---- must be rendered as leaflet ----
#   output$covidMap     <- renderLeaflet({ basemap })
#   # dyGraph chart ----
#   # ----- must be rendered as dyGraph
#   output$covidGraph = renderDygraph({ make_graph(covid19, FIP) })
#   # DT Table ----
#   # ---- must be rendered as DT
#   output$covidTable = renderDT({ make_table2(today, FIP) })
#   # output$covidPlotly = renderPlotly({ make_graph2(covid19, FIP) })
#   # Message to Display ----
#   # ---- must be rendered as text ----
#   output$covidMessage <- renderText(v$msg)
#
#   # Events ----
#   # ---- mouse ----
#
#   observeEvent(input$covidMap_marker_mouseover, {
#     txt = filter(today, fips == input$covidMap_marker_mouseover$id)
#     v$msg <- paste0("Mouse is over: ", txt$name, " (", txt$cases, " cases)")
#   })
#
#   observeEvent(input$covidMap_marker_mouseout, {
#     v$msg <- "Mouse is over: "
#   })
#
#   observeEvent(input$covidMap_marker_click, {
#     FIP <<- input$covidMap_marker_click$id
#     output$covidGraph = renderDygraph({ make_graph(covid19, FIP) })
#     leafletProxy('covidMap') %>% zoom_to_county(counties, FIP)
#     output$covidTable = renderDT({ make_table2(today, FIP) })
#     # output$covidPlotly = renderPlotly({ make_graph2(covid19, FIP) })
#   })
#
#   observe(
#     if(input$auto == ""){
#       NULL
#     } else {
#       FIP <<- input$auto
#       leafletProxy("covidMap") %>% zoom_to_county(counties, FIP)
#       output$covidChart <- renderDygraph({ make_graph(covid19, FIP) })
#       output$covidTable <- renderDT({ make_table2(today, FIP) })
#       # output$covidPlotly = renderPlotly({ make_graph2(covid19, FIP) })
#     }
#   )
# }






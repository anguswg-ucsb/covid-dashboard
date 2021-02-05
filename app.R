
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
  shinyDashboardThemes(
    theme = "grey_dark"
    ),
  dashboardHeader(title = "COVID-19 Dashboard"),
  dashboardSidebar(disable = TRUE),
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
                 DTOutput('covidTable'))

      ),

      column(width = 4,
             box(width = NULL, status = "primary",
                 title = "TOTAL CASES/DEATHS",
                 solidHeader = TRUE,
                 dygraphOutput('covidGraph')),
             # box(width = NULL, status = "primary",
             #     title = "NEW CASES",
             #     solidHeader = TRUE,
             #     plotlyOutput("covidNewCases")),
             tabBox(width = NULL,
             title = "Sample box",
                tabPanel("Daily Cases", plotlyOutput("covidNewCases")),
                tabPanel("Daily Deaths", plotlyOutput("covidNewDeaths")),
                tabPanel("Cumulative Cases", plotlyOutput("covidTotalCases")),
                tabPanel("Cumulative Deaths", plotlyOutput("covidTotalDeaths"))),
             valueBoxOutput("totalCases"),
             valueBoxOutput("totalDeaths"),
             valueBoxOutput("deathRate")
             # infoBox("Total cases", icon = icon("credit-card"), fill = TRUE),

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
  output$covidNewCases = renderPlotly({ daily_cases_graph(covid19, FIP) })
  output$covidNewDeaths <- renderPlotly({ daily_deaths_graph(covid19, FIP) })
  output$covidTotalCases = renderPlotly({ total_cases_graph(covid19, FIP) })
  output$covidTotalDeaths = renderPlotly({ total_deaths_graph(covid19, FIP) })
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
      paste0((death_info(covid19)[5]), "%"),
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
    output$covidNewCases = renderPlotly({ daily_cases_graph(covid19, FIP) })
    output$covidNewDeaths <- renderPlotly({ daily_deaths_graph(covid19, FIP) })
    output$covidTotalCases = renderPlotly({ total_cases_graph(covid19, FIP) })
    output$covidTotalDeaths = renderPlotly({ total_deaths_graph(covid19, FIP) })


  })

  observe(
    if(input$auto == ""){
      NULL
    } else {
      FIP <<- input$auto
      leafletProxy("covidMap") %>% zoom_to_county(counties, FIP)
      output$covidChart <- renderDygraph({ make_graph(covid19, FIP) })
      output$covidTable <- renderDT({ make_table2(today, FIP) })
      output$covidNewCases = renderPlotly({ daily_cases_graph(covid19, FIP) })
      output$covidNewDeaths <- renderPlotly({ daily_deaths_graph(covid19, FIP) })
      output$covidTotalCases = renderPlotly({ total_cases_graph(covid19, FIP) })
      output$covidTotalDeaths = renderPlotly({ total_deaths_graph(covid19, FIP) })
    })


}

shinyApp( ui, server )

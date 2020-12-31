
# Angus Watters
# Shiny UI app
# 12/31/2020

# Source helper functions -----
source('helpers.R')

# Initalize data
covid19  <-  read_covid19()
today    <-  today_centroids(counties, covid19)
basemap  <-  basemap(today)

# User interface ----
ui <- fluidPage(
  theme =  shinytheme("paper"),
  titlePanel(h1("COVID-19 Dashboard",
                style='background-color:#464646;color:#add8e6;padding-left: 15px;'),
             windowTitle = "COVID19-dashboard"),
  titlePanel('Angus Watters'),

  # Sidebar layout output definitions ----
  sidebarPanel(
    autocomplete_input("auto", "Search for a County:",
                       value = "",
                       max_options = 5,
                       structure(today$fips, names = today$name)),
    # Output: Message ----
    textOutput("covidMessage", container = h4),
    # Output: Table -----
    DTOutput('covidTable')
  ),

  # Main panel for displaying outputs ----
  mainPanel(
    # Output: Map ----
    leafletOutput('covidMap'),

    dygraphOutput('covidGraph')
  )
)

# Server logic ----
server <- function(input, output, session) {
  # Global variables initialized ----
  FIP <- today$fips[which.max(today$cases)]
  v   <- reactiveValues(msg = "")
  # Leaflet Map ----
  # ---- must be rendered as leaflet ----
  output$covidMap     <- renderLeaflet({ basemap })
  # dyGraph chart ----
  # ----- must be rendered as dyGraph
  output$covidGraph = renderDygraph({ make_graph(covid19, FIP) })
  # DT Table ----
  # ---- must be rendered as DT
  output$covidTable = renderDT({ make_table2(today, FIP) })

  # Message to Display ----
  # ---- must be rendered as text ----
  output$covidMessage <- renderText(v$msg)

  # Events ----
  # ---- mouse ----

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
  })

  observe(
    if(input$auto == ""){
      NULL
    } else {
      FIP <<- input$auto
      leafletProxy("covidMap") %>% zoom_to_county(counties, FIP)
      output$covidChart <- renderDygraph({ make_graph(covid19, FIP) })
      output$covidTable <- renderDT({ make_table2(today, FIP) })
    }
  )
}

shinyApp( ui, server )




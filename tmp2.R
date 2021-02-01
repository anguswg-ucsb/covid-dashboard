


pop = readxl::read_excel('PopulationEstimates.xls', skip = 2) %>%
  select(state = State, pop_19 = POP_ESTIMATE_2019, fips = FIPStxt)

pop$fips <- as.numeric(pop$fips)


covid19 = inner_join(covid19, select(pop, pop_19, fips), by = 'fips')

covid19 <- covid19 %>%
  group_by(county) %>%
  mutate(new_cases = cases - lag(cases)) %>%
  mutate(cases_per_cap = cases/pop_19, new_cases_per_cap = new_cases/pop_19)

new_cases <- covid19 %>%
  group_by(county, state, date) %>%
  mutate(total_cases = sum(cases)) %>%
  arrange(desc(date)) %>%
  group_by(county) %>%
  mutate(new_cases = total_cases - lag(total_cases))

covid19 <- covid19 %>%
  group_by(state) %>%
  mutate(id = row_number())

new_cases <- new_cases %>%
  group_by(state) %>%
  mutate(id = row_number())

new_cases <- left_join(new_cases, select(covid19, state, county, fips, id), by = c("state", "id"))

new_cases <- select(new_cases, state, county, date:id, fips = fips.x)

new_cases <- select(new_cases, !id)

new_cases <- covid19 %>%
  group_by(state, date) %>%
  summarise(county = county, fips = fips, cases = sum(cases, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(state) %>%
  mutate(new_cases = cases - lag(cases)) %>%
  mutate(rolling_mean = rollmean(new_cases, 7, fill = NA, align = 'right'))

# Fit and exponetial model for fun
exponential.model <- lm(log(new_cases)~ date, data = subset)
# use the model to predict a what a expoential curve would look like
subset$expCases = ceiling(exp(predict(exponential.model, list(date = subset$date))))

subset <- new_cases %>% filter(fips == 6001)
plot_ly(subset,
        x = ~date,
        y = ~new_cases,
        name = "Daily cases",
        color = I('aquamarine3'),
        type = "bar") %>%
  add_trace(x = ~date, y = ~rolling_mean,
            type = "scatter",
            mode = "lines",
            name = "Rolling mean",
            color = I('darkcyan')) %>%
add_trace(x = ~date, y = ~expCases,
          type = "scatter",
          mode = "lines",
          name = "Exponential",
          color = I('darkblue')) %>%
  layout(yaxis = list(title = 'Daily cases'), xaxis = list(title = "Date"), barmode = 'group')
library(zoo)
library(plotly)

make_graph2 = function(covid19, FIP){
  new_cases <- covid19 %>%
    group_by(state, date) %>%
    summarise(county = county, fips = fips, cases = sum(cases, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(state) %>%
    mutate(new_cases = cases - lag(cases)) %>%
    mutate(rolling_mean = rollmean(new_cases, 7, fill = NA, align = 'right'))

  # Fit and exponetial model for fun
  exponential.model <- lm(log(new_cases)~ date, data = subset)
  # use the model to predict a what a expoential curve would look like
  subset$expCases = ceiling(exp(predict(exponential.model, list(date = subset$date))))

  subset <- new_cases %>% filter(fips == FIPS)
  plot_ly(subset,
          x = ~date,
          y = ~new_cases,
          name = "Daily cases",
          color = I('aquamarine3'),
          type = "bar") %>%
    add_trace(x = ~date, y = ~rolling_mean,
              type = "scatter",
              mode = "lines",
              name = "Rolling mean",
              color = I('darkcyan')) %>%
    add_trace(x = ~date, y = ~expCases,
              type = "scatter",
              mode = "lines",
              name = "Exponential",
              color = I('darkblue')) %>%
    layout(yaxis = list(title = 'Daily cases'), xaxis = list(title = "Date"), barmode = 'group')
}
new_cases <- covid19 %>%
  group_by(date) %>%
  summarise(cases = sum(cases, na.rm = TRUE)) %>%
  mutate(new_cases = cases - lag(cases)) %>%
  mutate(rolling_mean = rollmean(new_cases, 7, fill = NA, align = 'right'))

 plot_ly(new_cases,
  x = ~date,
  y = ~new_cases,
  name = "Daily cases",
  color = I('aquamarine3'),
  type = "bar") %>%
  add_trace(x = ~date, y = ~rolling_mean,
            type = "scatter",
            mode = "lines",
            color = I('darkcyan'))

 plot_ly(deaths,
         x = ~date,
         y = ~new_deaths,
         name = "Daily deaths",
         color = I('darkred'),
         type = "bar") %>%
   add_trace(x = ~date, y = ~rolling_mean_deaths,
             type = "scatter",
             mode = "lines",
             color = I('red'))

deaths <- covid19 %>%
  group_by(date) %>%
  summarise(deaths = sum(deaths, na.rm = TRUE)) %>%
  mutate(new_deaths = deaths - lag(deaths)) %>%
  mutate(rolling_mean_deaths = rollmean(new_deaths, 7, fill = NA, align = 'right'))

new_cases %>% ggplot(aes(x = date, y = new_cases)) +
  geom_col(col = 'aquamarine4', fill = 'aquamarine3') +
  geom_line(aes(y=rolling_mean))

deaths %>% ggplot(aes(x = date, y = new_deaths)) +
  geom_col(col = 'darkred')

# CASES PER 100,000 RESIDENTS
cases_per_100k = covid19 %>%
  filter(date > max(date) - 7) %>%
  group_by(county, pop_19) %>%
  mutate(cases_100k = new_cases / (pop_19 / 100000)) %>%
  mutate(cases_100k = (sum(cases_100k)/(7)))

# 7 - DAY ROLLING MEAN
covid19 = covid19 %>%
  group_by(county, date) %>%
  mutate(total_cases = sum(cases, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(county) %>%
  mutate(new_cases = cases - lag(cases),
         rolling_mean = rollmean(new_cases, 7, fill = NA, align = 'right'))

# 7 - DAY ROLLING MEAN PER CAPITA
ca_covid  = ca_covid %>%
  group_by(county, date) %>%
  mutate(pop_19 = sum(pop_19)) %>%
  ungroup() %>%
  group_by(county) %>%
  mutate(rolling_mean_per_cap = rollmean(new_cases_per_cap, 7, fill = NA, align = 'right'))

covid_tmp = covid %>%
  filter(state == tmp_state) %>%
  group_by(county) %>%
  mutate(new_cases = cases - lag(cases)) %>%
  mutate(cases_per_cap = cases/pop_19, new_cases_per_cap = new_cases/pop_19)
ca_rolling = covid_tmp %>%
  group_by(state, date) %>%
  summarise(cases = sum(cases, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(state) %>%
  mutate(new_cases = cases - lag(cases),
         rolling_mean = rollmean(new_cases, 7, fill = NA, align = 'right'))

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
#
# ui <- dashboardPage(
#   dashboardHeader(title = "COVID-19 Dashboard"),
#   dashboardSidebar(),
#   dashboardBody(
#     fluidRow(
#       column(width = 7,
#              box(width = NULL, solidHeader = TRUE,
#                  leafletOutput("covidMap", height = 650)),
#              box(width = NULL, title = "Statistics",
#                  solidHeader = TRUE,
#                  DTOutput('covidTable')),
#               autocomplete_input("auto", "Search for a County:",
#                                     value = "",
#                                     max_options = 5,
#                                     structure(today$fips, names = today$name))
#              ),
#
#        column(width = 4,
#             box(width = NULL, status = "success",
#                 title = "Total cases and deaths",
#                 solidHeader = TRUE,
#                 dygraphOutput('covidGraph')),
#             box(width = NULL, status = "info",
#                 title = "New cases",
#                 solidHeader = TRUE,
#                 plotlyOutput("covidPlotly"))
#                     )
#     )
#   )
# )
#
#
# server <- function(input, output, session) {
#   # Global variables initialized
#   FIP <- today$fips[which.max(today$cases)]
#   v   <- reactiveValues(msg = "")
#
#   # output$Source <- renderMenu({
#   #     menuItem("Source", icon = icon("file-code-o"),
#   #              href = "https://anguswg-ucsb.github.io/geog176A.html")
#   # })
#   output$covidMap     <- renderLeaflet({ basemap })
#   output$covidGraph = renderDygraph({ make_graph(covid19, FIP) })
#   output$covidTable = renderDT({ make_table2(today, FIP) })
#   output$covidPlotly = renderPlotly({ make_graph2(covid19, FIP) })
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
#     output$covidPlotly = renderPlotly({ make_graph2(covid19, FIP) })
#
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
#       output$covidPlotly = renderPlotly({ make_graph2(covid19, FIP) })
#     })
#
#
# }
#
# shinyApp( ui, server )
#       # autocomplete_input("auto", "Search for a County:",
#       #                        value = "",
#       #                        max_options = 5,
#       #                        structure(today$fips, names = today$name)),
#       # fillPage(
#       #   tags$style(type = "text/css", "html, body {width:100%; height:100%}"),
#       #   leafletOutput("covidMap", width = "100%", height = "100%")
#       #
#       # ),
# #       div(class="outer",
# #           tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
# #           leafletOutput("covidMap", width = "100%", height = "100%")),
# #       absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
# #                     draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
# #                     width = 330, height = "auto"),
# #
# #       autocomplete_input("auto", "Search for a County:",
# #                          value = "",
# #                          max_options = 5,
# #                          structure(today$fips, names = today$name)),
# #       # box(leafletOutput('covidMap'), height=1000),
# #       box(title = "Total cases and deaths", solidHeader = TRUE, dygraphOutput('covidGraph')),
# #       box(DTOutput('covidTable')),
# #       box(plotlyOutput("covidPlotly"))
# #       )
# #   )
# # )
#
# server <- function(input, output, session) {
#   # Global variables initialized ----
#   FIP <- today$fips[which.max(today$cases)]
#   v   <- reactiveValues(msg = "")
#
#   # Leaflet Map ----
#   # # ---- must be rendered as leaflet ----
#   # output$Source <- renderMenu({
#   #     menuItem("Source", icon = icon("file-code-o"),
#   #              href = "https://anguswg-ucsb.github.io/geog176A.html")
#   # })
#   output$covidMap     <- renderLeaflet({ basemap })
#   output$covidGraph = renderDygraph({ make_graph(covid19, FIP) })
#   output$covidTable = renderDT({ make_table2(today, FIP) })
#   output$covidPlotly = renderPlotly({ make_graph2(covid19, FIP) })
#     observeEvent(input$covidMap_marker_mouseover, {
#       txt = filter(today, fips == input$covidMap_marker_mouseover$id)
#       v$msg <- paste0("Mouse is over: ", txt$name, " (", txt$cases, " cases)")
#     })
#
#     observeEvent(input$covidMap_marker_mouseout, {
#       v$msg <- "Mouse is over: "
#     })
#
#     observeEvent(input$covidMap_marker_click, {
#       FIP <<- input$covidMap_marker_click$id
#       output$covidGraph = renderDygraph({ make_graph(covid19, FIP) })
#       leafletProxy('covidMap') %>% zoom_to_county(counties, FIP)
#       output$covidTable = renderDT({ make_table2(today, FIP) })
#       output$covidPlotly = renderPlotly({ make_graph2(covid19, FIP) })
#
#     })
#
#     observe(
#         if(input$auto == ""){
#           NULL
#         } else {
#           FIP <<- input$auto
#           leafletProxy("covidMap") %>% zoom_to_county(counties, FIP)
#           output$covidChart <- renderDygraph({ make_graph(covid19, FIP) })
#           output$covidTable <- renderDT({ make_table2(today, FIP) })
#           output$covidPlotly = renderPlotly({ make_graph2(covid19, FIP) })
#         })
#
#
# }
#
# shinyApp( ui, server )

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







library(shiny)
library(shiny.semantic)
library(semantic.dashboard)
library(plotly)
library(DT)

# plot_ly(subset2,
#         x = ~date,
#         y = ~new_cases,
#         name = "Daily cases",
#         color = I('aquamarine3'),
#         type = "bar") %>%
#   add_trace(x = ~date, y = ~rolling_mean,
#             type = "scatter",
#             mode = "lines",
#             name = "Rolling mean",
#             color = I('darkcyan')) %>%
#   # add_trace(x = ~date, y = ~expCases,
#   #           type = "scatter",
#   #           mode = "lines",
#   #           name = "Exponential",
#   #           color = I('darkblue')) %>%
#   layout(yaxis = list(title = 'Daily cases'), xaxis = list(title = "Date"), barmode = 'group')
install.packages("semantic.dashboard")
ui <- dashboardPage(
  dashboardHeader(dropdownMenuOutput("dropdown"),
                  dropdownMenu(type = "notifications",
                               taskItem("Project progress...", 50.777, color = "red")),
                  dropdownMenu(icon = icon("red warning sign"),
                               notificationItem("This is an important notification!", color = "red"))),
  dashboardSidebar(side = "left",
                   sidebarMenu(
                     menuItem(tabName = "plot_tab", text = "My plot", icon = icon("home")),
                     menuItem(tabName = "table_tab", text = "My table", icon = icon("smile")))),
  dashboardBody(
    tabItems(
      tabItem(tabName = "plot_tab",
              fluidRow(
                valueBox("Unread Mail", 44, icon("mail"), color = "blue", width = 5)),
              fluidRow(
                box(title = "Sample box", color = "blue", width = 11,
                    selectInput(inputId =  "variable1", choices = names(mtcars),
                                label = "Select first variable", selected = "mpg"),
                    selectInput(inputId =  "variable2", choices = names(mtcars),
                                label = "Select second variable", selected = "cyl"),
                    plotlyOutput("mtcars_plot")),
                tabBox(title = "Sample box", color = "blue", width = 5,
                       collapsible = FALSE,
                       tabs = list(
                         list(menu = "First Tab", content = "Some text..."),
                         list(menu = "Second Tab", content = plotlyOutput("mtcars_plot2"))
                       )))),
      tabItem(tabName = "table_tab",
              fluidRow(
                valueBox("Unread Mail", 144, icon("mail"), color = "blue", width = 6, size = "small"),
                valueBox("Spam", 20, icon("mail"), color = "red", width = 5, size = "small"),
                valueBox("Readed Mail", 666, icon("mail"), color = "green", width = 5, size = "small")
              ),
              fluidRow(
                box(title = "Classic box", color = "blue", ribbon = FALSE,
                    title_side = "top left", width = 14,
                    tags$div(
                      dataTableOutput("mtcars_table")
                      , style = paste0("color:", semantic_palette[["blue"]], ";"))
                ))))
  ), theme = "darkly"
)

server <- function(input, output) {

  output$mtcars_plot <- renderPlotly(plot_ly(mtcars, x = ~ mtcars[ , input$variable1],
                                             y = ~ mtcars[ , input$variable2],
                                             type = "scatter", mode = "markers")
  )
  output$mtcars_plot2 <- renderPlotly(plot_ly(mtcars, x = ~ mtcars[ , input$variable1],
                                              y = ~ mtcars[ , input$variable2],
                                              type = "scatter", mode = "markers"))

  output$mtcars_table <- renderDataTable(mtcars, options = list(dom = 't'))

  output$dropdown <- renderDropdownMenu({
    dropdownMenu(messageItem("User", "Test message", color = "teal", style = "min-width: 200px"),
                 messageItem("Users", "Test message", color = "teal", icon = "users"),
                 messageItem("See this", "Another test", icon = "warning", color = "red"))
  })
}

shinyApp(ui, server)

ui <- dashboardPage(
  dashboardHeader(dropdownMenuOutput("dropdown"),
                  dropdownMenu(type = "notifications",
                               taskItem("Project progress...", 50.777, color = "red")),
                  dropdownMenu(icon = icon("red warning sign"),
                               notificationItem("This is an important notification!", color = "red"))),
  dashboardSidebar(side = "left",
                   sidebarMenu(
                     menuItem(tabName = "plot_tab", text = "My plot", icon = icon("home")),
                     menuItem(tabName = "table_tab", text = "My table", icon = icon("smile")))),
  dashboardBody(
    tabItems(
      tabItem(tabName = "plot_tab",
              fluidRow(
                valueBox("Unread Mail", 44, icon("mail"), color = "blue", width = 5)),
              fluidRow(
                box(title = "Sample box", color = "blue", width = 11,
                    selectInput(inputId =  "variable1", choices = names(mtcars),
                                label = "Select first variable", selected = "mpg"),
                    selectInput(inputId =  "variable2", choices = names(mtcars),
                                label = "Select second variable", selected = "cyl"),
                    plotlyOutput("mtcars_plot")),
                tabBox(title = "Sample box", color = "blue", width = 5,
                       collapsible = FALSE,
                       tabs = list(
                         list(menu = "First Tab", content = "Some text..."),
                         list(menu = "Second Tab", content = plotlyOutput("mtcars_plot2"))
                       )))),
      tabItem(tabName = "table_tab",
              fluidRow(
                valueBox("Unread Mail", 144, icon("mail"), color = "blue", width = 6, size = "small"),
                valueBox("Spam", 20, icon("mail"), color = "red", width = 5, size = "small"),
                valueBox("Readed Mail", 666, icon("mail"), color = "green", width = 5, size = "small")
              ),
              fluidRow(
                box(title = "Classic box", color = "blue", ribbon = FALSE,
                    title_side = "top left", width = 14,
                    tags$div(
                      dataTableOutput("mtcars_table")
                      , style = paste0("color:", semantic_palette[["blue"]], ";"))
                ))))
  ), theme = "darkly"
)

server <- function(input, output) {

  output$mtcars_plot <- renderPlotly(plot_ly(mtcars, x = ~ mtcars[ , input$variable1],
                                             y = ~ mtcars[ , input$variable2],
                                             type = "scatter", mode = "markers")
  )
  output$mtcars_plot2 <- renderPlotly(plot_ly(mtcars, x = ~ mtcars[ , input$variable1],
                                              y = ~ mtcars[ , input$variable2],
                                              type = "scatter", mode = "markers"))

  output$mtcars_table <- renderDataTable(mtcars, options = list(dom = 't'))

  output$dropdown <- renderDropdownMenu({
    dropdownMenu(messageItem("User", "Test message", color = "teal", style = "min-width: 200px"),
                 messageItem("Users", "Test message", color = "teal", icon = "users"),
                 messageItem("See this", "Another test", icon = "warning", color = "red"))
  })
}

shinyApp(ui, server)
source('docs/R/helpers.R')
counties = readRDS("./data/counties.rds")
covid19 = read_covid19()
today   = today_centroids(counties, covid19)

infoBox("New cases", icon = icon("credit-card"), fill = TRUE),
infoBoxOutput("newCases"),

output$newCases <- renderInfoBox({
  infoBox(
    "New cases", paste0((cases_info(covid19)[3])), icon = icon("list"),
    color = "purple", fill = TRUE
    # COUNTY DAILY CASES GRAPH
cases_info = function(covid19){
      subset3 <- covid19 %>%
        group_by(date) %>%
        summarise(deaths = sum(deaths, na.rm = TRUE), cases = sum(cases, na.rm = TRUE)) %>%
        mutate(new_cases = cases - lag(cases)) %>%
        arrange(desc(date)) %>%
        slice_max(1)
}

      # Fit and exponetial model for fun
      # exponential.model <- lm(log(new_cases)~ date, data = subset)
      # # use the model to predict a what a expoential curve would look like
      # subset$expCases = ceiling(exp(predict(exponential.model, list(date = subset$date))))

      gg_1 = ggplot(subset2, aes(x = date, y = new_cases)) +
        geom_bar(col = 'aquamarine4', fill = 'aquamarine3', stat = 'identity') +
        geom_line(aes(y = rolling_mean), col = "darkgreen", size = 0.7) +
        labs(title = 'DAILY NEW CASES',
             x = 'DATE',
             y = 'NEW CASES',
             subtitle = 'Data Source: The New York Times') +
        theme(axis.text.x = element_text(size = 10),
              axis.text.y = element_text(size = 10),
              axis.title.x = element_text(size = 10),
              axis.title.y = element_text(size = 10, hjust = 0.5),
              plot.title = element_text(hjust = 0.5, size = 20),
              plot.subtitle = element_text(size = 14),
              legend.title = element_text(face = "bold", size = 10, hjust = 0.5),
              legend.title.align = 0.5,
              legend.text = element_text(face = "bold", size = 12))
      ggplotly(gg_1)
total_cases = covid19 %>%
  group_by(county) %>%
  arrange(desc(date)) %>%
  slice_max(1) %>%
  ungroup() %>%
  summarise(total = sum(cases))
#
# library(tidyverse)
# library(lubridate)
# library(rvest)
# library(stringdist)
# install.packages("stringdist")
# # Function to read the raw CSV files. The files are aggregated to the country
# # level and then converted to long format
#
# clean_jhd_to_long <- function(df) {
#   df_str <- deparse(substitute(df))
#   var_str <- substr(df_str, 1, str_length(df_str) - 4)
#
#   df %>% group_by(`Country/Region`) %>%
#     filter(`Country/Region` != "Cruise Ship") %>%
#     select(-`Province/State`, -Lat, -Long) %>%
#     mutate_at(vars(-group_cols()), sum) %>%
#     distinct() %>%
#     ungroup() %>%
#     rename(country = `Country/Region`) %>%
#     pivot_longer(
#       -country,
#       names_to = "date_str",
#       values_to = var_str
#     ) %>%
#     mutate(date = mdy(date_str)) %>%
#     select(country, date, !! sym(var_str))
# }
#
# confirmed_raw <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
# deaths_raw <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
#
#
# jh_covid19_data <- clean_jhd_to_long(confirmed_raw) %>%
#   full_join(clean_jhd_to_long(deaths_raw))
#
#
# # Next, I pull official country level indicators from the UN Statstics Division
# # to get country level identifiers.
#
# jhd_countries <- tibble(country = unique(jh_covid19_data$country)) %>% arrange(country)
#
# ctry_ids <- read_html("https://unstats.un.org/unsd/methodology/m49/") %>%
#   html_table()
# un_m49 <- ctry_ids[[1]]
# colnames(un_m49) <- c("country", "un_m49", "iso3c")
#
#
# # Merging by country name is messy. I start with a fuzzy matching approach
# # using the {stringdist} package
#
# ctry_names_dist <- matrix(NA, nrow = nrow(jhd_countries), ncol = nrow(un_m49))
# for(i in 1:length(jhd_countries$country)) {
#   for(j in 1:length(un_m49$country)) {
#     ctry_names_dist[i,j]<-stringdist(tolower(jhd_countries$country[i]),
#                                      tolower(un_m49$country[j]))
#   }
# }
#
# min_ctry_name_dist <- apply(ctry_names_dist, 1, min)
#
# matched_ctry_names <- NULL
#
# for(i in 1:nrow(jhd_countries)) {
#   un_m49_row <- match(min_ctry_name_dist[i], ctry_names_dist[i,])
#   if (length(which(ctry_names_dist[i,] %in% min_ctry_name_dist[i])) > 1) un_m49_row <- NA
#   matched_ctry_names <- rbind(matched_ctry_names,
#                               tibble(
#                                 jhd_countries_row = i,
#                                 un_m49_row = un_m49_row,
#                                 jhd_ctry_name = jhd_countries$country[i],
#                                 un_m49_name = ifelse(is.na(un_m49_row), NA,
#                                                      un_m49$country[un_m49_row])
#                               ))
# }
#
# # This matches most cases well but some cases need to be adjusted by hand.
# # In addition there are two jurisdictions (Kosovo, Taiwan)
# # that cannot be matched as they are no 'country' as far as the U.N.
# # Statistics Devision is concerned.
#
# # WATCH OUT: The data from JHU is subject to change without notice.
# # New countries are being added and names/spelling might change.
# # Also, in the long run, the data provided by the UNSD might change.
# # Inspect 'matched_ctry_names' before using the data.
#
# matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Bolivia"] <- 27
# matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Brunei"] <- 35
# matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Congo (Brazzaville)"] <- 54
# matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Congo (Kinshasa)"] <- 64
# matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "East Timor"] <- 222
# matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Iran"] <- 109
# matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Korea, South"] <- 180
# matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Kosovo"] <- NA
# matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Moldova"] <- 181
# matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Russia"] <- 184
# matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Taiwan*"] <- NA
# matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Tanzania"] <- 236
# matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "United Kingdom"] <- 235
# matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "US"] <- 238
# matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Venezuela"] <- 243
#
# # Last Step: Match country identifier data and save file (commented out here)
# jhd_countries %>%
#   left_join(matched_ctry_names %>%
#               select(jhd_ctry_name, un_m49_row),
#             by = c(country = "jhd_ctry_name")) %>%
#   left_join(un_m49 %>% mutate(un_m49_row = row_number()), by = "un_m49_row") %>%
#   rename(country = country.x) %>%
#   select(country, iso3c)  -> jhd_countries
#
# jh_covid19_data <- jh_covid19_data %>% left_join(jhd_countries) %>%
#   select(country, iso3c, date, confirmed, deaths)
#
# totals = jh_covid19_data %>% filter(county = "US")
# # write_csv(jh_covid19_data, sprintf("jh_covid19_data_%s.csv", Sys.Date()))
# in


########################
######## DYGRAPH #######
########################
# Filter the input data to LA County
subset = filter(covid19, fips == 6037)

# DYGRAPHS uses the rownames of a data.frame to define the X-AXIS of a plot
# So lets set the rownames of the "subset" to the date attribute
rownames(subset) <- subset$date

# Select only the cases and deaths from the subset data to plot and provide a title and axis labels for the graph
dygraph(data = select(subset, cases, deaths),
        main = paste0("COVID-19 Trend: ", subset$name[1]),
        ylab = 'Number of Cases/Deaths',
        xlab = 'Date') %>%
  # This options shades the area under the line plots
  # First we define the size of the circles, then th opacity of the shading, and last the size of the line
  dyHighlight(highlightCircleSize = 4,
              highlightSeriesBackgroundAlpha = .7,
              highlightSeriesOpts = list(strokeWidth = 3)) %>%
  dyOptions(stackedGraph = TRUE,
            colors = c("darkcyan", "darkred"))

########################
######## LEAFLET #######
########################

# define a unique color pallete using the name you select
pal = colorNumeric("YlOrRd", domain = today$size, n = 100)


map =leaflet(data = today) %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  addScaleBar("bottomleft") %>%
  addCircleMarkers(
    fillColor = ~pal(size), # the cicle inside color
    color = 'black',
    weight = 0.2,  # the circle border color
    fillOpacity = 0.5,      # The opacity of circle inside color
    radius = ~size*2,       # the circle size
    layerId = ~fips,        # the circle ID (needed for Shiny!)
    label   = ~name         # how should the markers be labeled?
  )

map

######## MAKE DATA TABLES ######

# OPTION 1: Sorting a table by state & counts:

# Fitler today's data to the LA County
myfip  = filter(today, fips == 6037)

# Filter todays data to the state of myfip
mydata = filter(today, state == myfip$state) %>%
  arrange(desc(cases)) %>% # Arrange the cases from largest to smallest
  st_drop_geometry() %>%
  select(County = county, Cases = cases, Deaths = deaths) %>% # Keep only the County name, cases and deaths
  mutate(DeathRate = paste0(100* round(Deaths/Cases,2), "%")) # Create a new variable called Death Rate and save it as a string for printing


# Make an interactive Table!
datatable(mydata,
          caption = paste('COVID-19 Statistics', myfip$state, myfip$date))


# OPTION 2: webscraping

myfip = filter(today, fips == 6037)
# Build a Wikipedia URL using the data attribute 'name'
url = paste0('https://en.wikipedia.org/wiki/',  gsub(" ", "_", myfip$name))
# Steps:
## 1. Read the HTML from the URL
## 2. Read the Nodes from the HTML page and select the table with a class infobox
## 3. Convert raw HTML node to a data.frame
links = read_html(url) %>%
  html_nodes("table.infobox") %>%
  html_table(fill= TRUE)
# Keep only the first element of the list (we dont care about the HTML stuff)
data = links[[1]]

## Make a datatable from the resulting data.frame and turn off paging
datatable(data,
          caption = paste('Wikipedia Information:', myfip$name),
          colnames = rep("", ncol(data)))

















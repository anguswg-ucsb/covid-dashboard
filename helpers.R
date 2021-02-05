
# Data Manipulation
library(tidyverse)    # data.frames
library(sf)       # Spatial

# Interactive Data Viz
library(leaflet)  # Maps
library(dygraphs) # Charts
library(DT)       # tables
library(rvest)    # webscraping
library(plotly)
library(zoo)

# Shiny
library(dqshiny)    # auto complete
library(shiny)       # Starting Reactivity
library(dashboardthemes) # themes
library(shinydashboard)

# # county-level timeseries  COVID-19 cases and deaths
#
# url ='https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
#
# # Here we are reading the URL directly using the url variable created above
# covid19 = read.csv(url, stringsAsFactors = FALSE)
#
# covid19  = covid19 %>%
#   mutate(date = as.Date(date),
#          name = paste0(county, " County, ", state))
#
# # Filter the covid19 data to only those records were fips equals 6037
# test = filter(covid19, fips == 6037)
#
# # Plot the data
# plot(x = test$date, # define the x axis variable using all the dates in test
#      y = test$cases, # define the y axis variable
#      type  = "l", # we wnat a line "l" plot
#      xlab = "Date", ylab = "Cases", # refine the x and y labels
#      main = test$name[1]) # define the plot title.
#
#
# # US county geometries
# counties = readRDS("./data/counties.rds")
#
#
# today = filter(covid19, date == max(date)) %>% # filter all data to the maximum datae
#   left_join(st_centroid(counties), by = 'fips') %>%  # cast county geometries to centroid, and join by fip
#   na.omit() %>% # remove NA values
#   mutate(size = abs(cases - mean(cases)) / sd(cases)) # Compute a scaled case count
#
#
#
# # Plot and size (cex) the cnetroid data
# plot(today$geometry, cex = today$size)

########## FUNCTIONS ###########
#Load Spatial Data
counties = readRDS("counties.rds")

# Read in COVID-19 Timesries from URL
read_covid19 = function(){
  url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
  cc = read.csv(url, stringsAsFactors = FALSE) %>%
    mutate(date = as.Date(date),
           fips = as.numeric(fips),
           name = paste0(county, " County, ", state))
}

# Join County Data with
today_centroids = function(counties, covid_data){
  filter(covid_data, date == max(date)) %>%
    left_join(st_centroid(counties), by = 'fips') %>%
    na.omit() %>%
    mutate(size = abs(cases - mean(cases)) / sd(cases)) %>%
    st_as_sf()
}


basemap = function(today){
  pal = colorNumeric("viridis", domain = today$size, n = 50)
  pal2 <- colorNumeric("viridis", domain = today$cases, n = 50)
  leaflet(data = today) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addScaleBar("bottomleft") %>%
    addCircleMarkers(
      fillColor = ~pal(size),
      color = 'black',
      weight = 0.2,
      fillOpacity = 0.5,
      radius = ~size*2,
      layerId = ~fips,
      label   = ~name) %>%
    addLegend("bottomright",
              pal = pal2,
              values = ~cases,
              title = paste("COVID Cases\n", max(today$date)),
              opacity = 1) %>%
    setView(lng = -98, lat= 38, zoom = 4)
}


zoom_to_county = function(map, counties, FIP){
  # Filter the counties to the input FIP code
  shp = filter(counties, fips == FIP)
  # Build a buffered bounding box to center the map on:
  bounds = shp %>%
    # make bounding box
    st_bbox() %>%
    # Make spatial
    st_as_sfc() %>%
    # Buffer to .1 degree
    st_buffer(.1) %>%
    # make new bounding box
    st_bbox() %>%
    # extract coordinates as vector
    as.vector()
  # Clear all current shapes (remember county centroids are currently markers!)
  clearShapes(map) %>%
    # Add the county shape making the outline color red and the fill an opaque white
    addPolygons(data = shp,
                color = "red",
                fillColor  = "grey",
                fillOpacity = .3) %>%
    # Fly the leaflet map to the buffered boundary
    flyToBounds(bounds[1], bounds[2], bounds[3], bounds[4])
}

make_table = function(today, FIP){
  myfip = filter(today, fips == FIP)

  url = paste0('https://en.wikipedia.org/wiki/',  gsub(" ", "_", myfip$name)) %>%
    read_html() %>%
    html_nodes("table.infobox") %>%
    html_table(fill= TRUE)

  l = url[[1]]
  # remove rows where the columns are identical
  ll = l[!l[,1] == l[,2],]

  ## Make a datatable from the resulting data.frame and turn off paging
  datatable(ll,
            caption = paste('Wikipedia Information:', myfip$name),
            # Turn off some of the interactive components for this table.
            options = list(paging = FALSE, searching = FALSE, ordering = FALSE),
            colnames = rep("", ncol(ll)))
}

## Build State Ranking table
make_table2 = function(today, FIP){
  myfips = filter(today, fips == FIP)

  # Filter todays data to the state of the input FIP
  mydata = filter(today, state == myfips$state) %>%
    arrange(desc(cases)) %>%
    st_drop_geometry() %>%
    select(County = county, Cases = cases, Deaths = deaths) %>%
    mutate(DeathRate = paste0(100* round(Deaths/Cases,2), "%")) %>%
    head(10)

  # Make an interactive Table! with a caption
  datatable(mydata, caption = paste('COVID-19 Statistics', myfips$state, myfips$date),
            options = list(paging = FALSE, searching = FALSE))
}

make_table_2 = function(today, FIP){
  myfip  = filter(today, fips == FIP)

# Filter todays data to the state of myfip
  mydata = filter(today, state == myfip$state) %>%
    arrange(desc(cases)) %>% # Arrange the cases from largest to smallest
    st_drop_geometry() %>%
    select(County = county, Cases = cases, Deaths = deaths) %>% # Keep only the County name, cases and deaths
    mutate(DeathRate = paste0(100* round(Deaths/Cases,2), "%")) # Create a new variable called Death Rate and save it as a string for printing



  datatable(mydata,
          caption = paste('COVID-19 Statistics', myfip$state, myfip$date))

}

# The graph requires you COVID data and a FIP code as input
make_graph = function(covid19, FIP){

  subset = filter(covid19, fips == FIP)
  rownames(subset) <- subset$date

  # Fit and exponetial model for fun
  exponential.model <- lm(log(cases)~ date, data = subset)
  # use the model to predict a what a expoential curve would look like
  subset$expCases = ceiling(exp(predict(exponential.model, list(date = subset$date))))

  dygraph(data = select(subset, cases, deaths, expCases),
          main = paste0(" ", subset$name[1]),
          ylab = 'CASES/DEATHS',
          xlab = 'DATE') %>%
    dyHighlight(highlightCircleSize = 4,
                highlightSeriesBackgroundAlpha = .7,
                highlightSeriesOpts = list(strokeWidth = 3)) %>%
    dyOptions(colors = c("darkcyan", "darkred", 'black'), strokeWidth = 3, stackedGraph = TRUE)
}

# COUNTY DAILY CASES GRAPH
daily_cases_graph = function(covid19, FIP){
  subset2 <- covid19 %>% filter(fips == FIP)

  subset2 <- subset2 %>%
    group_by(state, date) %>%
    summarise(county = county, fips = fips, cases = sum(cases, na.rm = TRUE)) %>%
    mutate(new_cases = cases - lag(cases)) %>%
    mutate(rolling_mean = rollmean(new_cases, 7, fill = NA, align = 'right'))

  gg_1 = ggplot(subset2, aes(x = date, y = new_cases)) +
    geom_col(col = 'aquamarine4', fill = 'aquamarine3') +
    geom_line(aes(y = rolling_mean), col = "darkgreen", size = 0.5) +
    labs(x = 'DATE',
         y = 'DAILY CASES',
         subtitle = 'Data Source: The New York Times') +
    theme_classic() +
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
}


total_cases_graph = function(covid19, FIP){
  subset4 <- covid19 %>% filter(fips == FIP)

  gg_3 <- ggplot(subset4, aes(date, cases)) +
    geom_col(col = 'aquamarine4', fill = 'aquamarine3') +
    labs(x = 'DATE',
         y = 'CASES') +
    theme_classic() +
    theme(axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10, hjust = 0.5),
          plot.title = element_text(hjust = 0.5, size = 20),
          plot.subtitle = element_text(size = 14),
          legend.title = element_text(face = "bold", size = 10, hjust = 0.5),
          legend.title.align = 0.5,
          legend.text = element_text(face = "bold", size = 12))

  ggplotly(gg_3)

}

total_deaths_graph = function(covid19, FIP){
  subset4 <- covid19 %>% filter(fips == FIP)

  gg_3 <- ggplot(subset4, aes(date, deaths)) +
    geom_col(col = 'aquamarine4', fill = 'aquamarine3') +
    labs(x = 'DATE',
         y = 'DEATHS') +
    theme_classic() +
    theme(axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10, hjust = 0.5),
          plot.title = element_text(hjust = 0.5, size = 20),
          plot.subtitle = element_text(size = 14),
          legend.title = element_text(face = "bold", size = 10, hjust = 0.5),
          legend.title.align = 0.5,
          legend.text = element_text(face = "bold", size = 12))

  ggplotly(gg_3)

}

daily_deaths_graph = function(covid19, FIP){
  # NEW DEATHS --- COUNTY
  subset3 <- covid19 %>% filter(fips == FIP)

  subset3 <- subset3 %>%
    group_by(state, date) %>%
    summarise(county = county, fips = fips, deaths = sum(deaths, na.rm = TRUE)) %>%
    mutate(new_deaths = deaths - lag(deaths)) %>%
    mutate(rolling_mean = rollmean(new_deaths, 7, fill = NA, align = 'right')) %>%
    filter(new_deaths >= 0, rolling_mean >= 0)

  gg_1 = ggplot(subset3, aes(x = date, y = new_deaths)) +
    geom_col(col = 'aquamarine4', fill = 'aquamarine3') +
    geom_line(aes(y = rolling_mean), col = "darkgreen", size = 0.5) +
    labs(x = 'DATE',
         y = 'DEATHS') +
    theme_classic() +
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

}

cases_info = function(covid19){
  subset3 <- covid19 %>%
    group_by(date) %>%
    summarise(deaths = sum(deaths, na.rm = TRUE), cases = sum(cases, na.rm = TRUE)) %>%
    mutate(new_cases = cases - lag(cases)) %>%
    arrange(desc(date)) %>%
    slice_max(1) %>%
    select(2:4) %>%
    formatC(format="d", big.mark=",")
}

death_info = function(covid19){
  subset4 <- covid19 %>%
    group_by(date) %>%
    summarise(deaths = sum(deaths, na.rm = TRUE), cases = sum(cases, na.rm = TRUE)) %>%
    mutate(new_cases = cases - lag(cases)) %>%
    mutate(death_rate = 100*round(deaths/cases, 3)) %>%
    arrange(desc(date)) %>%
    slice_max(1)
}



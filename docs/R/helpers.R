
# Data Manipulation
library(dplyr)    # data.frames
library(sf)       # Spatial

# Interactive Data Viz
library(leaflet)  # Maps
library(dygraphs) # Charts
library(DT)       # tables
library(rvest)    # webscraping

# Shiny
library(dqshiny)    # auto complete
library(shiny)       # Starting Reactivity
library(shinythemes) # themes

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
  read.csv(url, stringsAsFactors = FALSE) %>%
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

# The graph requires you COVID data and a FIP code as input
make_graph = function(covid19, FIP){

  subset = filter(covid19, fips == FIP)
  rownames(subset) <- subset$date

  # Fit and exponetial model for fun
  exponential.model <- lm(log(cases)~ date, data = subset)
  # use the model to predict a what a expoential curve would look like
  subset$expCases = ceiling(exp(predict(exponential.model, list(date = subset$date))))


  # !!!! This is were you put you code!!!!

  dygraph(data = select(subset, cases, deaths, expCases),
          main = paste0("COVID-19 Trend: ", subset$name[1]),
          ylab = 'Number of Cases/Deaths',
          xlab = 'Date') %>%
    dyHighlight(highlightCircleSize = 4,
                highlightSeriesBackgroundAlpha = .7,
                highlightSeriesOpts = list(strokeWidth = 3)) %>%
    dyOptions(colors = c("darkcyan", "darkred", 'black'))
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
              opacity = 1)
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

make_table = function(today, FIPS){
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

make_table_2 = function(today, FIPS){
  myfip  = filter(today, fips == 6037)

# Filter todays data to the state of myfip
  mydata = filter(today, state == myfip$state) %>%
    arrange(desc(cases)) %>% # Arrange the cases from largest to smallest
    st_drop_geometry() %>%
    select(County = county, Cases = cases, Deaths = deaths) %>% # Keep only the County name, cases and deaths
    mutate(DeathRate = paste0(100* round(Deaths/Cases,2), "%")) # Create a new variable called Death Rate and save it as a string for printing



  datatable(mydata,
          caption = paste('COVID-19 Statistics', myfip$state, myfip$date))

}



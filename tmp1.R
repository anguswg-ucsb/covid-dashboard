
source('docs/R/helpers.R')
counties = readRDS("./data/counties.rds")
covid19 = read_covid19()
today   = today_centroids(counties, covid19)














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

















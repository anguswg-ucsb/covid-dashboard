


source('docs/R/helpers.R')

counties = readRDS("counties.rds")
covid19 = read_covid19()

today   = today_centroids(counties, covid19)
counties = readRDS("counties.rds")
pop = readxl::read_excel('PopulationEstimates.xls', skip = 2) %>%
  select(state = State, pop_19 = POP_ESTIMATE_2019, fips = FIPStxt)
pop$fips <- as.numeric(pop$fips)

tmp1 <- filter(covid19, date == max(date)) %>%
  left_join(st_centroid(counties), by = 'fips') %>%
  na.omit() %>%
  mutate(size = abs(cases - mean(cases)) / sd(cases)) %>%
  st_as_sf()

circs <- covid19 %>%
  left_join(st_centroid(counties), by = 'fips') %>%
  na.omit()

circ2 <- circs %>%
  group_by(county, state) %>%
  filter(date > max(date) - 8) %>%
  arrange(date) %>%
  mutate(new_cases = cases - lag(cases))

circ2 = inner_join(circ2, select(pop, pop_19, fips), by = 'fips')

circ3 <- circ2 %>%
  mutate(cases_per_100k = (new_cases /pop_19)*100000) %>%
  na.omit() %>%
  mutate(rmean_new = rollmean(new_cases, 7, fill = NA, align = 'right'),
         rmean_percap = rollmean(cases_per_100k, 7, fill = NA, align = 'right')) %>%
  na.omit()


circ4 <- circ3 %>%
  filter(rmean_new >= 0.0,
         rmean_percap >= 0.0) %>%
  ungroup() %>%
  mutate(size2 = abs(rmean_new - mean(rmean_new)) / sd(rmean_new),
         size3 = abs(rmean_percap - mean(rmean_percap)) / sd(rmean_percap)) %>%
  filter(state != "Alaska") %>%
  st_as_sf()

pal = colorNumeric("inferno", reverse= TRUE, domain = today$size, n = 50)
pal2 <- colorNumeric("inferno", reverse = TRUE, domain = today$cases, n = 50)
pal3 = colorNumeric("viridis", reverse= TRUE, domain = circ4$size3, n = 50)
pal4 <- colorNumeric("viridis", reverse = TRUE, domain = circ4$rmean_percap, n = 50)

pal5 = colorNumeric("viridis", reverse= TRUE, domain = circ4$size2, n = 50)
pal6 <- colorNumeric("viridis", reverse = TRUE, domain = circ4$rmean_new, n = 50)
leaflet(data = circ4) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addScaleBar("bottomleft") %>%
  # addCircleMarkers(
  #   fillColor = ~pal4(rmean_percap),
  #   color = 'black',
  #   weight = 0.2,
  #   fillOpacity = 0.5,
  #   radius = ~size3*2,
  #   layerId = ~fips,
  #   label   = ~name) %>%
  addCircleMarkers(
    fillColor = ~pal5(size2),
    color = 'black',
    weight = 0.2,
    fillOpacity = 0.5,
    radius = ~size2*2,
    layerId = ~fips,
    label   = ~name) %>%
  addLegend("bottomright",
            pal = pal4,
            values = ~rmean_new,
            title = paste("COVID Cases\n", max(today$date)),
            opacity = 1) %>%
  setView(lng = -98, lat= 38, zoom = 4)
# basemap = function(today){

pal = colorNumeric("inferno", reverse= TRUE, domain = today$size, n = 50)
pal2 <- colorNumeric("inferno", reverse = TRUE, domain = today$cases, n = 50)
pal3 = colorNumeric("viridis", reverse= TRUE, domain = circ4$size3, n = 50)
pal4 <- colorNumeric("viridis", reverse = TRUE, domain = circ4$rmean_percap, n = 50)

pal5 = colorNumeric("viridis", reverse= TRUE, domain = circ4$size2, n = 50)
pal6 <- colorNumeric("viridis", reverse = TRUE, domain = circ4$rmean_new, n = 50)

pop = readxl::read_excel('PopulationEstimates.xls', skip = 2) %>%
  select(state = State, pop_19 = POP_ESTIMATE_2019, fips = FIPStxt)
pop$fips <- as.numeric(pop$fips)


rollmean_map <- covid19 %>%
  left_join(st_centroid(counties), by = 'fips') %>%
  na.omit()

rollmean_map <- rollmean_map %>%
  group_by(county, state) %>%
  filter(date > max(date) - 8) %>%
  arrange(date) %>%
  mutate(new_cases = cases - lag(cases))

rollmean_map = inner_join(rollmean_map, select(pop, pop_19, fips), by = 'fips')

rollmean_map <- rollmean_map %>%
  mutate(cases_per_100k = (new_cases /pop_19)*100000) %>%
  na.omit() %>%
  mutate(rmean_new = rollmean(new_cases, 7, fill = NA, align = 'right'),
         rmean_percap = rollmean(cases_per_100k, 7, fill = NA, align = 'right')) %>%
  na.omit()

rollmean_map <- rollmean_map %>%
  filter(rmean_new >= 0.0,
         rmean_percap >= 0.0) %>%
  ungroup() %>%
  mutate(size2 = abs(rmean_new - mean(rmean_new)) / sd(rmean_new),
         size3 = abs(rmean_percap - mean(rmean_percap)) / sd(rmean_percap)) %>%
  filter(state != "Alaska") %>%
  st_as_sf()

today_centroids_2 <- function(counties, pop, covid19) {
  rollmean_map <- covid19 %>%
    left_join(st_centroid(counties), by = 'fips') %>%
    na.omit()

  rollmean_map <- rollmean_map %>%
    group_by(county, state) %>%
    filter(date > max(date) - 8) %>%
    arrange(date) %>%
    mutate(new_cases = cases - lag(cases))

  rollmean_map = inner_join(rollmean_map, select(pop, pop_19, fips), by = 'fips')

  rollmean_map <- rollmean_map %>%
    mutate(cases_per_100k = (new_cases /pop_19)*100000) %>%
    na.omit() %>%
    mutate(rmean_new = rollmean(new_cases, 7, fill = NA, align = 'right'),
           rmean_percap = rollmean(cases_per_100k, 7, fill = NA, align = 'right')) %>%
    na.omit()

  rollmean_map <- rollmean_map %>%
    filter(rmean_new >= 0.0,
           rmean_percap >= 0.0) %>%
    ungroup() %>%
    mutate(size2 = abs(rmean_new - mean(rmean_new)) / sd(rmean_new),
           size3 = abs(rmean_percap - mean(rmean_percap)) / sd(rmean_percap)) %>%
    filter(state != "Alaska") %>%
    st_as_sf()
}

rollmeanmap <- function(rollmean_map) {
  pal3 = colorNumeric("viridis", reverse= TRUE, domain = rollmean_map$size3)
  pal4 <- colorNumeric("viridis", reverse = TRUE, domain = rollmean_map$rmean_percap)
  leaflet(data = rollmean_map) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addScaleBar("bottomleft") %>%
      addCircleMarkers(
        fillColor = ~pal3(size3),
        color = 'black',
        weight = 0.2,
        fillOpacity = 0.5,
        radius = ~size3*2,
        layerId = ~fips,
        label   = ~name) %>%
      addLegend("bottomright",
                pal = pal4,
                values = ~rmean_percap,
                title = paste("New cases/100k \n", max(today$date)),
                opacity = 1) %>%
      setView(lng = -98, lat= 38, zoom = 4)
}
# }

# per_capita_graph <- function(covid19, FIPS) {
cases_per_cap <- covid19 %>%
  group_by(county, state) %>%
  filter(date > max(date) - 8) %>%
  arrange(date) %>%
  mutate(new_cases = cases - lag(cases))

cases_per_cap = inner_join(cases_per_cap, select(pop, pop_19, fips), by = 'fips')

cases_per_cap <- cases_per_cap %>%
  mutate(cases_per_100k = (new_cases /pop_19)*100000) %>%
  na.omit() %>%
  mutate(cases_100k = (sum(cases_per_100k)/(7)))

cases_per_cap <- cases_per_cap %>%
  left_join(st_centroid(counties), by = 'fips') %>%
  st_as_sf()

tmp1 <- cases_per_cap %>%
  slice(n=7)

counties <- USAboundaries::us_counties() %>%
  filter(!state_name %in% c("Alaska", "Hawaii", "Puerto Rico"))

covid_spatial = covid19 %>%
  group_by(county) %>%
  arrange(desc(date)) %>%
  slice(n = 1)


# SELECT DESIRED COLUMNS FROM COUNTIES SF DATAFRAME

counties = counties %>%
  select(state = state_name, county = name, fips = geoid)

counties$fips <- as.numeric(counties$fips)
# counties = counties %>% select(state, county, fips)

# JOIN DATAFRAME OF 58 COUNTIES W/ RESPECTIVE GEOMETRIES
covid_spatial = left_join(covid_spatial, select(counties, fips), by = 'fips') %>%
  st_as_sf()

covid_spatial <-  covid_spatial %>%
  filter(!state %in% c("Alaska", "Hawaii", "Puerto Rico"))


df1 = cases_per_cap %>%
  group_by(county) %>%
  arrange(desc(date)) %>%
  slice(n =1)

covid_spatial = left_join(covid_spatial, select(df1, county, cases_100k), by = 'county')

ggplot() +
  geom_sf(data = conus) +
  geom_sf(data = tmp1, aes(fill = cases_100k))



conus <- USAboundaries::us_counties()
class(conu)


tmp3 = inner_join(tmp3, select(pop, pop_19, fips), by = 'fips')

cases_per_cap <- tmp3 %>%
  filter(date > max(date) - 7) %>%
  group_by(county, pop_19) %>%
  mutate(cases_per_100k = (new_cases /pop_19)*100000) %>%
  mutate(cases_100k = (sum(cases_100k)/(7))) %>%
  mutate(state = 'CA')
hist(cases_per_cap$cases_per_100k, breaks = 100)
pal = colorNumeric("inferno", reverse= TRUE, domain = today$size, n = 50)
pal2 <- colorNumeric("inferno", reverse = TRUE, domain = today$cases, n = 50)
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
states <- geojsonio::geojson_read("json/us-states.geojson", what = "sp")
m <- leaflet(conus) %>%
  setView(-96, 37.8, 4) %>%
  addTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
leaflet(conus) %>%
  addTiles(conus) %>%
  addPolygons(weight = 2, fillOpacity = 0.5)
pal = colorNumeric("inferno", reverse= TRUE, domain = conus$aland, n = 4000)
pal2 <- colorNumeric("inferno", reverse = TRUE, domain = today$cases, n = 50)
leaflet()%>%
  addProviderTiles("OpenStreetMap.Mapnik")%>%
  setView(lat = 39.8283, lng = -98.5795, zoom = 4) %>%
  addPolygons(
    data = conus,
    fillColor = ~pal(conus$aland))
# NEW CASES --- COUNTY
subset2 <- covid19 %>% filter(fips == FIP)

subset2 <- subset2 %>%
  group_by(state, date) %>%
  summarise(county = county, fips = fips, cases = sum(cases, na.rm = TRUE)) %>%
  mutate(new_cases = cases - lag(cases)) %>%
  mutate(rolling_mean = rollmean(new_cases, 7, fill = NA, align = 'right'))

# Fit and exponetial model for fun
# exponential.model <- lm(log(new_cases)~ date, data = subset)
# # use the model to predict a what a expoential curve would look like
# subset$expCases = ceiling(exp(predict(exponential.model, list(date = subset$date))))


gg_1 = ggplot(subset2, aes(x = date, y = new_cases)) +
  geom_col(col = 'Brick Red', fill = 'aquamarine3') +
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

#################################################################

# NEW DEATHS --- COUNTY
subset3 <- covid19 %>% filter(fips == 6001)

subset3 <- subset3 %>%
  group_by(state, date) %>%
  summarise(county = county, fips = fips, deaths = sum(deaths, na.rm = TRUE)) %>%
  mutate(new_deaths = deaths - lag(deaths)) %>%
  mutate(rolling_mean = rollmean(new_deaths, 7, fill = NA, align = 'right')) %>%
  filter(new_deaths >= 0, rolling_mean >= 0)

gg_1 = ggplot(subset3) +
  geom_col(aes(date, new_deaths),fill = 'tomato3', col = "firebrick4", size = 0.1, alpha = 0.5) +
  geom_line(aes(date, y = rolling_mean), col = "darkred", size = 0.6) +
  labs(x = 'DATE',
       y = 'DAILY DEATHS',
       subtitle = 'Data Source: The New York Times') +
  # scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
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
ggplotly(gg_1, tooltip = c("x", "y")) %>%
  style(hoverlabel = label) %>%
  config(displayModeBar = FALSE)

font = list(
  family = 'Arial',
  size = 15,
  color = 'white')
label = list(
  bgcolor = '#232F34',
  bordercolor = 'transparent',
  font = font)
#################################################################
total_cases_graph = function(covid19, FIP){
  subset4 <- covid19 %>% filter(fips == FIP)

  gg_3 <- ggplot(subset4, aes(date, cases)) +
    geom_col(col = 'aquamarine4', fill = 'aquamarine3') +
    labs(x = 'DATE',
         y = 'TOTAL DEATHS') +
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
         y = 'DEATHS',
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
# CUMULATIVE CASES --- COUNTY
subset4 <- covid19 %>% filter(fips == 6001)


gg_3 <- ggplot(subset4, aes(date, cases)) +
    geom_col(col = 'aquamarine4', fill = 'aquamarine3') +
     labs(x = 'DATE',
       y = 'DAILY DEATHS',
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

ggplotly(gg_3)

##############################################################

# TOTAL CASES --- USA
usa_total_cases = function(covid19){
  total_cases <- covid19 %>%
    group_by(date) %>%
    summarize(cases = sum(cases, na.rm = TRUE)) %>%
    arrange(desc(date)) %>%
    slice(n = 1:320) %>%
    rename(Date = date)

  usa_cases = ggplot(total_cases) +
    geom_col(aes(x = Date, y = cases), fill = 'skyblue3',
             # col = 'black',
             # size = 0.1,
             alpha = 0.6) +
    labs(x = '',
         y = '') +
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA), labels = comma) +
    theme_classic() +
    theme(axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8))
  ggplotly(usa_cases, tooltip = c("x", "y")) %>%
    style(hoverlabel = label) %>%
    config(displayModeBar = FALSE)

}
# TOTAL DEATHS --- USA
usa_total_deaths = function(covid19){
  total_deaths <- covid19 %>%
    group_by(date) %>%
    summarize(deaths = sum(deaths, na.rm = TRUE)) %>%
    arrange(desc(date)) %>%
    slice(n = 1:320) %>%
    rename(Date = date)

  usa_deaths = ggplot(total_deaths) +
    geom_col(aes(x = Date, y = deaths), fill = 'tomato3',
             # col = "darkred",
             # size = 0.1,
             alpha = 0.5) +
    labs(x = '',
         y = '') +
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA), labels = comma) +
    theme_classic() +
    theme(axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8))
  ggplotly(usa_deaths, tooltip = c("x", "y")) %>%
    style(hoverlabel = label) %>%
    config(displayModeBar = FALSE)

}

total_cases <- covid19 %>%
  group_by(date) %>%
  summarize(cases = sum(cases, na.rm = TRUE)) %>%
  arrange(desc(date)) %>%
  slice(n = 1:320) %>%
  rename(Date = date)

usa_cases = ggplot(total_cases) +
  geom_col(aes(x = Date, y = cases), fill = 'skyblue3',
           # col = 'black',
           # size = 0.1,
           alpha = 0.6) +
  labs(x = '',
       y = '') +
  # scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA), labels = comma) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))
ggplotly(usa_cases, tooltip = c("x", "y")) %>%
  style(hoverlabel = label) %>%
  config(displayModeBar = FALSE)

##############################################################

# TOTAL DEATHS --- USA

total_deaths <- covid19 %>%
  group_by(date) %>%
  summarize(deaths = sum(deaths, na.rm = TRUE)) %>%
  arrange(desc(date)) %>%
  slice(n = 1:320) %>%
  rename(Date = date)

usa_deaths = ggplot(total_deaths) +
  geom_col(aes(x = Date, y = deaths), fill = 'tomato3',
           # col = "darkred",
           # size = 0.1,
           alpha = 0.5) +
  labs(x = '',
       y = '') +
  # scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA), labels = comma) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))
ggplotly(usa_deaths, tooltip = c("x", "y")) %>%
  style(hoverlabel = label) %>%
  config(displayModeBar = FALSE)










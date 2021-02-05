


source('docs/R/helpers.R')
counties = readRDS("./data/counties.rds")
covid19 = read_covid19()
today   = today_centroids(counties, covid19)

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










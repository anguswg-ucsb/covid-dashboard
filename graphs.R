


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

#################################################################

# NEW DEATHS --- COUNTY
subset3 <- covid19 %>% filter(fips == 6001)

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
ggplotly(gg_1)

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

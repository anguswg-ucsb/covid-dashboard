


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

covid19 <- covid19 %>%
  group_by(state) %>%
  mutate(id = row_number())

new_cases <- new_cases %>%
  group_by(state) %>%
  mutate(id = row_number())

subset <- new_cases %>% filter(fips == 6019)

library(zoo)
library(plotly)

make_graph2 = function(covid19, FIP){
  new_cases <- covid19 %>%
    group_by(state, date) %>%
    summarise(cases = sum(cases, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(state) %>%
    mutate(new_cases = cases - lag(cases)) %>%
    mutate(rolling_mean = rollmean(new_cases, 7, fill = NA, align = 'right'))

  subset = filter(covid19, fips == FIP)
  rownames(subset) <- subset$date
  rm(new_cases)
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

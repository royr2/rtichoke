library(lubridate)
library(data.table)
library(dplyr)
library(tidyr)

# Update JHU data -------------------------------------------------------------
system("git submodule update --remote")

# +++++++++++++++++++++++++++++++++++++
# Daily data
# +++++++++++++++++++++++++++++++++++++

# List all files available ----------------------------------------------------
files <- list.files("jhu_data/COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/", pattern = "*.csv")

# Read only the date component ------------------------------------------------
files <- stringr::str_split(files, ".csv")
files <- sapply(files, "[", 1)

# Pick only those files that are within the last 6 months ---------------------
files <- as.Date(files, format = "%m-%d-%Y")
files <- files[files > Sys.Date() %m+% months(-6)]

# Paste with full location ----------------------------------------------------
files <- format(files, "%m-%d-%Y")
files <- paste0("jhu_data/COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/", files, ".csv")

# Initialise an empty data table ----------------------------------------------
daily_data <- data.table()

# Create column classes -------------------------------------------------------

col_select <- c(
  Province_State = "character",
  Country_Region = "character",
  Admin2 = "character",
  Last_Update = "character",
  Lat = "numeric",
  Long_ = "numeric",
  Confirmed = "numeric",
  Deaths = "numeric",
  Recovered = "numeric",
  Active = "numeric"
)

# Progress bar ----------------------------------------------------------------
pb <- txtProgressBar(min = 1, max = length(files), style = 3)
k <- 1

# Read in each file -----------------------------------------------------------
for(f in files){
  
  df <- fread(f, select = col_select)
  
  daily_data <- rbind(daily_data, df, fill = T)
  
  # Update progress bar
  setTxtProgressBar(pb, value = k)
  k <- k + 1
}

# Map 3 letter country codes --------------------------------------------------
codes <- fread("country_codes.csv")
codes[,Country_Code := `Alpha-3 code`]
codes[,Country_Region := Country]
codes <- codes[,.(Country_Region, Country_Code)]

daily_data <- codes[daily_data, on = .(Country_Region)]  

# Join Country level Lats and Longs --------------------------------------------
country_lat_long <- fread("country_lat_long.csv")
country_lat_long[,Country_Region := Country_Name]
country_lat_long <- country_lat_long[,.(Country_Region, Latitude, Longitude)]
daily_data <- country_lat_long[daily_data, on = .(Country_Region)]  

# Write to disk ---------------------------------------------------------------
daily_data %>% 
  mutate(Last_Update = as.Date(Last_Update, format = "%Y-%m-%d")) %>% 
  arrange(Country_Region, Province_State, Last_Update) %>% 
  group_by(Country_Region, Province_State, Admin2, Last_Update) %>% 
  filter(row_number() == 1) %>% 
  fwrite("daily_data.csv")

# +++++++++++++++++++++++++++++++++++++
# Time series
# +++++++++++++++++++++++++++++++++++++

# Daily TS confirmed Cases ----------------------------------------------------

df <- fread("jhu_data/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

# There are cases where the dataset has both country level numbers and state level numbers
# Solution is to separate the two groups and join them back removing the duplicates

x <- df[`Province/State` == ""]
x <- x %>% pivot_longer(cols = 5:ncol(x), names_to = "date", values_to = "confirmed")

y <- df[`Province/State` != ""]
y <- y %>% 
  pivot_longer(cols = 5:ncol(y), names_to = "date", values_to = "confirmed") %>% 
  arrange(`Province/State`, `Country/Region`) %>% 
  group_by(`Country/Region`, date) %>% 
  summarise(confirmed = sum(confirmed))

# Names of duplicated country names
names_x <- unique(x$`Country/Region`)
names_y <- unique(y$`Country/Region`)
exclude <- names_y[names_y %in% names_x]

# Final data set
bind_rows(x, y %>% filter(! `Country/Region` %in% exclude)) %>% 
  fwrite("daily_confirmed.csv")

# Daily TS Death Cases ----------------------------------------------------
df <- fread("jhu_data/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

# There are cases where the dataset has both country level numbers and state level numbers
# Solution is to separate the two groups and join them back removing the duplicates

x <- df[`Province/State` == ""]
x <- x %>% pivot_longer(cols = 5:ncol(x), names_to = "date", values_to = "deaths")

y <- df[`Province/State` != ""]
y <- y %>% 
  pivot_longer(cols = 5:ncol(y), names_to = "date", values_to = "deaths") %>% 
  arrange(`Province/State`, `Country/Region`) %>% 
  group_by(`Country/Region`, date) %>% 
  summarise(deaths = sum(deaths))

# Names of duplicated country names
names_x <- unique(x$`Country/Region`)
names_y <- unique(y$`Country/Region`)
exclude <- names_y[names_y %in% names_x]

# Final data set
bind_rows(x, y %>% filter(! `Country/Region` %in% exclude)) %>% 
  fwrite("daily_deaths.csv")

# Daily TS Recovered Cases ----------------------------------------------------
df <- fread("jhu_data/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")

# There are cases where the dataset has both country level numbers and state level numbers
# Solution is to separate the two groups and join them back removing the duplicates

x <- df[`Province/State` == ""]
x <- x %>% pivot_longer(cols = 5:ncol(x), names_to = "date", values_to = "recovered")

y <- df[`Province/State` != ""]
y <- y %>% 
  pivot_longer(cols = 5:ncol(y), names_to = "date", values_to = "recovered") %>% 
  arrange(`Province/State`, `Country/Region`) %>% 
  group_by(`Country/Region`, date) %>% 
  summarise(recovered = sum(recovered))

# Names of duplicated country names
names_x <- unique(x$`Country/Region`)
names_y <- unique(y$`Country/Region`)
exclude <- names_y[names_y %in% names_x]

# Final data set
bind_rows(x, y %>% filter(! `Country/Region` %in% exclude)) %>% 
  fwrite("daily_recovered.csv")

# Daily data for use in app ---------------------------------------------------
daily_confirmed <- fread("daily_confirmed.csv")
daily_death <- fread("daily_deaths.csv")
daily_recovered <- fread("daily_recovered.csv")
country_lat_long <- fread("country_lat_long.csv")

daily_data <- daily_confirmed %>% 
  left_join(daily_death) %>% 
  left_join(daily_recovered) %>% 
  rename("country" = `Country/Region`, 
         "lat" = Lat, 
         "long" = Long) %>% 
  left_join(country_lat_long %>% select(-Country), 
            by = c("country" = "Country_Name")) %>% 
  mutate(fatal_rate = ifelse(confirmed == 0, 0, deaths/confirmed)) %>% 
  
  # Make data plot ready
  mutate(confirmed_readable = scales::label_number_si(accuracy = 0.1)(confirmed), 
         deaths_readable = scales::label_number_si(accuracy = 0.1)(deaths), 
         recovered_readable = scales::label_number_si(accuracy = 0.1)(recovered), 
         fatal_readable = scales::percent(fatal_rate, accuracy = 0.1), 
         text = paste0("<b>", country, "</b><br>", 
                       "Confirmed: <b>", confirmed_readable, "</b><br>", 
                       "Death: <b>", deaths_readable, "</b><br>", 
                       "Fatality Ratio: <b>", fatal_readable, "</b><br>"))

fwrite(daily_data, "daily_data.csv")

# Chart data ------------------------------------------------------------------
plotdf <- daily_data %>%
  mutate(date = as.Date(date, "%m/%d/%y")) %>%
  arrange(country, date) %>%
  mutate(lag_confirmed = lag(confirmed),
         lag_deaths = lag(deaths),
         
         confirmed_daily = confirmed - lag_confirmed,
         deaths_daily = deaths - lag_deaths,
         deaths_daily = ifelse(deaths_daily < 0, 0, deaths_daily),
         
         confirmed_daily_lag = lag(confirmed_daily),
         deaths_daily_lag = lag(deaths_daily),
         
         confirmed_daily_growth = (confirmed_daily/confirmed_daily_lag - 1),
         deaths_daily_growth = (deaths_daily/deaths_daily_lag - 1))

fwrite(plotdf, "plot_data.csv")
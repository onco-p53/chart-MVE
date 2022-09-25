## R Script to process data exported from Chart MVE TS controller nitrogen tanks ##
# Author: B.S. Weir (2022)

# ==== Load all the packages needed ====

library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(skimr)
library(janitor)
library(ggrepel)

#this code *REQUIRES* R 4.2 or later due to UTF-8
R.version.string #check version

# ==== Load data ====

#load tank 1 data
tank1.df <- read_csv("39567300.CSV",
                     guess_max = Inf,
                     show_col_types = FALSE)
glimpse(tank1.df)

#load tank 2 data
tank2.df <- read_csv("39560200.CSV",
                     guess_max = Inf,
                     show_col_types = FALSE)
glimpse(tank2.df)


#load tank 3 data
tank3.df <- read_csv("39561500.CSV",
                     guess_max = Inf,
                     show_col_types = FALSE)
glimpse(tank3.df)

# ==== Clean the raw Data ====


## Tank 1 ----------------------------------------------------------

tank1clean.df <- tank1.df  |>
  slice(1:(n() - 109)) |> # cuts off early values from factory and cooling
  filter(!is.na(Level)) |> # removes change parameter lines
  filter(User == "0") |> # restrict to the main user
  mutate(Temp_top = str_remove_all(`Temp A`, "�C")) |>  # removes temperature symbol
  mutate(Temp_bottom = str_remove_all(`Temp B`, "�C")) |>
  mutate(Level = str_remove_all(Level, " mm")) |> # removes mm
  mutate(Usage = str_remove_all(Usage, " mm/day")) |> # removes mm/day
  mutate(date_time = dmy_hm(paste(Date, Time))) |> # merge date and time columns
  mutate(tank = "Tank 1") |>
  clean_names() |>
  glimpse()

tail(tank1clean.df) #use this to make sure the data starts at the right place

#this code selects only the columns we care about and reconverts to the right format
tank1lite.df <- tank1clean.df  |>
  select("tank",
         "date_time",
         "temp_top",
         "temp_bottom",
         "level",
         "usage",
         "status") |>
  filter(status != "ZO"| is.na(status)) |> #removing rows with level zeroing (ZO) status
  type_convert() |>
  glimpse()

skim(tank1lite.df)

tank1lite.df |>
  write_csv(file = './outputs/tank1.csv')

## Tank 2 ----------------------------------------------------------

tank2clean.df <- tank2.df  |>
  slice(1:(n() - 119)) |> # cuts off early values from factory and cooling
  filter(!is.na(Level)) |> # removes change parameter lines
  filter(User == "0") |> # restrict to the main user
  mutate(Temp_top = str_remove_all(`Temp A`, "�C")) |>  # removes temperature symbol
  mutate(Temp_bottom = str_remove_all(`Temp B`, "�C")) |>
  mutate(Level = str_remove_all(Level, " mm")) |> # removes mm
  mutate(Usage = str_remove_all(Usage, " mm/day")) |> # removes mm/day
  mutate(date_time = dmy_hms(paste(Date, Time))) |> # merge date and time columns [tank 2 time is different!]
  mutate(tank = "Tank 2") |>
  clean_names() |>
  glimpse()

tail(tank2clean.df) #use this to make sure the data starts at the right place

#this code selects only the columns we care about and reconverts to the right format
tank2lite.df <- tank2clean.df  |>
  select("tank",
         "date_time",
         "temp_top",
         "temp_bottom",
         "level",
         "usage",
         "status") |>
  filter(status != "ZO"| is.na(status)) |> #removing rows with level zeroing (ZO) status
  type_convert() |>
  glimpse()

skim(tank2lite.df)

tank2lite.df |>
  write_csv(file = './outputs/tank2.csv')

## Tank 3 ----------------------------------------------------------


tank3clean.df <- tank3.df  |>
  slice(1:(n() - 110)) |> # cuts off early values from factory and cooling
  filter(!is.na(Level)) |> # removes change parameter lines
  #filter(User == "11") |> # restrict to the main user - user changed sep 2022
  mutate(Temp_top = str_remove_all(`Temp A`, "�C")) |>  # removes temperature symbol
  mutate(Temp_bottom = str_remove_all(`Temp B`, "�C")) |>
  mutate(Level = str_remove_all(Level, " mm")) |> # removes mm
  mutate(Usage = str_remove_all(Usage, " mm/day")) |> # removes mm/day
  mutate(date_time = dmy_hm(paste(Date, Time))) |> # merge date and time columns
  mutate(tank = "Tank 3") |>
  clean_names() |>
  glimpse()

tail(tank3clean.df) #use this to make sure the data starts at the right place

#this code selects only the columns we care about and reconverts to the right format
tank3lite.df <- tank3clean.df  |>
  select("tank",
         "date_time",
         "temp_top",
         "temp_bottom",
         "level",
         "usage",
         "status") |>
  filter(status != "ZO"| is.na(status)) |> #removing rows with level zeroing (ZO) status
  type_convert() |>
  glimpse()

tank3lite.df |>
  write_csv(file = './outputs/tank3.csv')

# ==== Plot data ====

## Tank 1 ----------------------------------------------------------


#usage
ggplot(tank1lite.df, aes(x = date_time, y = usage)) +
  theme_bw() +
  labs(title = "Nitrogen usage and codes for the last month for Tank 1") +
  labs(x = "Date", y =  "Usage (mm/day)" , fill = "") +
  geom_step(color = "darkgreen", fill = "darkgreen", size = 1) +
  geom_text(aes(x = date_time, y = 20, label = status),
            position = position_jitter(width = 0, height = 20)) +
  scale_x_datetime(limits = as_datetime(c("2022-09-01","2022-09-24")),
                   date_breaks = "2 days",
                   date_labels = "%d %b")
ggsave(file = './outputs/tank1-usage.png',
       width = 8,
       height = 6)

## Tank 2 ----------------------------------------------------------


#usage
ggplot(tank2lite.df, aes(x = date_time, y = usage)) +
  theme_bw() +
  labs(title = "Nitrogen usage per day of Tank 2") +
  labs(x = "Date", y =  "Usage (mm/day)" , fill = "") +
  geom_step(color = "darkgreen", fill = "darkgreen", size = 1) +
  geom_text(aes(x = date_time, y = 20, label = status),
            position = position_jitter(width = 0, height = 20)) +
  scale_x_datetime(date_breaks = "2 days", date_labels = "%d %b")
ggsave(file = './outputs/tank2-usage.png',
       width = 8,
       height = 6)

## Tank 3 ----------------------------------------------------------

#top and bottom temperature
ggplot(tank3lite.df, aes(x = date_time)) +
  theme_bw() +
  labs(title = "Temperature of Tank 3") +
  labs(x = "Date", y =  "Temperature" , fill = "") +
  geom_line(aes(y = temp_top), color = "darkred", size = 1) +
  geom_line(aes(y = temp_bottom), color = "steelblue", size = 1) +
  geom_text_repel(aes(x = date_time, y = -185, label = status), vjust = 5) +
  scale_x_datetime(date_breaks = "7 days", date_labels = "%d %b")
ggsave(file = './outputs/tank3-temp2.png',
       width = 8,
       height = 6)

#top and bottom temperature - jitter option
ggplot(tank3lite.df, aes(x = date_time)) +
  theme_bw() +
  labs(title = "Temperature of Tank 3") +
  labs(x = "Date", y =  "Temperature" , fill = "") +
  geom_line(aes(y = temp_top), color = "darkred", size = 1) +
  geom_line(aes(y = temp_bottom), color = "steelblue", size = 1) +
  geom_text(aes(x = date_time, y = -187, label = status),
            position = position_jitter(width = 0, height = 4)) +
  scale_x_datetime(date_breaks = "7 days", date_labels = "%d %b")
ggsave(file = './outputs/tank3-temp.png',
       width = 8,
       height = 6)

#Level
ggplot(tank3lite.df, aes(x = date_time, y = level)) +
  theme_bw() +
  labs(title = "Nitrogen levels of Tank 3") +
  labs(x = "Date", y =  "Level (mm)" , fill = "") +
  geom_line(color = "blue", size = 1) +
  geom_text(aes(x = date_time, y = 230, label = status),
            position = position_jitter(width = 0, height = 20)) +
  scale_x_datetime(date_breaks = "2 days", date_labels = "%d %b")
ggsave(file = './outputs/tank3-level.png',
       width = 8,
       height = 6)

#usage
ggplot(tank3lite.df, aes(x = date_time, y = usage)) +
  theme_bw() +
  labs(title = "Nitrogen usage per day of Tank 3") +
  labs(x = "Date", y =  "Usage (mm/day)" , fill = "") +
  geom_step(color = "darkgreen", fill = "darkgreen", size = 1) +
  geom_text(aes(x = date_time, y = 20, label = status),
            position = position_jitter(width = 0, height = 20)) +
  scale_x_datetime(date_breaks = "2 days", date_labels = "%d %b")
ggsave(file = './outputs/tank3-usage.png',
       width = 8,
       height = 6)



# Combined plots ----------------------------------------------------------

## Combine data frames ----------------------------------------------------

combined.df <- bind_rows(tank1lite.df, tank2lite.df) |>
  bind_rows(tank3lite.df)

skim(combined.df)

## Nitrogen usage ---------------------------------------------------------

ggplot(combined.df, aes(x = date_time, y = usage, colour = tank)) +
  theme_bw() +
  labs(title = "Nitrogen usage per day") +
  labs(x = "Date", y =  "Usage (mm/day)") +
  geom_line(size = 1) +
  scale_x_datetime(date_breaks = "7 days", date_labels = "%d %b",
                   limits = c(as_datetime("2022-04-03"), NA)) +
  facet_grid(rows = vars(tank))
ggsave(file = './outputs/tank-combined-usage.png',
       width = 8,
       height = 6)

## Nitrogen usage rolling average---------------------------------------------------------

library(tidyquant)

ggplot(combined.df, aes(x = date_time, y = usage, colour = tank)) +
  theme_bw() +
  labs(title = "Nitrogen usage per day") +
  labs(x = "Date", y =  "Usage (mm/day)") +
  geom_ma(n = 140, size = 1, linetype= "solid") +
  scale_x_datetime(date_breaks = "14 days", date_labels = "%d %b",
                   limits = c(as_datetime("2022-04-03"), NA)) +
  facet_grid(rows = vars(tank))
ggsave(file = './outputs/tank-combined-usage-rolling.png',
       width = 8,
       height = 6)

## Top temperature ---------------------------------------------------------

ggplot(combined.df, aes(x = date_time, y = temp_top, colour = tank)) +
  theme_bw() +
  labs(title = "Top tank temperature") +
  labs(x = "Date", y =  "degrees (°C)") +
  geom_line(size = 1) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%d %b",
                   limits = c(as_datetime("2022-04-03"), NA)) +
  facet_grid(rows = vars(tank))
ggsave(file = './outputs/tank-combined-temp-top.png',
       width = 8,
       height = 6)


## Levels ---------------------------------------------------------

ggplot(combined.df, aes(x = date_time, y = level, colour = tank)) +
  theme_bw() +
  labs(title = "Nitrogen levels - past month") +
  labs(x = "Date", y =  "level (mm)") +
  geom_line(size = 1) +
  geom_hline(yintercept = 210) +
  scale_x_datetime(limits = as_datetime(c("2022-09-01", NA)),
    date_breaks = "2 days",
    date_labels = "%d %b") +
  facet_grid(rows = vars(tank))
ggsave(file = './outputs/tank-combined-levels.png',
       width = 8,
       height = 6)

## Levels ---rolling ------------------------------------------------------

ggplot(combined.df, aes(x = date_time, y = level, colour = tank)) +
  theme_bw() +
  labs(title = "Nitrogen levels") +
  labs(x = "Date", y =  "level (mm)") +
  geom_ma(n = 70, size = 1, linetype= "solid") +
  geom_hline(yintercept = 210) +
  scale_x_datetime(
    date_breaks = "1 month",
    date_labels = "%d %b") +
  facet_grid(rows = vars(tank))
ggsave(file = './outputs/tank-combined-levels-rolling.png',
       width = 8,
       height = 6)


# Stats ----------------------------------------------------------

#Somehow want to have time between fills


# extract all fills

tank2fills.df <- tank2lite.df |> 
  filter(Status == "F") |> 
  select("tank",
         "date_time",
         "Status")

tank2fills.df |>
write_csv(file = './outputs/tank2fills.csv')


# From Aaron H --------------------------------------------------------

#days between fills
fill = tank3lite.df %>%
  filter(status == "F") %>% 
  arrange(date_time) %>% 
  mutate(fill_days = c(NA, as.numeric(diff(date_time)/1440))) %>% 
  drop_na() %>% 
  mutate(fill_event = seq_along(fill_days))
ggplot(fill, aes(x = fill_event, y = fill_days)) +
  geom_line(size = 1, col = "darkblue") +
  geom_point(size = 3, col = "darkblue") +
  scale_x_continuous(breaks = c(1:length(fill$fill_event))) +
  xlab("\nFill event") +
  ylab("Days between fills\n") +
  theme_classic()
ggsave(file = './outputs/days-between-fills.png',
       width = 8,
       height = 6)


usage = tank3lite.df %>% 
  #filter(status != "F" | is.na(status)) %>% 
  filter(hour(date_time) == 0) %>% 
  distinct() %>%
  arrange(date_time) %>% 
  mutate(usage_daily = c(NA, diff(level))) %>% 
  filter(usage_daily < 0) %>% 
  mutate(days = seq_along(usage_daily)) 
ggplot(usage, aes(x = days, y = -1*usage_daily)) +
  geom_line(size = 1, col = "darkblue") +
  geom_point(size = 3, col = "darkblue") +
  scale_x_continuous(breaks = c(1:length(days))) +
  ylim(c(0, max(usage$days))) +
  xlab("\nDays (excluding fill days)") +
  ylab("Daily usage (mm/day)\n") +
  theme_classic()
ggsave(file = './outputs/daily-usage.png',
                        width = 8,
                        height = 6)



# testing --------------------------------------------------------


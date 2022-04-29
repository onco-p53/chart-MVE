## R Script to process data exported from Chart MVE TS controller nitrogen tanks ##
# Author: B.S. Weir (2022)

# ==== Load all the packages needed ====

library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(skimr)
library(janitor)
library(ggrepel)

#the code REQUIRES R 4.2 or greater due to UTF-8
R.version.string #check version

# ==== Load data ====

#load tank 1 data
tank1.df <- read_csv("39567301.CSV",
                     guess_max = Inf,
                     show_col_types = FALSE)
glimpse(tank1.df)


#load tank 3 data
tank3.df <- read_csv("39561501.CSV",
                                guess_max = Inf,
                                show_col_types = FALSE)

glimpse(tank3.df)

# ==== Clean the raw Data ====


## Tank 1 ----------------------------------------------------------

tank1clean.df <- tank1.df  |> 
  slice(1:(n() - 97)) |> # cuts off early values from factory (was 40)
  filter(!is.na(Level)) |> # removes change parameter lines
  filter(User == "0") |> # restrict to the main user
  mutate(Temp_top = str_remove_all(`Temp A`, "�C")) |>  # removes temperature symbol
  mutate(Temp_bottom = str_remove_all(`Temp B`, "�C")) |> 
  mutate(Level = str_remove_all(Level, " mm")) |> # removes mm
  mutate(Usage = str_remove_all(Usage, " mm/day")) |> # removes mm/day
  mutate(date_time = dmy_hm(paste(Date, Time))) |> # merge date and time columns
  mutate(tank = "Tank 1") |> 
  glimpse()

tail(tank1clean.df) #use this to make sure the data starts at the right place

#this code selects only the columns we care about and reconverts to the right format
tank1lite.df <- tank1clean.df  |>
  select("tank", "date_time","Temp_top", "Temp_bottom", "Level", "Usage", "Status") |>
  mutate(Status = str_remove_all(Status, "ZO")) |> #removing level zeroing status
  type_convert() |> 
  glimpse()

skim(tank1lite.df)

tank1lite.df |> 
  write_csv(file='./outputs/tank1.csv')

## Tank 2 ----------------------------------------------------------


## Tank 3 ----------------------------------------------------------


tank3clean.df <- tank3.df  |> 
  slice(1:(n() - 97)) |> # cuts off early values from factory (was 40)
  filter(!is.na(Level)) |> # removes change parameter lines
  filter(User == "11") |> # restrict to the main user
  mutate(Temp_top = str_remove_all(`Temp A`, "�C")) |>  # removes temperature symbol
  mutate(Temp_bottom = str_remove_all(`Temp B`, "�C")) |> 
  mutate(Level = str_remove_all(Level, " mm")) |> # removes mm
  mutate(Usage = str_remove_all(Usage, " mm/day")) |> # removes mm/day
  mutate(date_time = dmy_hm(paste(Date, Time))) |> # merge date and time columns
  mutate(tank = "Tank 3") |> 
  glimpse()

tail(tank3clean.df) #use this to make sure the data starts at the right place

#this code selects only the columns we care about and reconverts to the right format
tank3lite.df <- tank3clean.df  |>
  select("tank", "date_time","Temp_top", "Temp_bottom", "Level", "Usage", "Status") |>
  mutate(Status = str_remove_all(Status, "ZO")) |> #removing level zeroing status
  type_convert() |> 
  glimpse()

tank3lite.df |> 
write_csv(file='./outputs/tank3.csv') 

# ============ Plot data ================

## Tank 1 ----------------------------------------------------------


## Tank 2 ----------------------------------------------------------


## Tank 3 ----------------------------------------------------------

#top and bottom temperature
ggplot(tank3lite.df, aes(x= date_time)) +
  theme_bw() +
  labs(title = "Temperature of Tank 3") +
  labs(x = "Date", y =  "Temperature" , fill = "") +
  geom_line(aes(y = Temp_top), color = "darkred", size = 1) +
  geom_line(aes(y = Temp_bottom), color = "steelblue", size = 1) +
  geom_text_repel(aes(x = date_time, y = -185, label = Status), vjust = 5) +
  scale_x_datetime(date_breaks = "2 days", date_labels = "%d %b")
ggsave(file='./outputs/tank3-temp2.png', width=8, height=6)

#top and bottom temperature - jitter option
ggplot(tank3lite.df, aes(x= date_time)) +
  theme_bw() +
  labs(title = "Temperature of Tank 3") +
  labs(x = "Date", y =  "Temperature" , fill = "") +
  geom_line(aes(y = Temp_top), color = "darkred", size = 1) +
  geom_line(aes(y = Temp_bottom), color = "steelblue", size = 1) +
  geom_text(aes(x = date_time, y = -187, label = Status), 
            position = position_jitter(width=0, height=4)) +
  scale_x_datetime(date_breaks = "2 days", date_labels = "%d %b")
ggsave(file='./outputs/tank3-temp.png', width=8, height=6)

#Level
ggplot(tank3lite.df, aes(x= date_time, y = Level)) +
  theme_bw() +
  labs(title = "Nitrogen levels of Tank 3") +
  labs(x = "Date", y =  "level (mm)" , fill = "") +
  geom_line(color = "blue", size = 1) +
  geom_text(aes(x = date_time, y = 230, label = Status), 
            position = position_jitter(width=0, height=20)) +
  scale_x_datetime(date_breaks = "2 days", date_labels = "%d %b")
ggsave(file='./outputs/tank3-level.png', width=8, height=6)

#usage
ggplot(tank3lite.df, aes(x= date_time, y = Usage)) +
  theme_bw() +
  labs(title = "Nitrogen usage per day of Tank 3") +
  labs(x = "Date", y =  "usage (mm/day)" , fill = "") +
  geom_step(color = "darkgreen", fill = "darkgreen", size = 1) +
  geom_text(aes(x = date_time, y = 20, label = Status), 
            position = position_jitter(width=0, height=20)) +
  scale_x_datetime(date_breaks = "2 days", date_labels = "%d %b")
ggsave(file='./outputs/tank3-usage.png', width=8, height=6)





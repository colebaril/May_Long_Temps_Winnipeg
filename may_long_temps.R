require(pacman)
pacman::p_load(tidyverse, weathercan, lutz, sf, here, janitor)

# VICTORIA DAY DATES ----

may_long_dates <- read_csv(here("Raw/may_long_dates - Sheet1.csv")) %>% 
  clean_names() %>% 
  filter(area != "New Brunswick") %>% select(-last_col()) %>% 
  unite(date, date:year, sep = ", ") %>% 
  mutate(date = as.Date(date, "%B %d, %Y"))

may_long_sun <- may_long_dates %>% 
  mutate(date = date - 1)

may_long_sat <- may_long_dates %>% 
  mutate(date = date - 2)

may_long <- rbind(may_long_dates, may_long_sun, may_long_sat) %>% 
  arrange(desc(date))
  
  
# WEATHER DATA ----

stations_dl()

wpg_stations <- stations_search("Winnipeg", interval = "day") 

wpg_stations_2023 <- wpg_stations %>% 
  filter(end == "2023") %>% 
  select(station_id) %>% as.list()

wpg_weather_dl <- weather_dl(station_ids = 27174, start = "1996-05-01", end = "2023-05-01", interval = "day")

wpg_weather <- wpg_weather %>% 
  select(date, year, month, day, mean_temp, max_temp, min_temp)

# PLOT ----

may_long %>% 
  left_join(wpg_weather, by = "date") %>% 
  group_by(year) %>% 
  summarise(mean_min_temp = mean(min_temp),
            mean_max_temp = mean(max_temp)) %>% 
  add_row(year = "2023", mean_max_temp = 26, mean_min_temp = 14) %>% 
  drop_na() %>% 
  pivot_longer(!year, names_to = "stat", values_to = "degrees") %>% 
  ggplot(aes(x = year, y = degrees, fill = stat, group = stat)) +
  geom_col() +
  geom_smooth(se = FALSE, colour = "black") +
  scale_fill_manual(values = c("firebrick3", "skyblue")) +
  facet_grid(stat ~ ., labeller = as_labeller(c(mean_max_temp = "Mean Max Temp °C",
                                                mean_min_temp = "Mean Min Temp °C"))) +
  theme_bw() +
  labs(title = "High and Low Temperatures in Winnipeg over May Long Weekends, 1997-2023",
       subtitle = "Highs and Lows averaged over Saturday, Sunday and Monday for each May Long Weekend. 2023 temperatures are predicted.",
       caption = "Viz: Cole Baril - colebaril.ca | Software: R | Data: Environment Canada",
       x = "Year",
       y = "Average Temperature (°C) Over May Long Weekend") +
  theme(legend.position = "none",
        strip.text = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))
  

library(tidyverse)
library(leaflet)
library(htmltools)

ev_stations <- read_csv('electric_stations.csv')

names(ev_stations) <- gsub("\\s+",".",names(ev_stations))

head(ev_stations)


labs <- lapply(seq(nrow(ev_stations)), function(i) {
  paste(sep = ' ', ev_stations[i, "Station.Name"],
          ev_stations[i, "Street.Address"],
          ev_stations[i, "City"],
          ev_stations[i, "ZIP"]) 
})

leaflet(data = ev_stations) %>% 
  addTiles() %>% 
  addMarkers(lng = ev_stations$Longitude, 
             lat = ev_stations$Latitude, 
             popup = ~htmlEscape(lapply(labs, htmltools::HTML)), 
             clusterOptions = markerClusterOptions())


ev_stations %>% 
  group_by(State) %>% 
  mutate(count_state = n()) %>% 
  ggplot() + 
  geom_bar(aes(x = reorder(State,-count_state), y = stat(count), fill = State)) + 
  labs(title="Charging Stations by State", x="State", y="Count")

                                  
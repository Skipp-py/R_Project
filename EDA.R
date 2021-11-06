library(tidyverse)
library(leaflet)
library(geojsonio)
library(htmltools)
library(lubridate)

ev_stations <- read_csv('electric_stations.csv')
ev_registration <- read_csv('EV_Registration_20.csv')
states <- geojsonio::geojson_read( x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json", what = "sp")

names(ev_stations) <- gsub("\\s+",".",names(ev_stations))

ev_stations$Open.Date <- mdy(ev_stations$Open.Date)

head(ev_stations)


labs <- lapply(seq(nrow(ev_stations)), function(i) {
  paste(sep = ' ', ev_stations[i, "Station.Name"],
          ev_stations[i, "Street.Address"],
          ev_stations[i, "City"],
          ev_stations[i, "ZIP"]) 
})

ev_map <- leaflet(data = ev_stations) %>% 
  addTiles() %>% 
  addMarkers(lng = ev_stations$Longitude, 
             lat = ev_stations$Latitude, 
             popup = ~htmlEscape(lapply(labs, htmltools::HTML)), 
             clusterOptions = markerClusterOptions())

ev_stations_leveled <- ev_stations %>% 
  group_by(State) %>%
  summarise(Level_1 = sum(EV.Level1.EVSE.Num,na.rm = T), 
            Level_2 = sum(EV.Level2.EVSE.Num,na.rm = T), 
            Level_3 = sum(EV.DC.Fast.Count,na.rm = T)) %>%
  pivot_longer(cols = Level_1:Level_3,
               names_to = "Level",
               values_to = "Count") %>% 
  ggplot(aes(x=reorder(State,-Count), y=Count, fill=Level)) + 
  geom_col() +
  labs(title="Charging Stations by State", x="State", y="Count")

ev_station_state <- ev_stations %>% 
  filter(Open.Date < "2021-01-01") %>%
  group_by(State) %>% 
  select(Abbrev = State) %>%
  summarise(Stations = n())

ev_stations_vehicles <- left_join(ev_station_state, ev_registration) %>% 
  rename(Registered_Vehicles =`Registration Count`) %>%
  mutate(Vehicles_Per_Station = Registered_Vehicles/Stations)

labels <- sprintf("<strong>%s</strong><br/>%g cars/station",
                  states$name, ev_stations_vehicles$Vehicles_Per_Station) %>% lapply(htmltools::HTML)
bins <- c(0,10,20,30,40,50,60,Inf)
pal <- colorBin("YlOrRd", domain = ev_stations_vehicles$Vehicles_Per_Station, bins = bins)

states$name
ev_stations_vehicles$State

leaflet(states) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
  addPolygons(
    fillColor = ~pal(ev_stations_vehicles$Vehicles_Per_Station),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = 'auto')) %>%
  addLegend(pal = pal, values = ~ev_stations_vehicles$Vehicles_Per_Station, opacity = 0.7, title = NULL,
            position = 'bottomright')


















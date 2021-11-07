library(tidyverse)
library(leaflet)
library(geojsonio)
library(htmltools)
library(lubridate)

## Reading in Data

# Reading in EV Stations
ev_stations <- read_csv('electric_stations.csv')

# Reading in Registered EVs
ev_registration <- read_csv('EV_Registration_20.csv')

## Cleaning Data

# Fixing Data Table Headers
names(ev_stations) <- gsub("\\s+",".",names(ev_stations))

# Converting Openings to Datetime 
ev_stations$Open.Date <- mdy(ev_stations$Open.Date)

## Manipulating Data

# Counting Stations by State
ev_station_state <- ev_stations %>% 
  filter(Open.Date < "2021-01-01") %>%
  group_by(State) %>% 
  select(Abbrev = State) %>%
  summarise(Stations = n())

# Joining Station Data with Registration Data
ev_stations_vehicles <- left_join(ev_station_state, ev_registration) %>% 
  rename(Registered_Vehicles =`Registration Count`) %>%
  mutate(Vehicles_Per_Station = Registered_Vehicles/Stations) %>%
  arrange(State) %>%
  add_row(Abbrev='PR', Stations=26,State='Puerto Rico',Registered_Vehicles=1051,Vehicles_Per_Station=1051/26)

# Pivoting Data to EV Station Levels (charging pumps) by State
ev_station_levels <- ev_stations %>% 
  group_by(State) %>%
  summarise(Level_1 = sum(EV.Level1.EVSE.Num,na.rm = T), 
            Level_2 = sum(EV.Level2.EVSE.Num,na.rm = T), 
            Level_3 = sum(EV.DC.Fast.Count,na.rm = T)) %>%
  pivot_longer(cols = Level_1:Level_3,
               names_to = "Level",
               values_to = "Count") 

## Graphing the Data

# Creating Bar Chart of EV Pump Levels by State
station_levels_col <- ev_station_levels %>% 
  ggplot(aes(x=reorder(State,-Count), y=Count, fill=Level)) + 
  geom_col() +
  labs(title="Charging Stations by State", x="State", y="Count")
station_levels_col

# Creating Bar Chart of Registered Vehicles by State
registered_ev_col <- ev_stations_vehicles %>% 
  ggplot(aes(x=reorder(Abbrev,-Registered_Vehicles),y=Registered_Vehicles,fill=Abbrev)) + 
  geom_col() +
  labs(title="Registered EV by State", x="State", y="Registered EVs")
registered_ev_col

## Mapping the Data

# Creating State Polygons
states <- geojsonio::geojson_read(x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json", 
                                  what = "sp")
# Creating Marker Labels
lab_1 <- sprintf("<strong>%s</strong><br/>%s<br/>%s, %s", 
                  ev_stations$Station.Name, 
                  ev_stations$Street.Address, 
                  ev_stations$City, ev_stations$ZIP) %>% 
  lapply(htmltools::HTML)

# Creating Overlay Labels
lab_2 <- sprintf("<strong>%s</strong><br/>%1.0f cars/station",
                 ev_stations_vehicles$State, 
                 ev_stations_vehicles$Vehicles_Per_Station) %>% 
  lapply(htmltools::HTML)

# Creating Overlay Bins
bins <- c(0,10,20,30,40,50,60,Inf)

# Creating Overlay Colors
pal <- colorBin("YlOrRd", 
                domain = ev_stations_vehicles$Vehicles_Per_Station, 
                bins = bins)

# Putting the Map Together and Visualizing
map <- leaflet(states) %>%
  addTiles() %>%
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
    group = "Polygons",
    label = lab_2,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", 
                   padding = "3px 8px"),
      textsize = "15px",
      direction = 'auto')) %>%
  addLegend(pal = pal, 
            values = ~ev_stations_vehicles$Vehicles_Per_Station, 
            opacity = 0.7, title = NULL,
            position = 'bottomright') %>%
  addMarkers(lng = ev_stations$Longitude, 
             lat = ev_stations$Latitude, 
             label = lab_1,
             labelOptions = labelOptions(
               style = list('font-weight'= 'normal',
                            padding="3px 8px"),
               textsize = '15px',
               direction = 'auto'
             ),
             clusterOptions = markerClusterOptions()) %>%
  groupOptions('Polygons', zoomLevels = 5:0)
map

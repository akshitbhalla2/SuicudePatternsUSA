#Using R leaflets to understand homicides in Alaska

rm(list=ls())
setwd("~/R PROJECTS")

#Producing a null world map
library("leaflet")
m <- leaflet()

#Producing a world map
library("magrittr")
m <- leaflet() %>%
  addTiles()

#Zooming into Alaska
m <- leaflet() %>%
  addTiles() %>%
  setView(lng = -149.4937,
          lat = 64.2008,
          zoom = 4)

#Better Picture
m <- leaflet() %>%
  addProviderTiles(providers$Stamen.Toner) %>% #Leaflet extras website shows the many "providers" one can choose from
  setView(lng = -149.4937,
          lat = 64.2008,
          zoom = 4)

#START ACTUAL PROGRAM

#Dividing regions
library("rgdal") #R Geodata Abstraction Library
ak_counties <-
  readOGR(dsn = "LEAFLET/data/tl_2013_02_cousub/tl_2013_02_cousub.shp") #Path of file we want to read
m <- leaflet() %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  setView(lng = -149.4937,
          lat = 64.2008,
          zoom = 4) %>%
  addPolygons(
    data = ak_counties,
    color = '#660000',
    weight = 1,
    smoothFactor = 0.5
  )

#Reading the homicide dataset
fbi_data <- read.csv("LEAFLET/data/database.csv")

#Selecting Alaska
library("dplyr")
ak <- filter(fbi_data, State == "Alaska")
ak <- mutate(ak, address = paste(City, State, "United States"))
address <- unique(ak$address)

#Determining coordinates
library("ggmap")
geocodes <- geocode(address, source = "dsk")

#If any addresses are missing due to time outs, do this:
address_and_coords <-
  data.frame(address = address,
             lon = geocodes$lon,
             lat = geocodes$lat)
counter <- 0
while (sum(is.na(address_and_coords$lon)) > 0 && counter < 10) {
  missing_address <- address_and_coords %>%
    filter(is.na(lon) == T)
  
  address <- missing_address$address
  
  geocodes <- geocode(as.character(address), source = "dsk")
  address_and_coords <-  address_and_coords %>%
    filter(is.na(lon) == F)
  
  new_address <- data.frame(address = address,
                            lon = geocodes$lon,
                            lat = geocodes$lat)
  
  address_and_coords <- rbind(address_and_coords, new_address)
  
  counter <- counter + 1
}
ak <- left_join(ak, address_and_coords, by = "address")

#Introduce "White Noise" i.e. to avoid making cooridnates of places the same
ak$lon <- jitter(ak$lon, factor = 1)
ak$lat <- jitter(ak$lat, factor = 1)


#Select unsolved crimes
ak_unsolved <- ak %>%
  filter(Crime.Type == "Murder or Manslaughter") %>%
  filter(Crime.Solved == "No")

#Label unsolved crimes
ak_unsolved$label <- paste(
  "<p>",
  ak_unsolved$City,
  "</p>",
  "<p>",
  ak_unsolved$Month,
  " ",
  ak_unsolved$Year,
  "</p>",
  "<p>",
  ak_unsolved$Victim.Sex,
  " ",
  ak_unsolved$Victim.Age,
  "</p>",
  "<p>",
  ak_unsolved$Victim.Race,
  "</p>",
  "<p>",
  ak_unsolved$Weapon,
  "</p>"
)

#Add circles on unsolved crimes
library("htmltools")
m <- m %>%
  addCircleMarkers(
    lng = ak_unsolved$lon,
    lat = ak_unsolved$lat,
    color = "red",
    weight = 1,
    radius = 5,
    group = "unsolved",
    label = lapply(ak_unsolved$label, HTML),
    clusterOptions = markerClusterOptions(showCoverageOnHover = F)
  )

#Select solved crimes
ak_solved <- ak %>%
  filter(Crime.Solved == "Yes") %>%
  filter(Crime.Type == "Murder or Manslaughter")

#Label solved crimes
ak_solved$label <- paste(
  "<p>",
  ak_solved$City,
  "</p>",
  "<p>",
  ak_solved$Month,
  " ",
  ak_solved$Year,
  "</p>",
  "<p>",
  ak_solved$Victim.Sex,
  " ",
  ak_solved$Victim.Age,
  "</p>",
  "<p>",
  ak_solved$Victim.Race,
  "</p>",
  "<p>",
  ak_solved$Weapon,
  "</p>"
)

#Add circles on solved crimes
m <- m %>%
  addCircleMarkers(
    lng = ak_solved$lon,
    lat = ak_solved$lat,
    color = "blue",
    weight = 1,
    radius = 5,
    group = "solved",
    label = lapply(ak_solved$label, HTML),
    clusterOptions = markerClusterOptions(showCoverageOnHover = F)
  )

#Overlaying groups
m <- m %>%
  addLayersControl(overlayGroups = c("solved", "unsolved"),
                   options = layersControlOptions(collapsed = F))

m

#############################################################################################

#Creating across US states
us <- fbi_data %>%
  mutate(Solved = ifelse(Crime.Solved == "Yes", 1, 0)) %>%
  filter(Crime.Type == "Murder or Manslaughter") %>%
  group_by(State) %>%
  summarise(Num.Murders = n(),
            Num.Solved = sum(Solved)) %>%
  mutate(Num.Unsolved = Num.Murders - Num.Solved,
         Solve.Rate = Num.Solved / Num.Murders)

states <-
  readOGR(dsn = "LEAFLET/data/cb_2016_us_state_500k/cb_2016_us_state_500k.shp")

#To check if us$State and states$NAME have the same names: is.element(us$State, states$NAME)
#Since elements in us$State and states$NAME don't have the same name, we make changes
levels(us$State)[40] <- "Rhode Island"

#To check if states$NAME and us$State have the same names: is.element(states$NAME, us$State)
#Since elements in states$NAME and us$State don't have the same name, we make changes
states <- subset(states, is.element(states$NAME, us$State))

#Reordering rows
us <- us[order(match(us$State, states$NAME)), ]

#Creating labels for US States
labels <- paste(
  "<p>",
  us$State,
  "</p>",
  "<p>",
  "Solve Rate: ",
  round(us$Solve.Rate, digits = 3),
  "</p>",
  sep = ""
)

#Creating "bins" to assign colors to states
bins <- 3:10
bins <- bins / 10
pal <- colorBin("RdYlBu", domain = us$Solve.Rate, bins = bins)
m <- leaflet() %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  addPolygons(
    data = states,
    weight = 1,
    smoothFactor = 0.5,
    color = "white",
    fillOpacity = 0.8,
    fillColor = pal(us$Solve.Rate),
    highlight = highlightOptions(
      weight = 5,
      color = "#666666",
      # dashArray = "",
      fillOpacity = 0.7,
      bringToFront = T
    ),
    label = lapply(labels, HTML)
  ) %>%
  addLegend(
    pal = pal,
    values = us$Solve.Rate,
    opacity = 0.7,
    position = "topright"
  )
m
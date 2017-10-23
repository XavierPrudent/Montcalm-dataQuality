
source("/Users/lavieestuntoucan/Civilia/tech/general/load_R_pkg.R")

## Poteau data
d <- read.xlsx("/Users/lavieestuntoucan/Documents/projets_perso/Start-up/Civilia/projets/Montcalm/data/poteau-coord-2017.xlsx")

## Plot the poteau positions
map <- leaflet() %>% addTiles()%>% 
  setView(lng=coord.SaintLin$lon, lat=coord.SaintLin$lat, zoom = 11)
map1 <- addCircles(map,
                   lng=d$Longitude,
                   lat=d$Latitude,
                   stroke=FALSE,
                   color="red",
                   radius=300,
                   label=paste("Poteau",d$Arret,",",d$Rues),
                   fillOpacity=1) 



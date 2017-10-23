source("/Users/lavieestuntoucan/Civilia/tech/general/load_R_pkg.R")
source("/Users/lavieestuntoucan/Civilia/projets/Montcalm/tech/data-cleaning/R/fine_tuning_stops.R")

## Read excel file and save as rds to speed up later
# d <- read.xlsx("/Users/lavieestuntoucan/Civilia/projets/Montcalm/data/Transport MRC Montcalm.xlsx",sheet = "Feuil1")
# saveRDS(d,"/Users/lavieestuntoucan/Civilia/projets/Montcalm/data/Transport MRC Montcalm.Rds")

## Open data
d <- readRDS("/Users/lavieestuntoucan/Civilia/projets/Montcalm/data/db/Transport MRC Montcalm.Rds")

## Correct data from excel 
d <- correct_data(d)

## List of all stops
list.stops <- as.data.frame(unique(c(d$AdresseArrivee,d$AdresseDepart))) #1025
colnames(list.stops) <- "stops"
list.stops$stops <- as.character(list.stops$stops)
list.stops$initNames <- list.stops$stops

## Fine tuning with stops
list.stops <- correct_ads(list.stops)

## Separate poteaux from private adresses
list.non.pot.stops <- define_nonpot_stops()
list.pot.stops <- define_pot_stops()

list.pot <- list.stops %>% filter(grepl(pattern="PO",x=stops) | 
                                    grepl(pattern="#",x=stops) |
                                    stops %in% list.pot.stops &
                                    !(stops %in% list.non.pot.stops)) #667

list.priv <- list.stops %>% filter(!grepl(pattern="PO",x=stops) & 
                                     !grepl(pattern="#",x=stops) &
                                     !(stops %in% list.pot.stops) | 
                                     stops %in% list.non.pot.stops ) #367

## Save cleaned data
saveRDS(d,"/Users/lavieestuntoucan/Civilia/projets/Montcalm/data/db/Transport MRC Montcalm_cleaned.Rds")
d <- readRDS("/Users/lavieestuntoucan/Civilia/projets/Montcalm/data/db/Transport MRC Montcalm_cleaned.Rds")

## List of origins and destinations
list.gps <- vector("list", nrow(list.priv)) 

for( i in 1:nrow(list.priv)){
  print(i)
  coord <- geocode(list.priv$stops[i]) 
  
  if( is.na(coord$lon) | is.na(coord$lat) ){
    list.gps[[i]][1] <- -1
    list.gps[[i]][2] <- -1
  }else{
    list.gps[[i]][1] <- coord$lon
    list.gps[[i]][2] <- coord$lat
  }
  cat(paste0(coord$lon,"   ", coord$lat,"\n",sep=" "))
}

## Save for later
 saveRDS(list.gps,"/Users/lavieestuntoucan/Civilia/projets/Montcalm/tech/data-cleaning/out/list_gps.Rds")
 saveRDS(list.priv,"/Users/lavieestuntoucan/Civilia/projets/Montcalm/tech/data-cleaning/out/list_prive.Rds")
 saveRDS(list.pot,"/Users/lavieestuntoucan/Civilia/projets/Montcalm/tech/data-cleaning/out/list_pot.Rds")
 
 ## Fix last missing info
list.gps <- readRDS("/Users/lavieestuntoucan/Civilia/projets/Montcalm/tech/data-cleaning/out/list_gps.Rds")
list.priv <- readRDS("/Users/lavieestuntoucan/Civilia/projets/Montcalm/tech/data-cleaning/out/list_prive.Rds")
list.pot <- readRDS("/Users/lavieestuntoucan/Civilia/projets/Montcalm/tech/data-cleaning/out/list_pot.Rds")

## Merge coord and adresses
list.gps <- ldply(list.gps)
colnames(list.gps) <- c("lon","lat")
list.priv.gps <- cbind(list.priv,list.gps)

## Set lat lon by hand
list.priv.gps <- set_latlon(list.priv.gps)

## Keep ads with coord
list.priv.gps <- list.priv.gps %>% filter(lon != -1 & lat != -1) #325

## Save for later
saveRDS(list.priv.gps,"/Users/lavieestuntoucan/Civilia/projets/Montcalm/tech/data-cleaning/out/list_prive_withCoord.Rds")
list.priv.gps <- readRDS("/Users/lavieestuntoucan/Civilia/projets/Montcalm/tech/data-cleaning/out/list_prive_withCoord.Rds")

## Map of Saint Lin
map <- leaflet() %>% addTiles()%>% 
  setView(lng=coord.SaintLin$lon, lat=coord.SaintLin$lat, zoom = 11)

## Add the prov ads
map1 <- addCircles(map,
                   lng=list.priv$lon,
                   lat=list.priv$lat,
                   label=list.priv$stops,
                   stroke=FALSE,
                   color="red",
                   radius=300,
                   fillOpacity=1) 





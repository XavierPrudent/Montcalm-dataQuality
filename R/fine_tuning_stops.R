
####################################

define_pot_stops <- function(){
  mylist <- c("P0162 MORSE-  HÉLÈNE,STE-JULIENNE,Québec")
  return(mylist)
}
define_nonpot_stops <- function(){
  
  mylist <- c("202 DES CHAMPS / LALIBERTÉ,ST-ROCH-DE-L'ACHIGAN,Québec",
              "2175 PLACE MALO   - POLY. HAVRE-JEUNESSE,NA,Québec",
              "916 LADOUCEUR /POLY. THERESE MARTIN,NA,Québec",
              "25 LAPOINTE,SAINT-LIN-LAURENTIDES,Québec",
              "BOITE POSTAUX,ST-ROCH-DE-L'ACHIGAN,Québec",
              "4567 DU MONT-PONTBRIAND,RAWDON,Québec",
              "SOURCES ET LAPOINTE,ST-CALIXTE,Québec",
              "240 1ERE AVENUE BEAUPORT,NA,Québec",
              "1150 LEOPOLD,STE-JULIENNE,Québec",
              "240 1ERE AVENUE BEAUPORT,ST-CALIXTE,Québec" ,
              "CH. DE LA FOURCHE/DUPONT,NA,Québec",
              "CH. DE LA FOURCHE/DUPONT,STE-JULIENNE,Québec",
              "DÉP. DÉPANNEUR C.R./DE LA FOURCHE/1798 DU PONT,STE-JULIENNE,Québec"  , 
              "1798 DU PONT,STE-JULIENNE,Québec"   ,
              "2175 PLACE MALO   - POLY. HAVRE-JEUNESSE,STE-JULIENNE,Québec"  ,      
              "DOMAINE DU REPOS - ROUTE 346,ST-ALEXIS (PAROISSE),Québec",     
              "BUREAU DE POSTES,SAINT-LIN-LAURENTIDES,Québec")
  return(mylist)
}

####################################
correct_data <- function(data){
  
  ## Keep collective transport
  data <- data %>% filter(grepl(pattern="C",x=Matricule) & Matricule != "C9999")
  
  ## Correct Date and time
  data$DateTransport <- convertToDate(data$DateTransport)
  data$HeureDepart <- convertToDateTime(data$HeureDepart)
  
  ## Remove city code
  data$AdresseDepart <- substr(data$AdresseDepart, 1, nchar(data$AdresseDepart)-3)
  data$AdresseArrivee <- substr(data$AdresseArrivee, 1, nchar(data$AdresseArrivee)-3)
  
  ## Add city name
  data$AdresseDepart <- paste0(data$AdresseDepart,data$MunicipaliteDepart,",Québec")
  data$AdresseArrivee <- paste0(data$AdresseArrivee,data$MunicipaliteArrivee,",Québec")
  
  return(data)
}

####################################
correct_ads <- function(data){
  
  ## Correct adresses
  data$stops <- gsub( " *\\(.*?\\) *", "", data$stops)
  data$stops <- gsub( " *\\(.*?\\, *", "", data$stops)
  data$stops <- gsub( "P0", "PO", data$stops)
  
  data$stops <- gsub( "000 SALLE OPALE", "SALLE OPALE", data$stops)
  data$stops <- gsub( "9999 SALLE OPALE", "SALLE OPALE", data$stops)
  data$stops <- gsub( "50 RANG DU CORDON", "RANG DU CORDON", data$stops)
  data$stops <- gsub( "140 MOULIN ROUGE", "140 rue du MOULIN ROUGE", data$stops)
  data$stops <- gsub( "500 VIMONT", "500 rue de VIMONT", data$stops)
  data$stops <- gsub( "425 AUMONT", "425 rue de AUMONT", data$stops)
  data$stops <- gsub( "65 CLAIRON CROISE AUBIN,SAINT-LIN-LAURENTIDES,Québec", "CLAIRON ET AUBIN,SAINT-LIN-LAURENTIDES,Québec", data$stops)
  data$stops <- gsub( "1317 CROISSANT DES HÊTRE,STE-JULIENNE,Québec", "1317 DES HÊTRE,STE-JULIENNE,Québec", data$stops)
  data$stops <- gsub( "HALTE VERDURE-  RTE 125,STE-JULIENNE,Québec", "1763 route 125,STE-JULIENNE,Québec", data$stops)
  data$stops <- gsub( "1635 CÔTE  ST-JOSEPH,SAINT-LIN-LAURENTIDES,Québec", "1631 CÔTE  ST-JOSEPH,SAINT-LIN-LAURENTIDES,Québec", data$stops)
  data$stops <- gsub( "ROMÉO LAPIERRE - LAMOUREUX,SAINT-LIN-LAURENTIDES,Québec", "CHEMIN ROMÉO-LAPIERRE ET RUE LAMOUREUX,SAINT-LIN-LAURENTIDES,Québec", data$stops)
  data$stops <- gsub( "20 ST-CHARLES-BOROMÉE,NA,Québec", "20 ST-CHARLES-BORROMÉE,Joliette,Québec", data$stops)
  data$stops <- gsub( "20 ST-CHARLES-BORROMÉE,NA,Québec", "20 ST-CHARLES-BORROMÉE,Joliette,Québec", data$stops)
  data$stops <- gsub( "CROISÉ 335 ET RUE DU BOIS,ST-CALIXTE,Québec", "ROUTE 335 ET RUE DU BOIS,ST-CALIXTE,Québec", data$stops)
  data$stops <- gsub( "RIVIERE SUD / CLAUDETTE,SAINT-LIN-LAURENTIDES,Québec", "RIVIERE SUD ET CLAUDETTE,SAINT-LIN-LAURENTIDES,Québec", data$stops)
  data$stops <- gsub( "CROISÉ RUE DU BOISÉ ET LAC BOB,ST-CALIXTE,Québec", "RUE DU BOISÉ ET LAC BOB,ST-CALIXTE,Québec", data$stops)
  data$stops <- gsub( "MAURICE/ LAMOUREUX,STE-JULIENNE,Québec", "MAURICE ET LAMOUREUX,STE-JULIENNE,Québec", data$stops)
  data$stops <- gsub( "CROISÉ DES SOURCES ET LAPOINTE,ST-CALIXTE,Québec", "SOURCES ET LAPOINTE,ST-CALIXTE,Québec", data$stops)
  data$stops <- gsub( "P0162 MORSE-  HÉLÈNE,NA,Québec", "P0162 MORSE-  HÉLÈNE,STE-JULIENNE,Québec", data$stops)
  data$stops <- gsub( "30 ST-ANNE,ST-JACQUES,Québec", "30 rue ST-ANNE,ST-JACQUES,Québec", data$stops)
  data$stops <- gsub( "DOMAINE DU REPOS - ROUTE 346,ST-ALEXIS (PAROISSE),Québec", "DOMAINE DU REPOS - ROUTE 346,ST-ALEXIS,Québec", data$stops)
  
  # data$stops <- gsub( "", "", data$stops)

  return(data)
}

set_latlon <- function(data){
  
  data <- data %>% filter(!grepl(pattern="NANA",x=stops) &
                            stops != "1475 ROUTE 341,ST-JACQUES,Québec" &
                            stops != "144 ST-JOSPEH,NA,Québec" &
                            stops != "3217 1E AVENUE,NA,Québec" &
                            stops != "925 FRESNIÈRE,SAINT-EUSTACHE,Québec")
  
  data[data$stops=="1631 CÔTE  ST-JOSEPH,SAINT-LIN-LAURENTIDES,Québec",c("lat","lon")]<-c(45.854093,-73807418)
  data[data$stops=="202 DES CHAMPS / LALIBERTÉ,ST-ROCH-DE-L'ACHIGAN,Québec",c("lat","lon")]<-c(45.851201,-73.594672)
  data[data$stops=="900 RANG 8 OUEST,ST-CALIXTE,Québec",c("lat","lon")]<-c(45.955191,-73.896783)
  data[data$stops=="DOMAINE DU REPOS - ROUTE 346,ST-ALEXIS (PAROISSE),Québec",c("lat","lon")]<-c(45.961903,-73.698120)
  data[data$stops=="26 ROUTE 341,ST-ROCH-DE-L'ACHIGAN,Québec",c("lat","lon")]<-c(45.869020,-73.537396)
  data[data$stops=="2339 MONTÉE DUQUETTE,STE-JULIENNE,Québec",c("lat","lon")]<-c(45.959734,-73.718451)
  data[data$stops=="COIN RANG ST-JOSEPH/RTE 335,STE-JULIENNE,Québec",c("lat","lon")]<-c(45.960331,-73.716154)
  data[data$stops=="CH. DE LA FOURCHE/DUPONT,STE-JULIENNE,Québec",c("lat","lon")]<-c(45.947995,-73.734122)
  data[data$stops=="CH. DE LA FOURCHE/DUPONT,NA,Québec",c("lat","lon")]<-c(45.947995,-73.734122)
  data[data$stops=="928 ST-LOUIS,NA,Québec",c("lat","lon")]<-c(46.021501,-73.450083)
  data[data$stops=="MAURICE/ LAMOUREUX,NA,Québec",c("lat","lon")]<-c(45.953208,-73.764653)
  data[data$stops=="195 JUTRAS,NA,Québec",c("lat","lon")]<-c(45.838729,-73.916784)
  data[data$stops=="20 ST-CHARLES-BORROMÉ,NA,Québec",c("lat","lon")]<-c(46.026228,-73.436777)
  data[data$stops=="108 DES PERVENCHES,NA,Québec",c("lat","lon")]<-c(45.831321,-73.755226)
  data[data$stops=="COIN MONTÉE CRÉPEAU ET RANG 4,ST-CALIXTE,Québec",c("lat","lon")]<-c(45.901060,-73.863794)
  data[data$stops=="1041 RANG DOUBLE,NA,Québec",c("lat","lon")]<-c(45.877150,-73.808898)
  data[data$stops=="942 ST-LOUIS,NA,Québec",c("lat","lon")]<-c(46.021312,-73.450048)
  data[data$stops=="RUE DU BOISÉ ET LAC BOB,ST-CALIXTE,Québec",c("lat","lon")]<-c(45.928552,-73.826910)
  data[data$stops=="132 RICARD,ST-ALEXIS (VILLAGE),Québec",c("lat","lon")]<-c(45.943785, -73.597801)
  data[data$stops=="460 DU PARC,NA,Québec",c("lat","lon")]<-c(45.851474, -73.753135)
  data[data$stops=="DES AMIS/JOIE,STE-JULIENNE,Québec",c("lat","lon")]<-c(45.990370, -73.712171)
  data[data$stops=="151 RAND DE L'ÉGLISE,NA,Québec",c("lat","lon")]<-c(45.989731, -73.528453)
  data[data$stops=="1000 BOUL. STE-ANNE,NA,Québec",c("lat","lon")]<-c(46.03922,-73.45667 )
  data[data$stops=="200 ST-MARC,NA,Québec",c("lat","lon")]<-c(46.028481, -73.443323)
  data[data$stops=="1270  LADOUCEUR  -,NA,Québec",c("lat","lon")]<-c(46.03379,-73.41830 )
  data[data$stops=="1270 LADOUCEUR,NA,Québec",c("lat","lon")]<-c(46.03379,-73.41830 )
  data[data$stops=="1728 RTE 335,NA,Québec",c("lat","lon")]<-c(45.899619, -73.761339)
  data[data$stops=="CH. DE LA FOURCHE/DUPONT,NA,Québec",c("lat","lon")]<-c(45.94799,-73.73412)
  data[data$stops=="197 INDUSTRIEL,NA,Québec",c("lat","lon")]<-c(45.842998, -73.757710)
   data[data$stops=="188 23E AVENUE,NA,Québec",c("lat","lon")]<-c(45.864930, -73.758068)
  
  # data[data$stops=="",c("lat","lon")]<-c()
  # 
  return(data)
}

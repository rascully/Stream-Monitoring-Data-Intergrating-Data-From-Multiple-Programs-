install.packages("rgdal")
install.packages("downloader")
install.packages('sp')
library(rgdal)
library(downloader) 
library(sp)

wd=getwd()

 
#URL Location of the AIM GeoDataBase if the location changes this will need to be updated 
#fileURL<- "https://gis.blm.gov/AIMDownload/LayerPackages/BLM_AIM_AquADat.zip"

#Download the file to the Data file
#download(fileURL, "Data/BLM.zip" )

#Unzip the file into the Data File 
#unzip("Data/BLM.zip", exdir="Data")

#Define the file path to the geodata base, if the BLM changes their file structure this will need to be updated 
#fgdb=path.expand('Data/BLM_AIM_AquADat/v104/AquADat_data.gdb')


#Read the Geodatabase layer into a file 
#BLM_geo<- readOGR(dsn=fgdb)
#BLM_projection <- BLM_geo@proj4string
#BLM <- BLM_geo@data
#write the datafile to the datafile in the repository
#write.csv(BLM,"Data/BLM_geodatabase.csv")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Pull BLM data from ArcGIS rest services 
install.packages("sf")
install.packages("tmap")
install.packages("httr")
install.packages("data.table")
install.packages("tidverse")

library(sf)
library(tmap)
library(httr)
library(data.table)
library(tidyverse)
library(sp)
  
url <- list(hostname = "gis.blm.gov/arcgis/rest/services",
            scheme = "https",
            path = "hydrography/BLM_Natl_AIM_AquADat/MapServer/0/query",
            query = list(
              where = "1=1",
              outFields = "*",
              returnGeometry = "true",
              f = "geojson")) %>% 
            setattr("class", "url")
request <- build_url(url)
BLM <- st_read(request) 

#write the datafile to the datafile in the repository
BLM_data_frame <- as.data.frame(BLM)
write.csv2(BLM_data_frame,"Data/BLM.csv", row.names=FALSE)

#save the BLM data as a GeoJSON file
st_write(BLM, dsn="Data/BLM.GeoJSON", layer="BLM", driver="GeoJSON")

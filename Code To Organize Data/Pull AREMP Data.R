#get AREMP data from geodata 

install.packages("rgdal")
install.packages("downloader")
install.packages('sp')
install.packages('geojsonio')
install.packages("sjmisc")
install.packages("sbtools")
library(rgdal)
library(downloader) 
library(sp)
library(sf)
library(tidyverse)
library(geojsonio)
library(sjmisc)
library(sbtools)


#Get URL of the AREMP dataset from ScienceBase 
authenticate_sb("rscully@usgs.gov", "PNAMPusgs28!")
sb_id <-"5e3dbb2ee4b0edb47be3d646"
sb_programs <- item_get(sb_id)
web_links<- item_get_fields(sb_id, "webLinks")

for(i in 1:length(web_links)){ 
  title = web_links[[i]][["title"]]
   if(grepl("Data", title)){
   fileURL <-  web_links[[i]][["uri"]]
   }
}

wd=getwd()

projection<- "+proj=longlat +datum=WGS84 +no_defs"

#Download the file to the Data file in the local repository 
download(fileURL, paste0(wd,"/Data/NwfpWatershedCondition20yrReport.gdb.zip" ))

#Unzip the file into the Data file in the local repository
unzip("Data/NwfpWatershedCondition20yrReport.gdb.zip", exdir="Data")

#Define the file path to the geodata base, if the ARAMP changes their file structure this will need to be updated 
path <- '/Data/NwfpWatershedCondition20yrReport.gdb'
fgdb <- paste0(wd, path)

#invistigate the layers in the AREMP geodatabase 
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)

#load the locations, stream and habitat data from the ARAMP geodatabase file 
locations   <- st_read(dsn=fgdb, layer = fc_list[10])
data        <- st_read(dsn=fgdb, layer = fc_list[11])

#macro and temp data is a score summarized by watershed need to figure out how to get the metric data 
macro       <- st_read(dsn=fgdb, layer = fc_list[6])
temp        <- st_read(dsn=fgdb, layer = fc_list[3])

#rename a columen in the data file so the locaitons and the data can be joined on that columen 
names(data)[names(data) == "site_id"] <- "SITE_ID"

#Join the location information and the metric data 
AREMP <- right_join(locations, data, by="SITE_ID")

#Transform to a standard system 
a_WGS84 <- st_transform(AREMP, crs="+proj=longlat +datum=WGS84 +no_defs")

#pull coordinates out of shapefile 
lat_long <- do.call(rbind, st_geometry(a_WGS84)) %>% 
  as_tibble(.name_repair = "unique") %>% setNames(c("longitude","lattitude", "c3", "c4"))

# create a table of the AREMP data with lat, and long 
table       <- (st_geometry(AREMP)<- NULL)
AREMP_csv   <- bind_cols(AREMP, lat_long)

file_name <- paste0(wd, "/Data/AREMP.csv")
write.csv(AREMP_csv, file=file_name, row.names=FALSE)
item_update_files(sb_id, file_name, title="")

#Save a GeoSJSON file need to check this code
#st_write(a_WGS84,dsn="Data/AREMP.GeoJSON", layer="AREMP", driver="GeoJSON")



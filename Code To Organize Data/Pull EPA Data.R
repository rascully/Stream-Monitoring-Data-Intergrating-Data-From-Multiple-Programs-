
#Code to pull and orginize EPA data. The EPA data needs to be documented in ScienceBase Item as a child of the https://www.sciencebase.gov/catalog/item/5e3db67ae4b0edb47be3d600
#see the convention in the 0405 Item: https://www.sciencebase.gov/catalog/item/5ea9d6a082cefae35a21ba5a. Use the named using the convention "Data <Name Of Data Type>". 
# For example Data Location. 

install.packages('tidyverse')
install.packages('dplyr')
install.packages('sbtools')
install.packages("RCurl")
library(tidyverse)
library(dplyr)
library(sbtools)
library(RCurl)


SBUserName  <- readline(prompt="ScienceBase User Name: ")
SBPassword  <- readline(prompt="ScienceBase Password: ")
authenticate_sb(SBUserName, SBPassword)
#Run the function to pull and orginize the0405 and 0809. Metadata for data files are stored in ScienceBase
orginize_EPA_data("5ea9d6a082cefae35a21ba5a", "0405")
orginize_EPA_data("5e3db6a4e4b0edb47be3d602", "0809")


orginize_EPA_data <- function(sb_id,years){
  
#Download the list of weblinks holding the EPA data from the ScienceBase Item holding the data 
  web_links<- item_get_fields(sb_id, "webLinks")
  
#Open all the data sets and save with the name from the metadata  
    for(i in 1:length(web_links)){ 
      if(web_links[[i]][["type"]]=='webLink') {
      print(i)
      name      <- gsub(' ', '_', web_links[[i]][["title"]])
      assign(name,tbl_df(read.csv(text=getURL(web_links[[i]][["uri"]]))))
      }
    } 
  
  
# Identify the data sets that are the physical habitat data   
  object_habitat=ls(pattern="Data_Physical_Habitat")
  
  
  # NEEE A WAY TO SORT OUT THE @))$ FIELS VS THE OTHER YEARS?? 
#if there is more then one physical habitat data set join into one 
  if(length(object_habitat)>1){
      one <- get(object_habitat[1])
      two <- get(object_habitat[2])
      Data_Habitat <- left_join(one, two, b= c("SITE_ID", "YEAR", "VISIT_NO")) 
      rm(list=object_habitat)
    }
  
  data_objects = ls(pattern="Data")
  data_objects <- data_objects[(data_objects != "Data_Site_Information")]
 
#join pysical habiat datasets to data site information  
  for(x in 1:length(data_objects)){
      if(x==1) {
        data <- full_join(get("Data_Site_Information"), get(data_objects[x]), by=c('SITE_ID', 'VISIT_NO', 'YEAR'))
      } else { 
        data <- left_join(data, get(data_objects[x]), by=c('SITE_ID', 'VISIT_NO', 'YEAR'))
        }
} 

#remove columes with .y indicating duplicate columens 
data<- select(data, -contains(".y"))
#Rename columens with .x, so that field names match the orginal fields in the metadata 
names(data) <- str_remove(names(data), ".x")

#Save the file 
file_name <- paste0(getwd(),"/Data/EPA_NARS_Data",years,".csv")
write.csv(data, file=file_name, row.names=FALSE)
item_update_files(sb_id, file_name, title="")
} 


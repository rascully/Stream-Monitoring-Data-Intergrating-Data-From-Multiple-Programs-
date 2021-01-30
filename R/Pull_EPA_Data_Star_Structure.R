
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
sb_id<- "5ea9d6a082cefae35a21ba5a"

#Download the list of weblinks holding the EPA data from the ScienceBase Item holding the data 
  web_links<- item_get_fields(sb_id, "webLinks")
  
#Open all the data sets documented on ScienceBase and save with the name from the metadata  
   for(i in 1:length(web_links)){ 
    if(web_links[[i]][["type"]]=='webLink') {
     name      <- gsub(' ', '_', web_links[[i]][["title"]])
     assign(name,as_tibble(read.csv(web_links[[i]][["uri"]])))
    }
  }  

#The list of years the EPA data is collected and organized by  
list_years <- c(2004, 2008)
  
  for (y in 1:length(list_years)){
    #Identify the list of data sets that are within a year 
        datasets_by_year <- (ls(pattern=list_years[y]))
    # Identify the data sets that are the physical habitat data   
        object_habitat = datasets_by_year[str_detect(datasets_by_year, "Data_Physical_Habitat")]
    #if there is more then one physical habitat data set join into one 
        if(length(object_habitat)>1){
            one <- get(object_habitat[1])
            two <- get(object_habitat[2])
            name <- paste0("Data_Habitat_", list_years[y])
            assign(paste0("Data_Habitat_", list_years[y]),left_join(one, two, b= c("SITE_ID", "YEAR", "VISIT_NO"))) 
            rm(list=object_habitat)
          }
        
      data_objects <- (ls(pattern=list_years[y]))
      #join physical habitat data sets to site information  
        for(x in 1:length(data_objects)){
            if(x==1) {
              data <- full_join(get(paste0("Data_Site_Information_", list_years[y])), get(data_objects[x]), by=c('SITE_ID', 'VISIT_NO', 'YEAR'))
            } else { 
              data <- left_join(data, get(data_objects[x]), by=c('SITE_ID', 'VISIT_NO', 'YEAR'))
              }
          } 
      
      #remove columns with .y indicating duplicate columns 
      data<- select(data, -contains(".y"))
      #Rename columns with .x, so that field names match the original fields in the metadata 
      names(data) <- str_remove(names(data), ".x")
      
      #Save the file to ScienceBase Item. If new add, if already exists just update the existing fiel. 
      short_name = paste0(Sys.Date(),"_Tidy_NARS_",list_years[y],".csv")
      file_name <- paste0(getwd(),"/Data/", short_name)
      write.csv(data, file=file_name, row.names=FALSE)
        
    if(any(str_detect(item_list_files(sb_id)$fname, short_name))){
        item_replace_files(sb_id, file_name, title="")
        print(1)
        } else {
          item_append_files(sb_id, file_name)
          print(2)
      }
      
      # Update the lastProcessed date to indicate the last time the code was run 
      sb_dates <- item_get_fields(sb_id, c('dates'))
      
      for(d in 1:length(sb_dates)){ 
        if(sb_dates[[d]][["type"]]=='lastProcessed') {
          sb_dates[[d]][["dateString"]] <- Sys.Date() 
         items_update(sb_id, info = list(dates = sb_dates)) 
        }
      }  
      
} 
  
  
  
 


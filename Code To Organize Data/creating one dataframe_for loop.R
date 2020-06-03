install.packages("dplyr") 
install.packages("openxlsx")
install.packages("tidyverse")
install.packages("readxl")
install.packages("sbtools")

# Create one dataframe of a subset of metrcs across the four prorams 
one_data_frame <- function() {
    library(dplyr)
    library(readxl)
    library(tidyverse)
    library(openxlsx)
    library(sf)
    library(tmap)
    library(httr)
    library(data.table)
    library(sp)
    library(sbtools)
    library(rgdal)
        
#wd
    wd <- "C:/Users/rscully/Documents/Projects/Habitat Data Sharing/2019_2020/Code/tributary-habitat-data-sharing-/"
   
#open the the metadata file 
    
    metadata  <- as_tibble(read_xlsx(paste0(wd,"Data/Metadata.xlsx") , 3))
    SN        <- select(metadata, c(Category, LongName, Field, DataType ,AREMPColumn, BLMColumn, EPA2008Column, EPA2004Column, PIBOColumn, Subset_of_Metrics))
    SN        <- as_tibble(lapply(SN, as.character))
    
    
    #Covnert blankes to missing values 
    SN  <-  mutate_all(SN, funs(na_if(.,"")))

    # Extract the subset of metrics we are focusing on 
    subset_metrics <- SN %>% 
                    filter(Subset_of_Metrics== "x")
    
    #save the list of the subset of metrics
    write.csv(subset_metrics, file="Data/SubSetOfMetricNames.csv", row.names=FALSE)
    
   # return(subset_metrics)
  
    #Create a variable holding the short names 
    short_names <- subset_metrics$Field
    data_types  <- subset_metrics$DataType
   
    #list of unique data types 
    unique_data_types <- unique(data_types)
    
    
    #Create a empty dataframe with the short names 
    all_data <- data.frame(matrix(ncol = length(short_names), nrow = 0))
    colnames(all_data) <- short_names
  
  #list of programs. Removed PIBO before publishing to ScienceBase (2020_3_17)     
  program <-c('EPA2004','EPA2008', 'BLM','AREMP')
        
        #For loop to add data from each program to one data set 
        for(i in 1:length(program)) {
            #Load the data 
                if (program[i]=="EPA2008"){
                    data <-as_tibble(read.csv("Data/EPA_subset.csv"))
                  } else if (program[i]=="EPA2004") {
                    data<- as_tibble(read.csv("Data/EPA_Subset_2004.csv"))
                  } else if (program[i]=="BLM") { 
                    #create a URL to access the BLM Data
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
                    BLM <- st_read(request, stringsAsFactors = TRUE) #Load the file from the Data file 
                    data <- as_tibble(BLM)
                  } else if (program[i]=="PIBO"){ 
                    data <- as_tibble(read_xlsx("Data/PIBO_2013.xlsx", 2))
                  } else if (program[i]== "AREMP") {
                    data <- as_tibble(read.csv("Data/AREMP.csv"))
                  }
         
          #create a column name to reference 
          column <- paste0(program[i],"Column")
          c      <- ((names(subset_metrics)==column)==TRUE)
          
          # Create a subset of metrics  
          program_metric_names <- as.data.frame(subset_metrics[c])
          
          #index of the locations where there is a column name 
          index = !is.na(program_metric_names)
          
          #Clear SubSetData variable 
          SubSetData <- 0
          
          #Subset the data from the master dataframe 
          SubSetData <- data %>%
            select(program_metric_names[index])
          
          #Rename to the standard columen names 
          colnames(SubSetData) <- short_names[index]
          
          #Use index to sub set the data_types to the set of metrics that are in the proram dataset
          p_data_types = data_types[index]
          
          #Assign a datatypes to each metric so it mactches the data frame   
          SubSetData[p_data_types== "Double"]     <- sapply(SubSetData[p_data_types== "Double"], as.double)
          SubSetData[p_data_types== "Character"]  <- sapply(SubSetData[p_data_types=="Character"], as.character)
          SubSetData[p_data_types== "Date"]       <- sapply(SubSetData[p_data_types=="Date"], as.character)
          SubSetData[p_data_types== "Interger"]   <- sapply(SubSetData[p_data_types=="Interger"], as.character)
          
  #Add a columen to id Program 
          if(str_detect(program[i], "EPA")){ 
            SubSetData$Program <- "EPA"
            } else { 
          SubSetData$Program <- program[i]
          } 
  #Add the program data to the full data set 
          all_data=bind_rows(all_data, SubSetData)
  }
          
        plot(all_data$verbatimLongitude, all_data$verbatimLatitude)
        
        all_data2 = all_data %>%
                        filter(!is.na(verbatimLongitude) & !is.na(verbatimLatitude))

#Write data to a .csv
    file_path <- paste0(getwd(), "/Data/All_Data.csv")
    write.csv(all_data2, file=file_path, row.names=FALSE)
       
#Write the intergrated dataset to ScenceBase   
    authenticate_sb("rscully@usgs.gov", "PNAMPusgs28!")
    sb_id = "5e3c5883e4b0edb47be0ef1c"
    item_replace_files(sb_id,file_path, title = "Intergrated Dataset")  
        
#create a list of sties with unique locations 
    u_locations <- select(all_data2, (c(locationID, verbatimLatitude, verbatimLongitude, verbatimWaterbody, Program)))
    unique_locations <- distinct(u_locations)
    unique_path <- paste0(getwd(), "/Data/unique_locations.csv")
    write.csv(unique_locations, file=unique_path, row.names=FALSE)
    #Update ScienceBase Item   
    item_replace_files(sb_id,unique_path, title ="A list of unique data collection locations")  
        
#Save to GEOJason 
    locations <- select(all_data2, one_of("verbatimLongitude", "verbatimLatitude"))
    data      <- select(all_data2, -contains(c("verbatimLongitude","verbatimLatitude")))
    spatial_data = SpatialPointsDataFrame(locations, data, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
    json_file <- paste0(getwd(), "/Data/data_all.geojson")
    writeOGR(spatial_data,'Data/data_all.geojson', layer="", driver="GeoJSON")
    #Update ScienceBase Item 
    item_replace_files(sb_id, json_file, title="Intergrated Dataset") 


}



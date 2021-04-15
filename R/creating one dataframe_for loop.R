#####Using the data exchange specifications combined monitoring stream data from multiple sources. #####
# Load the libraries 
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
   

# Load the functions to build the data tables to the Stream Monitoring Data
# Exchange Specifications in this repository: https://github.com/rascully/Stream-Monitoring-Data-Exchange-Specifications

#List of programs to integrated data from. Can add PIBO when they release their data    
program <-c("NRSA","AREMP", "AIM")


##### Sign into ScienceBase and pull data set information #####
SBUserName  <- readline(prompt="ScienceBase User Name: ")
SBPassword  <- readline(prompt="ScienceBase Password: ")
authenticate_sb(SBUserName, SBPassword)

#ScienceBase ID of the parent item for integrating stream habitat metrics 
sb_id <-"5e3c5883e4b0edb47be0ef1c"
  
#Get the list of programs from the parent ScienceBase Item 
sb_child <- item_list_children(sb_id)
  
######Create the Record Level Data Table #####
  #Based on the data exchange specifications we need to create a record level table to document the metadata
  #about the each data sets. Download the 
  
#Git Hub link to the Record Level table template 
github_link <- "https://raw.githubusercontent.com/rascully/Stream-Monitoring-Data-Exchange-Specifications/master/Tables/RecordLevel_table.csv" 
temp_file <- tempfile(fileext = ".csv")
req <- GET(github_link, 
           # authenticate using GITHUB_PAT
             authenticate(Sys.getenv("GITHUB_PAT"), ""),
             # write result to disk
             write_disk(path = temp_file))
Record_level<- read.csv(temp_file)
unlink(temp_file)
  
#create an empty dataframe to fill with the record level data exchange specifications 
record_level_table     <- data.frame(matrix(ncol=length(Record_level$Term), nrow=length(sb_child))) 
  
record_level_table     <-  record_level_table %>% 
                             mutate_all(as.character)
  
#name the columns in the dataframe with the Record Level terms
colnames(record_level_table)  <- Record_level$Term  

# Fill in the Reach Level table with information about each data set 
  for(i in 1:length(sb_child)){ 
    record_level_table$datasetName[i]    <- sb_child[[i]][["title"]]
    record_level_table$datasetID[i]      <- sb_child[[i]][["id"]]
    }
  record_level_table$modified    <-   Sys.Date()
  record_level_table$type        <- "Stream Habitat Moitoring Data"
  
##### Open Git File Of Crosswalk #####
    
   # metadata  <- as_tibble(read_xlsx(paste0(wd,"/Metadata.xlsx") , 3))
    github_link <-"https://raw.githubusercontent.com/rascully/Stream-Monitoring-Data-Exchange-Specifications/master/Tables/Crosswalk.csv"
    temp_file <- tempfile(fileext = ".csv")
    
    # get the git file and save the temp 
    req <- GET(github_link, 
               # authenticate using GITHUB_PAT
               authenticate(Sys.getenv("GITHUB_PAT"), ""),
               # write result to disk
               write_disk(path = temp_file))
    
    cross_walk      <- read.csv(file= temp_file)
    cross_walk      <- cross_walk %>% 
                        mutate_all(na_if, "")
    CW              <- select(cross_walk, c(LongName, measurementTerm, DataType ,AREMPField, AIMField, NRSAField, PIBOField))
    subset_metrics  <- as_tibble(lapply(CW, as.character))
    subset_methods  <- select(cross_walk, contains("Term")| contains("MethodID")| contains("Unit") |"measurementType"|"measurementID")
    
    
    #array of the field names used in the combined data set  
    short_names <- CW$measurementTerm
    #array of a list of the data types of each variable 
    data_types  <- CW$DataType
   
    #list of unique data types 
    unique_data_types <- unique(data_types)
    
    #Create a empty dataframe with the field names  
    
    all_data <- data.frame(matrix(ncol = length(short_names), nrow = 1))
    colnames(all_data) <- short_names
    
##### For loop to add data from each program to one data set #####
  for(p in program) {
   
        #Load the data 
           if (p=="NRSA"){
             #Download the data from ScienceBase 
             file_name<- paste0(getwd(),"/data/", "Tidy_NRSA_Data_Set.csv")
             data <-as_tibble(read.csv(file_name))
                   
              #Fill in Reach Level table 
                index <- str_detect(record_level_table$datasetName, "EPA")
                record_level_table$InstitutionID[index]<- "EPA"
                record_level_table$CollectionID[index]<- "NRSA"
                datasetID <- record_level_table$datasetID[index]
                 
                } else if (p=="AIM") { 
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
                    
                    #Fix the date 
                    data$DT <- as.POSIXct(data$DT/1000, origin="1970-01-01")
                    data$DT <- str_remove(data$DT, "PDT")
                    
                  #Fill in Record Level table 
                    index <- str_detect(record_level_table$datasetName, "BLM")
                    record_level_table$InstitutionID[index]<- "BLM"
                    record_level_table$CollectionID[index]<- "AIM"
                    datasetID <- record_level_table$datasetID[index]
                  
                  } else if (p=="PIBO"){ 
                      data <- as_tibble(read_xlsx("Data/PIBO_2013.xlsx", 2))
                   #Fill in record_level table 
                      index <- str_detect(record_level_table$datasetName, "PIBO")
                      record_level_table$InstitutionID[index]<- "USFS"
                      record_level_table$CollectionID[index]<- "PIBO"
                      datasetID <- record_level_table$datasetID[index]
                 
                  } else if (p== "AREMP") {
                  #Download the data table from the ScienceBase item
                    file_name<- paste0(getwd(),"/data/", "Tity_AREMP_Data_Set.csv")
                    data <-as_tibble(read.csv(file_name))
                  #Fill in Record Level table 
                    index <- str_detect(record_level_table$datasetName, "AREMP")
                    record_level_table$InstitutionID[index]<- "USFS"
                    record_level_table$CollectionID[index]<- "AREMP"
                    datasetID <- record_level_table$datasetID[index]
              }
         
          #create a column name to reference 
          column <- paste0(p,"Field")
          #Data frame of the the names of the fields in the cross walk 
          program_metric_names <- subset_metrics %>% 
              select(c(column,"measurementTerm", "DataType")) %>% 
              drop_na(column)

          
          #check the metrics in the cross_walk are in the data set and create a vector of the 
          #names for the final data set 
          
          specific_names <- as.vector(unlist(select(program_metric_names, column)))
          
          CW_names_index        <- names(data) %in% specific_names
          CW_names              <- names(data)[CW_names_index]
          data_set_names_index  <- specific_names %in% CW_names 
          data_set_names        <- program_metric_names$measurementTerm[data_set_names_index]
        
          #array of cross walk names that at this time are not in the origin data sets 
          CW_names_not_found  <- names(data)[!CW_names_index]
          
          
          #Clear SubSetData variable 
          SubSetData <- 0
          #Subset the data from the master dataframe 
          SubSetData <- data %>%
            select(CW_names)
          
          #Rename to the standard column names to the master data set names 
          for(n in 1:length(data_set_names)){
            if (n ==1)  {data_set_name <- character(length(data_set_names))
                        ordered_data_type <- character(length(data_set_name))}
            program_name          =  names(SubSetData)[n]
            name_index            <- specific_names == program_name
            data_set_name[n]      <- program_metric_names$measurementTerm[name_index]
            ordered_data_type[n]  <- program_metric_names$DataType[name_index]
          } 
          
          
          n = 0
          colnames(SubSetData)<- data_set_name
        
          
          #Assign a datatypes to each metric so it matches the data frame   
          SubSetData[ordered_data_type== "Numeric"]    <- sapply(SubSetData[ordered_data_type== "Numeric"], as.double)
          SubSetData[ordered_data_type== "String"]     <- sapply(SubSetData[ordered_data_type=="String"], as.character)
          SubSetData[ordered_data_type== "Date"]       <- sapply(SubSetData[ordered_data_type=="Date"], as.character)
          SubSetData[ordered_data_type== "Interger"]   <- sapply(SubSetData[ordered_data_type=="Interger"], as.character)
          
          #Add a column a program and Program 
          SubSetData$Program <- p
          SubSetData$datasetID <-  datasetID
          #Add the program data to the full data set 
          all_data=bind_rows(all_data, SubSetData)
    }
      

  plot(all_data$verbatimLongitude, all_data$verbatimLatitude)
  all_data2 = all_data %>%
              filter(!is.na(verbatimLongitude) & !is.na(verbatimLatitude))
  
  #Fill in location data with the verbatimLocation, 
  #as we add additional data sets we will need to think about creating a locationID
  #for the integrated data set. Just to make sure that programs don't repeat locationIDs 
  all_data2$locationID  <- all_data2$verbatimlocationID
  
  #create a list of sites with unique locations 
  u_locations <- select(all_data2, (c(locationID, verbatimLatitude, verbatimLongitude,
                                      verbatimWaterbody, Program)))
  unique_locations <- distinct(u_locations)
  unique_path <- paste0(getwd(), "/Data/unique_locations.csv")
  write.csv(unique_locations, file=unique_path, row.names=FALSE)
  
  
####Subset the data set to match the data exchange specifications documented on https://github.com/rascully/Stream-Monitoring-Data-Exchange-Specifications#####
  
  github_link <- "https://raw.githubusercontent.com/rascully/Stream-Monitoring-Data-Exchange-Specifications/master/Tables/Location_table.csv" 
  temp_file <- tempfile(fileext = ".csv")
  req <- GET(github_link, 
             # authenticate using GITHUB_PAT
             authenticate(Sys.getenv("GITHUB_PAT"), ""),
             # write result to disk
             write_disk(path = temp_file))
  
  Location_table<- read.csv(temp_file)
  location_table <- all_data2 %>% 
                    select(one_of(c("datasetID", "Program", Location_table$measurementTerm))) %>% 
                    distinct()
   
  
#Build the event table 
  github_link <- "https://raw.githubusercontent.com/rascully/Stream-Monitoring-Data-Exchange-Specifications/master/Tables/Event_table.csv" 
  temp_file <- tempfile(fileext = ".csv")
  req <- GET(github_link, 
             # authenticate using GITHUB_PAT
             authenticate(Sys.getenv("GITHUB_PAT"), ""),
             # write result to disk
             write_disk(path = temp_file))
  Event_table <- read.csv(temp_file)    
  event_table <- all_data2 %>% 
                  select(one_of(c("Program","locationID", Event_table$measurementTerm)))
  event_table$ver
  
#Create the MeasuremetOrFact table
 
 measurement_names <- CW %>% 
                    filter(str_detect(CW$Category, "ControlledVocabulary")) %>% 
                    select(measurementTerm) %>% 
                    pull()


 measurement <- all_data2 %>% 
   select(Program, eventID, EventDate, measurement_names) %>% 
   add_column(measurementMethod= "", measurementID ='', measurementUnit='', measurementRemarks='', measurementType='') %>% 
   rename(measurementDeterminedDate=EventDate, measurementDeterminedBy=Program)
 
 measurment_or_fact_table <- measurement %>% 
          pivot_longer(cols = measurement_names, 
                       names_to ="measurementTerm", values_to="measurementValue") %>% 
                       drop_na(measurementValue) 
 

for(term in unique(measurment_or_fact_table$measurementTerm)){ 
  #Add the measurmentID to the measurement or fact table 
   method_info <- subset_methods %>% 
     filter(Term== term)
   
   m_index                                            <- measurment_or_fact_table$measurementTerm==term
   measurment_or_fact_table$measurementID[m_index]    <- select(method_info, "measurementID")
   measurment_or_fact_table$measurementUnit[m_index]  <- select(method_info, Unit)
   #add the measurement type 
   measurment_or_fact_table$measurementType[m_index]  <- select(method_info, "VocabularyCatagory")
     
   for(p in program){
   # Add the link to the MonitoringResources.org 
     metric_field <- paste0(p, "CollectionMethodID")  
     mr_method <- method_info %>% 
         select(contains(metric_field)) 
    
     index <- measurment_or_fact_table$measurementTerm==term & measurment_or_fact_table$measurementDeterminedBy==program
     measurment_or_fact_table$measurementMethod[index] <- mr_method
 
     }
 } 

measurment_or_fact_table <- measurment_or_fact_table %>% 
                                 relocate(c("eventID",  "measurementType", "measurementID","measurementTerm","measurementValue", "measurementUnit",
                                            "measurementDeterminedDate", "measurementDeterminedBy","measurementMethod", "measurementRemarks" ))  
        
#Save the data set 
 list_of_datasets <- list("Record_level" = record_level_table, "location"= location_table, "Event"= event_table,
                          "Measurment_or_Fact"= measurment_or_fact_table)
 
 write.xlsx(list_of_datasets, file = "data/Integrated Data Set.xlsx") 
 
#Write data to a .csv
    file_name <- paste0("/Data/", Sys.Date(), "All_Data.csv")
    file_path <- paste0(getwd(), "/Data/Flat Integrated Data Set.csv")
    write.csv(all_data2, file=file_path, row.names=FALSE)
       
#Write the integrated data set to ScenceBase   
    sb_id = "5e3c5883e4b0edb47be0ef1c"
    file_name = "data/Integrated Data Set.xlsx"
    item_replace_files(sb_id,file_name, title = "IntegratedDataSet")  

#Update ScienceBase Item   
    item_replace_files(sb_id,unique_path, title ="A list of unique data collection locations")  

# Update the last Processed date to indicate the last time the code was run 
    sb_dates <- item_get_fields(sb_id, c('dates'))
    
    for(d in 1:length(sb_dates)){ 
      if(sb_dates[[d]][["type"]]=='lastProcessed') {
        sb_dates[[d]][["dateString"]] <- Sys.Date() 
        items_update(sb_id, info = list(dates = sb_dates)) 
      }
    }      






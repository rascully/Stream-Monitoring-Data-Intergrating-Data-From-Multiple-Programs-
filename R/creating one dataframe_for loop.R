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
program <-c('NRSA2004','NRSA2008','AIM','AREMP')


##### Sign into ScienceBase and pull data set information #####
SBUserName  <- readline(prompt="ScienceBase User Name: ")
SBPassword  <- readline(prompt="Password: ")
authenticate_sb(SBUserName, SBPassword)

#ScienceBase ID of the parent item for integrating stream habitat metrics 
  sb_id <-"5e3c5883e4b0edb47be0ef1c"
  
#Get the list of programs from the parent ScienceBase Item 
  sb_child <- item_list_children(sb_id)
  #child_id <- rbind(title,id)

######Create the Record Level Data Table #####
  #Git Hub link to the Record Level table template 
  github_link <- "https://raw.githubusercontent.com/rascully/Stream-Monitoring-Data-Exchange-Specifications/master/Tables/RecordLevel_table.csv" 
  temp_file <- tempfile(fileext = ".csv")
  req <- GET(github_link, 
             # authenticate using GITHUB_PAT
             authenticate(Sys.getenv("GITHUB_PAT"), ""),
             # write result to disk
             write_disk(path = temp_file))
  RL<- read.csv(temp_file)
  unlink(temp_file)
  reach_level_table            <- data.frame(matrix(ncol=length(RL$Term), nrow=length(sb_child))) #create an empty dataframe to fill with the record level data exchange specifications 
  
  reach_level_table %>% 
    mutate_all(as.character)
  #name the columns 
  colnames(reach_level_table)  <- RL$Term #name the columns in the dataframe with the Record Level terms 
  
  for(i in 1:length(sb_child)){ 
    reach_level_table$datasetName[i] <- sb_child[[i]][["title"]]
    reach_level_table$datasetID[i]      <- sb_child[[i]][["id"]]
    }
  reach_level_table$modified    <-   Sys.Date()
  reach_level_table$type        <- "Stream Habitat Moitoring Data"
  
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
    
    metadata <- read.csv(file= temp_file)
    SN        <- select(metadata, c(Category, LongName, Term, DataType ,AREMPField, AIMField, NRSA2008Field, NRSA2004Field, PIBOField))
    SN        <- as_tibble(lapply(SN, as.character))
    subset_metrics <- SN 
    
    
    #Convert blanks to missing values 
    #SN  <-  mutate_all(SN, funs(na_if(.,"")))

    # Extract the subset of metrics we are focusing on 
    #subset_metrics <- SN %>% 
     #               filter(SubsetOfMetrics== "x" | InDES == 'x')
    
    #save the list of the subset of metrics
    #write.csv(subset_metrics, file="SubSetOfMetricNames.csv", row.names=FALSE)
    
   # return(subset_metrics)
  
    #Create a variable holding the short names 
    short_names <- SN$Term
    data_types  <- SN$DataType
   
    #list of unique data types 
    unique_data_types <- unique(data_types)
    
    #Create a empty dataframe with the short names 
    all_data <- data.frame(matrix(ncol = length(short_names), nrow = 1))
    colnames(all_data) <- short_names
    

  #For loop to add data from each program to one data set 
  for(i in 1:length(program)) {
      #Program name removing year   
        if (str_detect(program[i], "NRSA")){ 
            p <-  "NRSA"
           }else{ 
             p <- program[i] 
            }

        # find the ScienceBase ID for the program documentation 
        index       <- str_detect(reach_level_table$datasetName, p)
        sb_id_child <- reach_level_table$datasetID[index]
        files       <- item_list_files(sb_id_child)
          
            #Load the data 
                if (program[i]=="NRSA2008"){
               #Download the data from ScienceBase 
                   f_name <-  files$fname[str_detect(files$fname, "2008")]
                    file_name<- paste0(getwd(),"/data/", "EPA_2008.csv")
                    item_file_download(sb_id_child, names= f_name, 
                             destinations = file.path(file_name),overwrite_file = T)
                    data <-as_tibble(read.csv(file_name))
                   
                #Fill in Reach Level table 
                    index <- str_detect(reach_level_table$datasetName, "EPA")
                    reach_level_table$InstitutionID[index]<- "EPA"
                    reach_level_table$CollectionID[index]<- "NARS"
                 
                   } else if (program[i]=="NRSA2004") {
                #Download the data from ScienceBase
                     f_name <-  files$fname[str_detect(files$fname, "2004")]
                     file_name<- paste0(getwd(),"/data/", "EPA_2004.csv")
                     item_file_download(sb_id_child, names= f_name, 
                                        destinations = file.path(file_name),overwrite_file = T)
                     data<- as_tibble(read.csv(file_name))
                    
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
                  #Fill in Reach Level table 
                    index <- str_detect(reach_level_table$datasetName, "BLM")
                    reach_level_table$InstitutionID[index]<- "BLM"
                    reach_level_table$CollectionID[index]<- "AIM"
                  
                  } else if (program[i]=="PIBO"){ 
                    data <- as_tibble(read_xlsx("Data/PIBO_2013.xlsx", 2))
                    #Fill in Reach_level table 
                    index <- str_detect(reach_level_table$datasetName, "PIBO")
                    reach_level_table$InstitutionID[index]<- "USFS"
                    reach_level_table$CollectionID[index]<- "PIBO"
                  } else if (program[i]== "AREMP") {
                #Download the data table from the ScienceBase item 
                    f_name <-  files$fname[str_detect(files$fname)]
                    file_name<- paste0(getwd(),"/data/", "AREMP.csv")
                    item_file_download(sb_id_child, names= f_name, 
                                       destinations = file.path(file_name),overwrite_file = T)
                    data <-as_tibble(read.csv(file_name))
                    
                  
                #Fill in Reach Level table 
                    index <- str_detect(reach_level_table$datasetName, "AREMP")
                    reach_level_table$InstitutionID[index]<- "USFS"
                    reach_level_table$CollectionID[index]<- "AREMP"
              }
         
          #create a column name to reference 
          column <- paste0(program[i],"Field")
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
          
          #Use index to sub set the data_types to the set of metrics that are in the program data set
          p_data_types = data_types[index]
          
          #Assign a datatypes to each metric so it matches the data frame   
          SubSetData[p_data_types== "Double"]     <- sapply(SubSetData[p_data_types== "Double"], as.double)
          SubSetData[p_data_types== "Character"]  <- sapply(SubSetData[p_data_types=="Character"], as.character)
          SubSetData[p_data_types== "Date"]       <- sapply(SubSetData[p_data_types=="Date"], as.character)
          SubSetData[p_data_types== "Interger"]   <- sapply(SubSetData[p_data_types=="Interger"], as.character)
          
           #Add a column to id Program 
          if(str_detect(program[i], "NRSA")){ 
            SubSetData$Program <- "NRSA"
            } else { 
            SubSetData$Program <- program[i]
           } 
         #Add the program data to the full data set 
          all_data=bind_rows(all_data, SubSetData)
        # Update the data set with data exchange specifications 
          
  }
          
  
    
    plot(all_data$verbatimLongitude, all_data$verbatimLatitude)
  all_data2 = all_data %>%
              filter(!is.na(verbatimLongitude) & !is.na(verbatimLatitude))
        
#Add a metadata to the data set columns from the data exchange specifications  
#The nature of the resources in-stream habitat data 
      all_date2 %>%
        mutate(type= "Stream_Habitat_Metrics") %>%
        mutate(modified = Sys.Date())  %>%
        
#Write data to a .csv
    file_path <- paste0(getwd(), "/Data/All_Data.csv")
    write.csv(all_data2, file=file_path, row.names=FALSE)
       
#Write the integrated dataset to ScenceBase   
    sb_id = "5e3c5883e4b0edb47be0ef1c"
    item_repslace_files(sb_id,file_path, title = "Intergrated Dataset")  
    
    
    if(any(str_detect(item_list_files(sb_id)$fname, short_name))){
      item_replace_files(sb_id, file_name, title="")
      print(1)
    } else {
      #if the file is not already associated with the item save to SceicneBase
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


#create a list of sites with unique locations 
    u_locations <- select(all_data2, (c(locationID, verbatimLatitude, verbatimLongitude, verbatimWaterbody, Program)))
    unique_locations <- distinct(u_locations)
    unique_path <- paste0(getwd(), "/Data/unique_locations.csv")
    write.csv(unique_locations, file=unique_path, row.names=FALSE)

#Update ScienceBase Item   
    item_replace_files(sb_id,unique_path, title ="A list of unique data collection locations")  
    
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
    
#Save to GEOJason 
    locations <- select(all_data2, one_of("verbatimLongitude", "verbatimLatitude"))
    data      <- select(all_data2, -contains(c("verbatimLongitude","verbatimLatitude")))
    spatial_data = SpatialPointsDataFrame(locations, data, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
    json_file <- paste0(getwd(), "/Data/data_all.geojson")
    writeOGR(spatial_data,'Data/data_all.geojson', layer="", driver="GeoJSON")
#Update ScienceBase Item 
    item_replace_files(sb_id, json_file, title="Intergrated Dataset") 
    
# Update the last Processed date to indicate the last time the code was run 
    sb_dates <- item_get_fields(sb_id, c('dates'))
    
    for(d in 1:length(sb_dates)){ 
      if(sb_dates[[d]][["type"]]=='lastProcessed') {
        sb_dates[[d]][["dateString"]] <- Sys.Date() 
        items_update(sb_id, info = list(dates = sb_dates)) 
      }
    }      






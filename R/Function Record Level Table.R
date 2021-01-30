library(tidyverse)
library(httr)
#library(xlsx)

RL_Table <- function(datasetID,program){

#Pull the record level data exchange specifications from table from the GIT Repository https://github.com/rascully/Stream-Monitoring-Data-Exchange-Specifications
#github_link <- "https://github.com/rascully/Stream-Monitoring-Data-Exchange-Specifications/blob/master/Tables/RecordLevel.xlsx?raw=true"
github_link <- "https://raw.githubusercontent.com/rascully/Stream-Monitoring-Data-Exchange-Specifications/master/Tables/RecordLevel_table.csv" 


temp_file <- tempfile(fileext = ".csv")
req <- GET(github_link, 
           # authenticate using GITHUB_PAT
           authenticate(Sys.getenv("GITHUB_PAT"), ""),
           # write result to disk
           write_disk(path = temp_file))

#RL <- readxl::read_excel(temp_file) #read in the record level data exchange specifications 
RL<- read.csv(temp_file)
unlink(temp_file)
 
 
table            <- data.frame(matrix(ncol=length(RL$Term), nrow=1)) #create an empty dataframe to fill with the record level data exchange specifications 
table %>% 
  mutate_all(as.character)

colnames(table)  <- RL$Term #name the columns in the dataframe with the Record Level terms 


      table$datasetID                =   datasetID
     # table$type                    = 
      table$modified                 =   Sys.Date()
      table$rightsHolder             =   program
      #RL_table$bibilographicCititation  =   
      #RL_table$InstitutionID            =   
      #RL_table$CollectionID             = 
      #RL_table$datasetName              = 
      #RL_table$institutionCode          = 
      #RL_table$basisOfRecord            = 
      #RL_table$informationWithheld      =
     
      #Authenticate ScienceBase
  SBUserName  <- readline(prompt="ScienceBase User Name: ")
  SBPassword  <- readline(prompt="Password: ")
  authenticate_sb(SBUserName, SBPassword)
      
  #ScienceBase ID of the parent item for integrating stream habitat metrics 
  sb_id <-"5e3c5883e4b0edb47be0ef1c"
  
  #Get the list of programs from the parent ScienceBase Item 
  sb_child <- item_list_children(sb_id)
  sb_child   
    

  for(i in 1:length(sb_child)){ 
    table$datasetName <- sb_child[[i]][["title"]]
    table$datasetID   <- sb_child[[i]][["id"]]
    if(str_detect(sb_child[[i]][["title"]], "EPA")){ 
      table$InstituionID <- "EPA"
    } if else (str_dect { 
      SubSetData$Program <- program[i]
    } 
  }
  
   
       
      return(table)
}



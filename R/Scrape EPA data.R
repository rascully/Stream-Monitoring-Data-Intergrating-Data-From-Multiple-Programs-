#####Scraping EPA data from the EPA data webpage #####


library(tidyverse)
library(rvest)
library(stringr)
library(httr)

######Download all the data links from the EPA web site#####
content <- read_html("https://www.epa.gov/national-aquatic-resource-surveys/data-national-aquatic-resource-surveys")

tables <- content %>% 
  html_table(fill = TRUE) 

EPA_table <- tables[[1]]

## Data links
web <- content %>%
  html_nodes("table tr")%>%
  html_nodes(xpath="//td[3]") %>%  ## xpath
  html_nodes("a") %>%
  html_attr("href")

EPA_table$web1 <- web  ## add files column

## metadata links accordingly
web2 <- content %>%
  html_nodes("table tr") %>%
  html_nodes(xpath="//td[4]") %>%  ## xpath
  html_nodes("a") %>%
  html_attr("href")


EPA_table[EPA_table$Metadata %in% "", "Metadata"] <- NA
EPA_table[!is.na(EPA_table$Metadata), "web2"] <- web2

#####Sort out the NARS data#####
NARS <- EPA_table %>% 
          filter(str_detect(Survey, "Streams"))

#####Join the tables to create one data set of all the stream data#####

location_data <- NARS %>% 
                filter(str_detect(Indicator, "Site Information"))

for (i in 1:length(location_data$web1)) { 
  print(i)
  url_link <- paste0("https://www.epa.gov", location_data$web1[i])
  temp_file <- tempfile(fileext = ".csv")
  download.file(url_link, temp_file)
  if (i ==1) { 
    data1       <- read.csv(temp_file)
    data1$HUC2  <- as.character(data1$HUC2)
    data1$HUC8  <- as.character(data1$HUC8)
   # data1$EPA_REG <- as.character(data1$EPA_REG)
    } else { 
    data2     <- read.csv(temp_file)
   
     if(any(str_detect(names(data2),"HUC8"))==T) {
        print("yes")
        if (is.character(data2$HUC8)==F) {
                data2$HUC8  <- as.character(data2$HUC8)
                print("yes yes") }
        } 
    if(any(str_detect(names(data2),"EPA_REG"))==T) {
      print("yes 2")  
      data2$EPA_REG <- as.character(data2$EPA_REG)
      } 
    
    data1     <- bind_rows(list(data1, data2))
  }
  unlink(temp_file)
  data2 <- 0 
} 




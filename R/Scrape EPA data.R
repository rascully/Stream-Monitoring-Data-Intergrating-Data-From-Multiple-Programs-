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

#####Join the location tables to create one data set of all the stream data#####

l_data_sets <- c("WSA Verification - Data",  "External Data: Site Information - Data", "NRSA 1314 Site Information - Data")
location_data <- NARS %>% 
                filter(str_detect(Indicator, "Site"))

location_data <- NARS %>% 
  filter(str_detect(web1, "siteinfo")) %>% 
  filter(!str_detect(web1,"ext"))


for(i in 1:length(location_data$web1)) { 
 # print(location_data$Survey[i])
  url_link <- paste0("https://www.epa.gov", location_data$web1[i])
  temp_file <- tempfile(fileext = ".csv")
  download.file(url_link, temp_file)
  if (i ==1) { 
    data1       <- read.csv(temp_file)
    # 2004 data location column header don't match the other two data sets, need to check with the EPA to see if this data is compadable 
    data1 <- data1 %>% rename(LON_DD83 = LON_DD, 
                              LAT_DD83= LAT_DD) 
    #Convert HUC  data to characters not integers 
    data1[str_detect(names(data1), "HUC")] <- data1 %>% 
                                                select(contains("HUC")) %>% 
                                                mutate_each(as.character) 
    
    } else { 
    data2     <- read.csv(temp_file)
    #convert data type to characters 
    data2[str_detect(names(data2), "HUC")] <- data2 %>% 
                                              select(contains("HUC")) %>% 
                                              mutate_each(as.character)
  if(any(str_detect(names(data2),"EPA_REG"))==T) {
       data2$EPA_REG <- as.character(data2$EPA_REG)
      } 
    data1     <- bind_rows(list(data1, data2))
  }
  unlink(temp_file)
} 

##### Convert string dates to dates #####
data1$DATE_COL <- as.Date.character(data1$DATE_COL, format="%m/%d/%Y" )
data1$PUBLICATION_DATE <- as.Date.character(data1$PUBLICATION_DATE, format="%m/%d/%Y" )

blank_year             <- is.na(data1$YEAR)
data1$YEAR[blank_year] <- format(data1$DATE_COL[blank_year],format="%Y")
data1$YEAR<- as.integer(data1$YEAR)

# Check the longitude to make sure all are negative because this data set is all collected west of the prime meridian 
if(any(data1$XLON_DD>0, na.rm=TRUE)== T) {
  postive_index <- data1$XLON_DD >0 & !is.na(data1$XLON_DD)
  data1$XLON_DD[postive_index] <- data1$XLON_DD[postive_index]*(-1)
}

#####Join the data tables to the location tables 
# Identify the data tables of metrics we want to join
macroinvertebrates <-NARS %>% 
  filter(str_detect(Indicator, "Benthic Macroinvertebrates")) %>%  
  filter(str_detect(Data, "Metric"))

water_chem <-NARS %>% 
  filter(str_detect(Indicator, "Water Chemistry")) %>%  
  filter(str_detect(Data, "Indicator"))

phys_hab <-NARS %>% 
  filter(str_detect(Indicator, "Physical Habitat")) %>%  
  filter(str_detect(web1, paste(c("phabmet", "phabmed"), collapse = "|"))) 

metric_list <- rbind(macroinvertebrates, water_chem)
metric_list <- rbind(metric_list, phys_hab)


for(link in metric_list$web1){ 
  url_link <- paste0("https://www.epa.gov", link)
  temp_file <- tempfile(fileext = ".csv")
  download.file(url_link, temp_file)
  data_set<- read.csv(temp_file)
  if (any(str_detect(names(data_set), "YEAR"))==F) {
    data_set$DATE_COL <- as.Date.character(data_set$DATE_COL, format="%m/%d/%Y")  
    data_set <- mutate(data_set, YEAR = as.integer(format(data_set$DATE_COL,format="%Y")))
    }
  data1 <- left_join(data1, data_set, by=c("SITE_ID","YEAR", "VISIT_NO"))
  unlink(temp_file)
} 

data1<- data1 %>% 
        mutate(DATE_COMBIND = Sys.Date()) %>% 
        mutate(PROGEAM = "EPA")

write.csv(data1, file=paste0("Data/", Sys.Date(), "EPA_dataset.csv"))

#Save the file to ScienceBase Item. If new add, if already exists just update the existing file. 
short_name = paste0(Sys.Date(),"_Tidy_NARS_",list_years[y],".csv")
file_name <- paste0(getwd(),"/Data/", short_name)
write.csv(data, file=file_name, row.names=FALSE)


#a project looking at the taxi time data available on the web from CODA
# also trying out web scraping

#---- Initialise

library(dplyr)
library(tidyr)
library(rvest)
library(purrr)
library(readxl)
library(ggplot2)


#---- Download the Files

codapage <- read_html("https://www.eurocontrol.int/articles/coda-publications")
#pull out all of the links
clinks <- codapage %>% 
  html_nodes("a") %>% html_attr("href")

#then just the taxis - these have 'taxi' in them, but are also a mix of pdf and excel
dl <- 
  data.frame(link = clinks, stringsAsFactors = FALSE) %>% 
  filter(grepl("taxi", link)) %>% 
  mutate(
    #add the root page
    flink = paste0("https://www.eurocontrol.int", link),
    #pull out the final bit which is the filename (except for some which have a ?xxx at the end)
    #by stripping out anything leading up to a / and anything after a ?
    file = gsub("([^/]*/)|([?][^?]*)", "", link))

#maybe we should check here that the file names are unique...

#save the files to the data folder
#on unix-alike, ie mac, you can pass a vector of addresses and names
download.file(dl$flink, paste0("data/", dl$file), method="libcurl")

#---- Merge the files
# or at least the xls ones

# do this in a way that doesn't need the list above
#first the excel files (assume we're in the project directory)
xlf <- list.files(path="data/",
                  pattern="?(.xlsx)")
#summarise details
xld <- data_frame(file=xlf, id=1:length(xlf)) %>% 
  mutate(
    season = case_when(
      grepl("summer",file, ignore.case = T)|grepl("?-s[0:9]?",file, ignore.case = T) ~ "Summer",
    T ~ "Winter"),
    year = substr(gsub("([^[:digit:]]*)|(20)","",file),1,2),
    #yr text is just the first letter of the season plus year digits, 2 digits, 4 digits could be 1516 
    #caution this version not valid for years 2020 onwards!
    yrtext = paste0(year, substr(season,1,1)))

#bang them together
#we will need to tidy later!
tim<- paste0("data/",xlf) %>% 
  #merge the files and include an ID
  map_dfr(read_xlsx, .id = 'id') %>% 
  #add the date details
  mutate(id = as.integer(id)) %>% 
  left_join(xld %>% select(id, yrtext, season, year))

#make the names tidier
names(tim) <- gsub("[ ()]","",names(tim)) 

#careful, all this is case sensitive
tim <- tim %>% 
  mutate(TXI = coalesce(MeanTXIMins, MeanTXImins),
         TXO = coalesce(MeanTXOMins, MeanTXOmins),
         phase = if_else(is.na(TXI), "Out", "In"),
         mean = coalesce(TXI, TXO)) %>% 
  select(-starts_with("MeanTX"),
         -TXI, -TXO) %>% 
  #get rid of odd cases (such as disclaimer!)
  filter(!is.na(mean)) %>% 
  rename( "Name" = AirportName, 
          "WTC" = WakeTurbulenceCategory,
          "p10" = '10thPctl',
          "p90" = '90thPctl',
         "stDev" = StandardDeviation,
         "median" = Median) %>% 
  #in WTC data, airport details are not repeated so fill down
  fill(ICAO, IATA, Name) %>% 
  #create tidy WTC flag
  mutate(hasWTC = if_else(is.na(WTC), F, T))

#now gather columns
timb <- tim %>% 
  gather("measure", "time", c(5:8,14))

#--- First Graph


ggplot(timb %>% filter(hasWTC == F, 
                       IATA %in% c("LHR", "CDG", "AMS", "FRA", "MAD"),
                       measure != "stDev"),
       aes(year, time, colour = measure)) +
  geom_point() + xlab("Season") + ylab("Taxi Time (mins)") +
  facet_grid(IATA~phase*season)

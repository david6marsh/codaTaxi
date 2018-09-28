#a project looking at the taxi time data available on the web from CODA
# also trying out web scraping

#---- Initialise

library(dplyr)
library(tidyr)
library(rvest)
library(purrr)
library(stringr)
library(readxl)
library(ggplot2)
library(pdftools)


#---- Download the Files
#if you've already done this, you can skip to the next section

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

# do this in a way that doesn't need the list above
#get the excel or pdf files (assume we're in the project directory), ignore anything else in the data directory
xlf <- list.files(path="data/",
                  pattern="?(.xlsx)|(.pdf)")
#summarise details
xld <- data_frame(file=xlf) %>% 
  mutate(
    season = case_when(
      grepl("summer",file, ignore.case = T)|grepl("?-s[0:9]?",file, ignore.case = T) ~ "Summer",
    T ~ "Winter"),
    year = substr(gsub("([^[:digit:]]*)|(20)","",file),1,2),
    #yr text is just the first letter of the season plus year digits, 2 digits, 4 digits could be 1516 
    #caution this version not valid for years 2020 onwards!
    yrtext = paste0(year, substr(season,1,1)),
    #file type is just the file extension
    fileType = str_split_fixed(file, fixed("."), 2)[,2],
    wake = str_detect(file, "wake")) %>% 
  #can't take pdf and WTC so remove
  filter(!(wake == T & fileType == "pdf")) %>% 
  group_by(fileType) %>% 
  mutate(id = 1:n())

#bang the xlsx files together
#we will need to tidy later!
tim<- paste0("data/", xld$file[xld$fileType == "xlsx"])  %>% 
  #merge the files and include an ID
  map_dfr(read_xlsx, .id = 'id') %>% 
  #add the date details
  mutate(id = as.integer(id),
         fileType = "xlsx") %>% 
  left_join(xld %>% select(id, fileType, yrtext, season, year))

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


#we need to extract pdf text, and convert to dataframe for map_dfr to work cleanly
#so a simple wrapper for pdftools::pdf_text
pdf_df <- function(x){
  data_frame(text = pdf_text(x))
}

#bang the pdf files together
#wake files are awkward, so ignore them
timp <- paste0("data/", xld$file[xld$fileType == "pdf" & !xld$wake])  %>%
  map_dfr(pdf_df, .id = 'id') %>% 
  separate_rows(text, sep="\n") %>% 
  #we only want rows that start IATA-ICAO (4 letter, 3 letter)
  filter(str_detect(text, "^(\\s)*[[:upper:]]{4}(\\s)*[[:upper:]]{3}(\\s)(.)*")) %>% 
  #makes life easier if we assume a single space occurs only within an airport long name
  #multiple spaces mark column boundaries
  #almost true, so start with that and tidy up after
  mutate(text = str_replace_all(text,"(\\s){2,}","#"),
         #remove first # or leading space if any
         text = str_replace(text, "^[#\\s]*", "")) %>%
  #if at this stage we've not 7 #, then change the first space to a #
  mutate(text2 = if_else(str_count(text, "#")==7, 
                         text,
                         str_replace(text, "(\\s)", "#"))) %>% 
  #works for all but one line (with no airport name)
  #so if at this stage still don't have 7 #, then assume we need a blank name
  mutate(text2 = if_else(str_count(text2, "#")==7, 
                         text2,
                         str_replace(text2, "(#[[:upper:]]{3}#)", "\\1unknown#"))) %>% 
  #then split and label
  separate(text2, c("ICAO", "IATA", "Name", "mean", "stDev", "p10", "median", "p90"), sep="#") %>% 
  #convert to numeric
  #make last-minute switch from European , as decimal point to .
  mutate_at(vars("mean", "stDev", "p10", "median", "p90"), function(x)(as.numeric(str_replace(x, ",", ".")))) %>% 
  #add the date details
  #WTC is always false because we can't scrape them for the moment
  mutate(id = as.integer(id),
         hasWTC = F,
         fileType = "pdf") %>% 
  left_join(xld %>% select(id, fileType, yrtext, season, year, file)) %>% 
  mutate(phase = if_else(str_detect(file, "out" ), "Out", "In")) %>% 
  select(-text, -file)

#now gather columns
timb <- tim %>% 
  bind_rows(timp) %>% 
  gather("measure", "time", c(5:8,15)) %>% 
  #multiple spelling (upcase lower case) of the full airport name, so focus on one
  #the first one is the one with more lower case
  group_by(ICAO, IATA) %>% 
  arrange(ICAO, IATA, Name) %>% 
  mutate(Name = first(Name)) %>% 
  #finally sort by time, for neatness
  arrange(ICAO, IATA, yrtext)


#---- First Graph

AP_large <- c("LHR", "CDG", "AMS", "FRA", "MAD")
AP_ran <- sample(unique(timp$IATA), 6)
ggplot(timb %>% filter(hasWTC == F, 
                       IATA %in% AP_large,
                       measure != "stDev"),
       aes(year, time, colour = measure, shape = season)) +
  geom_point() +  
  xlab("Year") + ylab("Taxi Time (mins)") +
  facet_grid(IATA~phase*season)

#---- Save and export
saveRDS(timb, "data/tidytaxitimes.RDS")

#also export for export
write.csv(timb, "data/tidytaxitimes.csv",
          row.names = F)
#for Tableau, untidy can be better
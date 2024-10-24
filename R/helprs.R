# This script writes a reproducible pipeline for integrating, manipulating, and exploring datasets
# downloaded from BioMark PIT tagging systems

###### clear work environment ##########
#rm(list=ls())

library(devtools)

#Load package dependencies
library(chron) #to read in times to pit_mastr file
library(ggthemes)
library(reshape)
library(tidyverse) #to do everything (mostly formating/manipulation of dfs)
library(reshape2)
library(stringr)
library(ggplot2) #will use to plot summary stats and
library(lubridate) #to convert and work with dates
#library(anytime)
library(readxl) #use to read in xlsx files from readers
library(mark) #use to extract dates from log files
library(gridExtra)
#Run scripts with functions

### MACY NOTES: ####
# Consider ways to perform QA/QC checks? 1) set list of permissible calls within each function
# Write objects to reuse date and time patterns!

# Need to create function to run dx check on readers working

# Flexible for HEX vs DEC?

# check availability of R package name
#library(available)
#available::available('pitmastr')


# i) Files to be supplied by user: ####
## reader files within parent directory
## M1 <- metadata file 1 containing S/Ns associated with site names (likely to match naming in file pathways)
## M2 <- metadata file 2 containing tag IDS and biological information on tagged individual (sex, species, date tagged, etc); *currently deals with only metrics that are not time-varying


#place individual log files within these folders.
#Do search for .log within directory and copy/paste

# ii) format files for compatibility

m1 <- format_m1(path.csv = "/Users/mkailing/Dropbox/Kailing_Projects/pitmastr/dummy_MW/sn_map.csv",
                  date1 = start,
                  date2 = end)

#m1.test <- read.csv("/Users/mkailing/Dropbox/Kailing_Projects/pitmastr/dummy_MW/sn_map.csv")

## 1) DATA ASSEMBLY ####

# Depending on need and structure of file storage and type, user may want one of the following functions

### 1a) Single Folder; Single File Type ####

# Test extract_log function with BC, NE and MR files

BC1 = extract_log("/Users/mkailing/Dropbox/MIDWEST_WNS/MIDWEST_PIT/DATA/BAY_CITY/LOGGER_ENT1")

NE2 = extract_log("/Users/mkailing/Dropbox/Kailing_Projects/pitmastr/dummy_MW/NEDA_MINE/LOGGER_ENT2",
                  map.file1 = m1)

## MAIDEN ROCK 1
MR1 = extract_log("/Users/mkailing/Dropbox/Kailing_Projects/pitmastr/test_MW/MAIDEN_ROCK/LOGGER_ENT1")

# Test extract_txt with CL1 test files
CL1a = extract_txt("/Users/mkailing/Dropbox/Kailing_Projects/pitmastr/dummy_mw/CLINTON/LOGGER_ENT1")

# Test extract_xlsx with CL1 test files
CL1b = extract_xlsx("/Users/mkailing/Dropbox/Kailing_Projects/pitmastr/dummy_MW/CLINTON/LOGGER_ENT1",
                    map.file1 = m1)
#CL1b = extract_xlsx("/Users/mkailing/Dropbox/Kailing_Projects/pitmastr/test_MW")


# If there is a manageable amount of dfs, user can merge all together manually using rbind
pit_tmp <- rbind(CL1a, CL1b, MR1, NE2, NE4)
# or merge desired dfs with detections directly
#pit_tmp <- rbind(extract_txt())

# OR user can use this function but must specify filetype
# #integ_extract <- function(path, filetype){
#   setwd(path)
#   if (filetype == ".log") {
#     #setwd(path)
#     log_files <- list.files(path = path, pattern = filetype)
#     out <- data.frame()
#     for (i in 1:length(log_files)) {
#       eg <-readLines(log_files[i]) #this makes distinct lines for log files
#       sn <-grep('\\d{4}[.]\\d{4}', eg, value = TRUE)
#       tmp<-as.data.frame(paste(grep("^TAG: ", eg, value=TRUE),sn[i])) #makes dataframe containing only tag detections
#       out<-rbind(out,tmp)
#     }
#     colnames(out)<-'xx'
#     x1<-data.frame(date=str_extract(out$xx, "^[0-9]{2}/[0-9]{2}/[0-9]{4}?"), #substr(out$xx,start=9,stop=18) #str_extract_date(out$xx, format("%m/%d/%Y")
#                    time=chron(times=str_extract(out$xx, "[0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{2}")),
#                    id=str_extract(out$xx, "\\d{3}[.]\\d{12}"),
#                    sn = str_extract(out$xx, "\\d{4}[.]\\d{4}")) #time=substr
#     x1$id<-paste("P_",x1$id,sep="");
#     #  x1$id<-gsub("[.]","",x1$id);
#     return(x1)
#   }
#   if (filetype == ".txt") {
#     #setwd(path)
#     txt_files <- list.files(path = path, pattern = filetype)
#     out <- data.frame()
#     for (i in 1:length(txt_files)) {
#       eg <-readLines(txt_files[i]) #this reads in and makes distinct lines for log files
#       tmp <- data.frame(grep("\\d{3}[.]\\d{12}", eg, value=TRUE))
#       out<-rbind(out,tmp)
#     }
#     colnames(out)<-'xx'
#     x1<-data.frame(date = str_extract(out$xx, "^[0-9]{2}/[0-9]{2}/[0-9]{4}?"), #will this ever change?
#                    time=chron(times=str_extract(out$xx, "[0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{2}")), #will this ever change?
#                    id = str_extract(out$xx, "\\d{3}[.]\\d{12}"),
#                    sn = str_extract(out$xx, "\\d{4}[.]\\d{4}")
#     )
#     x1$id<-paste("P_",x1$id,sep="");
#     return(x1)
#   }
#   if (filetype == ".xlsx") {
#     #setwd(path)
#     xlsx_files <- list.files(path = path, pattern = filetype)
#     out <- data.frame()
#     for (i in 1:length(xlsx_files)) {
#       eg <-read_excel(xlsx_files[i]) #this reads in and makes distinct lines for log files
#       out<-rbind(out,eg)
#     }
#     x1<-out %>%
#       select(1, 2, 9, 5) %>% #select the rows we want to include from xlsx file (as is it is matching the compatible output with other file types)
#       rename(date = 1,
#              time = 2,
#              id = 3,
#              sn = 4)
#     x1$time <- chron(times=gsub('.{1}$',"",x1$time));
#     x1$id<-paste("P_",x1$id,sep="")
#     return(x1)
#   }
# }
# 
# 
# CL2a = integ_extract(path = "/Users/mkailing/Dropbox/Kailing_Projects/pitmastr/pitmastr_test",
#                    filetype = ".txt")
# CL2b = integ_extract(path = "/Users/mkailing/Dropbox/Kailing_Projects/pitmastr/pitmastr_test",
#                     filetype = ".xlsx")


### 1b) Multiple folders; Multiple file types ####
# Now, function to join all files within parent directory (where all files are stored)
# this function is for accessing log files that are stored within multiple sub-directories (site -> reader)
# search is automated through directory/subdirectories to find .log, .txt, or .xlsx files and pull out detections

# setwd("/Users/mkailing/Dropbox/Kailing_Projects/pitmastr/test_MW/NEDA_MINE")
# string<-"LOGGER_"

#directory <- "/Users/mkailing/Dropbox/Kailing_Projects/pitmastr/dummy_MW"
#tag.code = "DEC"
#string = "LOGGER_"
# setwd(directory)

# User must define a character string common to all subdirectories containing reader files;
# in our example "LOGGER_" is included in every folder name
workhorse <- function(directory, 
                      string, 
                      tag.code = c("DEC","HEX","NUM"),
                      remove.dup = "N"){
  #create objects that define patterns for dates, times, ids using regexs
  date.regex = "[0-9]{2}/[0-9]{2}/[0-9]{4}"
  time.regex = "[0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{2}"
  sn.regex = "\\d{4}[.]\\d{4}"
  if (tag.code == "DEC"){
    id.regex = "\\d{3}[.]\\d{12}"
    } else
  if (tag.code == "HEX") {
    id.regex = "\\d{1}[A-Z]{1}\\d{1}[.]\\d{7}[A-Z]{1}\\d{2}"
    } else
  if (tag.code == "NUM"){
    id.regex = "\\d{15}"
    }
  setwd(directory)
  dirs <- list.dirs()
  #use this function to get full file path name, so we can filter to directories that contain the string we want (ie those containing reader files /'LOGGER_')
  parent_prefix <- function(x, path = basename(normalizePath(".")), sep = ""){
    y = paste(getwd(), x, sep = sep)
    gsub("[.]","",y)
  }
  dirs = parent_prefix(dirs)
  dirs = dirs[grep(string,dirs)]
  df <- data.frame()
  for (i in 1:length(dirs)){
    all = data.frame()
    setwd(dirs[i]) 
    log_files <- list.files(pattern = ".log")
    if (length(log_files)>0) {
      out1 <- data.frame()
      for (i in 1:length(log_files)) {
        eg <-readLines(log_files[i]) #this makes distinct lines for log files
        serial.num <- as.character(grep("S/N: ", eg, value = TRUE))
        tmp<-as.data.frame(paste(grep("^TAG: ", eg, value=TRUE),
                               as.numeric(gsub("\\D.\\D","",serial.num)),
                               as.character(getwd()), sep = " ")) #makes dataframe containing only tag detections
        out1<-rbind(out1,tmp)
        }
    colnames(out1) <- 'xx'
    x1<-data.frame(date=str_extract(out1$xx, date.regex),
                   time=chron(times=str_extract(out1$xx, time.regex)),
                   id=str_extract(out1$xx, id.regex),
                   serial.num = str_extract(out1$xx, sn.regex),
                   path = str_extract(out1$xx, getwd()),
                   source = ".log") #time=substr
    #x1$id<-paste("P_",x1$id,sep="")
    all <- rbind(all, x1)
    }
    txt_files <- list.files(pattern = ".txt")
    if (length(txt_files)>0) {
      out2 <- data.frame()
      for (i in 1:length(txt_files)) {
        eg <-readLines(txt_files[i]) #this reads in and makes distinct lines for log files
        tmp <- as.data.frame(paste(grep(id.regex, eg, value=TRUE),
                                   as.character(getwd(), sep =" ")))
        out2<-rbind(out2,tmp)
        }
      colnames(out2)<-'xx'
      x2<-data.frame(date = str_extract(out2$xx, date.regex),
                     time=chron(times=str_extract(out2$xx, time.regex)),
                     id = str_extract(out2$xx, id.regex),
                     serial.num = str_extract(out2$xx, sn.regex),
                     path = str_extract(out2$xx, getwd()),
                     source = ".txt")
      #x2$id<-paste("P_",x2$id,sep="")
      all <- rbind(all, x2)
      }
    xlsx_files <- list.files(pattern = ".xlsx") #### MACY CHECK THAT THIS XLSX RUNS AND RBINDS PROPERLY!!
    if (length(xlsx_files)>0){
      out3 <- data.frame()
      for (i in 1:length(xlsx_files)) {
        eg <-read_excel(xlsx_files[i]) #this reads in and makes distinct lines for log files
        out3<-rbind(out3,eg)
        }
      x3<-out3 %>%
      select(1, 2, 9, 5) %>% #select the rows we want to include from xlsx file (as is, it is matching the compatible output with other file types)
      rename(date = 1,
             time = 2,
             id = 3,
             serial.num = 4) %>%
        mutate(path = getwd(),
               source = ".xlsx")
    x3$time <- chron(times=gsub('.{1}$',"",x3$time));
    #x3$id<-paste("P_",x3$id,sep="")
    all <- rbind(all, x3)
    }
    setwd(directory)
    m1 <- data.frame(lapply(read.csv('sn_map.csv'),as.character)) %>%
      mutate(start.date = format(as.Date(start.date,"%m/%d/%y"), "%Y-%m-%d"), #formats date reader was deployed at site.ent. alternative format: "%m/%d/%Y"
             end.date = ifelse(is.na(end.date)|end.date=="", #if no end date provided (reader is still deployed),
                               format(Sys.Date(),"%Y-%m-%d"), #this will assign current date for end date
                               format(as.Date(end.date, "%m/%d/%y"),"%Y-%m-%d")),
             serial.num = ifelse(is.na(serial.num), "0000.0000",format(as.numeric(serial.num), nsmall = 4))) %>% #ifelse(is.na(serial.num),"0000.0000", serial.num))
      mutate_at(vars(start.date,end.date), as.Date)
    #this match will work if the same reader (ie serial number) has always been at the same site/entrance
    all$site <- m1$site[match(all$serial.num, m1$serial.num)]
    all$ent <- m1$ent[match(all$serial.num, m1$serial.num)]
    #but, if readers were moved between sites.ents, adjust based on dates: MACY NEEDS TO SOLVE THIS!
    #all$site <- m1$site[match(all$serial.num, m1$serial.num)]
    #all$ent <- m1$ent[match(all$serial.num, m1$serial.num)]
    df <- rbind(df, all) #%>%
      #mutate(pit_id = paste("P_",df$id,sep=""))
    }
  if (remove.dup=="Y") {
    #df = df[!duplicated(df),]
    #df = df %>% filter(!duplicated(cbind(date,time,id))) #remove rows of duplicated (regardless of file source)
    df = df %>% distinct(date, time, id, .keep_all = TRUE)
    #date, times, and pit_id (ie no bat can be detected >1x at same timepoint)
  }
  return(df)
}




#sort out how to different patterns of HEX and DEC codes
"\\d{3}[.]\\d{12}"
z <- "999000000007425 HYK23-ih7.fhohwheni2863_ 999.000000007425 3D8.0000001D01 "
#hc <- str_extract(z, "\\d{1}[A-Z0-9]{1,2}[.]\\d{7}[A-Z0-9]{1}\\d{2}")
hex <- str_extract(z, "\\d{1}[A-Z0-9]{2}[.][A-Z0-9]{10}")
dec <- str_extract(z, "\\d{3}[.]\\d{12}")
num <- str_extract(z, "\\d{15}")

#hex code: 3E7.0000001D01

## Consider: m1 gets read in and formatted (esp dates) outside of workhorse function
# m1$start.date=mdy(m1$start.date)
# m1$end.date=mdy(m1$end.date)
# all$date=mdy(all$date)
#
# all$site <- m1$site[match(all$serial.num, m1$serial.num)][all$date>=m1$start.date&all$date<m1$end.date]
# all$ent <- m1$ent[match(all$serial.num, m1$serial.num)][all$date>=m1$start.date&all$date<m1$end.date]

# Test workhorse function using test_MW
mastr_pit <- workhorse(directory = "/Users/mkailing/Dropbox/Kailing_Projects/pitmastr/dummy_MW",
                       tag.code = "DEC",
                       string = "LOGGER_")

mastr_pit2 <- workhorse(directory = "/Users/mkailing/Dropbox/Kailing_Projects/pitmastr/dummy_MW",
                       string = "LOGGER_",
                       tag.code = "DEC",
                       remove.dup = "Y") #drops duplicate detections, retains only first source


macy_pitall <- workhorse(directory = "/Users/mkailing/Dropbox/MIDWEST_WNS/MIDWEST_PIT/DATA",
                         string = "LOGGER_",
                         tag.code = "DEC",
                         remove.dup = "Y")

#check no duplicate rows were carried over after using remove.dup arguement
mastr_dups = mastr_pit[duplicated(mastr_pit[,c("date", "time", "id")]),]

## Perhaps no longer needed - skip
# #combine_single <- function(directory, string, filetype){
#   setwd(directory)
#   log_dirs <- list.dirs()
# #use this function to get full file path name, so we can filter to desired directories (ie those containing .log files)
#   parent_prefix <- function(x, path = basename(normalizePath(".")), sep = ""){
#     y = paste(getwd(), x, sep = sep)
#     gsub("[.]","",y)
#   }
#   log_dirs = parent_prefix(log_dirs)
#   log_dirs = log_dirs[grep(string,log_dirs)]
# 
#   all = data.frame()
#   for (i in 1:length(log_dirs)){
#     setwd(log_dirs[i])
#     log_files <- list.files(pattern = filetype)
#     out <- data.frame()
#     for (i in 1:length(log_files)) {
#       eg <-readLines(log_files[i]) #this makes distinct lines for log files
#       serial.num <- as.character(grep("S/N: ", eg, value = TRUE))
#       tmp<-as.data.frame(paste(grep("^TAG: ", eg, value=TRUE),
#                                as.numeric(gsub("\\D.\\D","",serial.num)),
#                                as.character(getwd()), sep = " ")) #makes dataframe containing only tag detections
#       out<-rbind(out,tmp)
#     }
#     all <- rbind(all, out)
#   }
#   names(all)[1]<-'xx'
#   x1<-data.frame(date=as.Date(substr(all$xx,start=9,stop=18), format = "%m/%d/%Y"),
#                  time=chron(times=substr(all$xx,start=20,stop=32)),
#                  id=substr(all$xx,start=33,stop=48),
#                  serial.num = as.character(substr(all$xx, start = 50, stop = 58)),
#                  path = substr(all$xx, start=59, stop = length(all$xx)))
#   x1$id<-gsub("[.]","",x1$id); x1$id<-paste("P_", x1$id, sep="")
#   setwd(directory)
#   m1 <- as.data.frame(lapply(read.csv('sn_map.csv'),as.character)) %>%
#     mutate(start.date = as.Date(start.date, format = "%m/%d/%Y"),
#            end.date = as.Date(end.date, format = "%m/%d/%Y"))
#   m1 <- data.frame(lapply(read.csv('sn_map.csv'),as.character))
#   #m1[1,2]=as.Date.character(lapply(m1[1,2],format = "%m/%d/%Y"))
#   #m1$end.date=ifelse(!(is.na(m1$end.date)|m1$end.date==""), m1$end.date, format(Sys.Date(),"%m/%d/%Y"))
#   x1$site <- m1$site[match(x1$serial.num, m1$serial.num)]
#   x1$ent <- m1$ent[match(x1$serial.num, m1$serial.num)]
#   return(x1)
# #}


# this is specific for MW data; format pit_id to match pit_mastr ids
# pl<-6
# mastr_file$id=as.character(mastr_file$id) #Classify 'id' as character to paste decimal and match master file
# mastr_file$pit_id<-paste(substr(mastr_file$id,1,pl-1),".",substr(mastr_file$id,pl,nchar(mastr_file$id)),sep = "")
# unique(mastr_file$pit_id)

#### end workhorse ####

# mastr_pit <- workhorse(directory = "/Users/mkailing/Dropbox/Kailing_Projects/pitmastr/dummy_MW",
#                        string = "LOGGER_",
#                        map.file1 = m1)

mastr_pit <- workhorse(directory = "/Users/mkailing/Dropbox/Kailing_Projects/pitmastr/dummy_MW",
                       string = "LOGGER_",
                       map.file1 = m1)

# mastr_pit <- workhorse(directory = "/Users/mkailing/Dropbox/MIDWEST_WNS/MIDWEST_PIT/DATA",
#                        string = "LOGGER_",
#                        map.file1 = m1,
#                        remove.dup = "Y")

mastr_pit$pit_id = mastr_pit$id
mastr_pit$pit_id<-paste("P_",mastr_pit$pit_id,sep="")


### 1c) Create pit_working file by merging mastr_file with m2 ####


# Read in m2 file
#m2.test <- read.csv("~/Dropbox/MIDWEST_WNS/DATA/midwest_master.csv")

m2 <- format_m2("~/Dropbox/MIDWEST_WNS/DATA/midwest_master.csv",
                date1 = date,
                id.colname = pit_id,
                keep.cols = c(sex, species, age, mass, site)
                )

#err.ids = m2[duplicated(m2$pit_id)|duplicated(m2$pit_id, fromLast = TRUE),] #each bat should only be invcluded 1x in m2;
# these 5 bats have multiple assignments from their initial 'tag date'

# Check if tags in error table can be resolved, if not, remove pit_ids that are in error df
# m2 = m2 %>%
#   filter(!(pit_id %in% err.ids$pit_id)) %>%
#   select(-samp.event)

#check that no duplicates lingered; should be 0
#print(sum(duplicated(m2$pit_id)))

#write.csv(m2, file="/Users/mkailing/Dropbox/Kailing_Projects/pitmastr/images/m2.csv", row.names = FALSE)

# test integrate_id function that merges individual metadata from m2 to ids associated with detections
#pit_working <- integrate_ids(mastr_pit, m2, joiner = 'pit_id') #%>%

#working_pit <- left_join(mastr_pit, m2, by = 'pit_id')
working_pit <- integrate_ids(mastr_pit, m2, 
                             joiner = 'pit_id',
                             drop.tt = "Y") # FUNCTIONALLY THIS IS JUST A LEFT JOIN THAT RENAMES COLUMNS AND REMOVES TEST TAGS??

# from here, users should make sure that dates are formatted correctly across all columns

## NETWORK ANALYSIS ####

# write function to set up network objects at various level (sampling units, ie reader)
networkit_workit <- function(df){
  unit.vec <- unique(working_pit$reader)
  va <- data.frame()
  ed <- data.frame()
  pl <- list()
  #tg <- data.frame()
  for (i in 1:length(unit.vec)){
    x <- working_pit %>%
      filter(reader == unit.vec[i],
             sex=="F"|sex=="M") %>%
      group_by(date, pit_id,
               #hour(hms(time)),
               species, sex) %>%
      summarise(n = 1) %>%
      ungroup()
    
    #set up matrix
    x.cmx <- crossprod(table(x[1:2]))
    diag(x.cmx) <- 0
    x.netdf <- as.data.frame(x.cmx)
    
    #set up edge table
    x.ed <- x.netdf %>%
      mutate(from = rownames(.)) %>%
      gather(to, n, 1:ncol(x.netdf)) %>%
      filter(n!=0) %>%
      mutate(reader = unit.vec[i])
    
    #set up node/vertices
    x.va <- x %>% 
      rename(node = pit_id) %>%
      # group the data by the 'node' column
      group_by(node) %>%
      # summarize the data, calculating the total occurrences ('n') for each 'node'
      summarise(n = sum(n)) %>%
      mutate(reader = unit.vec[i]) %>%
      #relocate(node) %>%
      ungroup()
    
    x.va$sex = x$sex[match(x.va$node, x$pit_id)]
    x.va$species = x$species[match(x.va$node, x$pit_id)]
    
    va <- rbind(va, x.va)
    ed <- rbind(ed, x.ed)
    
    ig <- graph_from_data_frame(d = x.ed, vertices = x.va, directed = FALSE)
    
    x.tg <- as_tbl_graph(ig) %>%
      activate(nodes) %>%
      mutate(label = name)
    
    #set.seed(1991)
    
    # plot network
    p <- ggraph(graph = x.tg, layout = "kk") +
      ggtitle(unit.vec[i]) +
      #facet_edges(~reader) +
      geom_edge_arc(aes(edge_width = x.ed$n,
                        edge_alpha = x.ed$n),
                    colour = "black",
                    lineend = "round",
                    strength = .1) +
      geom_node_point(aes(size = x.va$n,
                          color = x.va$sex)) +
      # geom_node_text(aes(label = name), 
      #                repel = TRUE, 
      #                point.padding = unit(0.2, "lines"), 
      #                size = (net_objs[[1]]$n), 
      #                colour = "gray10") +
      #scale_edge_width(range = c(0.1,2.5)) +
      #scale_edge_alpha(range = c(0.1,0.3)) +
      #scale_size_continuous(range = c(1,5)) +
      theme_graph(background = 'white') +
      guides(edge_width = 'none',
             edge_alpha = 'none')
    
    #pl <- p[i]
  }
  return(list(va, ed, pl))
}


# li = list('java','python')
# li2 <- append(li,'r')
# print(li2)
#print(net_objs[[3]])

# create objects for network plot and analyses
net_objs <- networkit_workit(working_pit)
View(net_objs[[1]]);View(net_objs[[2]])
print(net_objs[[3]][[1]])

library(igraph)
library(tidygraph)
library(ggraph)
library(cowplot)

ig <- graph_from_data_frame(d = net_objs[[2]], vertices = net_objs[[1]], directed = FALSE)

x.tg <- as_tbl_graph(ig) %>%
  activate(nodes) %>%
  mutate(label = name#,
         #samp.unit == unit.list[i]
         )
#map(x.tg, .f = )


## manual code for creating network df ####
pit_sum <- working_pit %>%
#  subset(reader==reader.list[i]) %>%
  group_by(date, pit_id,
           #hour(hms(time)),
           species, sex) %>%
  summarise(n = 1) #%>%

net_cmx <- crossprod(table(pit_sum[1:2])) #%>%
diag(net_cmx) <- 0

net_df <- as.data.frame(net_cmx)

pit_sum %>% dplyr::rename(node = pit_id) %>%
  # group the data by the 'node' column
  dplyr::group_by(node, species, sex) %>%
  # summarize the data, calculating the total occurrences ('n') for each 'node'
  dplyr::summarise(n = sum(n)) -> pit_va

pit_ed <- net_df %>%
  mutate(from = rownames(.)) %>%
  gather(to, n, 1:ncol(net_df)) %>%
  filter(n!=0)

## end manual code for creating network objects ####


set.seed(1991)

x.tg %>%
  ggraph(layout = "kk") +
  facet_edges(~reader) +
  geom_edge_arc(colour = "black",
                lineend = "round",
                strength = .1,
                aes(edge_width = net_objs[[2]]$n,
                    alpha = net_objs[[2]]$n)) +
  geom_node_point(aes(size = net_objs[[1]]$n,
                      color = net_objs[[1]]$sex #,
                      #shape = net_objs$site.x
                      )) +
  # geom_node_text(aes(label = name), 
  #                repel = TRUE, 
  #                point.padding = unit(0.2, "lines"), 
  #                size = (net_objs[[1]]$n), 
  #                colour = "gray10") +
  scale_edge_width(range = c(0.1,2.5)) +
  scale_edge_alpha(range = c(0.1,0.3)) +
  scale_size_continuous(range = c(1,5)) +
  theme_graph(background = 'white') +
  guides(#edge_width = 'none',
    edge_alpha = 'none')

# write function to set up network viz objects (igraph and tidygraph)
# graph_objects <- function (net_objs, samp.unit) {
#   
# }

pit_ig <- graph_from_data_frame(d=pit_ed, vertices = pit_va, directed=FALSE)
pit_tg <- as_tbl_graph(pit_ig) %>%
  activate(nodes) %>%
  mutate(label=name)

set.seed(1991)

x.tg %>%
  ggraph(layout = "kk") +
  facet_edges(~reader) +
  geom_edge_arc(colour = "black",
                lineend = "round",
                strength = .1,
                aes(edge_width = net_objs[[2]]$n,
                    alpha = net_objs[[2]]$n)) +
  # geom_node_point(aes(size = net_objs[[1]]$n,
  #                     color = net_objs[[1]]$sex,
                      #shape = pit_va$site.x
                      # )) +
  # geom_node_text(aes(label = name), 
  #                repel = TRUE, 
  #                point.padding = unit(0.2, "lines"), 
  #                size = (net_objs[[1]]$n), 
  #                colour = "gray10") +
  scale_edge_width(range = c(0.1,2.5)) +
  scale_edge_alpha(range = c(0.1,0.3)) +
  scale_size_continuous(range = c(1,5)) +
  theme_graph(background = 'white') +
  guides(#edge_width = 'none',
         edge_alpha = 'none')

# calculate degree centrality
dg_pit <- pit_ed[rep(seq_along(pit_ed$n),pit_ed$n), 1:2]
dgg_pit <- graph_from_edgelist(as.matrix(dg_pit), directed = T)
degree(dgg_pit) %>%
  as.data.frame() %>%
  rownames_to_column('node') %>%
  rename(degree_centrality = 2) %>%
  arrange(-degree_centrality) -> pit_dc_tbl

# extract most central nodes
names(degree(dgg_pit))[which(degree(dgg_pit)==max(degree(dgg_pit)))]

# calculate betweenness centrality
betweenness(dgg_pit) %>%
  as.data.frame() %>%
  rownames_to_column('node') %>%
  rename(betweenness_centrality = 2) %>%
  arrange(-betweenness_centrality) -> pit_bc_tbl
# extract most betweenness centrality
names(igraph::betweenness(dgg_pit))[which(igraph::betweenness(dgg_pit) == max(igraph::betweenness(dgg_pit)))]


## DATA MANIPULATION ####

#Now, manipulate pit_working to format for specific needs
# cmpr_id <- grouped by individual (pit_id)
# cmpr_time <- grouped by time unit of interest (pop-level summaries?)
# Need dfs that contain all possible dates readers were on (using test tag file) for survival object

#without assigning reader and site using m1, the site here is site.tagged. Need to resolve where site.detected gets looped in?
cmpr_ids_weeks <- compress_ids_date(x = working_pit, obs.groups =  c(pit_id, sex, site), 
                                    obs.unit = "weeks", date.colname = date)
cmpr_ids_months <- compress_ids_date(x = working_pit, obs.groups = c(pit_id, sex, site), 
                                     obs.unit = "months", date.colname = date)

## Make a similar function but on a nightly basis, where you can count hourly detections ????

# Format for survival analysis ####

# figure out formatting for test tag detections to become sampling events


#filter to test tag detections only from mastr_pit
#mastr_tt <-  mastr_pit[mastr_pit$pit_id %in% m1$test.tag,]
#use this file to create sampling events for survival analysis

#test_tags <- read.csv("/Users/mkailing/Dropbox/Kailing_Projects/DATA/test_tags_temp.csv")

system_ops <- readr_check(x = mastr_pit,
                           y = m1,
                           site.col = path, #if serial numbers and readers can't be matched from m1, we can use the path for reader name
                           min.reads = 3) %>%
  subset(year(date)>2016)

plot_system_ops <- readr_viz(df = system_ops,
                             site.col = path)


#tt.list$date=format(as.Date(tt.list$date, format = "%m/%d/%Y"),"%Y-%m-%d")

pw.tmp <- pit_working %>%
  mutate(pit_date.detected = paste(pit_id, date.detected, sep='.')) %>%
  group_by(pit_id, site.detected, ent) %>%
  #mutate(detected = 1) %>%
  expand(date.detected = tt.list$date.detected) %>% #format(as.Date(tt.list$date, format = "%m/%d/%y"), "%Y-%m-%d")) %>%
  mutate(reader_date = paste(site.detected, ent, date.detected, sep = "."),
         pit_date.detected = paste(pit_id, date.detected, sep = "_")) %>%
  filter(reader_date %in% tt.list$reader_date) %>%
  ungroup() %>%
  #group_by(pit_id, date.detected) %>%
  mutate(detected = ifelse(pit_date.detected %in% pit_working$pit_date.detected, "Y","N"))
  #select(-reader_date)

#check that expand df worked
pit6745 <- pw.tmp %>%
  subset(pit_id == 'P_989.001032566745')
print(min(pit6745$date.detected))
#earliest date for NEDA 4, is Sept 2, 2020
#confirm with tt.list df

#Check with another example
pit7046 <- pw.tmp %>%
  subset(pit_id == 'P_989.001032567046')
print(min(pit7046$date.detected))
#earliest date for MR 1 is 

## need to expand working to include all unique dates on test tags to characterize 'sampling'
## need to filter out dates that occurred prior to tagging efforts!

# format_m1 <-function(path.csv, 
#                        date1, date2,
#                        date.format) {
#   x <- data.frame(lapply(read.csv(path.csv),as.character)) %>%
#     rename(start.date := {{date1}},
#            end.date := {{date2}}) %>%
#     mutate(start.date = format(as.Date(start.date,date.format), "%Y-%m-%d"),
#            end.date = ifelse(is.na(end.date)|end.date=="",
#                              format(Sys.Date(), "%Y-%m-%d"),
#                              format(as.Date(end.date,date.format), "%Y-%m-%d")),
#            serial.num = ifelse(is.na(serial.num), NA, format(as.numeric(serial.num), nsmall = 4)))
#   return(x)
# }


# EXTRA CODE: ####

# # for now, try import of various file types to figure out format
# clinton.xlsx <- readxl::read_excel("/Users/mkailing/Dropbox/Kailing_Projects/pitmastr/test_MW/CLINTON/LOGGER_ENT1/clinton_test.xlsx")
#
# CL1.xlsx <- clinton.xlsx %>%
#   select(1,2,9,5) %>%
#   rename(date = 1, time = 2, id = 3, sn = 4)
#
#
# clinton.txt <- data.frame(grep("\\d{3}[.]\\d{12}", #find rows containing DEC pit_ids
#                                read.table("/Users/mkailing/Dropbox/Kailing_Projects/pitmastr/test_MW/CLINTON/LOGGER_ENT1/clinton_test.txt", sep = "\t")$V1,
#                                value=TRUE)) %>%
#   rename(xx=1)
#
# CL1.txt = data.frame(date = substr(clinton.txt$xx, start = 1, stop=11),
#                      time=chron(times = substr(clinton.txt$xx, start = 14, stop=25)),
#                      id = str_extract(clinton.txt$xx, "\\d{3}[.]\\d{12}"),
#                      sn = str_extract(clinton.txt$xx, "\\d{4}[.]\\d{4}") #this returns NA if no pattern matches
# )
#
#
# CL1 = extract_dets("/Users/mkailing/Dropbox/Kailing_Projects/pitmastr/test_MW/CLINTON/LOGGER_ENT1",
#                    filetype = "*.txt")


#####NEDA ENT 4######
###main entrance

#setwd("test_MW/NEDA/LOGGER_ENT4") #- only need to do this if not specifying a path in list.files f(x)

#######Bring in and extract tag lines only
log_files <- list.files(pattern="*.log")
out<-data.frame()
for(i in 1:length(log_files)) {
  eg<-readLines(log_files[i]) #this makes distinct lines from log files
  tmp<-as.data.frame(grep("^TAG: ", eg, value=TRUE)) #makes dataframe containing only tag detections
  out<-rbind(out,tmp) #figure out why we need 'out' AND 'tmp' df; could be necessary for looping through multiple log files?;
  #to minimie redundancy can we just rename tmp to out?
}
colnames(out)<-"xx"
x1<-data.frame(date=substr(out$xx,start=9,stop=18),time=chron(times=substr(out$xx,start=20,stop=32)),id=substr(out$xx,start=33,stop=50)) #time=substr
x1$id<-gsub("[.]","",x1$id); x1$id<-paste("P_",x1$id,sep="")

x1$site="NEDA MINE"
x1$ent="ent4"
head(x1)
tail(x1)
unique(x1$id)

#write.csv(x1,"/Users/JosephHoyt/Dropbox/WNS_MIDWEST/MIDWEST_PIT/DATA/XIND_CSV_files/Neda_ENT4.csv",row.names=F)
#write.csv(x1,"/Users/mkailing/Dropbox/MIDWEST_WNS/MIDWEST_PIT/DATA/XIND_CSV_files/Neda_ENT4.csv",row.names=F)

#####NEDA ENT 1######
###culvert
#set the directory for Neda entrance 1
setwd("/Users/JosephHoyt/Dropbox/WNS_MIDWEST/MIDWEST_PIT/DATA/NEDA/LOGGER_ENT1")
#setwd("/Users/mkailing/Dropbox/MIDWEST_WNS/MIDWEST_PIT/DATA/NEDA/LOGGER_ENT1")

#######Bring in and extract tag lines only
log_files <- list.files(pattern="*.log")
out<-data.frame()
for(i in 1:length(log_files)) {
  eg<-readLines(log_files[i])
  tmp<-as.data.frame(grep("^TAG: ", eg, value=TRUE))
  out<-rbind(out,tmp)
}
colnames(out)<-"xx"
x2<-data.frame(date=substr(out$xx,start=9,stop=18),time=chron(times=substr(out$xx,start=20,stop=32)),id=substr(out$xx,start=33,stop=50))
x2$id<-gsub("[.]","",x2$id); x2$id<-paste("P_",x2$id,sep="")

x2$site="NEDA MINE"
x2$ent="ent1"
head(x2)
tail(x2)

#write.csv(x2,"/Users/JosephHoyt/Dropbox/WNS_MIDWEST/MIDWEST_PIT/DATA/XIND_CSV_files/Neda_ENT1.csv",row.names=F)
#write.csv(x2,"/Users/mkailing/Dropbox/MIDWEST_WNS/MIDWEST_PIT/DATA/XIND_CSV_files/Neda_ENT1.csv",row.names=F)

#####NEDA ENT 2######
###far entrance
#set the directory for Neda entrance 2

#setwd("/Users/JosephHoyt/Dropbox/WNS_MIDWEST/MIDWEST_PIT/DATA/NEDA/LOGGER_ENT2")
#setwd("/Users/mkailing/Dropbox/MIDWEST_WNS/MIDWEST_PIT/DATA/NEDA/LOGGER_ENT2")

#######Bring in and extract tag lines only
log_files <- list.files(pattern="*.log")
out<-data.frame()
for(i in 1:length(log_files)) {
  eg<-readLines(log_files[i])
  tmp<-as.data.frame(grep("^TAG: ", eg, value=TRUE))
  out<-rbind(out,tmp)
}
colnames(out)<-"xx"
x3<-data.frame(date=substr(out$xx,start=9,stop=18),time=chron(times=substr(out$xx,start=20,stop=32)),id=substr(out$xx,start=33,stop=50))
x3$id<-gsub("[.]","",x3$id); x3$id<-paste("P_",x3$id,sep="")

x3$site="NEDA MINE"
x3$ent="ent2"
head(x3)
tail(x3)

#write.csv(x3,"/Users/JosephHoyt/Dropbox/WNS_MIDWEST/MIDWEST_PIT/DATA/XIND_CSV_files/Neda_ENT2.csv",row.names=F)
#write.csv(x3,"/Users/mkailing/Dropbox/MIDWEST_WNS/MIDWEST_PIT/DATA/XIND_CSV_files/Neda_ENT2.csv",row.names=F)

#####NEDA ENT 5######
###cupola
#set the directory for Neda entrance 2

#setwd("/Users/JosephHoyt/Dropbox/WNS_MIDWEST/MIDWEST_PIT/DATA/NEDA/LOGGER_ENT5")
#setwd("/Users/mkailing/Dropbox/MIDWEST_WNS/MIDWEST_PIT/DATA/NEDA/LOGGER_ENT5")

#######Bring in and extract tag lines only
log_files <- list.files(pattern="*.log")
out<-data.frame()
for(i in 1:length(log_files)) {
  eg<-readLines(log_files[i])
  tmp<-as.data.frame(grep("^TAG: ", eg, value=TRUE))
  out<-rbind(out,tmp)
}
colnames(out)<-"xx"
x4<-data.frame(date=substr(out$xx,start=9,stop=18),time=chron(times=substr(out$xx,start=20,stop=32)),id=substr(out$xx,start=33,stop=50))
x4$id<-gsub("[.]","",x4$id); x4$id<-paste("P_",x4$id,sep="")

x4$site="NEDA MINE"
x4$ent="ent5"
head(x4)
tail(x4)

#write.csv(x4,"/Users/JosephHoyt/Dropbox/WNS_MIDWEST/MIDWEST_PIT/DATA/XIND_CSV_files/Neda_ENT5.csv",row.names=F)
#write.csv(x3,"/Users/mkailing/Dropbox/MIDWEST_WNS/MIDWEST_PIT/DATA/XIND_CSV_files/Neda_ENT5.csv",row.names=F)



#####MAIDEN ROCK#####

#####MAIDEN ROCK ENT 4#####
###big culvert
#serial  2037.3617
#setwd("/Users/JosephHoyt/Dropbox/WNS_MIDWEST/MIDWEST_PIT/DATA/MAIDEN_ROCK/LOGGER_ENT4")
#setwd("/Users/mkailing/Dropbox/MIDWEST_WNS/MIDWEST_PIT/DATA/MAIDEN_ROCK/LOGGER_ENT4")

#######Bring in and extract tag lines only
log_files <- list.files(pattern="*.log")
out<-data.frame()
for(i in 1:length(log_files)) {
  eg<-readLines(log_files[i])
  tmp<-as.data.frame(grep("^TAG: ", eg, value=TRUE))
  out<-rbind(out,tmp)
}
colnames(out)<-"xx"
x5<-data.frame(date=substr(out$xx,start=9,stop=18),time=chron(times=substr(out$xx,start=20,stop=32)),id=substr(out$xx,start=33,stop=50))
x5$id<-gsub("[.]","",x5$id); x5$id<-paste("P_",x5$id,sep="")

x5$site="MAIDEN ROCK"
x5$ent="ent4"
head(x5)
tail(x5)

#write.csv(x5,"/Users/JosephHoyt/Dropbox/WNS_MIDWEST/MIDWEST_PIT/DATA/XIND_CSV_files/MR_ENT4.csv",row.names=F)

#####MAIDEN ROCK ENT 3#####
###pigeon alley
#serial
#setwd("/Users/JosephHoyt/Dropbox/WNS_MIDWEST/MIDWEST_PIT/DATA/MAIDEN_ROCK/LOGGER_ENT3")

#######Bring in and extract tag lines only
log_files <- list.files(pattern="*.log")
out<-data.frame()
for(i in 1:length(log_files)) {
  eg<-readLines(log_files[i])
  tmp<-as.data.frame(grep("^TAG: ", eg, value=TRUE))
  out<-rbind(out,tmp)
}
colnames(out)<-"xx"
x5a<-data.frame(date=substr(out$xx,start=9,stop=18),time=chron(times=substr(out$xx,start=20,stop=32)),id=substr(out$xx,start=33,stop=50))
x5a$id<-gsub("[.]","",x5a$id); x5a$id<-paste("P_",x5a$id,sep="")

x5a$site="MAIDEN ROCK"
x5a$ent="ent3"
head(x5a)
tail(x5a)

#write.csv(x5a,"/Users/JosephHoyt/Dropbox/WNS_MIDWEST/MIDWEST_PIT/DATA/XIND_CSV_files/MR_ENT3.csv",row.names=F)


#####MAIDEN ROCK ENT 2#####
###next to main door
#serial 1929.3117
#setwd("/Users/JosephHoyt/Dropbox/WNS_MIDWEST/MIDWEST_PIT/DATA/MAIDEN_ROCK/LOGGER_ENT2")
#setwd("/Users/mkailing/Dropbox/MIDWEST_WNS/MIDWEST_PIT/DATA/MAIDEN_ROCK/LOGGER_ENT2")

#######Bring in and extract tag lines only
log_files <- list.files(pattern="*.log")
out<-data.frame()
for(i in 1:length(log_files)) {
  eg<-readLines(log_files[i])
  tmp<-as.data.frame(grep("^TAG: ", eg, value=TRUE))
  out<-rbind(out,tmp)
}
colnames(out)<-"xx"
x6<-data.frame(date=substr(out$xx,start=9,stop=18),time=chron(times=substr(out$xx,start=20,stop=32)),id=substr(out$xx,start=33,stop=50))
x6$id<-gsub("[.]","",x6$id); x6$id<-paste("P_",x6$id,sep="")

x6$site="MAIDEN ROCK"
x6$ent="ent2"
head(x6)
tail(x6)

#write.csv(x6,"/Users/JosephHoyt/Dropbox/WNS_MIDWEST/MIDWEST_PIT/DATA/XIND_CSV_files/MR_ENT2.csv",row.names=F)
#write.csv(x3,"/Users/mkailing/Dropbox/MIDWEST_WNS/MIDWEST_PIT/DATA/XIND_CSV_files/MR_ENT2.csv",row.names=F)


#####MAIDEN ROCK ENT 1#####
###main door
#serial 1929.3116

#setwd("/Users/JosephHoyt/Dropbox/WNS_MIDWEST/MIDWEST_PIT/DATA/MAIDEN_ROCK/LOGGER_ENT1")
#setwd("/Users/mkailing/Dropbox/MIDWEST_WNS/MIDWEST_PIT/DATA/MAIDEN_ROCK/LOGGER_ENT1")

#######Bring in and extract tag lines only
log_files <- list.files(pattern="*.log")
out<-data.frame()
for(i in 1:length(log_files)) {
  eg<-readLines(log_files[i])
  tmp<-as.data.frame(grep("^TAG: ", eg, value=TRUE))
  out<-rbind(out,tmp)
}
colnames(out)<-"xx"
x7<-data.frame(date=substr(out$xx,start=9,stop=18),time=chron(times=substr(out$xx,start=20,stop=32)),id=substr(out$xx,start=33,stop=50))
x7$id<-gsub("[.]","",x7$id); x7$id<-paste("P_",x7$id,sep="")

x7$site="MAIDEN ROCK"
x7$ent="ent1"
head(x7)
tail(x7)

#write.csv(x7,"/Users/JosephHoyt/Dropbox/WNS_MIDWEST/MIDWEST_PIT/DATA/XIND_CSV_files/MR_ENT1.csv",row.names=F)
#write.csv(x3,"/Users/mkailing/Dropbox/MIDWEST_WNS/MIDWEST_PIT/DATA/XIND_CSV_files/MR_ENT1.csv",row.names=F)


#####BAY CITY####

#####BAY CITY ENT 1####
###metal grate entrance
#serial ?????

#setwd("/Users/JosephHoyt/Dropbox/WNS_MIDWEST/MIDWEST_PIT/DATA/BAY_CITY/LOGGER_ENT1")
#setwd("/Users/mkailing/Dropbox/MIDWEST_WNS/MIDWEST_PIT/DATA/BAY_CITY/LOGGER_ENT1")

#######Bring in and extract tag lines only
log_files <- list.files(pattern="*.log")
out<-data.frame()
for(i in 1:length(log_files)) {
  eg<-readLines(log_files[i])
  tmp<-as.data.frame(grep("^TAG: ", eg, value=TRUE))
  out<-rbind(out,tmp)
}
colnames(out)<-"xx"
x8<-data.frame(date=substr(out$xx,start=9,stop=18),time=chron(times=substr(out$xx,start=20,stop=32)),id=substr(out$xx,start=33,stop=50))
x8$id<-gsub("[.]","",x8$id); x8$id<-paste("P_",x8$id,sep="")

x8$site="BAY CITY"
x8$ent="ent1"
head(x8)
tail(x8)

#write.csv(x8,"/Users/JosephHoyt/Dropbox/WNS_MIDWEST/MIDWEST_PIT/DATA/XIND_CSV_files/BC_ENT1.csv",row.names=F)
#write.csv(x8,"/Users/mkailing/Dropbox/MIDWEST_WNS/MIDWEST_PIT/DATA/XIND_CSV_files/BC_ENT1.csv",row.names=F)

#####BAY CITY ENT 2####

###White culvert entrance
#serial ?????

#setwd("/Users/JosephHoyt/Dropbox/WNS_MIDWEST/MIDWEST_PIT/DATA/BAY_CITY/LOGGER_ENT2")
#setwd("/Users/mkailing/Dropbox/MIDWEST_WNS/MIDWEST_PIT/DATA/BAY_CITY/LOGGER_ENT2")

#######Bring in and extract tag lines only
log_files <- list.files(pattern="*.log")
out<-data.frame()
for(i in 1:length(log_files)) {
  eg<-readLines(log_files[i])
  tmp<-as.data.frame(grep("^TAG: ", eg, value=TRUE))
  out<-rbind(out,tmp)
}
colnames(out)<-"xx"
x9<-data.frame(date=substr(out$xx,start=9,stop=18),time=chron(times=substr(out$xx,start=20,stop=32)),id=substr(out$xx,start=33,stop=50))
x9$id<-gsub("[.]","",x9$id); x9$id<-paste("P_",x9$id,sep="")

x9$site="BAY CITY"
x9$ent="ent2"
head(x9)
tail(x9)

#write.csv(x9,"/Users/JosephHoyt/Dropbox/WNS_MIDWEST/MIDWEST_PIT/DATA/XIND_CSV_files/BC_ENT2.csv",row.names=F)
#write.csv(x9,"/Users/mkailing/Dropbox/MIDWEST_WNS/MIDWEST_PIT/DATA/XIND_CSV_files/BC_ENT2.csv",row.names=F)


##### CREATE MASTER FILE OF ALL PIT DATA #####

##########################
options(tz="US/Eastern")
library(dplyr)
library(reshape2)
library(reshape)
library(ggplot2)
library(ggthemes)
library(data.table)

#Read in files made above
# m1=read.csv("/Users/JosephHoyt/Dropbox/WNS_MIDWEST/MIDWEST_PIT/DATA/XIND_CSV_files/Neda_ENT4.csv")
# m2=read.csv("/Users/JosephHoyt/Dropbox/WNS_MIDWEST/MIDWEST_PIT/DATA/XIND_CSV_files/Neda_ENT1.csv")
# m3=read.csv("/Users/JosephHoyt/Dropbox/WNS_MIDWEST/MIDWEST_PIT/DATA/XIND_CSV_files/Neda_ENT2.csv")
# m4=read.csv("/Users/JosephHoyt/Dropbox/WNS_MIDWEST/MIDWEST_PIT/DATA/XIND_CSV_files/Neda_ENT5.csv")
# m5=read.csv("/Users/JosephHoyt/Dropbox/WNS_MIDWEST/MIDWEST_PIT/DATA/XIND_CSV_files/MR_ENT4.csv")
# m5a=read.csv("/Users/JosephHoyt/Dropbox/WNS_MIDWEST/MIDWEST_PIT/DATA/XIND_CSV_files/MR_ENT3.csv")
# m6=read.csv("/Users/JosephHoyt/Dropbox/WNS_MIDWEST/MIDWEST_PIT/DATA/XIND_CSV_files/MR_ENT2.csv")
# m7=read.csv("/Users/JosephHoyt/Dropbox/WNS_MIDWEST/MIDWEST_PIT/DATA/XIND_CSV_files/MR_ENT1.csv")
# m8=read.csv("/Users/JosephHoyt/Dropbox/WNS_MIDWEST/MIDWEST_PIT/DATA/XIND_CSV_files/BC_ENT1.csv")
# m9=read.csv("/Users/JosephHoyt/Dropbox/WNS_MIDWEST/MIDWEST_PIT/DATA/XIND_CSV_files/BC_ENT2.csv")


#bind it all together and set date
allpit=rbind(m1,m2,m3,m4,m5,m5a,m6,m7,m8,m9)
allpit$date<-as.Date(allpit$date,format("%m/%d/%Y"))
head(allpit)
tail(allpit)

unique(allpit$ent)
table(allpit$ent, allpit$site)

##Write master allpit.csv
#write.csv(allpit, "/Users/JosephHoyt/Dropbox/WNS_MIDWEST/MIDWEST_PIT/DATA/pit_master.csv",row.names = FALSE)

# library(DBI)
# library(odbc)

#can try somerhing like this if want to pull in mdb files directly
# connection <- dbConnect(odbc(), .connection_string = "Driver={Microsoft Access Driver (*.mdb, *.accdb)};
#                         Dbq = C:\\clinton_test.mdb;")
# testdf <- dbReadTable(connection, "TagMemory")






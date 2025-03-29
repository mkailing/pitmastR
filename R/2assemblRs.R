# Data assembly functions


# Massive function to assemble data from various folders and file types
### NOTES ON WORKHORSE: NEED TO DROP DUPLICATES THAT AREN'T TEST TAGS, BETTER: WRITE TEST TAG FILE FIRST!!

#' @title
#' Primary assembler for finding files containing and extracting tag reads
#' 
#' @description
#' Search across subfolders of a specified directory to extract all tag reads and assemble to single dataframe object
#' 
#' @param directory path to directory containing all the files saved from readers
#' @param string pattern of characters shared among parent folders containing the files from readers
#' @param remove.dup whether duplicate detections (no including reads of test tag) should be dropped; remove.dup defaults to FALSE such that all tag reads are retained
#' @param map.file1 name of m1, best used with an object returned from the 'format_m1' formatter function.
#' @export
#' @details 
#' This function iteratively searches among subfolders for each of the different file types in which tag reads can be stored. Each detection is stored as a row with the respected systems information (serial.number, site, folder location)
#' stored with the output. ELABORATE ON MORE SPECIFICS!
#' @examples
#' # pm <- workhorse(directory = "/Users/mkailing/Dropbox/Kailing_Projects/pitmastr/inst/dummy_MW",
#' # string="LOGGER",
#' # remove.dup = TRUE,
#' # map.file1 = m1)
workhorse <- function(directory, string, map.file1, remove.dup = FALSE){
  d1 <- getwd()
  d2 <- directory
  setwd(d2)
  #setwd(directory)
  dirs <- list.dirs()
  #use this function to get full file path name, so we can filter to desired directories (ie those containing reader files /'LOGGER_')
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
        serial.num <-  ifelse(length(grep("S/N: ", eg, value = TRUE))==0, '1111.1111', as.character(grep("S/N: ", eg, value = TRUE)))
        tmp<-as.data.frame(paste(grep("TAG: ", eg, value=TRUE),
                                 as.numeric(gsub("\\D.\\D", "", serial.num)),
                                 as.character(getwd()), sep = " ")) #makes dataframe containing only tag detections
        out1<-rbind(out1,tmp)
      }
      colnames(out1) <- 'xx'
      x1<-data.frame(date=stringr::str_extract(out1$xx, "[0-9]{2}/[0-9]{2}/[0-9]{4}"),
                     time=chron::chron(times=stringr::str_extract(out1$xx, "[0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{2}")),
                     id=stringr::str_extract(out1$xx, "\\d{3}[.]\\d{12}"),
                     serial.num = stringr::str_extract(out1$xx, "\\d{4}[.]\\d{4}"),
                     path = stringr::str_extract(out1$xx, getwd()),
                     source = ".log") #time=substr
      all <- rbind(all, x1)
    }
    txt_files <- list.files(pattern = ".txt")
    if (length(txt_files)>0) {
      out2 <- data.frame()
      for (i in 1:length(txt_files)) {
        eg <-readLines(txt_files[i]) #this reads in and makes distinct lines for log files
        tmp <- as.data.frame(paste(grep("\\d{3}[.]\\d{12}", eg, value=TRUE),
                                   as.character(getwd(), sep =" ")))
        out2<-rbind(out2,tmp)
      }
      colnames(out2)<-'xx'
      x2<-data.frame(date = stringr::str_extract(out2$xx, "^[0-9]{2}/[0-9]{2}/[0-9]{4}?"), #does this change with PC vs MAC?
                     time=chron::chron(times=stringr::str_extract(out2$xx, "[0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{2}")), #will this ever change?
                     id = stringr::str_extract(out2$xx, "\\d{3}[.]\\d{12}"),
                     serial.num = stringr::str_extract(out2$xx, "\\d{4}[.]\\d{4}"),
                     path = stringr::str_extract(out2$xx, getwd()),
                     source = ".txt")
      all <- rbind(all, x2)
    }
    xlsx_files <- list.files(pattern = ".xlsx") #### MACY CHECK THAT THIS XLSX RUNS AND RBINDS PROPERLY!!
    if (length(xlsx_files)>0){
      out3 <- data.frame()
      for (i in 1:length(xlsx_files)) {
        eg <- readxl::read_excel(xlsx_files[i]) #this reads in and makes distinct lines for .xlsx files
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
      x3$time <- chron::chron(times=gsub('.{1}$',"",x3$time))
      all <- rbind(all, x3)
    }
    # na.sn <- data.frame(unique(all$path[is.na(all$serial.num)])) %>%
    #   mutate(serial.num = sprintf("%09.4f", 0.9999+seq_along(1)))
    all = all %>% 
      mutate(serial.num = ifelse(is.na(serial.num),
                                 sprintf("%09.4f", 0.9999+seq_along(serial.num[is.na(serial.num)])),
                                 format(as.numeric(serial.num), nsmall = 4)),
             date = as.Date(format(as.Date(date,"%m/%d/%Y"),"%Y-%m-%d"))) %>%
      ungroup()
    
    if (missing(map.file1)){ #need to figure out how to work in test tag file build with no m1
      df <- rbind(df, all)
      
      #tt <- NULL
      }
    else {
      z = map.file1 %>%
      right_join(all, by = c("serial.num")) #%>% #inner_join(relationship = 'many-to-many')
    #filter(lubridate::`%within%`(date, lubridate::interval(start.date,end.date))) ## THIS WILL DROP ROWS THAT ARE NOT ASSOCIATED WITH A SERIAL NUMBER!!!!
    df <- rbind(df, z)
    
    #tt <- df[df$id %in% map.file1$test.tag,]
    }
  }
  df %>%
    mutate(reader2 = path,
           reader2 = gsub(d2, "", reader2),
           reader2 = gsub("/", " ", reader2)) -> df
  
  if (remove.dup==TRUE) {
    #df = df[!duplicated(df),]
    #df = df %>% filter(!duplicated(cbind(date,time,id))) #remove rows of duplicated (regardless of file source)

    #still need to return df and df3
    df1 <- df %>% distinct(date, time, id, .keep_all = TRUE)
    tt <- df[df$id %in% map.file1$test.tag,]
#    df2 = df[!distinct(date, time, id, .keep_all = TRUE)]
#    df3 = df[df$pit_id %in% m1$test.tag]
    #date, times, and pit_id (ie no bat can be detected >1x at same timepoint) # NEED TO SORT OUT WHAT TO DO WITH TEST TAGS -- WILL LIKELY BE DUPS AND NEED TO KEEP!
  }
  else {
    df1 <- df
    tt <- df[df$id %in% map.file1$test.tag,]
  }
  setwd(d1)
  #return(df1)
  return(list(df1,tt))
}


#how to handle missing serial numbers in txt or xls files?? assign placeholder and then match by directory?

# # Working with .log files in a single folder
# extract_log <- function(path,
#                         map.file1) {
#   setwd(path)
#   log_files <- list.files(path = path, pattern = ".log")
#   out <- data.frame()
#   for (i in 1:length(log_files)) {
#     eg <-readLines(log_files[i]) #this makes distinct lines of each log files
#     serial.num <- as.character(grep("S/N: ", eg, value = TRUE))
#     tmp<-as.data.frame(paste(grep("^TAG: ", eg, value=TRUE), serial.num)) #makes dataframe containing only tag detections
#     out<-rbind(out,tmp)
#   }
#   colnames(out)<-'xx'
#   x1<-data.frame(date=str_extract(out$xx, "[0-9]{2}/[0-9]{2}/[0-9]{4}"), #substr(out$xx,start=9,stop=18) #str_extract_date(out$xx, format("%m/%d/%Y")
#                  time=chron(times=str_extract(out$xx, "[0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{2}")),
#                  id=str_extract(out$xx, "\\d{3}[.]\\d{12}"),
#                  serial.num = str_extract(out$xx, "\\d{4}[.]\\d{4}")) #time=substr
#   x1 = x1 %>% mutate(serial.num = format(as.numeric(serial.num), nsmall = 4),
#                        date = as.Date(format(as.Date(date,"%m/%d/%Y"),"%Y-%m-%d")))
#   if (missing(map.file1)) 
#     return(x1) 
#   else
#     z = map.file1 %>% 
#     group_by(serial.num) %>%
#     inner_join(x1, by = "serial.num", relationship='many-to-many') %>%
#     filter(date %within% interval(start.date,end.date))
#   return(z)
# }
# 
# # Working with .txt files in a single folder
# extract_txt <- function(path,
#                         map.file1) {
#   setwd(path)
#   txt_files <- list.files(path = path, pattern = ".txt")
#   out <- data.frame()
#   for (i in 1:length(txt_files)) {
#     eg <-readLines(txt_files[i]) #this reads in and makes distinct lines for log files
#     tmp <- data.frame(grep("\\d{3}[.]\\d{12}", eg, value=TRUE))
#     out<-rbind(out,tmp)
#   }
#   colnames(out)<-'xx'
#   x1<-data.frame(date = str_extract(out$xx, "^[0-9]{2}/[0-9]{2}/[0-9]{4}?"),
#                  time=chron(times=str_extract(out$xx, "[0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{2}")), #will this ever change? Needs to be extended to decimal seconds to have full time
#                  id = str_extract(out$xx, "\\d{3}[.]\\d{12}"),
#                  serial.num = str_extract(out$xx, "\\d{4}[.]\\d{4}")
#   )
#   if (missing(map.file1)) 
#     return(x1) 
#   else
#     z = map.file1 %>% 
#     group_by(serial.num) %>%
#     inner_join(x1, by = "serial.num", relationship='many-to-many') %>%
#     filter(date %within% interval(start.date,end.date))
#   return(z)
# }
# 
# # Working with .xlsx files in a single folder
# extract_xlsx <- function(path,
#                          map.file1) {
#   setwd(path)
#   xlsx_files <- list.files(path = path, pattern = ".xlsx")
#   out <- data.frame()
#   for (i in 1:length(xlsx_files)) {
#     eg <-read_excel(xlsx_files[i]) #this reads in and makes distinct lines for log files
#     out<-rbind(out,eg)
#   }
#   x1<-out %>%
#     select(1, 2, 9, 5) %>% #select the rows we want to include from xlsx file (as is it is matching the compatible output with other file types)
#     rename(date = 1,
#            time = 2,
#            id = 3,
#            serial.num = 4)
#   x1$time <- chron(times=gsub('.{1}$',"",x1$time))
#   if (missing(map.file1)) 
#     return(x1) 
#   else
#     z = map.file1 %>% 
#     group_by(serial.num) %>%
#     inner_join(x1, by = "serial.num", relationship='many-to-many') %>%
#     filter(date %within% interval(start.date,end.date))
#   return(z)
# }
# 
#   #x1$id<-paste("P_",x1$id,sep="")
#   #x1$id<-gsub("[.]","",x1$id)
#   #return(x1)
# #}
# 
# 

# Data integration functions

#' @title
#' Primary integration function for biological data
#' 
#' @description
#' Conveniently joins biological variables 
#' 
#' @param x data object returned from 'workhorse' that contains tag reads
#' @param y data object returned from 'format_m2' that contains biological data
#' @param joiner name of shared columns in both x and y dataframes that associates tag reads to biological data (PIT tag ID)
#' @param remove.tt drop or keep reads of test tags that are listed in first map file (m1). This defaults to FALSE. If TRUE, test tag reads are ommitted from returned object
#' @export
#' @details 
#' Optional section! Functionally, this is a left_join that joins bio data in m2 file with tag reads, drops test tag reads, and renames columns for clarity.
#' Currently, this integration does not account for time-varying biological variables (body mass, age, etc)
#' 
#' @examples
#' # working_pit <- integrate_ids(mastr_pit, m2, 
#' # joiner = 'pit_id',
#' # remove.tt = "Y")
integrate_bio <- function(x, y,
                          joiner = c(),
                          remove.tt = FALSE){
  if (remove.tt == TRUE) {
    df <- left_join(x, y, by = {{joiner}}) #%>%
    #rename(site.detected = site.x,
    #       site.tagged = site.y) #%>%
    df <- df[!(df$id %in% m1$test.tag),]
  }
  else {
    df <- left_join(x, y, by = {{joiner}}) #%>%
    #rename(site.detected = site.x,
    #       site.tagged = site.y)
  }
  return(df)
}


#THIS FUNCTION NEEDS TO BE MODIFIED TO MATCH IN TIME-VARYING MORPHOMETRICS, BASED ON?!

# integrate_ids <- function(x, y, 
#                           joiner, 
#                           drop.tt = 'N'){
#   #anchor_by = quote(anchor_by)
#   if (drop.tt == "Y") {
#     df <- full_join(x, y, by = joiner,
#                     relationship = 'many-to-many') #%>%
#     #rename(site.detected = site.x,
#     #       site.tagged = site.y) #%>%
#     df <- df[!(df$pit_id %in% m1$test.tag),]
#   }
#   else {
#     df <- full_join(x, y, by = joiner,
#                     relationship = 'many-to-many') #%>%
#     #rename(site.detected = site.x,
#     #       site.tagged = site.y)
#   }
#   return(df)
# }


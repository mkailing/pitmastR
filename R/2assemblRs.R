# Data assembly functions


# Massive function to assemble data from various folders and file types
### NEED TO DROP DUPLICATES THAT AREN'T TEST TAGS!
workhorse <- function(directory, string, remove.dup = "N", map.file1){
  setwd(directory)
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
        serial.num <- as.character(grep("S/N: ", eg, value = TRUE))
        tmp<-as.data.frame(paste(grep("^TAG: ", eg, value=TRUE),
                                 as.numeric(gsub("\\D.\\D","",serial.num)),
                                 as.character(getwd()), sep = " ")) #makes dataframe containing only tag detections
        out1<-rbind(out1,tmp)
      }
      colnames(out1) <- 'xx'
      x1<-data.frame(date=str_extract(out1$xx, "[0-9]{2}/[0-9]{2}/[0-9]{4}"),
                     time=chron(times=str_extract(out1$xx, "[0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{2}")),
                     id=str_extract(out1$xx, "\\d{3}[.]\\d{12}"),
                     serial.num = str_extract(out1$xx, "\\d{4}[.]\\d{4}"),
                     path = str_extract(out1$xx, getwd()),
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
      x2<-data.frame(date = str_extract(out2$xx, "^[0-9]{2}/[0-9]{2}/[0-9]{4}?"), #does this change with PC vs MAC?
                     time=chron(times=str_extract(out2$xx, "[0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{2}")), #will this ever change?
                     id = str_extract(out2$xx, "\\d{3}[.]\\d{12}"),
                     serial.num = str_extract(out2$xx, "\\d{4}[.]\\d{4}"),
                     path = str_extract(out2$xx, getwd()),
                     source = ".txt")
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
      all <- rbind(all, x3)
    }
    all = all %>% mutate(serial.num = format(as.numeric(serial.num), nsmall = 4),
                         date = as.Date(format(as.Date(date,"%m/%d/%Y"),"%Y-%m-%d")))
    if (missing(map.file1)) {
      df <- rbind(df, all)
    }
    else {
      z = map.file1 %>%
      group_by(serial.num) %>%
      inner_join(all, by = "serial.num", relationship='many-to-many') %>%
      filter(date %within% interval(start.date,end.date)) ## THIS WILL DROP ROWS THAT ARE NOT ASSOCIATED WITH A SERIAL NUMBER!!!!
      df <- rbind(df, z) #%>%
    }
  }
  if (remove.dup=="Y") {
    #df = df[!duplicated(df),]
    #df = df %>% filter(!duplicated(cbind(date,time,id))) #remove rows of duplicated (regardless of file source)
    df = df %>% distinct(date, time, id, .keep_all = TRUE)
    #date, times, and pit_id (ie no bat can be detected >1x at same timepoint) # NEED TO SORT OUT WHAT TO DO WITH TEST TAGS -- WILL LIKELY BE DUPS AND NEED TO KEEP!
  }
  return(df)
}


# Working with .log files in a single folder
extract_log <- function(path,
                        map.file1) {
  setwd(path)
  log_files <- list.files(path = path, pattern = ".log")
  out <- data.frame()
  for (i in 1:length(log_files)) {
    eg <-readLines(log_files[i]) #this makes distinct lines of each log files
    serial.num <- as.character(grep("S/N: ", eg, value = TRUE))
    tmp<-as.data.frame(paste(grep("^TAG: ", eg, value=TRUE), serial.num)) #makes dataframe containing only tag detections
    out<-rbind(out,tmp)
  }
  colnames(out)<-'xx'
  x1<-data.frame(date=str_extract(out$xx, "[0-9]{2}/[0-9]{2}/[0-9]{4}"), #substr(out$xx,start=9,stop=18) #str_extract_date(out$xx, format("%m/%d/%Y")
                 time=chron(times=str_extract(out$xx, "[0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{2}")),
                 id=str_extract(out$xx, "\\d{3}[.]\\d{12}"),
                 serial.num = str_extract(out$xx, "\\d{4}[.]\\d{4}")) #time=substr
  x1 = x1 %>% mutate(serial.num = format(as.numeric(serial.num), nsmall = 4),
                       date = as.Date(format(as.Date(date,"%m/%d/%Y"),"%Y-%m-%d")))
  if (missing(map.file1)) 
    return(x1) 
  else
    z = map.file1 %>% 
    group_by(serial.num) %>%
    inner_join(x1, by = "serial.num", relationship='many-to-many') %>%
    filter(date %within% interval(start.date,end.date))
  return(z)
}

# Working with .txt files in a single folder
extract_txt <- function(path,
                        map.file1) {
  setwd(path)
  txt_files <- list.files(path = path, pattern = ".txt")
  out <- data.frame()
  for (i in 1:length(txt_files)) {
    eg <-readLines(txt_files[i]) #this reads in and makes distinct lines for log files
    tmp <- data.frame(grep("\\d{3}[.]\\d{12}", eg, value=TRUE))
    out<-rbind(out,tmp)
  }
  colnames(out)<-'xx'
  x1<-data.frame(date = str_extract(out$xx, "^[0-9]{2}/[0-9]{2}/[0-9]{4}?"),
                 time=chron(times=str_extract(out$xx, "[0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{2}")), #will this ever change? Needs to be extended to decimal seconds to have full time
                 id = str_extract(out$xx, "\\d{3}[.]\\d{12}"),
                 serial.num = str_extract(out$xx, "\\d{4}[.]\\d{4}")
  )
  if (missing(map.file1)) 
    return(x1) 
  else
    z = map.file1 %>% 
    group_by(serial.num) %>%
    inner_join(x1, by = "serial.num", relationship='many-to-many') %>%
    filter(date %within% interval(start.date,end.date))
  return(z)
}

# Working with .xlsx files in a single folder
extract_xlsx <- function(path,
                         map.file1) {
  setwd(path)
  xlsx_files <- list.files(path = path, pattern = ".xlsx")
  out <- data.frame()
  for (i in 1:length(xlsx_files)) {
    eg <-read_excel(xlsx_files[i]) #this reads in and makes distinct lines for log files
    out<-rbind(out,eg)
  }
  x1<-out %>%
    select(1, 2, 9, 5) %>% #select the rows we want to include from xlsx file (as is it is matching the compatible output with other file types)
    rename(date = 1,
           time = 2,
           id = 3,
           serial.num = 4)
  x1$time <- chron(times=gsub('.{1}$',"",x1$time))
  if (missing(map.file1)) 
    return(x1) 
  else
    z = map.file1 %>% 
    group_by(serial.num) %>%
    inner_join(x1, by = "serial.num", relationship='many-to-many') %>%
    filter(date %within% interval(start.date,end.date))
  return(z)
}

  #x1$id<-paste("P_",x1$id,sep="")
  #x1$id<-gsub("[.]","",x1$id)
  #return(x1)
#}



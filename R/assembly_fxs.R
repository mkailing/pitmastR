# Data assembly functions

workhorse <- function(directory, string, remove.dup = "N"){
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
      #x1$id<-paste("P_",x1$id,sep="") #this is specific to our datasets, not needed in package
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
      x2<-data.frame(date = str_extract(out2$xx, "^[0-9]{2}/[0-9]{2}/[0-9]{4}?"),
                     time=chron(times=str_extract(out2$xx, "[0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{2}")), #will this ever change?
                     id = str_extract(out2$xx, "\\d{3}[.]\\d{12}"),
                     serial.num = str_extract(out2$xx, "\\d{4}[.]\\d{4}"),
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
      mutate(start.date = format(as.Date(start.date,"%m/%d/%y"), "%m/%d/%Y"), #formats date reader was deployed at site.ent
             end.date = ifelse(is.na(end.date)|end.date=="", #if no end date provided (reader is still deployed),
                               format(Sys.Date(),"%m/%d/%Y"), #this will assign current date for end date
                               format(as.Date(end.date, "%m/%d/%y"), "%m/%d/%Y")))
    #this match will work if the same reader (ie serial number) has always been at the same site/entrance
    all$site <- m1$site[match(all$serial.num, m1$serial.num)]
    all$ent <- m1$ent[match(all$serial.num, m1$serial.num)]
    #but, if readers were moved between sites.ents, adjust based on dates: MACY NEEDS TO SOLVE THIS!
    #all$site <- m1$site[match(all$serial.num, m1$serial.num)]
    #all$ent <- m1$ent[match(all$serial.num, m1$serial.num)]
    df <- rbind(df, all) #%>%
  }
  if (remove.dup=="Y") {
    #df = df[!duplicated(df),]
    #df = df %>% filter(!duplicated(cbind(date,time,id))) #remove rows of duplicated (regardless of file source)
    df = df %>% distinct(date, time, id, .keep_all = TRUE)
    #date, times, and pit_id (ie no bat can be detected >1x at same timepoint)
  }
  return(df)
}

#hex code: 3E7.0000001D01


# Data manipulation functions


## This compresses to the number of unique observations within a time frame. ie: how many dates detected during week 40 of the year?
#' @title
#' Primary manipulator function to restructure data objects
#' 
#' @description
#' Restructure dataframes based on user-defined observation groups and time units
#' 
#' @param x main data object, preferably returned from 'workhorse'
#' @param obs.groups list of columns by which observations will be grouped
#' @param obs.unit time unit of which to summarize observations
#' @param date.colname name of the column containing dates
#' @export
#' @details 
#' Optional section! 
#' @examples
#' # ids_wks <- bundle_by(x = pm2,
#' # obs.groups =  c(id, sex, site.x),
#' # obs.unit = "weeks",
#' # date.colname = date)
bundle_by <- function(x, #name of working dataframe
                              obs.groups = c(id), #list of columns to group by
                              obs.unit = c("weeks","months","years"), #date units that the user wants to summarize activity by
                              date.colname) { #name of date column in x for function to search by
  if (obs.unit == "weeks") {
    y = x %>%
      #mutate(date.colname=as.Date.POSIXlt(date.colname)) %>%
      group_by(across({{obs.groups}}), lubridate::year({{date.colname}}), lubridate::month({{date.colname}}), lubridate::week({{date.colname}})) %>%
      summarise(n.days = length(unique({{date.colname}}))) #number of days they were active - can build this out to include more summary
    #    colnames(z)<-gsub("(date.detected)","",colnames(z))
    return(y)
  }
  if (obs.unit == "months") {
    y = x %>%
      group_by(across({{obs.groups}}), lubridate::year({{date.colname}}), lubridate::month({{date.colname}})) %>%
      summarise(n.days = length(unique({{date.colname}})))
  }
  if (obs.unit == "years") {
    y = x %>%
      group_by(across({{obs.groups}}), year({{date.colname}})) %>%
      summarise(n.days = length(unique({{date.colname}})))
  }
  return(y)
}


#' @title
#' Create survival object for use in 'survival' package
#' 
#' @description
#' Uses several previously built dataframes to create 'sampling data frame' from individuals observations for survival analyses
#' 
#' @param map.file1 main data object, preferably returned from 'workhorse'
#' @param obs.groups list of columns by which observations will be grouped
#' @param obs.unit time unit of which to summarize observations
#' @param date.colname name of the column containing dates
#' @export
#' @details 
#' Optional section! 
#' @examples
#' # ids_wks <- compress_ids_date(x = pm2,
#' # obs.groups =  c(id, sex, site.x),
#' # obs.unit = "weeks",
#' # date.colname = date)
pit_survival <- function(m1, pit_workr, readr_check, min.reads = 2, min.detects = 0){
  
  #establish all possible sampling dates between reader start and end date
  m1 %>%
    #group_by(reader) %>%
    transmute(reader, date = purrr::map2(start.date, end.date, seq, by = '1 day')) %>%
    tidyr::unnest(cols=c(date)) -> all.dates
  
  #determine reader operation dates based on test tag fires
  samp.days <- left_join(all.dates, readr_check, by = c('reader', 'date')) %>%
    group_by(reader) %>%
    mutate(tt.fires = ifelse(is.na(tt.fires), 0, tt.fires),
           op = ifelse(is.na(op), "UNK", op)) %>%
    filter(op == "YES"|tt.fires>min.reads)
  
  #create df of all possible dates an individual could be observed between capture date and study (ie last dates readers were on)
  ind.dates <- pit_workr %>%
    tidyr::drop_na(date.tagged) %>%
    group_by(id) %>%
    mutate(date.tagged = min(date.tagged)) %>%
    ungroup() %>%
    group_by(id, start.date, reader) %>%
    filter(row_number()==1) %>%
    ungroup() %>%
    group_by(id) %>%
    transmute(id, reader, date = purrr::map2(date.tagged, end.date, seq, by = '1 day')) %>%
    tidyr::unnest(cols = c(date, reader))
  
  #calculate the number of detections an individual was detected in a night to code event: FALSE = detected/censored, TRUE = not detected/dead
  pit_sumr <- pit_workr %>%
    tidyr::drop_na(date.tagged) %>%
    group_by(id) %>%
    mutate(date.tagged = min(date.tagged)) %>%
    ungroup() %>%
    group_by(id, date, reader, date.tagged) %>%
    summarise(tot.dets = length(unique(time)))
  
  #last, remove the dates readers were down (ie test tag fires below minimum acceptable defined by user) because there was no observation/follow-up occurring
  pit_obsr <- left_join(ind.dates, pit_sumr, by = c('id', 'date', 'reader')) %>%
    group_by(id) %>%
    tidyr::fill(c(reader, date.tagged), .direction = 'downup') %>%
    ungroup() %>%
    mutate(tot.dets = ifelse(is.na(tot.dets), 0, tot.dets)) %>%
    semi_join(samp.days, by = c('date','reader')) %>%
    group_by(id) %>%
    arrange(date) %>%
    mutate(time_since_lastobs = ifelse(date == min(date), 
                                       as.numeric(difftime(date, date.tagged, units = 'days')),
                                       as.numeric(difftime(date, lag(date), units = 'days'))),
           time_since_tagdate = as.numeric(difftime(date, date.tagged, units = 'days')),
           event = ifelse(tot.dets>=min.detects, FALSE, TRUE))
  
  return(pit_obsr)
}


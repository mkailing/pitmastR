# Format mapping files for consistency across objects

#' @title
#' Formatter for map1 file
#' 
#' @description
#' Formats the first map file that contains the names, deployment dates, and 
#' serial numbers of each reader to be compatible with 'workhorse' function.
#' 
#' @param path.csv Path to map1 file location
#' @param date1 Name of reader start date column
#' @param date2 Name of reader end date column
#' @export
#' @details 
#' this seems optional!
#' @examples
#' # m1 <- pitmastr:::format_m1(
#' # path.csv = "/Users/mkailing/Dropbox/Kailing_Projects/pitmastr/inst/dummy_MW/sn_map.csv",
#' # date1 = start,
#' # date2 = end)
format_m1 <-function(path.csv, 
                     date1, date2) {
  date.formats = c("ymd","mdy","mdY")
  x <- data.frame(lapply(read.csv(path.csv),as.character)) %>%
    rename(start.date := {{date1}},
           end.date := {{date2}}) %>%
    mutate(start.date = lubridate::parse_date_time(start.date,date.formats),
           end.date = lubridate::parse_date_time(end.date, date.formats),
           end.date = tidyr::replace_na(end.date, lubridate::parse_date_time(Sys.Date(), date.formats)),
           #fill in missing serial.nums with default values starting at 1
           serial.num = ifelse(is.na(serial.num), 
                               sprintf("%09.4f", seq_along(serial.num[is.na(serial.num)])), 
                               format(as.numeric(serial.num), nsmall = 4))) 
  return(x)
}

#' @title
#' Formatter for map2 file
#' 
#' @description
#' Return the second map file that contains the biological variables to link to Tag IDs 
#' in an object formatted to be compatible with an object from 'workhorse'.
#' 
#' @param path.csv Path to map2 file location
#' @param date1 Name of sample date column. Note: the biometrics in this column are what will be associated the pit_ids
#' @param id.colname Name of column where PIT tag IDs are stored.
#' @param keep.cols list of columns to keep from map2 file i.e., which variables do you want carried over?
#' @export
#' @details 
#' this seems optional!
#' 
#' @examples 
#' # m2 <- format_m2(
#' # path.csv = "/Users/mkailing/Dropbox/Kailing_Projects/pitmastr/inst/dummy_MW/sn_map.csv",
#' # date1 = date,
#' # id.colname = pit_id,
#' # keep.cols = c(sex, species, age, mass, site))
format_m2 <- function(path.csv,
                        date1,
                        id.colname = id,
                        keep.cols) {
  date.formats <- c("ymd","mdy","mdY")
  keepers <- expr({{keep.cols}})
  #ditchers <- expr({{ditch.rows}})
  x <- data.frame(read.csv(path.csv)) 
  y <- x %>%
    select({{date1}},{{id.colname}}, !!keepers) %>%
    filter(!is.na({{id.colname}})&{{id.colname}}!="") %>% #filter out empty rows in id column
    rename(date.tagged := {{date1}}) %>%
    mutate(date.tagged = lubridate::parse_date_time(date.tagged, date.formats)) %>%
    group_by({{id.colname}}) %>%
    arrange(date.tagged) %>%
    mutate(samp.event = as.integer(rank(date.tagged))) %>%
    ungroup() %>%
    filter(samp.event==1)
    #pivot_wider(names_from = samp.event, values_from = date.tagged)
  return(y)
}


#' @title
#' Conversion for dates based on 24-H to animal based dates
#' 
#' @description
#' Converts dates to begin at a user defined hour, if different than 12am to reflect the organisms day-cycle. If tag reads occur between defined hours,
#' then classify the read as the previous date. 
#' 
#' @param x dataframe to convert from
#' @param date.colname name of column containing date of tag reads. Defaults to 'date' as in objects returned from pitmastr functions
#' @param set.hour set the hour interval to adjust dates by. Defaults to 7, changing any reads that occur between 12am and 7am to be the previous dates
#' @export
#' @details 
#' this seems optional!
#' @examples
#' # TBD
# convert_batdate <- function(x, date.colname = date, set.hour = 7) {
#   x %>% 
#     mutate(new.date = ifelse(lubridate::hour(time)>=0 & lubridate::hour(time)<set.hour, {{date.colname}}-1, {{date.colname}}))
#   return(x)
  # x %>%
  #   mutate(newdate=date) # Create new column (duplicate 'date.detected' to get proper format) for 'bat.date')
  #  x$newdate[x$time>=0 & x$time<set_hour] = x$date[x$time>=0 & x$time<set_hour]-1 
# }


#test functionality


#test first mapping function
# m1 <- format_m1(path.csv = "/Users/mkailing/Dropbox/Kailing_Projects/pitmastr/inst/dummy_MW/sn_map.csv",
#                 date1 = start,
#                 date2 = end)



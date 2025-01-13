# Format mapping files for consistency across objects

#' @title
#' Formatter for map1 file
#' 
#' @description
#' format_m1 formats the map1 file to be compatible with pit_master
#' 
#' @details 
#' This function automatically formats the first map file that contains the 
#' names, deployment dates, and serial numbers of each reader.
#' @param path.csv Path to map1 file location
#' @param date1 Name of reader start date column
#' @param date2 Name of reader end date column
format_m1 <-function(path.csv, 
                     date1, date2) {
  date.formats = c("ymd","mdy","mdY")
  x <- data.frame(lapply(read.csv(path.csv),as.character)) %>%
    rename(start.date := {{date1}},
           end.date := {{date2}}) %>%
    mutate(start.date = parse_date_time(start.date,date.formats),
           end.date = parse_date_time(end.date, date.formats),
           end.date = replace_na(end.date, parse_date_time(Sys.Date(), date.formats)),
           serial.num = ifelse(is.na(serial.num), NA, format(as.numeric(serial.num), nsmall = 4)))
  return(x)
}

#' @title
#' formatter for map2 file
#' 
#' @description
#' format_m2 formats the map2 file
#' 
#' @details 
#' This function automatically formats the second map file that contains the 
#' biological information associated with pit_tag IDs.
#' @param path.csv Path to map2 file location
#' @param date1 Name of sample date column. Note: the biometrics in this column are what will be associated the pit_ids
#' @param id.colname Name of column where PIT tag IDs are stored.
#' @param keep.cols list of columns to keep from map2 file i.e., which variables do you want carried over?
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
    mutate(date.tagged = parse_date_time(date.tagged, date.formats)) %>%
    group_by({{id.colname}}) %>%
    arrange(date.tagged) %>%
    mutate(samp.event = as.integer(rank(date.tagged))) %>%
    ungroup() %>%
    filter(samp.event==1)
    #pivot_wider(names_from = samp.event, values_from = date.tagged)
  return(y)
}

# create_batdate <- function(x, date, hour.start, hour.end) {
#   x %>%
#     mutate(batdate=date) # Create new column (duplicate 'date.detected' to get proper format) for 'bat.date')
#   
#   x$batdate[x$time>=hour.start & x$batdate<hour.end] = x$batdate[x$time>=hour.start&x$hour.end<7]-1 # If bat is active in hours
# }

#pit_working
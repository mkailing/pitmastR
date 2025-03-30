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
#' # ids_wks <- compress_ids_date(x = pm2,
#' # obs.groups =  c(id, sex, site.x),
#' # obs.unit = "weeks",
#' # date.colname = date)
inds_by_dates <- function(x, #name of working dataframe
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

## This used the test tag dataframe to create a survival object
#' @title
#' Creates survival object
#' 
#' @description
#' Uses test tag fires to create 'sampling data frame' for survival analyses
#' 
#' @param x main data object, preferably returned from 'workhorse'
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



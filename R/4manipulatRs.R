# Data manipulation functions


## This compresses to the number of unique observations within a time frame. ie: how many dates detected during week 40 of the year?
# compress_ids_date <- function(x, #name of working dataframe
#                               obs.groups = c(pit_id), #list of columns to group by 
#                               obs.unit = c("weeks","months","years"), #date units that the user wants to count number of days active by
#                               date.colname) { #name of date column in x for function to search by
#   if (obs.unit == "weeks") {
#     y = x %>%
#       #mutate(date.colname=as.Date.POSIXlt(date.colname)) %>%
#       group_by(across({{obs.groups}}), year({{date.colname}}), month({{date.colname}}), week({{date.colname}})) %>%
#       summarise(n.days = length(unique({{date.colname}})))
#     #    colnames(z)<-gsub("(date.detected)","",colnames(z))
#     return(y)
#   }
#   if (obs.unit == "months") {
#     y = x %>%
#       group_by(across({{obs.groups}}), year({{date.colname}}), month({{date.colname}})) %>%
#       summarise(n.days = length(unique({{date.colname}})))
#   }
#   if (obs.unit == "years") {
#     y = x %>%
#       group_by(across({{obs.groups}}), year({{date.colname}})) %>%
#       summarise(n.days = length(unique({{date.colname}})))
#   }
#   return(y)
# }

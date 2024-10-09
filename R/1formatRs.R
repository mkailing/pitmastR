# Dataframe formatting functions

format_m1 <-function(path.csv, 
                       date1, date2,
                       date.format) {
  x <- data.frame(lapply(read.csv(path.csv),as.character)) %>%
    rename(start.date := {{date1}},
           end.date := {{date2}}) %>%
    mutate(start.date = format(as.Date(start.date,date.format), "%Y-%m-%d"),
           end.date = ifelse(is.na(end.date)|end.date=="",
                             format(Sys.Date(), "%Y-%m-%d"),
                             format(as.Date(end.date,date.format), "%Y-%m-%d")),
           serial.num = ifelse(is.na(serial.num), NA, format(as.numeric(serial.num), nsmall = 4)))
  return(x)
}


format_m2 <- function(path.csv,
                      date1, 
                      date.format,
                      id = id) {
  x <- data.frame(read.csv(path.csv)) %>%
    rename(date.tagged := {{date1}}) %>%
    mutate(date.tagged = as.Date(format(as.Date(as.character(date.tagged), date.format), "%Y-%m-%d"))) #%>%
    # group_by({{id}}) %>%
    # arrange(date.tagged) %>%
    # samp.event = rank(date.tagged)
}





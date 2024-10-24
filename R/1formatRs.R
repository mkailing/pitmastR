# Dataframe formatting functions


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

modify_dates <- function(x, hour.start, hour.end) {
  
}


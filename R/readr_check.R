readr_check <- function(mapfile, mastr_pit, cutoff = 2,
                        date1.format, date2.format){
  m1 <- data.frame(lapply(read.csv('sn_map.csv'),as.character)) %>%
    mutate(start.date = format(as.Date(start.date, date1.format), date2.format), #formats date reader was deployed at site.ent #"%m/%d/%y" #"%m/%d/%Y"
           end.date = ifelse(is.na(end.date)|end.date=="", #if no end date provided (reader is still deployed),
                             format(Sys.Date(), date2.format), #this will assign current date for end date if not provided
                             format(as.Date(end.date, date1.format), date2.format)))
  tt <- mastr_pit[mastr_pit$pit_id %in% m1$test.tag,]
  readr_summary <- mastr_tt %>%
    group_by(site, ent, date) %>%
    summarise(test.tag_checks = length(unique(time))) %>%
    mutate(operating = ifelse(test.tag_checks>=cutoff, "Y","N"))
  return(readr_summary)
}

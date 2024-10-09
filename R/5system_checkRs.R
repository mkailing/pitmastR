# Data integration functions

readr_check <- function(x, y, min.reads = 2){
  tt <- x[x$pit_id %in% y$test.tag,]
  z <- tt %>%
    group_by(reader, date) %>%
    summarise(tt.fires = length(unique(time))) %>%
    mutate(op = ifelse(tt.fires>=min.reads, "Y","N"))
  return(z)
}


integrate_ids <- function(x, y, anchor_by){
  #anchor_by = quote(anchor_by)
  df <- merge(x, y, by = anchor_by) %>% #this database has all detections that were matched to a pit_id in m2
    rename(date.detected = date.x,
           site.detected = site.x,
           site.tagged = site.y,
           date.tagged = date.y)
  return(df)
}
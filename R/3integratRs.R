# Data integration functions

integrate_ids <- function(x, y,
                          joiner = c(),
                          drop.tt = 'N'){
  #anchor_by = quote(anchor_by)
  if (drop.tt == "Y") {
    df <- left_join(x, y, by = {{joiner}}) #%>%
      #rename(site.detected = site.x,
      #       site.tagged = site.y) #%>%
    df <- df[!(df$pit_id %in% m1$test.tag),]
  }
  else {
    df <- left_join(x, y, by = {{joiner}}) #%>%
      #rename(site.detected = site.x,
      #       site.tagged = site.y)
  }
  return(df)
}


# integrate_ids <- function(x, y, 
#                           joiner, 
#                           drop.tt = 'N'){
#   #anchor_by = quote(anchor_by)
#   if (drop.tt == "Y") {
#     df <- full_join(x, y, by = joiner,
#                     relationship = 'many-to-many') #%>%
#     #rename(site.detected = site.x,
#     #       site.tagged = site.y) #%>%
#     df <- df[!(df$pit_id %in% m1$test.tag),]
#   }
#   else {
#     df <- full_join(x, y, by = joiner,
#                     relationship = 'many-to-many') #%>%
#     #rename(site.detected = site.x,
#     #       site.tagged = site.y)
#   }
#   return(df)
# }

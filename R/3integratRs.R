# Data integration functions


#' @title
#' Primary integration function for biological data
#' 
#' @description
#' Conveniently joins biological variables 
#' 
#' @param x data object returned from 'workhorse' that contains tag reads
#' @param y data object returned from 'format_m2' that contains biological data
#' @param joiner name of shared columns in both x and y dataframes that associates tag reads to biological data (PIT tag ID)
#' @param remove.tt drop or keep reads of test tags that are listed in first map file (m1). This defaults to FALSE. If TRUE, test tag reads are ommitted from returned object
#' @export
#' @details 
#' Optional section! Functionally, this is a left_join that joins bio data in m2 file with tag reads, drops test tag reads, and renames columns for clarity.
#' Currently, this integration does not account for time-varying biological variables (body mass, age, etc)
#' 
#' @examples
#' # working_pit <- integrate_ids(mastr_pit, m2, 
#' # joiner = 'pit_id',
#' # remove.tt = "Y")
integrate_ids <- function(x, y,
                          joiner = c(),
                          remove.tt = FALSE){
  if (remove.tt == TRUE) {
    df <- left_join(x, y, by = {{joiner}}) #%>%
      #rename(site.detected = site.x,
      #       site.tagged = site.y) #%>%
    df <- df[!(df$id %in% m1$test.tag),]
  }
  else {
    df <- left_join(x, y, by = {{joiner}}) #%>%
      #rename(site.detected = site.x,
      #       site.tagged = site.y)
  }
  return(df)
}

#THIS FUNCTION NEEDS TO BE MODIFIED TO MATCH IN TIME-VARYING MORPHOMETRICS!

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

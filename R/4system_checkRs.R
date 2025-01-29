# System check functions


#' @title
#' Evaluates system operation
#' 
#' @description
#' Summarizes the number of times a test tag was fired to determine if system was operating each day
#' 
#' @param x dataframe with all test tag reads
#' @param sys.colname name of the column where unique system names are stored (the grouping variable by which to summarize tag reads)
#' @param date1 date variable to summarize tag reads by
#' @param min.reads number of times a test tag should be read in a date to be considered. Defaults to 2.
#' @export
#' @details 
#' this seems optional!
#' @examples
#' # TBD
readr_check <- function(x, sys.colname, date1, min.reads = 2){
  #tt <- x[x$id %in% y$test.tag,] #this could be done from workhorse, stored as tt df object, then used as x
  z <- x %>%
    group_by({{sys.colname}}, {{date1}}) %>% #should have the option to use date or converted dates
    summarise(tt.fires = length(unique(time))) %>%
    mutate(op = ifelse(tt.fires>=min.reads, TRUE, FALSE))
  return(z)
}
# 
# readr_viz <- function(df, site.col) {
#   p <- ggplot(data = df, aes(x=date, y = tt.fires, fill = op)) +
#     facet_wrap(vars({{site.col}}), ncol = 1) +
#     geom_col(aes(x=date)) +
#     scale_fill_manual(values = c("red","blue")) +
#     theme_bw()
#   print(p)
# }

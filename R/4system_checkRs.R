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
#' @param min.reads number of times a test tag should be read in a date to be considered operating. Defaults to 2.
#' @export
#' @details 
#' this seems optional!
#' @examples
#' # TBD
readr_check <- function(x, sys.colname, date1, min.reads = 2){
  z <- x %>%
    group_by({{sys.colname}}, {{date1}}) %>% #should have the option to use date or converted dates
    summarise(tt.fires = length(unique(time))) %>%
    mutate(op = ifelse(tt.fires>=min.reads, TRUE, FALSE)) %>%
    rename(system = 1, date = 2)
  return(z)
}


#' @title
#' Visualizes system operation
#' 
#' @description
#' Plots the number of times a test tag was fired, indicating whether reader was on based on object returned by 'readr_check'.
#' 
#' @param df dataframe with all test tag reads
#' @param site.colname name of the column where unique system names are stored (the grouping variable by which to summarize tag reads)
#' @param date.colname name of the column where date variable (the time grouping variable by which to summarize tag reads)
#' @export
#' @details 
#' this seems optional!
#' @examples
#' # TBD
readr_viz <- function(df) {
  p <- ggplot(data = subset(df)) +
    facet_wrap(~system, scales = 'free') +
    geom_col(aes(x=date, y = tt.fires, fill = op)) +
    scale_fill_manual(name = "Reader on", values = c("red","blue")) +
    ylab("Count of test tag fires") +
    theme_bw() +
    theme(strip.background = element_blank(),
          legend.text = element_text(size=8),
          legend.title = element_text(size = 10))
  print(p)
}

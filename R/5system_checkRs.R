# Data integration functions

# readr_check <- function(x, y, site.col, min.reads = 2){
#   tt <- x[x$pit_id %in% y$test.tag,]
#   z <- tt %>%
#     group_by({{site.col}}, serial.num, date) %>%
#     summarise(tt.fires = length(unique(time))) %>%
#     mutate(op = ifelse(tt.fires>=min.reads, "Y","N"))
#   return(z)
# }
# 
# readr_viz <- function(df, site.col) {
#   p <- ggplot(data = df, aes(x=date, y = tt.fires, fill = op)) +
#     facet_wrap(vars({{site.col}}), ncol = 1) +
#     geom_col(aes(x=date)) +
#     scale_fill_manual(values = c("red","blue")) +
#     theme_bw()
#   print(p)
# }

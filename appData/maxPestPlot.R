maxPestPlot <- function(df, xVar, title){
  data <- df %>% dplyr::group_by(.dots = c(xVar, 'SampleID')) %>% dplyr::summarise(n_pest = sum(!is.na(Result.ug.l)))
  data <- data %>% dplyr::group_by(.dots = xVar) %>% dplyr::summarise(maxPest = max(n_pest))
  
  p <- plot_ly(data) %>% 
    add_bars(x=~get(xVar),
             y=~maxPest) %>% 
    layout(title = paste0(title),
           xaxis = list(title=""),
           yaxis = list(title="Number of Pesticide Ingredients"))
  return(p)
}

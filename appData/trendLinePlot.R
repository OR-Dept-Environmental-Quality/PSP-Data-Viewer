trendLinePlot <- function(data, title){
  trendData <- data %>% dplyr::group_by(Analyte, Year) %>%
    dplyr::summarise(
      Average = mean(ResultNA_0, na.rm = TRUE),
      Maximum = max(ResultNA_0, na.rm = TRUE),
      Median = median(ResultNA_0, na.rm = TRUE),
      Average_Det = ifelse(!is.na(mean(Result.ug.l, na.rm = TRUE)), mean(Result.ug.l, na.rm = TRUE), 0),
      Maximum_Det = ifelse(!is.infinite(max(Result.ug.l, na.rm = TRUE)), max(Result.ug.l, na.rm = TRUE), 0),
      Median_Det = ifelse(!is.na(median(Result.ug.l, na.rm = TRUE)), median(Result.ug.l, na.rm = TRUE), 0)
    )
  p <- plot_ly(trendData) %>%
    add_trace(
      type = 'scatter',
      mode = 'lines+markers',
      x = ~Year,
      y = ~Average_Det,
      showlegend = TRUE,
      color = ~Analyte,
      line = list(width = 4,
                  connectgaps = TRUE),
      marker = list(size = 10,
                    symbol = 'diamond')
    ) %>% 
  layout(legend = list(font = list(size = 18),
                       x = 1,
                       y = 0.9),
         title = paste(title),
         titlefont = list(size = 18),
         margin = list(t = 100,
                       l = 100),
         xaxis = list(title = "", showline = TRUE, ticks = "outside"),
         yaxis = list(
           title = "<b>Average Concentration of Detections (ug/L)",
           titlefont = list(size = 14),
           showline = TRUE,
           ticks = "outside",
           rangemode = 'tozero',
           range = c(0, max(trendData$Average_Det, na.rm = TRUE)*1.1)
         )
  )
  return(p)
}

#### Function for standard detection frequency plot (stacked bars with %ALR) ####

FreqPlot <-
  cmpfun(function(data, xlab, orderVar){
    data2 <- data[order(orderVar),]
    plot <- plot_ly(data2) %>% 
      add_bars(
        x = ~Analyte,
        y = ~Per_10,
        name = '<10% of benchmark',
        width = 0.75,
        hoverinfo = "text",
        hovertext = paste0(format(round(data2$Per_10, 2), nsmall = 2), " - <b><10% of benchmark"),
        color = I('#99CCCC')
      ) %>% 
      add_trace(type = 'bar',
                x = ~Analyte,
                y = ~Per10_50,
                name = '10-50%',
                width = 0.75,
                hoverinfo = "text",
                hovertext = paste0(format(round(data2$Per10_50, 2), nsmall = 2), " - <b>10-50%"),
                color = I('#336699')
      ) %>%
      add_trace(type = 'bar',
                x = ~Analyte,
                y = ~Per50_100,
                name = '50-100%',
                width = 0.75,
                hoverinfo = "text",
                hovertext = paste0(format(round(data2$Per50_100, 2), nsmall = 2), " - <b>50-100%"),
                color = I('#FF9900')
      ) %>%
      add_trace(type = 'bar',
                x = ~Analyte,
                y = ~Per100_,
                name = 'Over benchmark',
                width = 0.75,
                hoverinfo = "text",
                hovertext = paste0(format(round(data2$Per100_, 2), nsmall = 2), " - <b>Over benchmark"),
                color = I('#b30000')
      ) %>%
      add_trace(type = 'bar',
                x = ~Analyte,
                y = ~NoBench,
                name = 'No benchmark',
                width = 0.75,
                hoverinfo = "text",
                hovertext = paste0(format(round(data2$NoBench, 2), nsmall = 2), " - <b>No benchmark<br>"),
                color = I('grey')
      ) %>%
      add_trace(type = 'bar',
                x = ~Analyte,
                y = 0,
                name = "# of Samples",
                showlegend = FALSE,
                hoverinfo = 'text',
                hovertext = paste0("<b># of Samples: </b>", data2$NSamples)
      ) %>% 
      layout(barmode = 'stack',
             hovermode = 'x',
             yaxis = list(title = '<b>Detection Frequency (%)</b>',
                          range = c(0,100)),
             xaxis = list(title = '',
                          tickvals= ~Analyte,
                          ticktext= ~Analyte,
                          tickangle= -45),
             margin = list(b = 180)
      )
    return(plot)
  }
  )

#### Function to add custom configuration to plots ####

plotConfig <- function(p){
  layout(p, 
         titlefont = list(size=16),
         xaxis = list(titlefont=list(size=14),
                      tickfont=list(size=13)),
         yaxis = list(titlefont=list(size=14),
                      tickfont=list(size=12)),
         legend = list(font=list(size=14))) %>% 
    plotly::config(displayModeBar='hover', editable=TRUE, showTips=TRUE, showAxisDragHandles=TRUE, showAxisRangeEntryBoxes=TRUE, displaylogo=FALSE)
}

#### Function for Navbar with text elements ####

navbarPageWithText <- function(..., text) {
  navbar <- navbarPage(...)
  textEl <- tags$p(class = "navbar-text", text)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], textEl)
  navbar
}

#### Function to add download button to map ####

registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}
library(shiny)
library(leaflet)
library(geojsonio)
library(plotly)
library(ggplot2)

setwd("C:/dev/projects/Melbourne housing visualization/")
source("ui.R")

opts <- leafletOptions(zoomControl=F, minZoom = 10, maxZoom = 12)
baseMap <- leaflet(options = opts)
baseMap <- setView(baseMap, lat=-37.814, ln=144.96332, zoom=10)
baseMap <- addTiles(baseMap)

data <- read.csv("data/Melbourne_housing_FULL.csv")
data <- na.omit(data)

shape <- geojson_read("data/melbourne.geojson", what="sp")
m <- leaflet(shape, options = opts)
m <- setView(m, lat=-37.814, ln=144.96332, zoom=10)
m <- addTiles(m)

ui <- getUI(data)

data$selected_ <- rep(F, dim(data)[1])

server <- function(input, output, session){
  bPoints <- reactiveValues(p = data)
  lastActiveBrush <- reactiveValues(x=0)
  
  output$locations <- renderLeaflet({
    subData <- data[
      (as.Date(
        data$Date, format="%d/%m/%Y") >=
         as.Date(
           input$yearsSlider1[1],format="%d/%m/%Y")) & 
        (as.Date(
          data$Date, format="%d/%m/%Y") <= 
           as.Date(
             input$yearsSlider1[2], format="%d/%m/%Y")),
      ]
    subData <- na.omit(subData)
    subData <- data.frame(price = subData$Price, lat=subData$Lattitude, long=subData$Longtitude)
    
    if(input$showColor==T){
      pal_ <- colorQuantile("YlOrRd", domain = subData$price, n=9)
      
      baseMap %>% 
        addCircles(lng = subData$long, lat = subData$lat,
                   color = pal_(subData$price)) %>%
        addLegend(pal = pal_, values = subData$price, opacity = 0.7, title = NULL,
                  position = "bottomright", labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    f <- format(round(cuts[-n]), big.mark = " ")
                    t <- format(round(cuts[-1]), big.mark = " ")
                    cuts = paste0(f, " - ", t, " AUD")
                  })
    } else {
      addCircles(baseMap, lng = subData$long, lat = subData$lat)
    }
  })
  
  output$choro <- renderLeaflet({
    dates <- input[["yearsSlider1"]]
    names <- shape@data[["name"]]
    
    subTimeData <- data[(as.Date(
                        data[["Date"]], format="%d/%m/%Y") >=
                         as.Date(
                           dates[1], format="%d/%m/%Y")) & 
                      (as.Date(
                        data[["Date"]], format="%d/%m/%Y") <= 
                         as.Date(
                           dates[2], formaft="%d/%m/%Y")), ]
    
    outSubData <- data.frame(Suburb=character(),
                             Mean=double(),
                             Max=double(),
                             Min=double(),
                             Obs=integer())
    
    for (suburbName in shape@data[["name"]]) {
      vals <- subTimeData[subTimeData$Suburb == suburbName, 5]
      vals <- na.omit(vals)
      
      if (length(vals)>0) {
        rowDf <- data.frame(
          Suburb = suburbName,
          Mean = mean(vals),
          Max = max(vals),
          Min = min(vals),
          Obs = length(vals)
        )
      } else {
        rowDf <- data.frame(
          Suburb = suburbName,
          Mean = NA,
          Max = NA,
          Min = NA,
          Obs = NA
        )
      }
      outSubData <- rbind(outSubData, rowDf)
    }
    
    if(input$measure == "Mean"){
      d <- outSubData$Mean
    } else if (input$measure == "Min"){
      d <- outSubData$Min
    } else if (input$measure == "Max"){
      d <- outSubData$Max
    } else if (input$measure == "# Sold"){
      d <- outSubData$Obs
    }
    
    pal_ <- colorQuantile("YlOrRd", domain = d, n=8)
    
    labels <- sprintf(
      paste0(outSubData$Suburb, "<br>", 
             "Mean Price: ", format(round(as.numeric(outSubData$Mean)), big.mark=" "), " AUD<br>",
             "Min Price: ", format(round(as.numeric(outSubData$Min)), big.mark=" "), " AUD<br>",
             "Max Price: ", format(round(as.numeric(outSubData$Max)), big.mark=" "), " AUD<br>",
             "Number sold: ", outSubData$Obs
             )
    ) %>% lapply(htmltools::HTML)
    
    m %>% addPolygons(
      fillColor = pal_(d),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")
    ) %>% addLegend(pal = pal_, values = ~d, opacity = 0.7, title = NULL,
                    position = "bottomright", labFormat = function(type, cuts, p) {
                      n = length(cuts)
                      f <- format(round(cuts[-n]), big.mark = " ")
                      t <- format(round(cuts[-1]), big.mark = " ")
                      cuts = paste0(f, " - ", t, " AUD")
                    })
  })
  
  output$multiView1 <- renderPlot({
    brushed <- bPoints$p
    colors = rep("lightblue", dim(data)[1])
    
    if(lastActiveBrush$x==1){
      colors[brushed$selected_ == T] <- "lightpink"
    } else {
      colors[brushed$selected_ == T] <- "black"
    }
    
    ggplot(brushed,
           aes(Rooms, Price)) + geom_point(color=colors) +
      scale_x_continuous(breaks=scales::pretty_breaks(n = 12)) +
      scale_y_continuous(breaks=scales::pretty_breaks(n = 12),
                         labels = scales::number) +
      labs(y = "Price (AUD)")
  })
  
  output$multiView2 <- renderPlot({
    brushed <- bPoints$p
    colors = rep("lightblue", dim(data)[1])
    
    if(lastActiveBrush$x==2){
      colors[brushed$selected_ == T] <- "lightpink"
    } else {
      colors[brushed$selected_ == T] <- "black"
    }
    
    brushed$Distance <- as.numeric(brushed$Distance)
    
    ggplot(brushed,
           aes(Distance, Price)) + geom_point(color=colors) + 
      scale_x_continuous(breaks=scales::pretty_breaks(n = 10)) +
      scale_y_continuous(breaks=scales::pretty_breaks(n = 12),
                         labels = scales::number) +
      labs(y = "Price (AUD)") +
      labs(x = "Distance (km)")
  })
  
  output$multiView3 <- renderPlot({
    brushed <- bPoints$p
    colors = rep("lightblue", dim(data)[1])
    
    if(lastActiveBrush$x==3){
      colors[brushed$selected_ == T] <- "lightpink"
    } else {
      colors[brushed$selected_ == T] <- "black"
    }
    
    ggplot(brushed,
           aes(Landsize, Price)) + geom_point(color=colors) + scale_x_continuous(
             trans = "log",
             breaks = c(1, 10, 100, 1000, 10000, 100000)) +
      scale_y_continuous(breaks=scales::pretty_breaks(n = 12),
                         labels = scales::number) +
      labs(y = "Price (AUD)") +
      labs(x = "Landsize (in m^2)")
  })
  
  output$multiView4 <- renderPlot({
    brushed <- bPoints$p
    colors = rep("lightblue", dim(data)[1])
    
    if(lastActiveBrush$x==4){
      colors[brushed$selected_ == T] <- "lightpink"
    } else {
      colors[brushed$selected_ == T] <- "black"
    }
    
    brushed$Distance <- as.numeric(brushed$Distance)
    
    ggplot(brushed,
           aes(Distance, Rooms)) + geom_point(color=colors) +
      scale_x_continuous(breaks=scales::pretty_breaks(n = 10)) +
      scale_y_continuous(breaks=scales::pretty_breaks(n = 12)) +
      labs(x = "Distance (km)")
  })
  
  observeEvent(input$selectAll,{
    updateDateRangeInput(session, "yearsSlider1",
                         start=min(as.Date(data$Date, format="%d/%m/%Y")),
                         end=max(as.Date(data$Date, format="%d/%m/%Y")))
  })
  
  observeEvent(input$multiViewBrush1, {
    bPoints$p <- brushedPoints(data, input$multiViewBrush1, allRows = T)
    bPoints$p <- bPoints$p[order(bPoints$p$selected_), ]
    session$resetBrush("multiViewBrush2")
    session$resetBrush("multiViewBrush3")
    session$resetBrush("multiViewBrush4")
    lastActiveBrush$x=1
  })
  
  observeEvent(input$multiViewBrush2, {
    bPoints$p <- brushedPoints(data, input$multiViewBrush2, allRows = T)
    bPoints$p <- bPoints$p[order(bPoints$p$selected_), ]
    session$resetBrush("multiViewBrush1")
    session$resetBrush("multiViewBrush3")
    session$resetBrush("multiViewBrush4")
    lastActiveBrush$x=2
  })
  
  observeEvent(input$multiViewBrush3, {
    bPoints$p <- brushedPoints(data, input$multiViewBrush3, allRows = T)
    bPoints$p <- bPoints$p[order(bPoints$p$selected_), ]
    session$resetBrush("multiViewBrush2")
    session$resetBrush("multiViewBrush1")
    session$resetBrush("multiViewBrush4")
    lastActiveBrush$x=3
  })
  
  observeEvent(input$multiViewBrush4, {
    bPoints$p <- brushedPoints(data, input$multiViewBrush4, allRows = T)
    bPoints$p <- bPoints$p[order(bPoints$p$selected_), ]
    session$resetBrush("multiViewBrush2")
    session$resetBrush("multiViewBrush3")
    session$resetBrush("multiViewBrush1")
    lastActiveBrush$x=4
  })

  observeEvent(input$multiViewClick1, {
    bPoints$p <- data
    lastActiveBrush$x=0
  })
  
  observeEvent(input$multiViewClick2, {
    bPoints$p <- data
    lastActiveBrush$x=0
  })
  
  observeEvent(input$multiViewClick3, {
    bPoints$p <- data
    lastActiveBrush$x=0
  })
  
  observeEvent(input$multiViewClick4, {
    bPoints$p <- data
    lastActiveBrush$x=0
  })
}

shinyApp(ui=ui, server=server)

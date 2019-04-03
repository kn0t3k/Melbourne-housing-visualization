library(shiny)
library(leaflet)
library(geojsonio)
library(plotly)
library(ggplot2)
library(vioplot)

setwd("C:/dev/projects/melbourne-housing/")
source("ui.R")

opts <- leafletOptions(zoomControl=F, minZoom = 10, maxZoom = 12)
baseMap <- leaflet(options = opts)
baseMap <- setView(baseMap, lat=-37.814, ln=144.96332, zoom=10)
baseMap <- addTiles(baseMap)

data <- read.csv("Melbourne_housing_FULL.csv")

shape <- geojson_read("data/melbourne.geojson", what="sp")
m <- leaflet(shape, options = opts)
m <- setView(m, lat=-37.814, ln=144.96332, zoom=10)
m <- addTiles(m)

priceTypeSubData <- data[!is.na(data$Distance) & !is.na(data$Price), c(4, 5)]
priceTypeSubData <- na.omit(priceTypeSubData)

distancePriceRooms <- data[!is.na(data$Distance) & data$Distance!="#N/A" & !is.na(data$Price) & !is.na(data$Rooms), c(3, 5, 9)]
distancePriceRooms <- na.omit(distancePriceRooms)
distancePriceRooms$Distance <- as.numeric(distancePriceRooms$Distance)
distancePriceRoomsPlot <- plot_ly(x=distancePriceRooms$Distance, y=distancePriceRooms$Rooms, z=distancePriceRooms$Price,
        type="scatter3d", mode="markers", 
        color=distancePriceRooms$Price)

distancePrice <- data[!is.na(data$Distance) & !is.na(data$Price)  & data$Distance!="#N/A", c(5, 9)]
distancePrice <- na.omit(distancePrice)
distancePrice$Distance <- as.numeric(distancePrice$Distance)
distancePrice <- distancePrice[order(distancePrice$Distance), ]


priceRooms <- data[!is.na(data$Distance) & !is.na(data$Price), c(3, 5)]
priceRooms <- na.omit(priceRooms)

priceLand <- data[!is.na(data$Distance) & !is.na(data$Price), c(5, 14)]
priceLand <- na.omit(priceLand)


ui <- getUI(data)

server <- function(input, output, session){
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
    addCircles(baseMap, lng = subData$Longtitude, lat = subData$Lattitude)
  })
  
  output$locationsColor <- renderLeaflet({
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
    
    pal_ <- colorQuantile("YlOrRd", domain = subData$price, n=8)
    
    addCircles(baseMap, lng = subData$long, lat = subData$lat,
               color = pal_(subData$price))
  })
  
  output$locationsType <- renderLeaflet({
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
    subData <- data.frame(type = subData$Type, lat=subData$Lattitude, long=subData$Longtitude)
    
    pal_ <- colorNumeric("YlOrRd", domain = subData$type)
    
    addCircles(baseMap, lng = subData$long, lat = subData$lat,
               color = pal_(subData$type))
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
                      cuts = paste0(f, " - ", t)
                    })
  })
  
  output$distancePrice <- renderPlot({
    plot(distancePrice$Distance, distancePrice$Price)
  })
  
  output$distancePriceRooms <- renderPlotly({
    distancePriceRoomsPlot
  })
  
  output$priceType <- renderPlot({
    x1 <- priceTypeSubData$Price[priceTypeSubData$Type=="t"]
    x2 <- priceTypeSubData$Price[priceTypeSubData$Type=="h"]
    x3 <- priceTypeSubData$Price[priceTypeSubData$Type=="u"]
    vioplot(x1, x2, x3, names=c("t", "h", "u"))
  })
  
  output$priceRooms <- renderPlot({
    boxplot(priceRooms$Price~priceRooms$Rooms, varwidth=T, outline=F)
  })
  
  output$priceLand <- renderPlot({
    plot(priceLand$Landsize, priceLand$Price, log="x")
  })
  
  observeEvent(input$selectAll,{
    updateDateRangeInput(session, "yearsSlider1",
                         start=min(as.Date(data$Date, format="%d/%m/%Y")),
                         end=max(as.Date(data$Date, format="%d/%m/%Y")))
  })
}

shinyApp(ui=ui, server=server)

getUI <- function(completeData) {
  return (
    fluidPage(
      headerPanel("Melbourne housing market"),
        tabsetPanel(
          tabPanel(
            "Map",
            dateRangeInput("yearsSlider1",
                           label=h3("Select date range"),
                           format = "dd/mm/yyyy",
                           min="2016-01-28",
                           max="2018-03-17",
                           start="2017-01-01",
                           end="2017-04-01"),
            actionButton("selectAll", label = "Select all"),
            h3("Locations of sold houses"),
            leafletOutput("locations",
                          height=500),
            h3("Locations of sold houses with price range"),
            leafletOutput("locationsColor",
                          height=500),
            h3("Locations of sold houses with type"),
            leafletOutput("locationsType",
                          height=500),
            h3("Choropleth of suburbs"),
            selectInput("measure", label="Select measure (price)",
                        choices = c("Mean", "Min", "Max", "# Sold")),
            leafletOutput("choro",
                          height=500)
          ),
          tabPanel(
            "Stats",
            h3("Price dependent on distance from Central Business District"),
            plotOutput("distancePrice",
                       height = 500),
            h3("Price dependent on distance (from CBD) and # of rooms"),
            plotlyOutput("distancePriceRooms",
                         height = 500),
            h3("Price dependent on number of rooms"),
            plotOutput("priceRooms",
                       height = 500),
            h3("Price dependent on property type"),
            plotOutput("priceType",
                         height = 500),
            h3("Price dependent on land size"),
            plotOutput("priceLand",
                       height = 500)
          )
      )
    )
  )
}
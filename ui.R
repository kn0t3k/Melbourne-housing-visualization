getUI <- function(completeData) {
  return (
    fluidPage(
      headerPanel("Melbourne housing market"),
      dateRangeInput("yearsSlider1",
                     label=h3("Select date range"),
                     format = "dd/mm/yyyy",
                     min="2016-01-28",
                     max="2018-03-17",
                     start="2017-01-01",
                     end="2017-04-01"),
      actionButton("selectAll", label = "Select all"),
      checkboxInput("showColor", "Show Color", FALSE),
      h3("Locations of sold houses"),
      leafletOutput("locations",
                    height=500),
      h3("Choropleth of suburbs"),
      selectInput("measure", label="Select measure (price)",
                  choices = c("Mean", "Min", "Max", "# Sold")),
      leafletOutput("choro",
                    height=500),
      fluidRow(
        splitLayout(
          cellWidths = c("40%", "40%"), 
          plotOutput("multiView1",
                     height=300,
                     brush = brushOpts(
                       id = "multiViewBrush1",
                       resetOnNew=T
                     ),
                     click = "multiViewClick1"), 
         plotOutput("multiView2",
                    height=300,
                    brush = brushOpts(
                      id = "multiViewBrush2",
                      resetOnNew=T
                    ),
                    click = "multiViewClick2")
        )
      ),
      fluidRow(
        height=300,
        splitLayout(
          cellWidths = c("40%", "40%"), 
          plotOutput("multiView3",
                     height=300,
                     brush = brushOpts(
                       id = "multiViewBrush3",
                       resetOnNew=T
                     ),
                     click = "multiViewClick3"),
          plotOutput("multiView4",
                     height=300,
                     brush = brushOpts(
                       id = "multiViewBrush4",
                       resetOnNew=T
                     ),
                     click = "multiViewClick4")
        )
      )
    )
  )
}
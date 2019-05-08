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
                     end="2017-05-01"),
      actionButton("selectAll", label = "Select all"),
      checkboxInput("showColor", "Show Color", FALSE),
      h3("Locations of sold houses"),
      p("This map shows locations of sold houses in the selected date range."),
      p("You can enable color encoding by the check box above."),
      p("You can zoom in and out and move around the map with your mouse."),
      leafletOutput("locations",
                    height=500),
      h3("Choropleth of suburbs"),
      p("This map shows selected Melbourne suburbs and their statistics in the selected date range."),
      p("You can select a parameter of your interest from the list below."),
      p("If you hover over any suburb, a detail of it will show up."),
      selectInput("measure", label="Select measure (price)",
                  choices = c("Mean", "Min", "Max", "# Sold")),
      leafletOutput("choro",
                    height=500),
      br(),
      p("The following plots are interactive, you can select any area in one of them and the corresponding points will be highlighted in other plots."),
      p("Clicking or selecting a new selection will remove the old one."),
      p("Points in your last selection are light pink, corresponding points in different views are black."),
      p("Try to answer the following questions:"),
      p("  1. You want a house with 1 or 2 rooms, at most 20Km away from the center. What sort of price should you expect? How old will the house be?"),
      p("  2. You want a house with 3 or 4 rooms with price 4M+ AUD. Will these houses be in the center? Will they be decently new for the price?"),
      p("  3. You want a house less then 20Km from the center no matter the price. What selection in number of rooms will you have?"),
      p("  4. You like old houses and want to buy one for 2M-3M AUD built before 1900. Will you have a good choice to live in the city center? Will you have many rooms?"),
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
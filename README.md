Melbourne housing visualization

Project for PV251

Set the current working directory to where you scripts are located,
select all and run. Open in browser for better performance.

Data source:
https://www.kaggle.com/anthonypino/melbourne-housing-market
file: Melbourne_housing_FULL.csv

Melbourne map in geojson:
https://github.com/codeforamerica/click_that_hood/blob/master/public/data/melbourne.geojson

Contains data about Melourne housing market, prices, dates, landsizes, # of rooms etc.
Preprocessing is fairly simple, omit NAs, some other strange values.
Basically to create a particular view of the data, I select it somehow from the complete dataset, transform if neccessary (mainly because of the date format) and plot it.

UI is simple, created with Shiny.

There are two tabs, the first is a map with sold houses and some info about them
(mean, min, max prices, locations, number sold). You can select the date you
are interested in, or you can select all using the button.

The second tab is a global overwiev of the data, independent of the date input.
The plots are explained in the view or in the source code.

Inspired by the local situation on Czech housing market, too bad that data is not available :(.

If I was to improve something on this project, I would add colors to the graphs to make them look better.
Maybe adding more graphs with different view of the date would have more statistical impact.
Also the plots are missing titles and the axis might not be the best from strict statistical point of view, but they are more than sufficient for visualization purposes.

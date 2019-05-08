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

When running, don't forget to change your working directory in the app.R file.

UI is simple, created with Shiny.

Inspired by the local situation on Czech housing market, too bad that data is not available :(.

THe plots are described in the app.

Author: Martin Knotek

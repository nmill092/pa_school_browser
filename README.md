This app is live on [Shinyapps.io](https://fum725-nilemill.shinyapps.io/pa_school_browser). 

## About this app

This app uses data from the Pennsylvania School Data Project led by Research for Action to provide insights on public school performance throughout the state of Pennsylvania. The dataset is compiled from a variety of sources, including:

* Pennsylvania Department of Education
* Civil Rights Data Collection (CRDC)
* Common Core of Data (CCD)
* PA Safe Schools

Additional data (median income, school district population) comes from the United States Census Bureau's 2016-2020 American Community Survey (ACS).

## How to use this app 

The **State Overview** tab provides state and county-level metrics on school demographics, performance, and other socioeconomic factors. The **County Drilldown** tab provides a more granular look at county-level stats, while also allowing users to browse through a list of school districts in each county. Under **School District Detail,** you'll find a tool that allows you to compare school districts or counties on a specific variable of interest. Finally, the **School District Explorer** provides a map and full listing of all of Pennsylvania's 499 school districts (excluding Bryn Athyn School District, which does not contain any schools and was not included anywhere in this dashboard).

## Packages used 

* Data Cleaning/Manipulation
  * dplyr
  * tidyr
  * purrr
* UI
  * shiny
  * shinydashboard
  * shinycssloaders
  * shinydashboardPlus
  * shinyWidgets
* Mapping & Data Visualization
  * rgdal
  * leaflet
  * highcharter
  * DT
* Text Manipulation
  * HTMLtools
  * glue
* Other
  * [tidycensus](https://walker-data.com/tidycensus/index.html) (an amazing package for working with U.S. Census data) 

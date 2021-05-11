# AirDashboard - US Flight Delay Dashboard
--- 

**Creator: Yang Tai**

**Group of 1 person.**

**Link to Live Dashboard:** <a href="https://ytai9109.shinyapps.io/AirDashboard/" target="_blank">AirDashboard</a>

**Link to raw dashboard code:** <a href ="https://github.com/ytai9109/AirDashboard/blob/main/app.R" target="_blank">Raw Code</a> 


This readme explains how the US Flight Delay Dashboard works.

---
## Overview
<img src="images/image1.JPG?raw=true" width="80%">

This dashboard relies on the [data](https://www.bts.dot.gov/topics/airlines-and-airports/quick-links-popular-air-carrier-statistics) from the United States Department of Transportation - Bureau of Transportation Statistics. 

Dataset is approximately 49MB in size and contains rows of data ranging in time from Jun 2003 to the latest available Jan 2021. In total there are 294237 observations and 22 variables. Data contains columns of:
- year, month
- carrier code and carrier name
- airport code and airport name
- arrival flight count
- delay flight count
- counts for each type of delay
- number of minutes for each type of delay (not used)
which I will perform data wrangling on. 

There are six interactive plots in total. Five being Plotly/GGplot2 charts and one Leaflet map. Data is specifically wrangled from the master dataset for each of the interactive plots. Data wranging is done using the tidyverse library, specifically the Dplyr module. Certain columns were added on top of the master dataset, such as US regions and individual state names. These new columns were added conditionally based on existing element in the master dataset, using Dplyr methods. 

At startup, the dashboard loads the 49MB master dataset from a csv file located in my Github Repository. I realized that shinyapp.io itself may not be the best to host a large csv file. It then runs the lines of code that wrangles the data, this uses a large amount of resource as wrangled datasets (namely: dat1, dat2, dat3, dat4, dat5) can contain up to 2,000,000 observations.

I placed numerous reactive elements inside this dashboard, such that any interaction by the user to any of the widgets will be updated to the plot elements in real time. The framework of this app relies heavily on Shiny reactive programming functions. 

---
## Libraries Used

- **shiny** - For developing shiny app
- **shinyjs** - allows custom modifications to certain shiny elements
- **Shinydashboard** - creates the layout for this dashboard, uses Shiny programming conventions
- **Tidyverse** - contains dplyr and ggplot2 used to wrangle data and create plots, occasionally stringr is used
- **plotly** - used to create the pie chart, or whenever ggplotly does not support a plot type
- **Leaflet** - creates the map used in the dashboard, custom layer from Stadia Maps using their free API access
- **RColorBrewer** - import predefined palettes used to add colors in plots
- **htmltools** - adds the HTML() environment to format elements in the dashboard

---
## Dashboard Layout

I learned and used Shiny Dashboard instead of FlexDashboard to structure the layout. The reasons being that Shiny Dashboard allows more control over specific elements such as placements of the grids and the dimension of the boxes for each element compared to what FlexDashboard has to offer. Shiny Dashboard uses the syntax that is predefined by Shiny apps, compared to FlexDashboard, the structure and the syntax for a FlexDashboard app appears jumbled. It is much more convenient to customize and code following one system and syntax. 

Some features that I like in Shiny Dashboard:
- tabbed sidebar on the left
- collapsible side bar
- uses box structure for main body, boxes can be collapsed
- fluidRow() and column() allows fine control of the element placement in the box

On the left hand side of the dashboard, I created a pop-out side bar that can be hidden by pressing the button with the three dashes on the top right corner of the side bar. Tabs exist in the sidebar where you can click to change elements of the dashboard. Each tab displays either a plot, map, or a data table. 

---
## Total Delayed Flights

**Goal:** 

The aim of this plot is to display the total number of arrival and delayed flights, as well as the percentage of delayed flights out of all arrivial flights, per month from years 2003 to 2021 (where data is available). For each month, the arrival and delay counts are a sum of all the airports and airlines in the United States. This way, we can gain a quick visualization of which months the air traffic is the busiest and whihc months the air traffic is the most idle. We can choose to display either the counts, or the natural log of the counts, or the counts that have been demeaned. Uses 'dat1' dataset.

Below the plot, there are two tabs that you can expand. One being "Add Regression" and the other being "Predicting Data". Expanding the "Add Regression" tab you will see controls where you can add a fitted line to the plot. The user can choose between what types of fitted lines they would like to add. Expanding the "Predicting Data" tab, we can use the lm() and predict() to predict values several months ahead of the data currently selected. Instructions to use this function are provided in the dashboard. To ensure the validity of the predictions, user must select a sequential number of years such as 2018, 2019, 2020 in order to predict data for year 2021. Selecting random years such as 2005, 2013, 2015 will not yield any sound predictions, even though the functions will still try to fit the data. 

**Usage:**

In the box located above the plot, there are four user controls:
- Select Plot Type: Switch between line plot or bar plot, dataset loaded into both plots are the same, just different visualization methods
- Select Year: Allows multiple year to be displayed on the plot, selecting a year will load all the months in that year into the plot (where data is available)
- Select Data to Display: User can choose either to display the arrival count, delayed count, or delayed percentage (delayed count / arrival count). The y-axis should automatically scale with the values.
- Augment Data?: Only works when "Select Data to Display" is in either "Arrival Count" or "Delayed Count" modes. The choices "Natural log" takes the natural log of the counts, and "Demean" takes the the mean of all the counts in the selected years, and minus each observation with that mean.
- Expand "Add Regression" tab below the plot. "Select Regression Type" allows adding a fitted line on top of the plot. Parameters such as polynomial degree for polynomial fit and span for LOESS can be fine tuned.
- Expand "Predicting Data" tab below the plot. Instructions for using this section is written inside the dashboard. Fitted equation and fit summary is provided. Predicted values (from the fit equation) is shown on the right. 

**Technical Details:**
- uses tidyverse (dplyr, ggplot) library to wrangle data
- months have been automatically sorted using abbreviations instead of numbers such as 01, 02, 03 etc. The trick is to order the characters using factors. 
- fit using base stats library, data curve fitting on plotly chart uses geom_smooth(), which goes hand in hand with the lm() from the base library
- htmltools to wrap parts of the text in order to format using HTML
- Plot controls, Add Regression, Predicting Data makes full use of Shiny's reactive programming controls

**Data Wrangling:**
- group by year, then group by month
- take sum of arrival flights and sum of delayed flights within each group
- calculate percent delayed by delayed flight count / arrival flight count
- take natural log of arrival and delay flight counts
- demean the arrival and delay flight counts
- sort abbreviated months into order using factors


---
## Delays by Airport

<img src="images/image2.JPG?raw=true" width="80%">
<img src="images/image3.JPG?raw=true" width="80%">

**Goal:** 

There are two types of plots the user can select, bar chart or pie chart. The data loaded into both plots are essentially the same, with minor differences in the naming conventions of the delay causes. For a given airport, and a given year and month, we can display the number of delays broken up into different delay causes. There are seven reasons for delay: carrier, weather, National Airspace System (NAS), Security, Late Aircraft, Canceled, and Diverted. We can visualize these by either the choosing the counts or as a percentage of all delay reasons. Bar chart is built using ggplot2 and ggplotly, it uses the 'dat2' dataset. Pie chart is built purely in plotly because interactive pie charts were not supported in ggplotly, it uses the 'dat2_pie' dataset. 

**Usage:**

**For bar chart:**

There are three user controls above the plot:
- Select an Airport: Allows selection of a single airport.
- Select Month(s) to Display: Allows selection of either a single month or multiple months. If multiple months are selected, months are grouped in the bar chart and will be displayed side-by-side for each delay cause. This is a good means of comparing delay causes for different months. 
- Select Data to Display: Displays either by count or by percentage of all delay causes. Percentage is calculated by (count of a particular delay cause / sum of all counts of delay causes).

**For pie chart:**

There are two user controls above the plot:
- Select an Airport: Allows selection of a single airport.
- Select Month(s) to Display: Allows selection of a single month.

**Data Wrangling:**

**For bar chart:**
- add US States column
- group by year, then month, then airport
- for each group, calculate the counts for each delay reason
- calculate delayed reason percentage out of all delay counts for each delay reason
- turn wide data format into long data format

**For pie chart:**
- same as data used in bar chart, except change column element name so that it's easier for plotly

**Technical Details:**
- pie chart built using plotly, plotly options uses lists inside lists, possibly carried over from python conventions
- Custom hover info when mouse is hovering over plots, this uses HTML format from the htmltools library


---
## Delays by Type

<img src="images/image4.JPG?raw=true" width="80%">

**Goal:** 

The aim is to visualize how much impact is caused by each delay type to all the flights across the States using a scatter plot. Spatial information such as US regions, as well as the traffic size of the airport are maintained in this plot. For a given year and month, and for a given delay cause, the plot displays on the y-axes, the percentage of total delayed flights (calculated by delayed flight count / arrival flight count), and on the x-axes, the likelihood a delay reason is causing the delay (calculated by number of counts for that delay cause / counts for all delays). Each dot represents an airport. It is colored by US region and is sized by the number of arrival flights for that airport within the chosen time period. Uses 'dat3' dataset.

For example, we can see that in 2020/Apr, the scatter of the Canceled flight delays are pervasive through all US regions, possibly due to the onset of COVID, compared to the scatter in 2020/Feb. 

**Usage:**

There are two user controls above the plot:
- Select Month(s) to Display: Allows selection of a single month.
- Select Delay Cause: User can choose to visualize the delays throughout all US airports for a specific delay cause. 
Hovering over each scatter point will display the information related to that point, including the airport name, arrival count, delay count, percentage of delays, count for a specific delay cause, and percentage out of all delays for a specific delay cause. 

**Data wrangling:**
- takes the same dataset as "Delays by Type"
- adds a percent delay column where the percent delay is delayed count divided by arrival count
- since the dataset has already been sorted into airports, we don't need to specifically wrangle anything


---
## Delays by Airline

<img src="images/image5.JPG?raw=true" width="80%">

**Goal:** 

The aim is to display the delay percentage statistics of each airline carrier from June 2013 to January 2021 using a box and whiskers plot. The box and whiskers is plotted out of delay percentages in each month for a specific airline carrier. The delay percentage is calculated from (delay count / arrival count) for a given month. Using the box and whiskers plot, we can visualize the 25th and 75th percentile, median, and any outliers of the delay percentage of each carrier. There is also an option to display individual data points to generate the box plot. This is implemented using the geom_jitter() layer. Uses 'dat4' dataset. 

**Usage:**

There are two user controls above the plot:
- Select Airlines: Allows multiple selection of airline carriers to display on the plot. Recommended to have less than 10 selected. Color palette used in this plot supports up to 12 carriers simulataneously. 
- Visualize Indiv. Data Points?: Adds individual data points as a layer on top of the box and whiskers plot. This allows a simple visualization of where all the data points are located on the axes. Hover over each data point to see details. 

**Data Wrangling:**
- add US States column
- group by year, then month, then by airline carrier
- for each group, calculate the counts for each delay reason, as well as the total arrival count and total delay count
- order abbreivated months by 


---
## Map of Airports

<img src="images/image6.JPG?raw=true" width="80%">

**Goal:** 

This is a simple map that contains markers of each US airport in the dataset. Upon hovering the mouse over the marker, the popup label displays the name of the airport. Clicking on the marker will display a popup showing information including airport name, number of arrivals, number of delays, percent of delays for a specific year. User can choose to display information for a specific year. This map is implemented using Leaflet. Map layer from Stadia with free API access. 

**Usage:**

There is a one control above the plot:
- Select a Year: Dropdown menu allows user to select a year from 2004 to 2020. Selecting this will change the labels that pop up if the blue point is clicked. I have omitted years 2003 and 2021 because those years do not contain data for every month in the year. 

**Data Wrangling:**
- pre-wrangled dataset called "airport_locdata.csv" done in a different notebook, uploaded to my repo in Github. Essentially compiles a list of US airports used in this dashboard, and appends their respective longitude and latitude to each airport
- group by year, then by airport
- for each group, calculate the sum of arrival count and delay count, then calculate delay percentage
- using airport IATA code, join longitude and latitude data from "airdat" into this dataset, this is done using left_join()


---
## Raw Data

**Goal:** 

Displays the dataset used to visualize the plots. Does not have any specific usage except to provide user with some reference. 

**Usage:**

At the top of the data table, there is a selector that allows the user to select which dataset to display. 

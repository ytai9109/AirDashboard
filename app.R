# load in libraries
library(shiny)
library(shinydashboard)
library(shinyjs)
library(tidyverse)
library(plotly)
library(RColorBrewer)
library(leaflet)
library(htmltools)


################# DATA WRANGLING ####################

# read in the csv file
delaydat = read_csv("https://raw.githubusercontent.com/ytai9109/AirDashboard/main/848649355_12021_1941_airline_delay_causes.csv")
# delaydat = read_csv("848649355_12021_1941_airline_delay_causes.csv")

# -------- create delay plot ---------
# round carrier_ct, weather_ct, nas_ct, security_ct, late_aircraft_ct to whole numbers
delaydat = delaydat %>% 
    mutate(across(9:13, round, digits=0))

# group data for total arriving flights by month
# group data for total delayed flights by month
# sum of delayed flights + cancelled + diverted
# wrangle data for plotting, change months into factor and sort them by order
dat1 = delaydat %>%
    mutate(month = month.abb[month]) %>%
    unite(year, month, col = "year_month", sep="/", remove = FALSE) %>%
    group_by(year,month) %>%
    mutate(total_arrival_flights = sum(arr_flights, na.rm = TRUE)) %>%
    mutate(total_delayed_flights = sum(arr_del15, arr_cancelled, arr_diverted, na.rm = TRUE)) %>%
    mutate(percent_delayed = total_delayed_flights / total_arrival_flights) %>%
    distinct(year_month, .keep_all = TRUE) %>%
    ungroup() %>%
    select(year_month, year, month, total_arrival_flights, total_delayed_flights, percent_delayed) %>%
    # take ln of arrival and delay flight count
    mutate(ln_total_arrival_flights = log(total_arrival_flights)) %>% 
    mutate(ln_total_arrival_flights = ifelse(ln_total_arrival_flights < 0, 0, ln_total_arrival_flights)) %>%
    mutate(ln_total_delayed_flights = log(total_delayed_flights)) %>% 
    mutate(ln_total_delayed_flights = ifelse(ln_total_delayed_flights < 0, 0, ln_total_delayed_flights)) %>%
    mutate(demean_total_arrival_flights = round(total_arrival_flights - mean(total_arrival_flights), digits = 0)) %>%
    mutate(demean_total_delayed_flights = round(total_delayed_flights - mean(total_delayed_flights), digits = 0)) %>%
    group_by(year_month, year, month) %>%
    mutate(month = factor(month,levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))) %>%
    arrange(year, month)




#-------- ADD STATES AND REGION COLUMNS ---------
# sort airports into their US regions and States
delay_States = delaydat %>%
    #group into US Regions
    mutate(region = ifelse(grepl("AK:|HI:|WA:|OR:|ID:|MT:|WY:|CA:|NV:|UT:|CO:", airport_name), "West", "-")) %>%
    mutate(region = ifelse(grepl("ND:|MN:|WI:|MI:|SD:|IA:|NE:|KS:|MO:|IL:|IN:|OH:", airport_name), "Midwest", region)) %>%
    mutate(region = ifelse(grepl("AZ:|NM:|TX:|OK:", airport_name), "Southwest", region)) %>%
    mutate(region = ifelse(grepl("PA:|NY:|VT:|ME:|NH:|MA:|RI:|CT:|NJ:", airport_name), "Northeast", region)) %>%
    mutate(region = ifelse(grepl("AR:|LA:|MS:|AL:|GA:|FL:|KY:|TN:|MD:|DE:|WV:|DC:|VA:|NC:|SC:", airport_name), "Southeast", region)) %>%
    mutate(region = ifelse(grepl("West|Midwest|Southwest|Northeast|Southeast",region), region, "Outer")) %>%
    relocate(region, .after = month) %>%
    # group into US states (there are 52 states.....)
    mutate(state_abv = ifelse(grepl("AL:", airport_name), "AL", "-")) %>%
    mutate(state_full = ifelse(grepl("AL:", airport_name), "Alabama", "-")) %>%
    mutate(state_abv = ifelse(grepl("AK:", airport_name), "AK", state_abv)) %>%
    mutate(state_full = ifelse(grepl("AK:", airport_name), "Alaska", state_full)) %>%
    mutate(state_abv = ifelse(grepl("AS:", airport_name), "AS", state_abv)) %>%
    mutate(state_full = ifelse(grepl("AS:", airport_name), "American Samoa", state_full)) %>%
    mutate(state_abv = ifelse(grepl("AZ:", airport_name), "AZ", state_abv)) %>%
    mutate(state_full = ifelse(grepl("AZ:", airport_name), "Arizona", state_full)) %>%
    mutate(state_abv = ifelse(grepl("AR:", airport_name), "AR", state_abv)) %>%
    mutate(state_full = ifelse(grepl("AR:", airport_name), "Arkansas", state_full)) %>%
    mutate(state_abv = ifelse(grepl("CA:", airport_name), "CA", state_abv)) %>%
    mutate(state_full = ifelse(grepl("CA:", airport_name), "California", state_full)) %>%
    mutate(state_abv = ifelse(grepl("CO:", airport_name), "CO", state_abv)) %>%
    mutate(state_full = ifelse(grepl("CO:", airport_name), "Colorado", state_full)) %>%
    mutate(state_abv = ifelse(grepl("CT:", airport_name), "CT", state_abv)) %>%
    mutate(state_full = ifelse(grepl("CT:", airport_name), "Connecticut", state_full)) %>%
    mutate(state_abv = ifelse(grepl("DE:", airport_name), "DE", state_abv)) %>%
    mutate(state_full = ifelse(grepl("DE:", airport_name), "Delaware", state_full)) %>%
    mutate(state_abv = ifelse(grepl("DC:", airport_name), "DC", state_abv)) %>%
    mutate(state_full = ifelse(grepl("DC:", airport_name), "District of Columbia", state_full)) %>%
    mutate(state_abv = ifelse(grepl("FL:", airport_name), "FL", state_abv)) %>%
    mutate(state_full = ifelse(grepl("FL:", airport_name), "Florida", state_full)) %>%
    mutate(state_abv = ifelse(grepl("GA:", airport_name), "GA", state_abv)) %>%
    mutate(state_full = ifelse(grepl("GA:", airport_name), "Georgia", state_full)) %>%
    mutate(state_abv = ifelse(grepl("GU:", airport_name), "GU", state_abv)) %>%
    mutate(state_full = ifelse(grepl("GU:", airport_name), "Guam", state_full)) %>%
    mutate(state_abv = ifelse(grepl("HI:", airport_name), "HI", state_abv)) %>%
    mutate(state_full = ifelse(grepl("HI:", airport_name), "Hawaii", state_full)) %>%
    mutate(state_abv = ifelse(grepl("ID:", airport_name), "ID", state_abv)) %>%
    mutate(state_full = ifelse(grepl("ID:", airport_name), "Idaho", state_full)) %>%
    mutate(state_abv = ifelse(grepl("IL:", airport_name), "IL", state_abv)) %>%
    mutate(state_full = ifelse(grepl("IL:", airport_name), "Illinois", state_full)) %>%
    mutate(state_abv = ifelse(grepl("IN:", airport_name), "IN", state_abv)) %>%
    mutate(state_full = ifelse(grepl("IN:", airport_name), "Indiana", state_full)) %>%
    mutate(state_abv = ifelse(grepl("IA:", airport_name), "IA", state_abv)) %>%
    mutate(state_full = ifelse(grepl("IA:", airport_name), "Iowa", state_full)) %>%
    mutate(state_abv = ifelse(grepl("KS:", airport_name), "KS", state_abv)) %>%
    mutate(state_full = ifelse(grepl("KS:", airport_name), "Kansas", state_full)) %>%
    mutate(state_abv = ifelse(grepl("KY:", airport_name), "KY", state_abv)) %>%
    mutate(state_full = ifelse(grepl("KY:", airport_name), "Kentucky", state_full)) %>%
    mutate(state_abv = ifelse(grepl("LA:", airport_name), "LA", state_abv)) %>%
    mutate(state_full = ifelse(grepl("LA:", airport_name), "Louisiana", state_full)) %>%
    mutate(state_abv = ifelse(grepl("ME:", airport_name), "ME", state_abv)) %>%
    mutate(state_full = ifelse(grepl("ME:", airport_name), "Maine", state_full)) %>%
    mutate(state_abv = ifelse(grepl("MD:", airport_name), "MD", state_abv)) %>%
    mutate(state_full = ifelse(grepl("MD:", airport_name), "Maryland", state_full)) %>%
    mutate(state_abv = ifelse(grepl("MA:", airport_name), "MA", state_abv)) %>%
    mutate(state_full = ifelse(grepl("MA:", airport_name), "Massachusetts", state_full)) %>%
    mutate(state_abv = ifelse(grepl("MI:", airport_name), "MI", state_abv)) %>%
    mutate(state_full = ifelse(grepl("MI:", airport_name), "Michigan", state_full)) %>%
    mutate(state_abv = ifelse(grepl("MN:", airport_name), "MN", state_abv)) %>%
    mutate(state_full = ifelse(grepl("MN:", airport_name), "Minnesota", state_full)) %>%
    mutate(state_abv = ifelse(grepl("MS:", airport_name), "MS", state_abv)) %>%
    mutate(state_full = ifelse(grepl("MS:", airport_name), "Mississippi", state_full)) %>%
    mutate(state_abv = ifelse(grepl("MO:", airport_name), "MO", state_abv)) %>%
    mutate(state_full = ifelse(grepl("MO:", airport_name), "Missouri", state_full)) %>%
    mutate(state_abv = ifelse(grepl("MT:", airport_name), "MT", state_abv)) %>%
    mutate(state_full = ifelse(grepl("MT:", airport_name), "Montana", state_full)) %>%
    mutate(state_abv = ifelse(grepl("NE:", airport_name), "NE", state_abv)) %>%
    mutate(state_full = ifelse(grepl("NE:", airport_name), "Nebraska", state_full)) %>%
    mutate(state_abv = ifelse(grepl("NV:", airport_name), "NV", state_abv)) %>%
    mutate(state_full = ifelse(grepl("NV:", airport_name), "Nevada", state_full)) %>%
    mutate(state_abv = ifelse(grepl("NH:", airport_name), "NH", state_abv)) %>%
    mutate(state_full = ifelse(grepl("NH:", airport_name), "New Hampshire", state_full)) %>%
    mutate(state_abv = ifelse(grepl("NJ:", airport_name), "NJ", state_abv)) %>%
    mutate(state_full = ifelse(grepl("NJ:", airport_name), "New Jersey", state_full)) %>%
    mutate(state_abv = ifelse(grepl("NM:", airport_name), "NM", state_abv)) %>%
    mutate(state_full = ifelse(grepl("NM:", airport_name), "New Mexico", state_full)) %>%
    mutate(state_abv = ifelse(grepl("NY:", airport_name), "NY", state_abv)) %>%
    mutate(state_full = ifelse(grepl("NY:", airport_name), "New York", state_full)) %>%
    mutate(state_abv = ifelse(grepl("NC:", airport_name), "NC", state_abv)) %>%
    mutate(state_full = ifelse(grepl("NC:", airport_name), "North Carolina", state_full)) %>%
    mutate(state_abv = ifelse(grepl("ND:", airport_name), "ND", state_abv)) %>%
    mutate(state_full = ifelse(grepl("ND:", airport_name), "North Dakota", state_full)) %>%
    mutate(state_abv = ifelse(grepl("OH:", airport_name), "OH", state_abv)) %>%
    mutate(state_full = ifelse(grepl("OH:", airport_name), "Ohio", state_full)) %>%
    mutate(state_abv = ifelse(grepl("OK:", airport_name), "OK", state_abv)) %>%
    mutate(state_full = ifelse(grepl("OK:", airport_name), "Oklahoma", state_full)) %>%
    mutate(state_abv = ifelse(grepl("OR:", airport_name), "OR", state_abv)) %>%
    mutate(state_full = ifelse(grepl("OR:", airport_name), "Oregon", state_full)) %>%
    mutate(state_abv = ifelse(grepl("PA:", airport_name), "PA", state_abv)) %>%
    mutate(state_full = ifelse(grepl("PA:", airport_name), "Pennsylvania", state_full)) %>%
    mutate(state_abv = ifelse(grepl("PR:", airport_name), "PR", state_abv)) %>%
    mutate(state_full = ifelse(grepl("PR:", airport_name), "Puerto Rico", state_full)) %>%
    mutate(state_abv = ifelse(grepl("RI:", airport_name), "RI", state_abv)) %>%
    mutate(state_full = ifelse(grepl("RI:", airport_name), "Rhode Island", state_full)) %>%
    mutate(state_abv = ifelse(grepl("SC:", airport_name), "SC", state_abv)) %>%
    mutate(state_full = ifelse(grepl("SC:", airport_name), "South Carolina", state_full)) %>%
    mutate(state_abv = ifelse(grepl("SD:", airport_name), "SD", state_abv)) %>%
    mutate(state_full = ifelse(grepl("SD:", airport_name), "South Dakota", state_full)) %>%
    mutate(state_abv = ifelse(grepl("TN:", airport_name), "TN", state_abv)) %>%
    mutate(state_full = ifelse(grepl("TN:", airport_name), "Tennessee", state_full)) %>%
    mutate(state_abv = ifelse(grepl("TX:", airport_name), "TX", state_abv)) %>%
    mutate(state_full = ifelse(grepl("TX:", airport_name), "Texas", state_full)) %>%
    mutate(state_abv = ifelse(grepl("UT:", airport_name), "UT", state_abv)) %>%
    mutate(state_full = ifelse(grepl("UT:", airport_name), "Utah", state_full)) %>%
    mutate(state_abv = ifelse(grepl("VT:", airport_name), "VT", state_abv)) %>%
    mutate(state_full = ifelse(grepl("VT:", airport_name), "Vermont", state_full)) %>%
    mutate(state_abv = ifelse(grepl("VA:", airport_name), "VA", state_abv)) %>%
    mutate(state_full = ifelse(grepl("VA:", airport_name), "Virginia", state_full)) %>%
    mutate(state_abv = ifelse(grepl("VI:", airport_name), "VI", state_abv)) %>%
    mutate(state_full = ifelse(grepl("VI:", airport_name), "Virgin Islands", state_full)) %>%
    mutate(state_abv = ifelse(grepl("WA:", airport_name), "WA", state_abv)) %>%
    mutate(state_full = ifelse(grepl("WA:", airport_name), "Washington", state_full)) %>%
    mutate(state_abv = ifelse(grepl("WV:", airport_name), "WV", state_abv)) %>%
    mutate(state_full = ifelse(grepl("WV:", airport_name), "West Virginia", state_full)) %>%
    mutate(state_abv = ifelse(grepl("WI:", airport_name), "WI", state_abv)) %>%
    mutate(state_full = ifelse(grepl("WI:", airport_name), "Wisconsin", state_full)) %>%
    mutate(state_abv = ifelse(grepl("WY:", airport_name), "WY", state_abv)) %>%
    mutate(state_full = ifelse(grepl("WY:", airport_name), "Wyoming", state_full)) %>%
    mutate(state_abv = ifelse(grepl("TT:", airport_name), "TT", state_abv)) %>%
    mutate(state_full = ifelse(grepl("TT:", airport_name), "Trust Territory of the Pacific Islands", state_full)) %>%
    relocate(state_abv, .after = region) %>%
    relocate(state_full, .after = state_abv)

# for prediction 
# obtain number of observations
dat1_rowsize = dat1 %>% nrow()

# append new column ranging from 1 to dat1_rowsize
dat1_fit = dat1 %>% 
    add_column(count = c(1:dat1_rowsize)) %>%
    relocate(count, .before = year_month)


#-------- CREATE DELAY CAUSES PLOT ---------

# find the count for each mode of delay for each airport, for a specific year and month
# arrange by airport names
# get rid of airlines
# create full airport name
# create year/month column
# sort year/month column by month factors
# make into "long" data frame for plotting
# calculate percentage for a specific delay cause out of all delays for that specific airport for a given year and month
dat2 = delay_States %>% 
    group_by(year, month, airport) %>%
    mutate(delayed_count = sum(arr_del15, arr_cancelled, arr_diverted, na.rm = TRUE)) %>%
    mutate(carrier_count = sum(carrier_ct, na.rm = TRUE)) %>%
    mutate(weather_count = sum(weather_ct, na.rm = TRUE)) %>%
    mutate(nas_count = sum(nas_ct, na.rm = TRUE)) %>%
    mutate(security_count = sum(security_ct, na.rm = TRUE)) %>%
    mutate(late_aircraft_count = sum(late_aircraft_ct, na.rm = TRUE)) %>%
    mutate(canceled_count = sum(arr_cancelled, na.rm = TRUE)) %>%
    mutate(diverted_count = sum(arr_diverted, na.rm = TRUE)) %>%
    mutate(arrival_count = sum(arr_flights, na.rm = TRUE)) %>%
    arrange(year, month, airport) %>%
    distinct(year, month, airport, .keep_all = TRUE) %>%
    select(year, month, region, state_abv, state_full, airport, airport_name, 
           arrival_count, delayed_count, carrier_count, weather_count, 
           nas_count, security_count, late_aircraft_count, 
           canceled_count, diverted_count) %>%
    unite(airport, airport_name, 
          col = "full_airport_name", 
          sep = " | ", 
          remove = FALSE) %>%
    ungroup() %>%
    select(-airport, -airport_name) %>%
    mutate(month = month.abb[month]) %>%
    unite(year, month, col = "year_month", sep="/", remove = FALSE) %>%
    mutate(month = factor(month,levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))) %>%
    pivot_longer(cols = carrier_count:diverted_count, 
                 names_to = "delay_types", 
                 values_to = "count") %>%
    mutate(count_percent = ifelse(delayed_count == 0 & count == 0, 0, count/delayed_count)) %>%
    mutate(count_percent = round(count_percent * 100, digits = 2))

# rename delay types for pie chart
dat2_pie = dat2 %>%
    mutate(delay_types = replace(delay_types, delay_types == "carrier_count", "Carrier")) %>%
    mutate(delay_types = replace(delay_types, delay_types == "weather_count", "Weather")) %>%
    mutate(delay_types = replace(delay_types, delay_types == "nas_count", "NAS")) %>%
    mutate(delay_types = replace(delay_types, delay_types == "security_count", "Security")) %>%
    mutate(delay_types = replace(delay_types, delay_types == "late_aircraft_count", "Late Aircraft")) %>%
    mutate(delay_types = replace(delay_types, delay_types == "canceled_count", "Canceled")) %>%
    mutate(delay_types = replace(delay_types, delay_types == "diverted_count", "Diverted"))

# create character list for select inputs
list_of_yearmonths = dat2 %>% distinct(year_month) %>% pull(year_month)
list_of_yearmonths = rev(list_of_yearmonths)    # display latest first
list_of_airports = dat2 %>% distinct(full_airport_name) %>% pull(full_airport_name)
list_of_states = delay_States %>% distinct(state_full) %>% arrange(state_full) %>% pull(state_full)

# wrangle for scatter plot
# delay percent is the percentage of delayed flights out of all arrival flights
dat3 = dat2 %>%
    rename(delay_type_percent = count_percent) %>%
    rename(delay_type_count = count) %>%
    mutate(delay_percent = delayed_count / arrival_count) %>%
    relocate(delay_percent, .after = delayed_count)

# wrangle for boxplot
# preserve regions, states, carriers, airports
# delay stats grouped by carriers
dat4 = delay_States %>% 
    group_by(year, month, carrier) %>%
    mutate(delayed_count = sum(arr_del15, arr_cancelled, arr_diverted, na.rm = TRUE)) %>%
    mutate(Carrier = sum(carrier_ct, na.rm = TRUE)) %>%
    mutate(Weather = sum(weather_ct, na.rm = TRUE)) %>%
    mutate(NAS = sum(nas_ct, na.rm = TRUE)) %>%
    mutate(Security = sum(security_ct, na.rm = TRUE)) %>%
    mutate(`Late Aircraft` = sum(late_aircraft_ct, na.rm = TRUE)) %>%
    mutate(Canceled = sum(arr_cancelled, na.rm = TRUE)) %>%
    mutate(Diverted = sum(arr_diverted, na.rm = TRUE)) %>%
    mutate(arrival_count = sum(arr_flights, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(year, month, carrier) %>%
    select(year, month, state_abv, state_full, carrier, 
           carrier_name, airport, airport_name, 
           arrival_count, delayed_count, Carrier, Weather, 
           NAS, Security, `Late Aircraft`, 
           Canceled, Diverted) %>%
    # merge airport names together
    unite(airport, airport_name,
          col = "full_airport_name",
          sep = " | ",
          remove = TRUE) %>%
    # merge carrier names together
    unite(`carrier`, carrier_name,
          col = "full_carrier_name",
          sep = " | ",
          remove = FALSE) %>%
    # wrangle year/month
    mutate(month = month.abb[month]) %>%
    unite(year, month, col = "year_month", sep="/", remove = FALSE) %>%
    mutate(month = factor(month,levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))) %>%
    # turn wide data to long data
    pivot_longer(cols = Carrier:Diverted,
                 names_to = "delay_types",
                 values_to = "count") %>%
    # percentage for each delay type
    mutate(count_percent = ifelse(delayed_count == 0 & count == 0, 0, count/delayed_count)) %>%
    # percentage of delays out of all arrival flights
    mutate(delay_percent = ifelse(delayed_count == 0 & arrival_count == 0, 0, delayed_count/arrival_count)) %>%
    relocate(delay_percent, .after = delayed_count) %>%
    # convert to factors for box plots
    mutate(`carrier` = factor(`carrier`))

# obtain list of carriers
list_of_carriers = dat4 %>% distinct(full_carrier_name) %>% pull(full_carrier_name)

#----------------- CREATE MAP ---------------------

# import airport location csv
airdat = read_csv("https://raw.githubusercontent.com/ytai9109/AirDashboard/main/airport_locdata.csv")

# add latitude and longitude data to our list of airports
# group arrival, delay, delay percent by year
dat5 = delaydat %>%
    group_by(year, airport) %>%
    mutate(arrival_count = sum(arr_flights, na.rm = TRUE)) %>%
    mutate(delayed_count = sum(arr_del15, arr_cancelled, arr_diverted, na.rm = TRUE)) %>%
    mutate(delay_percent = delayed_count / arrival_count) %>%
    distinct(year, airport, .keep_all = TRUE) %>%
    select(year, airport, arrival_count, delayed_count, delay_percent) %>%
    # for each iata_code, join the lat and long into the data frame
    left_join(airdat, by = c("airport" = "iata_code")) %>%
    select(year, airport, name, latitude_deg, longitude_deg, arrival_count, delayed_count, delay_percent)


#####################################################
######################--UI--#########################
# Define ShinyDashboard layout
ui <- dashboardPage(
    
    ##############--HEADER--################
    dashboardHeader(title = "US Flight Delay Dashboard",
                    dropdownMenu(type = "messages",
                                 messageItem(
                                     from = "Creator: ",
                                     message = "Yang Tai",
                                     icon = icon("user-circle-o")
                                 ),
                                 messageItem(
                                     from = "Last Edited",
                                     message = "May 8th 2021",
                                     icon = icon("pencil")
                                 )
                    )
                    ),
    
    
    ##############--SIDEBAR--################
    dashboardSidebar(
        sidebarMenu(
            menuItem(text = "Total Delayed flights", 
                     tabName = "delayedFlights", 
                     icon = icon("chart-bar")
                     ),
            
            menuItem(text = "Delays by Airport", 
                     icon = icon("pie-chart"),
                     startExpanded = TRUE,
                     menuSubItem("Bar Chart", tabName = "delayCausesAirport_bar"),
                     menuSubItem("Pie Chart", tabName = "delayCausesAirport_pie")
                     ),
            
            menuItem(text = "Delays by Type",
                     tabName = "delayScatter",
                     icon = icon("bar-chart-o")
                     ),
            
            menuItem(text = "Delays by Airline",
                     tabName = "delayBoxplot",
                     icon = icon("bar-chart-o")
                     ),
            
            menuItem(text = "Map of Airports",
                     tabName = "map",
                     icon = icon("map-o")
                     ),
            
            menuItem(text = "Raw Data",
                     tabName = "rawData",
                     icon = icon("database"))
            
        )
        
        
    ),
    
    ##############--BODY--###############
    dashboardBody(
        tabItems(
            tabItem(tabName = "delayedFlights",
                    fluidRow(
                        box(width = 12,
                            collapsible = TRUE,
                            collapsed = FALSE,
                            column(width = 3,
                                   radioButtons(inputId = "p1GraphTypeSelect", 
                                                label = "Select Plot Type",
                                                choices = list("Line Plot" = "line", "Bar Plot" = "bar"), 
                                                selected = "line")
                                   ),
                            column(width = 3,
                                   selectizeInput(inputId = "p1YearSelect", 
                                                  label = "Select Year", 
                                                  choices = c(2003:2021), 
                                                  multiple = TRUE,
                                                  selected = c(2019, 2020),
                                                  options = list(placeholder = 'select a year'))
                                   ),
                            column(width = 3, 
                                   selectInput(inputId = "p1DataSelect", 
                                               label = "Select Data to Display", 
                                               choices = list("Arrival Count" = "arrival_count", 
                                                              "Delay Count" = "delay_count", 
                                                              "Percentage Delayed" = "percent_delay"),
                                               selected = "delay_count")
                                   ),
                            column(width = 3, 
                                   radioButtons(inputId = "p1DataManip",
                                                label = HTML("Augment Data?<br>Ensure \"Arrival Counts\" or \"Delay Counts\" is selected"),
                                                choices = list("Original" = "original",
                                                               "Natural Log" = "ln",
                                                               "Demean" = "demean"),
                                                selected = "original")
                                   )
                            )
                    ),
                    
                    fluidRow(
                        box(width = 12,
                            height = "600px",
                            plotlyOutput(outputId = "plot1", width = "100%", height = "580px")
                        )
                    ),
                    
                    fluidRow(
                        box(width = 12,
                            status = "warning",
                            title = HTML("<b>Add Regression</b>"),
                            collapsible = TRUE,
                            collapsed = TRUE,
                            column(width = 6,
                                   HTML("<span style=\"font-size:18px; font-weight:bold\">When using regression features, it is best to select years that are sequential. For example, selecting years 2018, 2019, 2020 will yield relevant regressions compared to selecting years 2004, 2007, 2018.</span>")
                                   ),
                            column(width = 6,
                                   radioButtons(inputId = "p1RegType", 
                                                label = "Select Regression Type",
                                                choices = list("None" = "none",
                                                               "Linear" = "linear",
                                                               "Polynomial" = "poly",
                                                               "Local (LOESS)" = "loess"), 
                                                selected = "none"),
                                   sliderInput(inputId = "p1FitPolyDegree", 
                                               label = HTML("Select Polynomial Fit Degree<br>Works only when \"Polynomial\" selected"), 
                                               min = 2,
                                               max = 4,
                                               step = 1,
                                               value = 3,
                                               ticks = FALSE),
                                   sliderInput(inputId = "spanSlider", 
                                               label = HTML("Span<br>Works only when \"LOESS\" selected<br>Smaller Values produce wigglier fits"), 
                                               min = 0.1, 
                                               max = 1,
                                               step = 0.025,
                                               value = 0.4)
                                   )
                            ),
                    ),
                    
                    fluidRow(
                        box(width = 12,
                            status = "warning",
                            title = HTML("<b>Predicting Data</b>"),
                            collapsible = TRUE,
                            collapsed = TRUE,
                            column(width = 6,
                                   HTML(paste0("<span style=\"font-size:16px;\"><b>Instructions for Predicting Data:</b><br>",
                                               "1. Select Years in the \"Select Years\" above the plot. It is best to select years that appear in a sequence, e.g. 2018, 2019, 2020.<br>",
                                               "2. Selct a \"Prediction Method\".<br>",
                                               "3. \"No. of Months Ahead to Predict\" starts from the final month in the selected year. e.g. If the final selected year is 2020, the first predicted month will be 2021/Jan.<br>",
                                               "** It is up to the user to interpret whether predicted values are valid. Negative values imply that predicted values do not hold any meaning.<br></span>")),
                                   radioButtons(inputId = "p1PredictMethod", 
                                                label = HTML("<span style=\"font-size:16px;\">Prediction Method</span>"),
                                                choices = list("Linear Regression" = "linear", 
                                                               "Polynomial Regression" = "poly"), 
                                                selected = "linear"),
                                   sliderInput(inputId = "p1PolyDegree",
                                               label = HTML("<span style=\"font-size:16px;\">Polynomial Degree<br>Ensure \"Polynomial Regression\" is selected</span>"),
                                               min = 2,
                                               max = 4,
                                               step = 1,
                                               value = 3,
                                               ticks = FALSE),
                                   numericInput(inputId = "p1PredictMonths", 
                                                label = HTML("<span style=\"font-size:16px;\">No. of Months Ahead to Predict (max 12 months)</span>"),
                                                min = 1,
                                                max = 12,
                                                step = 1,
                                                value = 6),
                                   htmlOutput(outputId = "p1Eqn"),
                                   HTML("<br>"),
                                   HTML("<span style=\"font-size:20px;font-weight:bold;\">Fit Summary:</span>"),
                                   verbatimTextOutput(outputId = "p1FitSummary")
                                   ),
                            column(width = 6,
                                   HTML("<span style=\"font-size:24px; font-weight:bold;\">Predicted Values</span>"),
                                   tableOutput(outputId = "p1PredictTable")
                                   )
                            )
                    )
            ),
            
            tabItem(tabName = "delayCausesAirport_bar",
                    fluidRow(
                        box(width = 12,
                            column(width = 4,
                                   # use selectize.js here
                                   selectInput(inputId = "p2AirportSelect",
                                               label = "Select an Airport",
                                               choices = list_of_airports,
                                               selected = "LAX | Los Angeles, CA: Los Angeles International", 
                                               selectize = TRUE)
                                   ),
                            column(width = 4,
                                   # use selectize.js here
                                   selectInput(inputId = "p2TimeSelect",
                                               label = "Select Month(s) to Display",
                                               choices = list_of_yearmonths,
                                               selected = "2020/Dec",
                                               selectize = TRUE,
                                               multiple = TRUE)
                                   ),
                            column(width = 4,
                                   selectInput(inputId = "p2DataSelect", 
                                               label = "Select Data to Display", 
                                               choices = list("By Count" = "count", 
                                                              "By Percentage" = "percentage"),
                                               selected = "count")
                                   )
                            )
                    ),
                    
                    fluidRow(
                        box(plotlyOutput(outputId = "plot2", width = "100%", height = "680px"), 
                            width = 12,
                            height = "700px"
                            )
                    )
            ),
            
            tabItem(tabName = "delayCausesAirport_pie",
                    fluidRow(
                        box(width = 12,
                            column(width = 4,
                                   #use selectize.js here
                                   selectInput(inputId = "p3AirportSelect",
                                               label = "Select an Airport",
                                               choices = list_of_airports,
                                               selected = "LAX | Los Angeles, CA: Los Angeles International", 
                                               selectize = TRUE)
                                   ),
                            column(width = 4,
                                   #use selectize.js here
                                   selectInput(inputId = "p3TimeSelect",
                                               label = "Select Month(s) to Display",
                                               choices = list_of_yearmonths,
                                               selectize = TRUE)
                                   ),
                            column(width = 4
                                   
                                   )
                            )
                    ),
                    
                    fluidRow(
                        box(width = 12,
                            height = "700px",
                            plotlyOutput(outputId = "plot3", width = "100%", height = "680px")
                            )
                    )
            ),
            
            tabItem(tabName = "delayScatter",
                    fluidRow(
                        box(width = 12,
                            column(width = 4,
                                   #use selectize.js here
                                   selectInput(inputId = "p4TimeSelect",
                                               label = "Select Month(s) to Display",
                                               choices = list_of_yearmonths,
                                               selectize = TRUE)
                                   ),
                            column(width = 4,
                                   selectInput(inputId = "p4DelayTypeSelect",
                                               label = "Select Delay Cause",
                                               choices = list("Carrier" = "carrier_count", 
                                                              "Weather" = "weather_count", 
                                                              "National Aviaton System (NAS)" = "nas_count",
                                                              "Security" = "security_count",
                                                              "Late Arrival Aircraft" = "late_aircraft_count",
                                                              "Canceled" = "canceled_count",
                                                              "Diverted" = "diverted_count"),
                                               selectize = TRUE)
                                   ),
                            column(width = 4
                                   
                                   )
                            )
                    ),
                    
                    fluidRow(
                        box(width = 12,
                            height = "700px",
                            plotlyOutput(outputId = "plot4", width = "100%", height = "680px")
                            )
                    )
            ),
            
            tabItem(tabName = "delayBoxplot",
                    fluidRow(
                        box(width = 12,
                            column(width = 6,
                                   selectInput(inputId = "p5AirlineSelect",
                                               label = "Select Airlines",
                                               width = "100%",
                                               choices = list_of_carriers,
                                               multiple = TRUE,
                                               selectize = TRUE,
                                               selected = c("AA | American Airlines Inc.",
                                                            "CO | Continental Air Lines Inc.",
                                                            "F9 | Frontier Airlines Inc.",
                                                            "MQ | American Eagle Airlines Inc.",
                                                            "MQ | Envoy Air",
                                                            "9E | Endeavor Air Inc.")
                                               )
                                   ),
                            column(width = 6,
                                   radioButtons(inputId = "p5DotEnable", 
                                                label = "Visualize Indiv. Data Points?",
                                                choices = list("Disable" = "disable", "Enable" = "enable"), 
                                                selected = "disable")
                                   )
                            )
                    ),
                    
                    fluidRow(
                        box(width = 12,
                            height = "700px",
                            plotlyOutput(outputId = "plot5", width = "100%", height = "680px"))
                    ),
            ),
            
            tabItem(tabName = "map",
                    fluidRow(
                        box(width = 12,
                            column(width = 4,
                                   selectInput(inputId = "mapYearSelect",
                                               label = "Select a year",
                                               choices = c(2004:2020),
                                               selectize = TRUE)
                            ),
                            column(width = 4),
                            column(width = 4)
                            
                        )
                    ),
                    fluidRow(
                        box(width = 12,
                            height = "700px",
                            leafletOutput(outputId = "USMap", height = "680px")
                        )
                    )
            ),
            
            tabItem(tabName = "rawData",
                    fluidRow(
                        box(width = 12,
                            column(width = 4,
                                   selectInput(inputId = "rawDataSelect",
                                               label = "Select Data to Display",
                                               choices = list("Master Data" = "master",
                                                              "Total Delayed Flights" = "tab1",
                                                              "Delays by Airport - Bar" = "tab2bar",
                                                              "Delays by Airport - Pie"  = "tab2pie",
                                                              "Delays by Type" = "tab3",
                                                              "Delays by Airline" = "tab4",
                                                              "Map" = "map"
                                                              ),
                                               selectize = TRUE)
                                   ),
                            column(width = 4),
                            column(width = 4)
                            )
                    ),
                    fluidRow(
                        box(width = 12, 
                            height = 800,
                            dataTableOutput(outputId = "delayRawData")
                            )
                    )
            )
        )
    )
)



#######################--SERVER--#########################

server <- function(input, output) {
    
    ############## REACTIVE ELEMENTS ################
    
    ########### TAB 1 ###########
    p1_selectedGraphType = reactive({
        if (input$p1GraphTypeSelect == "line") {
            geom_line(aes(group = 1))
        }
        else if (input$p1GraphTypeSelect == "bar") {
            geom_bar(stat = "identity", fill = "steelblue")
        }
    })
    
    p1_selectedGraph_addpoint = reactive({
        if (input$p1GraphTypeSelect == "line") {
            geom_point()
        }
        else if (input$p1GraphTypeSelect == "bar") {
            
        }
    })
    
    p1_selectedYData = reactive({
        if (input$p1DataSelect == "arrival_count") {
            if(input$p1DataManip == "original") {
                "total_arrival_flights"
            }
            else if (input$p1DataManip == "ln") {
                "ln_total_arrival_flights"
            }
            else if (input$p1DataManip == "demean") {
                "demean_total_arrival_flights"
            }
        }
        else if (input$p1DataSelect == "delay_count") {
            if(input$p1DataManip == "original") {
                "total_delayed_flights"
            }
            else if (input$p1DataManip == "ln") {
                "ln_total_delayed_flights"
            }
            else if (input$p1DataManip == "demean") {
                "demean_total_delayed_flights"
            }
        }
        else if (input$p1DataSelect == "percent_delay") {
            "percent_delayed"
        }
    })
    
    p1_ylabel = reactive({
        if (input$p1DataSelect == "arrival_count") {
            if(input$p1DataManip == "original") {
                "No. of Arrival Flights"
            }
            else if (input$p1DataManip == "ln") {
                "No. of Arrival Flights (natural log)"
            }
            else if (input$p1DataManip == "demean") {
                "No. of Arrival Flights (demeaned)"
            }
        }
        else if (input$p1DataSelect == "delay_count") {
            if(input$p1DataManip == "original") {
                "No. of Delayed Flights"
            }
            else if (input$p1DataManip == "ln") {
                "No. of Delayed Flights (natural log)"
            }
            else if (input$p1DataManip == "demean") {
                "No. of Delayed Flights (demeaned)"
            }
        }
        else if (input$p1DataSelect == "percent_delay") {
            "Percent Delayed"
        }
    })
    
    p1_RegSelect = reactive({
        if (input$p1RegType == "none") {
            
        }
        else if (input$p1RegType == "linear") {
            geom_smooth(aes(group = 1), method = "lm", fill = NA)
        }
        else if (input$p1RegType == "poly") {
            geom_smooth(aes(group = 1), method = "lm", formula = y ~ poly(x, input$p1FitPolyDegree), fill = NA)
        }
        else if (input$p1RegType == "loess") {
            geom_smooth(aes(group = 1), method = "loess", span = input$spanSlider, fill = NA)
        }
    })
    
    p1_PredictData = reactive({
        if (input$p1PredictMethod == "linear") {
            if (input$p1DataSelect == "arrival_count" & input$p1DataManip == "original") {
                lm(as.formula(total_arrival_flights ~ count), data = dat1_fit %>% filter(year %in% input$p1YearSelect))
            }
            else if (input$p1DataSelect == "delay_count" & input$p1DataManip == "original") {
                lm(as.formula(total_delayed_flights ~ count), data = dat1_fit %>% filter(year %in% input$p1YearSelect))
            }
            else if (input$p1DataSelect == "percent_delay" & input$p1DataManip == "original") {
                lm(as.formula(percent_delayed ~ count), data = dat1_fit %>% filter(year %in% input$p1YearSelect))
            }
        }
        else if (input$p1PredictMethod == "poly") {
            if (input$p1PolyDegree == 2) {
                if (input$p1DataSelect == "arrival_count" & input$p1DataManip == "original") {
                    lm(total_arrival_flights ~ poly(count, 2, raw=TRUE), data = dat1_fit %>% filter(year %in% input$p1YearSelect))
                }
                else if (input$p1DataSelect == "delay_count" & input$p1DataManip == "original") {
                    lm(total_delayed_flights ~ poly(count, 2, raw=TRUE), data = dat1_fit %>% filter(year %in% input$p1YearSelect))
                }
                else if (input$p1DataSelect == "percent_delay" & input$p1DataManip == "original") {
                    lm(percent_delayed ~ poly(count, 2, raw=TRUE), data = dat1_fit %>% filter(year %in% input$p1YearSelect))
                }
            }
            else if (input$p1PolyDegree == 3) {
                if (input$p1DataSelect == "arrival_count" & input$p1DataManip == "original") {
                    lm(total_arrival_flights ~ poly(count, 3, raw=TRUE), data = dat1_fit %>% filter(year %in% input$p1YearSelect))
                }
                else if (input$p1DataSelect == "delay_count" & input$p1DataManip == "original") {
                    lm(total_delayed_flights ~ poly(count, 3, raw=TRUE), data = dat1_fit %>% filter(year %in% input$p1YearSelect))
                }
                else if (input$p1DataSelect == "percent_delay" & input$p1DataManip == "original") {
                    lm(percent_delayed ~ poly(count, 3, raw=TRUE), data = dat1_fit %>% filter(year %in% input$p1YearSelect))
                }
            }
            else if (input$p1PolyDegree == 4) {
                if (input$p1DataSelect == "arrival_count" & input$p1DataManip == "original") {
                    lm(total_arrival_flights ~ poly(count, 4, raw=TRUE), data = dat1_fit %>% filter(year %in% input$p1YearSelect))
                }
                else if (input$p1DataSelect == "delay_count" & input$p1DataManip == "original") {
                    lm(total_delayed_flights ~ poly(count, 4, raw=TRUE), data = dat1_fit %>% filter(year %in% input$p1YearSelect))
                }
                else if (input$p1DataSelect == "percent_delay" & input$p1DataManip == "original") {
                    lm(percent_delayed ~ poly(count, 4, raw=TRUE), data = dat1_fit %>% filter(year %in% input$p1YearSelect))
                }
            }
         }
    })
    
    p1_PredictedValueLabel = reactive({
        if (input$p1DataSelect == "arrival_count" & input$p1DataManip == "original") {
            "Predicted Arrival Count"
        }
        else if (input$p1DataSelect == "delay_count" & input$p1DataManip == "original") {
            "Predicted Delay Count"
        }
        else if (input$p1DataSelect == "percent_delay" & input$p1DataManip == "original") {
            "Predicted Delay Percent"
        }
    })
    
    p1_PredictedEquation = reactive({
        if (input$p1PredictMethod == "linear") {
            HTML(paste0("<span style=\"font-size:20px;font-weight:bold;\">Equation: <br>y = ", round(coef(p1_PredictData())[2], digits = 6), "x + ", round(coef(p1_PredictData())[1], digits = 6), "</span>"))
        }
        else if (input$p1PredictMethod == "poly") {
            if (input$p1PolyDegree == 2) {
                HTML(paste0("<span style=\"font-size:20px;font-weight:bold;\">Equation: <br>y = ", round(coef(p1_PredictData())[3], digits = 4), "x<sup>2</sup>+", round(coef(p1_PredictData())[2], digits = 4), "x+", round(coef(p1_PredictData())[2], digits = 4), "</span>"))
            }
            else if (input$p1PolyDegree == 3) {
                HTML(paste0("<span style=\"font-size:20px;font-weight:bold;\">Equation: <br>y = ", round(coef(p1_PredictData())[4], digits = 4), "x<sup>3</sup>+", round(coef(p1_PredictData())[3], digits = 4), "x<sup>2</sup>+", round(coef(p1_PredictData())[2], digits = 4), "x+", round(coef(p1_PredictData())[1], digits = 4), "</span>"))
            }
            else if (input$p1PolyDegree == 4) {
                HTML(paste0("<span style=\"font-size:20px;font-weight:bold;\">Equation: <br>y = ", round(coef(p1_PredictData())[5], digits = 4), "x<sup>4</sup>+", round(coef(p1_PredictData())[4], digits = 4), "x<sup>3</sup>+", round(coef(p1_PredictData())[3], digits = 4), "x<sup>2</sup>+", round(coef(p1_PredictData())[2], digits = 4), "x+", round(coef(p1_PredictData())[1], digits = 4), "</span>"))
            }
        }
    })
    
    ########### TAB 2 ###########
    
    p2_selectedYData = reactive({
        if (input$p2DataSelect == "count") {
            "count"
        }
        else if (input$p2DataSelect == "percentage") {
            "count_percent"
        }
    })
    
    p2_ylabel = reactive({
        if (input$p2DataSelect == "count") {
            "Number of Counts"
        }
        else if (input$p2DataSelect == "percentage") {
            "Percentage (%)"
        }
    })
    
    
    ########### TAB 3 ###########
    
    p4_delaylabel = reactive({
        if (input$p4DelayTypeSelect == "carrier_count") {
            "Carrier"
        }
        else if (input$p4DelayTypeSelect == "weather_count") {
            "Weather"
        }
        else if (input$p4DelayTypeSelect == "nas_count") {
            "NAS"
        }
        else if (input$p4DelayTypeSelect == "security_count") {
            "Security"
        }
        else if (input$p4DelayTypeSelect == "late_aircraft_count") {
            "Late Aircraft Arrival"
        }
        else if (input$p4DelayTypeSelect == "canceled_count") {
            "Canceled Flight"
        }
        else if (input$p4DelayTypeSelect == "diverted_count") {
            "Diverted Flight"
        }
    })
    
    ########### TAB 4 ###########
    
    p5_enableDots = reactive({
        if (input$p5DotEnable == "disable") {
            
        }
        else if (input$p5DotEnable == "enable") {
            geom_jitter(aes(text = paste0("<b>Carrier:</b> ", .data[["full_carrier_name"]], "\n<b>Year/Month:</b> ", .data[["year_month"]], "\n<b>Delay Percent:</b> ", .data[["delay_percent"]])),
                        size = 1, 
                        stroke = 0.2, 
                        shape=16, 
                        alpha = 0.7, 
                        position=position_jitter(0.1))
        }
    })
    
    
    ########### TAB 5 ###########
    
    
    ########### TAB 6 ###########
    t6_rawDataSelect = reactive({
        if (input$rawDataSelect == "master") {
            delaydat
        }
        else if (input$rawDataSelect == "tab1") {
            dat1
        }
        else if (input$rawDataSelect == "tab2bar") {
            dat2
        }
        else if (input$rawDataSelect == "tab2pie") {
            dat2_pie
        }
        else if (input$rawDataSelect == "tab3") {
            dat3
        }
        else if (input$rawDataSelect == "tab4") {
            dat4
        }
        else if (input$rawDataSelect == "map") {
            dat5
        }
    })
    
    
    ############### PLOT OUTPUTS ###############
    
    # disables scientific notation
    options(scipen = 999)
    
    ###########################
    # output delay plot by year
    # dynamically change time
    # dynamically change y data --> total arrival flights, total delayed flights, percent delayed
    output$plot1 = renderPlotly({
        g1 = ggplot(data = dat1 %>% filter(year %in% input$p1YearSelect),
                    # seems that .data[["column"]] works the same as aes_string()
                    aes(x = .data[["year_month"]], 
                        y = .data[[p1_selectedYData()]],
                        text = paste0("<b>Year/Month:</b> ", .data[["year_month"]], "\n<b>Total arrival count:</b> ", .data[["total_arrival_flights"]], "\n<b>Total delayed count:</b> ", .data[["total_delayed_flights"]], "\n<b>Percentage delayed:</b> ", round(.data[["percent_delayed"]]*100, digits = 2), "%"))) +
            p1_selectedGraphType() +
            p1_selectedGraph_addpoint() +
            p1_RegSelect() + 
            labs(title = paste(p1_ylabel(), "in", paste(input$p1YearSelect, collapse = ', '), collapse = ' '),
                 x = "Year/Month",
                 y = p1_ylabel()) +
            scale_x_discrete(limits = dat1 %>% filter(year %in% input$p1YearSelect) %>% pull(`year_month`)) +
            theme(axis.text.x = element_text(angle = 60, hjust=1, vjust=0.2)) +
            theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold", margin = margin(t = 0, r = 0, b = 10, l = 0))) +
            theme(axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0), face = "bold")) +
            theme(axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0), face = "bold"))
        
        g1 = ggplotly(g1, tooltip = c("text", "y"))
        
        g1
    })
    
    
    ###########################
    # still TAB1
    # predict data
    
    output$p1PredictTable = renderTable({
        # start fitting
        fit = p1_PredictData()
        
        new_count = data.frame(count = c( (as.numeric(dat1_rowsize)+1) : (as.numeric(dat1_rowsize) + as.numeric(input$p1PredictMonths)) ))
        
        predict_output = as.data.frame(predict(fit, new_count))
        months_ahead = c(1: nrow(new_count))
        predict_output = cbind(months_ahead, predict_output)
        names(predict_output)[1] = "Months Ahead"
        names(predict_output)[2] = p1_PredictedValueLabel()
        
        predict_output
    },
    width = "100%",
    rownames = FALSE,
    colnames = TRUE,
    hover = TRUE,
    bordered = TRUE,
    digits = 6)
    
    output$p1FitSummary = renderPrint({
        summary(p1_PredictData())
    })
    
    output$p1Eqn = renderUI({
        p1_PredictedEquation()
    })
    
    
    ###########################
    # output delay causes by airport - bar edition
    # lays horizontally instead of vertically
    # dynamically select airport
    # dynamically select multiple Year/Month combos
    # dynamically select count or percentage y-data 
    output$plot2 = renderPlotly({
        g2 = ggplot(data = dat2 %>% 
                        filter(full_airport_name == input$p2AirportSelect) %>% 
                        filter(year_month %in% input$p2TimeSelect), 
                    aes(x = .data[["delay_types"]], 
                        y = .data[[p2_selectedYData()]], 
                        fill = .data[["year_month"]],
                        text = paste0("<b>Year/Month:</b> ", .data[["year_month"]], "\n<b>Delay Type:</b> ", .data[["delay_types"]], "\n<b>No. of Delays:</b> ", .data[["count"]], "\n<b>Percent of delays:</b> ", .data[["count_percent"]], "%"))) +
            geom_bar(stat = "identity", position='dodge') + 
            coord_flip() +
            labs(title = paste0("Delay Cause\n", input$p2AirportSelect, "\nDates: ", paste(input$p2TimeSelect, collapse = ', ')),
                x = "Delay Cause",
                y = p2_ylabel()) +
            scale_x_discrete(labels = rev(c("Carrier", "Weather", "NAS", "Security", "Late Aircraft", "Canceled", "Diverted")), 
                             limits = rev(c("carrier_count", "weather_count", "nas_count", "security_count", "late_aircraft_count", "canceled_count", "diverted_count"))) +
            theme(axis.text.x = element_text(hjust=1, vjust=0.2)) +
            scale_fill_discrete(name = "Year/Month") +
            theme(plot.title = element_text(size = 11, hjust = 0.5, face = "bold", margin = margin(t = 0, r = 0, b = 0, l = 0))) +
            theme(axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0), face = "bold")) +
            theme(axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0), face = "bold")) +
            theme(plot.margin = unit(c(1.8,0.5,0.5,0.5), "cm"))
        
        g2 = ggplotly(g2, tooltip = "text")
        
        # because ggplotly doesn't like formatting legends using scale_fill_discrete...
        # have to manually wrangle the plotly object
        for (i in 1:length(g2$x$data)){
            if (!is.null(g2$x$data[[i]]$name)){
                g2$x$data[[i]]$name = dat2 %>% 
                    filter(full_airport_name == input$p2AirportSelect) %>% 
                    filter(year_month %in% input$p2TimeSelect) %>%
                    distinct(year_month) %>% 
                    pull(year_month) %>% 
                    nth(i)
            }
        }
        
        g2
    })
    
    
    ###########################
    #output delay causes, pie chart edition
    # dynamically select airport
    # dynamically select multiple Year/Month combos
    # displays count and percentage data
    # use plot_ly() to build
    output$plot3 = renderPlotly({
        g3 = plot_ly(data = dat2_pie %>% filter(full_airport_name == input$p3AirportSelect) %>% filter(year_month == input$p3TimeSelect), 
                     labels = ~delay_types,
                     values = ~count,
                     type = "pie",
                     textposition = 'outside',
                     automargin = TRUE,
                     texttemplate = "<b>%{label}</b>\n%{percent}",
                     insidetextfont = list(color = '#FFFFFF'),
                     hovertemplate = "<b>%{label}</b>\nCount: %{value}<extra></extra>",
                     marker = list(line = list(color = '#FFFFFF', width = 1.5)),
                     showlegend = TRUE) %>%
            layout(title = list(text = paste0("<b>Delay Cause</b>\n", input$p3AirportSelect, "\nDate: ", input$p3TimeSelect), xanchor = "center"),
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   # default padding is 80px
                   margin = list(l = 80, r = 80, t = 120, b = 20, pad = 0),
                   legend = list(borderwidth = 2, 
                                 itemclick = FALSE, 
                                 itemdoubleclick = FALSE, 
                                 title = list(text = "<b>Delay Types<b>")))
        
        g3
    })
    
    ###########################
    
    output$plot4 = renderPlotly({
        
        g4 = ggplot(data = dat3 %>% filter(year_month == input$p4TimeSelect) %>% filter(delay_types == input$p4DelayTypeSelect), 
                    aes(x = delay_type_percent, y = delay_percent)) + 
            geom_point(aes(color = region, 
                           size = arrival_count,
                           text = paste0(full_airport_name, "\n<b>No. of arrivals:</b> ", arrival_count, "\n<b>No. of delays:</b> ", delayed_count ,"\n<b>Percent of total flights delayed:</b> ", round(delay_percent*100, digits = 2), "%\n<b>No. of ", p4_delaylabel(), " delays:</b> ", delay_type_count ,"\n<b>Percent of ", p4_delaylabel(), " delays:</b> ", round(delay_type_percent*100, digits = 2), "%") )) +
            scale_x_continuous(labels = function(x) paste0(x*100, '%')) +
            scale_y_continuous(labels = function(y) paste0(y*100, '%')) +
            labs(x = paste0("Likelihood ", p4_delaylabel(), " is causing delay"), 
                 y = 'Percentage of total delayed flights',
                 color = 'Region', 
                 size = 'No. of Arrivals',
                 title = paste0('Percentage of delayed flights vs \nlikelihood of ', p4_delaylabel(), ' causing the delay')) + 
            guides(size = FALSE) +
            theme(legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid')) +
            theme(plot.title = element_text(size = 11, hjust = 0.5, face = "bold", margin = margin(t = 0, r = 0, b = 10, l = 0))) +
            theme(axis.title.x = element_text(face = "bold")) +
            theme(axis.title.y = element_text(face = "bold")) +
            theme(plot.margin = unit(c(1,0.5,0.5,0.5), "cm"))
        
        g4 = ggplotly(g4, tooltip = "text")
        
        g4
        
    })
    
    ###########################################
    
    output$plot5 = renderPlotly({
        
        g5 = ggplot(data = dat4 %>% 
                        filter(full_carrier_name %in% input$p5AirlineSelect) %>% 
                        group_by(full_carrier_name) %>% 
                        distinct(year_month, .keep_all = TRUE), 
                    aes(x = str_wrap(full_carrier_name, width = 10), y = delay_percent, fill = full_carrier_name)) + 
            geom_boxplot() +
            p5_enableDots() +
            labs(x = 'Airline Carrier', 
                 y = 'Percentage of delayed flights',
                 title = 'Boxplot of Delay Percentage vs Airline Carrier from Jun/2013 to Jan/2021') +
            theme(plot.title = element_text(size = 11, hjust = 0.5, face = "bold", margin = margin(t = 0, r = 0, b = 10, l = 0))) +
            theme(axis.title.x = element_text(face = "bold")) +
            theme(axis.title.y = element_text(face = "bold")) +
            scale_fill_brewer(palette="Set3") +
            theme(plot.margin = unit(c(1,0.5,0.5,0.5), "cm"))
        
        g5 = ggplotly(g5, tooltip = "text")
        
        g5
        
    })
    
    ###########################################
    # US Map (Leaflet)
    
    output$USMap = renderLeaflet({
        
        dat5_selected = dat5 %>% filter(year %in% input$mapYearSelect)
        
        leaflet(data = dat5_selected) %>%
            setView(lat = 41.869561, lng = -120.035524, zoom = 3) %>%
            addTiles(urlTemplate = "https://tiles.stadiamaps.com/tiles/alidade_smooth_dark/{z}/{x}/{y}{r}.png") %>%
            addCircleMarkers(lng = ~longitude_deg,
                             lat = ~latitude_deg,
                             label = ~name,
                             labelOptions = labelOptions(textsize = "15px"),
                             popup = paste0("<b><span style=\"color:#164CA3; font-size:15px;\">Airport: </span></b>", "<span style=\"font-size: 15px\">", dat5_selected$name, "</span><br>", 
                                            "<b><span style=\"color:#0F630C; font-size:15px;\">No. of Arrivals: </span></b>", "<span style=\"font-size: 15px\">", dat5_selected$arrival_count, "</span><br>",
                                            "<b><span style=\"color:#0F630C; font-size:15px;\">No. of Delays: </span></b>", "<span style=\"font-size: 15px\">", dat5_selected$delayed_count, "</span><br>",
                                            "<b><span style=\"color:#0F630C; font-size:15px;\">Percent of delays: </span></b>", "<span style=\"font-size: 15px\">", round(dat5_selected$delay_percent * 100, digits = 2), "%</span>"
                             ),
                             radius = 1.5,
                             color = "turquoise",
                             fillOpacity = 0)
        
    })
    
    ###########################################
    # delay raw data goes here
    output$delayRawData = renderDataTable(
        t6_rawDataSelect(),
        options = list(pageLength = 7, 
                       autoWidth = TRUE, 
                       scrollX = TRUE, 
                       scrollCollapse = TRUE)
        )
    
    
}

###### DONT TOUCH THIS!!! #######
# Run the application 
shinyApp(ui = ui, server = server)

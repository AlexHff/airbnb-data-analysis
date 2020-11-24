library(shiny)
library(dplyr)
library(stringr)
library(ggplot2)
library(data.table)
library(leaflet)
library(plotly)

# Reading cleansed data
cities <- c("malaga", "mallorca", "sevilla", "florence", "bordeaux", "bordeaux", "bordeaux", "florence", "florence", "mallorca", "mallorca")
data_dates <- c("2020-06-30", "2020-09-19", "2020-06-29", "2020-08-31", "2020-09-19", "2020-08-29", "2020-07-25", "2020-07-23", "2020-06-19", "2020-08-28", "2020-06-25")
features <- c("property_type", "room_type", "accommodates", "bedrooms", "beds", "price", "minimum_nights", "maximum_nights", "availability_30", "price_30", "revenue_30")

# We are only interested in data between min_date and max_date
min_date <- '2020-05-01'
max_date <- '2020-11-01'

files_paths <- c()

# Read data in cities between min_date and max_date
for(city in cities){
    file_dir <- file.path(".", "data/data_cleansed", city)
    file_subdirs <- list.dirs(file_dir)
    file_subdirs <- file_subdirs[-1]
    
    for(file_subdir in file_subdirs){
        if(file_subdir < file.path(file_dir, min_date) | file_subdir > file.path(file_dir, max_date)  )
            file_subdirs = file_subdirs[file_subdirs != file_subdir]
    }
    files_paths <- c(files_paths, file_subdirs)
}
files_paths <- file.path(files_paths, "listings.csv")
listings <- 
    do.call(rbind,
            lapply(files_paths, read.csv, row.names=1))

## Preprocess
listings$price <- as.numeric(gsub("$", "", listings$price, fixed = TRUE))

# Define UI ----
ui <- fluidPage(
  titlePanel("Analysis 1"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "cities",
        label=("Select the cities you would like to compare"),
        choices = c(toupper(unique(cities)))
      ),
      dateRangeInput("dates", label = "Select a date range"),
      selectInput("feature", label = "Select a feature", features),
      selectInput("plot_type", label = "Select a plot type", c("histogram", "boxplot"))
    ),
    mainPanel(
      plotlyOutput("plot")
    ),
  ),
  
  hr(),
  titlePanel("Analysis 2"),
  sidebarLayout(
    sidebarPanel(
      selectInput("city", label = "Select a city", toupper(cities)),
    ),
    mainPanel(
      leafletOutput("map", height = 400, width = "100%"),
    ),
  ),
  
  hr(),
  span(icon("github"), a("View source code on GitHub", href = "https://github.com/alexhff/airbnb-data-analysis"))
)

# Define server logic ----
server <- function(input, output) {
  output$plot <- renderPlotly({
    withProgress(message = "Rendering...", value = 0.5, {
      if (length(input$cities) > 0)
        curListings <- listings[toupper(listings$city) %in% input$cities,]
      else
        curListings <- listings
      curListings <- filter(curListings, data_date >= input$dates[1])
      curListings <- filter(curListings, data_date <= input$dates[2])
      print(dim(curListings))
      if (input$plot_type == "histogram") {
        if (input$feature == "bedrooms") {
          p <- ggplot(curListings, aes(x=bedrooms)) +
            geom_histogram(color="black", fill="white") +
            geom_vline(aes(xintercept=mean(bedrooms)), color="blue", linetype="dashed", size=1)
        } else if (input$feature == "accommodates") {
          p <- ggplot(curListings, aes(x=accommodates)) +
            geom_histogram(color="black", fill="white") +
            geom_vline(aes(xintercept=mean(accommodates)), color="blue", linetype="dashed", size=1)
        } else if (input$feature == "beds") {
          p <- ggplot(curListings, aes(x=beds)) +
            geom_histogram(color="black", fill="white") +
            geom_vline(aes(xintercept=mean(beds)), color="blue", linetype="dashed", size=1)
        } else if (input$feature == "price") {
          p <- ggplot(curListings, aes(x=price)) +
            geom_histogram(color="black", fill="white") +
            geom_vline(aes(xintercept=mean(price)), color="blue", linetype="dashed", size=1)
        } else if (input$feature == "minimum_nights") {
          p <- ggplot(curListings, aes(x=minimum_nights)) +
            geom_histogram(color="black", fill="white") +
            geom_vline(aes(xintercept=mean(minimum_nights)), color="blue", linetype="dashed", size=1)
        } else if (input$feature == "maximum_nights") {
          p <- ggplot(curListings, aes(x=maximum_nights)) +
            geom_histogram(color="black", fill="white") +
            geom_vline(aes(xintercept=mean(maximum_nights)), color="blue", linetype="dashed", size=1)
        } else if (input$feature == "availability_30") {
          p <- ggplot(curListings, aes(x=availability_30)) +
            geom_histogram(color="black", fill="white") +
            geom_vline(aes(xintercept=mean(availability_30)), color="blue", linetype="dashed", size=1)
        } else if (input$feature == "price_30") {
          p <- ggplot(curListings, aes(x=price_30)) +
            geom_histogram(color="black", fill="white") +
            geom_vline(aes(xintercept=mean(price_30)), color="blue", linetype="dashed", size=1)
        } else if (input$feature == "revenue_30") {
          p <- ggplot(curListings, aes(x=revenue_30)) +
            geom_histogram(color="black", fill="white") +
            geom_vline(aes(xintercept=mean(revenue_30)), color="blue", linetype="dashed", size=1)
        } else {
          p <- ggplot()
        }
      } else {
        if (input$feature == "bedrooms") {
          p <- ggplot(curListings, aes(city, bedrooms)) +
            geom_boxplot(aes(colour = "red"), outlier.shape = NA) +
            stat_summary(fun=mean, geom="point") +
            scale_y_continuous(limits = quantile(curListings$bedrooms, c(0.1, 0.9), na.rm = T))
        } else if (input$feature == "price") {
          p <- ggplot(curListings, aes(city, price)) +
            geom_boxplot(aes(colour = "red"), outlier.shape = NA) +
            stat_summary(fun=mean, geom="point") +
            scale_y_continuous(limits = quantile(curListings$price, c(0.1, 0.9), na.rm = T))
        } else if (input$feature == "accommodates") {
          p <- ggplot(curListings, aes(city, accommodates)) +
            geom_boxplot(aes(colour = "red"), outlier.shape = NA) +
            stat_summary(fun=mean, geom="point") +
            scale_y_continuous(limits = quantile(curListings$accommodates, c(0.1, 0.9), na.rm = T))
        } else if (input$feature == "beds") {
          p <- ggplot(curListings, aes(city, beds)) +
            geom_boxplot(aes(colour = "red"), outlier.shape = NA) +
            stat_summary(fun=mean, geom="point") +
            scale_y_continuous(limits = quantile(curListings$beds, c(0.1, 0.9), na.rm = T))
        } else if (input$feature == "minimum_nights") {
          p <- ggplot(curListings, aes(city, minimum_nights)) +
            geom_boxplot(aes(colour = "red"), outlier.shape = NA) +
            stat_summary(fun=mean, geom="point") +
            scale_y_continuous(limits = quantile(curListings$minimum_nights, c(0.1, 0.9), na.rm = T))
        } else if (input$feature == "maximum_nights") {
          p <- ggplot(curListings, aes(city, maximum_nights)) +
            geom_boxplot(aes(colour = "red"), outlier.shape = NA) +
            stat_summary(fun=mean, geom="point") +
            scale_y_continuous(limits = quantile(curListings$maximum_nights, c(0.1, 0.9), na.rm = T))
        } else if (input$feature == "availability_30") {
          p <- ggplot(curListings, aes(city, availability_30)) +
            geom_boxplot(aes(colour = "red"), outlier.shape = NA) +
            stat_summary(fun=mean, geom="point") +
            scale_y_continuous(limits = quantile(curListings$availability_30, c(0.1, 0.9), na.rm = T))
        } else if (input$feature == "price_30") {
          p <- ggplot(curListings, aes(city, price_30)) +
            geom_boxplot(aes(colour = "red"), outlier.shape = NA) +
            stat_summary(fun=mean, geom="point") +
            scale_y_continuous(limits = quantile(curListings$price_30, c(0.1, 0.9), na.rm = T))
        } else if (input$feature == "revenue_30") {
          p <- ggplot(curListings, aes(city, revenue_30)) +
            geom_boxplot(aes(colour = "red"), outlier.shape = NA) +
            stat_summary(fun=mean, geom="point") +
            scale_y_continuous(limits = quantile(curListings$revenue_30, c(0.1, 0.9), na.rm = T))
        } else {
          p <- ggplot()
        }
        setProgress(1)
      }
    })
    ggplotly(p, tooltip = c("text"))
  })
  
  selected_city <- reactive({
    req(input$city)
  })
  
  # Geography concentration goes here
  output$map <- renderLeaflet({
    
    # Create a base map
    selected_city() %>% 
      leaflet(data = listings[toupper(listings$city) %in% input$city,]) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      # customise viewport to fit
      fitBounds(lng1 = min(listings$longitude), 
                lat1 = min(listings$latitude), 
                lng2 = max(listings$longitude), 
                lat2 = max(listings$latitude)) %>% 
      addMarkers(~longitude, ~latitude)
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)

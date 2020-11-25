library(dplyr)
library(stringr)
library(ggplot2)
library(data.table)

# a generic function to prepare data for a specific city, data_date
prepare_data <- function(city, data_date)
{
    # Cleaning listings dataframe
    
    listings_url <- file.path("../app/data/data_raw", city, data_date, "listings.csv.gz")
    calendar_url <- file.path("../app/data/data_raw", city, data_date, "calendar.csv.gz")
    
    print(paste0("reading data from ", listings_url))
    listings <- read.csv(gzfile(listings_url))
    print(paste0("reading data from ", calendar_url))
    calendar <- read.csv(gzfile(calendar_url))
    
    ## Add Keys: columns city and day date
    listings$city <- city
    listings$data_date <- data_date
    
    ## Select interesting columns
    ### Most columns don't contain interesting information
    columns_listings <- c("city", "data_date", "id", "neighbourhood_cleansed", 
                          "latitude", "longitude", 
                          "property_type", "room_type", "accommodates", "bedrooms", 
                          "beds", "price", "minimum_nights",  "maximum_nights")
    
    listings <- listings %>% 
        select(columns_listings) %>% 
        arrange(id)
    
    
    # Cleaning calendar dataframe
    
    ## arrange by id and date
    calendar <- calendar %>% 
        arrange(listing_id, date)
    
    ## add day number (starting first day)
    calendar <- calendar %>%
        group_by(listing_id) %>%
        mutate(day_nb = row_number()) %>%
        ungroup()
    
    ## change available column to binary
    calendar <- calendar %>%
        mutate(available = ifelse(available=="t", 1, 0))
    
    ## clean price column and transform to numeric
    calendar <- calendar %>%
        mutate(price = str_replace(price, "\\$", ""),
               adjusted_price = str_replace(adjusted_price, "\\$", ""))
    calendar <- calendar %>%
        mutate(price = str_replace(price, ",", ""),
               adjusted_price = str_replace(adjusted_price, ",", ""))
    calendar <- calendar %>%
        mutate(price = as.numeric(price),
               adjusted_price = as.numeric(adjusted_price))
    
    ## calculate estimated revenue for upcoming day
    calendar <- calendar %>%
        mutate(revenue = price*(1-available))
    
    ## calculate availability, price, revenue for next 30, 60 days ... for each listing_id
    calendar <- calendar %>%
        group_by(listing_id) %>%
        summarise(availability_30 = sum(available[day_nb<=30], na.rm = TRUE),
                  #availability_60 = sum(available[day_nb<=60], na.rm = TRUE),
                  #availability_90 = sum(available[day_nb<=90], na.rm = TRUE),
                  #availability_365 = sum(available[day_nb<=365], na.rm = TRUE),
                  price_30 = mean(price[day_nb<=30 & available==0], na.rm = TRUE),
                  #price_60 = mean(price[day_nb<=60 & available==0], na.rm = TRUE),
                  #price_90 = mean(price[day_nb<=90 & available==0], na.rm = TRUE),
                  #price_365 = mean(price[day_nb<=365 & available==0], na.rm = TRUE),
                  revenue_30 = sum(revenue[day_nb<=30], na.rm = TRUE),
                  #revenue_60 = sum(revenue[day_nb<=60], na.rm = TRUE),
                  #revenue_90 = sum(revenue[day_nb<=90], na.rm = TRUE),
                  #revenue_365 = sum(revenue[day_nb<=365], na.rm = TRUE)           
        )
    
    listings_cleansed <- listings %>% left_join(calendar, by = c("id" = "listing_id"))
    
    dir.create(file.path("../app/data/data_cleansed", city, data_date), recursive = TRUE)
    
    write.csv(listings_cleansed, file.path("../app/data/data_cleansed", city, data_date, "listings.csv"))
    print(paste0("saving data into ", file.path("../app/data/data_cleansed", city, data_date, "listings.csv")))
    
}


# Prepare data for multiple cities

cities <- c("malaga", "mallorca", "sevilla", "florence", "bordeaux", "bordeaux", "bordeaux", "florence", "florence", "mallorca", "mallorca")
data_dates <- c("2020-06-30", "2020-09-19", "2020-06-29", "2020-08-31", "2020-09-19", "2020-08-29", "2020-07-25", "2020-07-23", "2020-06-19", "2020-08-28", "2020-06-25")

for(i in 1:length(cities)){
    city <- cities[i]
    data_date <- data_dates[i]
    print("-------------------------------------------------")
    print(paste(c("Preparing data for", city, "compiled at", data_date), collapse = " "))
    prepare_data(city, data_date)
}


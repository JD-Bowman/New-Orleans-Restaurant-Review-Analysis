library(tidyverse)
library(jsonlite)
library(lubridate)
library(sf)
library(tigris)
library(scales)
library(viridis)

business_file <- "raw/yelp_academic_dataset_business.json"
review_file <- "raw/yelp_academic_dataset_review.json"

business_raw <- jsonlite::stream_in(
  file(business_file),
  verbose = FALSE
)

business <- business_raw |>
  select(
    business_id, name, address, city, state, postal_code,
    latitude, longitude, stars, review_count, categories
  )

business |>
  filter(state == "LA", city == "New Orleans") |>
  select(categories) |>
  head(10)

new_orleans_restaurants <- business |>
  filter(
    state == "LA",
    city == "New Orleans",
    !is.na(categories),
    str_detect(categories, regex("Restaurants", ignore_case = TRUE))
  ) |>
  mutate(
    postal_code = na_if(postal_code, ""),
    categories = str_squish(categories),
    business_name = str_squish(name)
  ) |>
  distinct(business_id, .keep_all = TRUE)

# Get IDs of New Orleans restaurants
restaurant_ids <- new_orleans_restaurants$business_id

review_chunks <- list()

# Read from review file, 10000 at a time
jsonlite::stream_in(
  file(review_file),
  handler = function(df) {
    
    filtered <- df %>%
      filter(business_id %in% restaurant_ids)
    
    if (nrow(filtered) > 0) {
      review_chunks[[length(review_chunks) + 1]] <<- filtered
    }
    
  },
  pagesize = 10000,
  verbose = TRUE
)

restaurant_reviews <- bind_rows(review_chunks)

restaurant_reviews <- restaurant_reviews |>
  select(business_id, user_id, stars, date, text, useful, funny, cool)

saveRDS(
  new_orleans_restaurants,
  "new_orleans_restaurants.rds"
)

saveRDS(
  restaurant_reviews,
  "restaurant_reviews.rds"
)

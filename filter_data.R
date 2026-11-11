library(tidyverse)
library(jsonlite)
library(lubridate)
library(sf)
library(tigris)
library(scales)
library(viridis)

options(tigris_use_cache = TRUE)

# Look for the amount of each dietary restriction tags
print(new_orleans_restaurants |>
        separate_rows(categories, sep = ",\\s*") |>
        filter(str_detect(categories, regex("Gluten|Vegan|Vegetarian|Halal|Kosher|Dairy|Organic|Raw|Health|Diet", ignore_case = TRUE))) |>
        count(categories, sort = TRUE))


# Define target tags
selected_diet_tags <- c("Vegetarian", "Vegan", "Gluten-Free")
pattern <- paste(selected_diet_tags, collapse = "|")

dietary_restaurants <- new_orleans_restaurants |>
  # 1. Filter for restaurants that have AT LEAST ONE target tag
  filter(str_detect(categories, regex(pattern, ignore_case = TRUE))) |>
  # 2. Add True/False flags for each individual tag
  mutate(
    has_gluten_free = str_detect(categories, regex("Gluten-Free", ignore_case = TRUE)),
    has_vegan       = str_detect(categories, regex("Vegan", ignore_case = TRUE)),
    has_vegetarian  = str_detect(categories, regex("Vegetarian", ignore_case = TRUE))
  )

# Join restaurant_reviews to dietary_restaurants by business_id
reviews_with_restaurant_info <- restaurant_reviews |>
  inner_join(dietary_restaurants, by = "business_id", suffix = c("_review", "_restaurant"))

louisiana_places <- tigris::places(
  state = "LA",
  cb = TRUE,
  year = 2024,
  class = "sf"
)


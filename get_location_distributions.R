# ZCTA:
# Census tract:
# Census block group: Not in comparison. Too small, I only have 129 restaurants
# Neighborhood statistical areas:
# Wards

restaurant_points <- dietary_restaurants |>
  filter(!is.na(longitude), !is.na(latitude)) |>
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )

# Check for mismatch between coordinates and cataloged place names
restaurants_with_place <- restaurant_points |>
  st_join(
    louisiana_places |>
      st_transform(st_crs(restaurant_points)) |>
      select(GEOID, place_name = NAME),
    join = st_within,
    left = TRUE
  )

restaurants_with_place |>
  st_drop_geometry() |>
  count(city, place_name, sort = TRUE)

# One mismatch: City greens, 5161 Citrus Blvd. -90.18689 29.96416

# Download Census Tracts for Orleans Parish (County FIPS 071)
orleans_tracts <- tracts(
  state = "LA",
  county = "Orleans",
  cb = TRUE,
  year = 2024,
  class = "sf"
) |>
  st_transform(st_crs(restaurant_points)) # Ensure CRS matches (EPSG 4326)

# Count across all tracts (including those with 0 dietary restaurants)
all_tract_counts <- orleans_tracts |>
  mutate(n_restaurants = lengths(st_intersects(orleans_tracts, restaurant_points))) |>
  st_drop_geometry() |>
  select(GEOID, tract_name = NAMELSAD, n_restaurants) |>
  arrange(desc(n_restaurants))

# Join Census Tract information onto dietary points
restaurant_points_with_location <- restaurant_points |>
  st_join(
    orleans_tracts |> 
      st_transform(st_crs(restaurant_points)) |>
      select(tract_geoid = GEOID, tract_name = NAMELSAD),
    join = st_within,
    left = TRUE  # Keep all restaurants even if unassigned
  )

tract_counts <- restaurant_points_with_location |>
  st_drop_geometry() |>
  count(tract_name, name = "n_restaurants", sort = TRUE)

print(tract_counts)
# Census tract data
# Number of areas: 46
# Restaurants Assigned: 128
# Unmatched Restaurants: 1
# Smallest area count: 1
# Largest area count: 23

# Zip codes
# Download ZCTAs for New Orleans (ZIP codes starting with "701")
orleans_zctas <- zctas(
  cb = TRUE,
  starts_with = "701",
  year = 2020,
  class = "sf"
) |>
  st_transform(st_crs(restaurant_points_with_location))

# Spatially join ZIP code column onto master location table
restaurant_points_with_location <- restaurant_points_with_location |>
  st_join(
    orleans_zctas |> 
      select(zip_code = ZCTA5CE20),  # ZCTA5CE20 holds the 5-digit ZIP code string
    join = st_within,
    left = TRUE
  )


# View restaurant count breakdown by ZIP code
zip_counts <- restaurant_points_with_location |>
  st_drop_geometry() |>
  count(zip_code, name = "n_restaurants", sort = TRUE)

print(zip_counts)

# Zip code data
# Number of areas: 13
# Restaurants Assigned: 129
# Unmatched Restaurants: 0
# Smallest area count: 1
# Largest area count: 26

# Neighborhood statistical areas
# Load or prepare neighborhood sf object
nola_neighborhoods <- st_read("raw/Neighborhood_Statistical_Area_20261110.geojson")

# Ensure CRS matches master location table (EPSG 4326)
nola_neighborhoods_prep <- nola_neighborhoods |>
  st_transform(st_crs(restaurant_points_with_location))

# Spatially join Neighborhood columns onto master location table
restaurant_points_with_location <- restaurant_points_with_location |>
  st_join(
    nola_neighborhoods_prep |> 
      select(neighborhood_name = gnocdc_lab),
    join = st_within,
    left = TRUE
  )

# View restaurant count breakdown by Neighborhood
neighborhood_counts <- restaurant_points_with_location |>
  st_drop_geometry() |>
  count(neighborhood_name, name = "n_restaurants", sort = TRUE)

print(neighborhood_counts)
# Neighborhood statistical area data:
# Number of areas: 29
# Restaurants assigned: 128
# Unmatched Restaurants: 1
# Smallest area count: 1
# Largest area count: 24

# Wards
# 1. Load or prepare Wards sf object
# (e.g., if loaded from a local file provided in your assignment folder):
nola_wards <- st_read("raw/Wards_20261110.geojson")

# Ensure CRS matches your master location table (EPSG 4326)
nola_wards_prep <- nola_wards |>
  st_transform(st_crs(restaurant_points_with_location))

# Spatially join Ward columns onto master location table
restaurant_points_with_location <- restaurant_points_with_location |>
  st_join(
    nola_wards_prep |> 
      select(ward_id = ward),
    join = st_within,
    left = TRUE
  )

# View restaurant count breakdown by Ward
ward_counts <- restaurant_points_with_location |>
  st_drop_geometry() |>
  count(ward_id, name = "n_restaurants", sort = TRUE)

print(ward_counts)

# Ward data:
# Number of areas: 17
# Restaurants Assigned: 128
# Unmatched Restaurants: 1
# Smallest area count: 1
# Largest area count: 24
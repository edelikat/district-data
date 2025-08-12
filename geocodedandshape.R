# Install if needed
install.packages(c("tidyverse", "sf", "tidygeocoder"))

# Load libraries
library(tidyverse)
library(sf)
library(tidygeocoder)


school_data <- read.csv("TN-School-Directory.csv", stringsAsFactors = FALSE)

#geocode school addressess
schools_geo <- school_data %>%
        geocode(address = address, method = "osm", lat = latitude, long = longitude)

#Find and deal with failed codes
failed_geocodes <- schools_geo %>%
        filter(is.na(latitude) | is.na(longitude))
write.csv(failed_geocodes, "failed_geocodes.csv", row.names = FALSE)

#convert to spatial format
schools_sf <- st_as_sf(schools_geo, coords = c("longitude", "latitude"), crs = 4326)

#read shapefiles
ldistricts_sf <- st_read("tl_2024_47_sldl.shp") %>%
        st_transform(crs = 4326)  # Match CRS with schools_sf
udistricts_sf <- st_read("tl_2024_47_sldu.shp") %>%
        st_transform(crs = 4326)  # Match CRS with schools_sf

#Check shapefiles - you should see mostly "multipolygon" or "polygon" types
st_geometry_type(ldistricts_sf) %>% table()
st_geometry_type(udistricts_sf) %>% table()

#Spatial join schools to districts
# Match schools to state House districts
schools_with_house <- st_join(schools_sf, ldistricts_sf, join = st_within)

# Match schools to state Senate districts
schools_with_senate <- st_join(schools_sf, udistricts_sf, join = st_within)

#Create csv and shapefiles
st_write(schools_with_house, "schools_with_house.geojson")  # for mapping
st_drop_geometry(schools_with_house) %>%
        write.csv("schools_by_house.csv", row.names = FALSE)  # for tabular review

st_write(schools_with_senate, "schools_with_senate.geojson")  # for mapping
st_drop_geometry(schools_with_senate) %>%
        write.csv("schools_by_senate.csv", row.names = FALSE)  # for tabular review

#verify alignment if needed
ggplot() +
        geom_sf(data = ldistricts_sf, fill = NA, color = "blue") +
        geom_sf(data = schools_sf, color = "red", size = 1) +
        theme_minimal()

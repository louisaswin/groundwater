

library(dataRetrieval)
state <- "NC"
start_date <- as.Date("2003-01-01")  # 20 years ago from 2023
end_date <- as.Date("2023-01-01")
parameter_codes <- c("72019", "62610")

?readNWISdata

gw_data <- readNWISdata(stateCd = state, 
                        parameterCd = parameter_codes, 
                        startDate = start_date, 
                        endDate = end_date, 
                        service = "gwlevels")
head(gw_data)
colnames(gw_data)

site_data <- readNWISsite(gw_data$site_no)
site_data_subset <- site_data[, c("site_no", "dec_lat_va", "dec_long_va")]
combined_data <- merge(gw_data, site_data_subset, by = "site_no")
head(combined_data)

library(sf)
library(ggplot2)
library(tigris)

wake_boundary <- counties(state = "NC", cb = TRUE) %>% 
  filter(NAME == "Wake")
# issue with dplyr and other package

nc_counties <- counties(state = "NC", cb = TRUE, year = 2021, class = "sf")
nc_counties
wake_boundary <- nc_counties[nc_counties$NAME == "Wake", ]
#fix

gw_sf <- st_as_sf(combined_data, coords = c("dec_long_va", "dec_lat_va"), crs = 4269)

#spatial join to filter only the groundwater sites within Wake County:
wake_gw <- st_join(gw_sf, wake_boundary)
# didnt work I dont think

ggplot() + 
  geom_sf(data = wake_boundary, fill = "lightgrey") +
  geom_sf(data = wake_gw, aes(color = lev_va)) +
  theme_minimal() +
  labs(title = "Groundwater Levels in Wake County")

ggplot() + 
  geom_sf(data = wake_boundary, fill = "lightgrey") + 
  geom_sf(data = gw_sf, aes(color = lev_va)) +
  theme_minimal() +
  labs(title = "Groundwater Levels in Wake County")

length(unique(wake_gw$site_no))
length(unique(gw_sf$site_no))



site_data <- readNWISsite(gw_data$site_no)
site_data_subset <- site_data[, c("site_no", "dec_lat_va", "dec_long_va")]
combined_data <- merge(gw_data, site_data_subset, by = "site_no")
head(combined_data)
library(lubridate)
library(dplyr)
range(combined_data$lev_dt)
combined_data1 = combined_data %>%
  mutate(lev_dt = as.Date(lev_dt),
         year = year(lev_dt)) %>%
  filter(year == 2022 | year == 2021,
         !is.na(lev_dt) & !is.na(lev_va))
head(combined_data1)
  
avg_meas = combined_data1 %>%
  group_by(site_no, year = year(lev_dt)) %>%
  summarize(
    count = n(),
    year_avg = mean(lev_va, na.rm = TRUE))

?states
nc_boundary <- states(cb = TRUE) %>% 
  filter(NAME == "NC")
# issue with dplyr and other package

state_boundaries <- states(cb = TRUE, year = 2021, class = "sf")
nc_boundary <- state_boundaries[state_boundaries$NAME == "North Carolina", ]
#fix


#convert gw data to sf object
?st_as_sf
gw_sf <- st_as_sf(combined_data1, coords = c("dec_long_va", "dec_lat_va"), crs = 4269)
# 4269 corresponds to the EPSG:4269, which represents the NAD83 (North American Datum 1983)
# which is used by USGS

#spatial join
nc_gw <- st_join(gw_sf, nc_boundary)

ggplot() + 
  geom_sf(data = nc_boundary, fill = "lightgrey") +
  geom_sf(data = nc_gw, aes(color = lev_va)) +
  theme_minimal() +
  labs(title = "Groundwater Levels in North Carolina")






library(lubridate)
avg_meas = gw_data %>%
  filter(!is.na(lev_dt) & !is.na(lev_va)) %>%
  mutate(lev_dt = as.Date(lev_dt)) %>%
  group_by(site_no, year = year(lev_dt)) %>%
  summarize(
    count = n(),
    year_avg = mean(lev_va, na.rm = TRUE))

site_data <- readNWISsite(gw_data$site_no)
site_data_subset <- site_data[, c("site_no", "dec_lat_va", "dec_long_va")]
combined_data3 <- merge(avg_meas, site_data_subset, by = "site_no")
head(combined_data3)
library(lubridate)
library(dplyr)
combined_data4 = combined_data3 %>%
  filter(year == 2022)
head(combined_data4)

gw_sf <- st_as_sf(combined_data4, coords = c("dec_long_va", "dec_lat_va"), crs = 4269)

nc_gw <- st_join(gw_sf, nc_boundary)

ggplot() + 
  geom_sf(data = nc_boundary, fill = "lightgrey") +
  geom_sf(data = nc_gw, aes(color = year_avg)) +
  theme_minimal() +
  labs(title = "Average Groundwater Levels in North Carolina in 2022", 
       caption = "Water-level value measured in feet below land surface") +
  scale_color_continuous(name = "Water-level")


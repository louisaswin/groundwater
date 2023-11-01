library(dataRetrieval)
state <- "NC"
start_date <- as.Date("2003-01-01")  # 20 years ago from 2023
end_date <- as.Date("2023-10-31")
parameter_codes <- c("72019", "62610")

?readNWISdata

gw_data <- readNWISdata(stateCd = state, 
                        parameterCd = parameter_codes, 
                        startDate = start_date, 
                        endDate = end_date, 
                        service = "gwlevels")
head(gw_data)
colnames(gw_data)

library(lubridate)
nc_gw = gw_data %>%
  mutate(lev_dt = as.Date(lev_dt)) %>%
  filter(!is.na(lev_dt) & !is.na(lev_va)) 

head(nc_gw)
summary(nc_gw)
sum(nc_gw$lev_age_cd == "P")
sum(nc_gw$lev_age_cd == "A")

unique(nc_gw$lev_status_cd)
sum(nc_gw$lev_status_cd == "1", na.rm = TRUE)

unique(nc_gw$lev_acy_cd)

unique(nc_gw$lev_src_cd)

# filter by these parameters to ensure the most accuracy in the data
# A = approved data for publication
# 1 = water level status was static (not affected by other conditions)
nc_gw1 = nc_gw %>%
  filter(lev_age_cd == "A",
         lev_status_cd == "1") %>%
  select(site_no, lev_dt, lev_va, lev_age_cd, lev_status_cd)

head(nc_gw1)


num_measures = nc_gw1 %>%
  group_by(site_no) %>%
  summarize(count = n())
head(num_measures)

summary(num_measures)
## possible considerations: how many datapoints should a site have in order for it to be included in regression

insuff_sites = num_measures %>%
  filter(count < 5)

#do not use these sites that have less than 5 measurements in regression

nc_gw2 = nc_gw1 %>%
  filter(!site_no %in% insuff_sites$site_no)

#check
num_measures2 = nc_gw2 %>%
  group_by(site_no) %>%
  summarize(count = n())
head(num_measures2)
summary(num_measures2)

summary(nc_gw2)


# getting rid of lev_age_cd and lev_status_cd columns because they should all be A and 1 respectively
# renaming columns for easier comprehension

nc_gw3 =  nc_gw2 %>%
  select(-lev_age_cd, -lev_status_cd) %>%
  rename(date = lev_dt, water_level = lev_va)
summary(nc_gw3)



# regression now



# split by site
site_list <- split(nc_gw3, nc_gw3$site_no)


library(lmtest)  # for regression diagnostics

# A function to make predictions for a given site
predict_site <- function(site_data) {
  
  # Convert datetime to days since the start
  site_data$days_since_start <- as.numeric(difftime(site_data$date, 
                                                    min(site_data$date), 
                                                    units = "days"))
  
  # Regression model
  model <- lm(water_level ~ days_since_start, data = site_data)
  
  # Predict for the next 5 years (1826 days, considering 365.25 days/year on average)
  last_day <- max(site_data$days_since_start)
  future_days <- seq(last_day + 1, by = 1, length.out = 1826)
  
  predictions <- predict(model, newdata = data.frame(days_since_start = future_days))
  
  return(predictions)
}

predictions_list <- lapply(site_list, predict_site)
head(predictions_list)

# this Predicts the groundwater levels for the next 5 years (which is approximately 1826 days considering an average year length of 365.25 days).





predict_site2 <- function(site_data) {
  
  # Convert datetime to days since the start
  site_data$days_since_start <- as.numeric(difftime(site_data$date, 
                                                    min(site_data$date), 
                                                    units = "days"))
  
  # Regression model using water_level column
  model <- lm(water_level ~ days_since_start, data = site_data)
  
  # Predict for every 30 days in the span of 5 years (60 months)
  last_day <- max(site_data$days_since_start)
  future_days <- seq(last_day + 30, by = 30, length.out = 60) 
  
  predictions <- predict(model, newdata = data.frame(days_since_start = future_days))
  
  return(predictions)
}

predictions_list2 <- lapply(site_list, predict_site2)
predictions_list2[[1]]



# tentative code
# Assuming predictions_list is a list where each item corresponds to a site's predictions

# Combine site IDs with predictions
site_predictions <- data.frame(
  site_no = names(predictions_list),
  last_month_prediction = sapply(predictions_list, function(x) tail(x, n=1)) # Extract the last prediction for each site
)

# Find the site with the lowest prediction for the last month
lowest_site <- site_predictions[which.max(site_predictions$last_month_prediction), ]
#since water_level represents Water-level value in feet below land surface, we want the max

print(lowest_site)





# sites with lowest water levels already
head(nc_gw3)
nc_gw4 = nc_gw3 %>%
  arrange(desc(water_level))
top200 = head(nc_gw4, 200)
lowestsites = unique(top200$site_no)
# could maybe sort by some sort of water level measurement here 
# unlikely that a site with their groundwater at a normal level will deplete so much in a relatively short time frame
# but what time frame do we think we are looking at
# population could influence
# but could only track sites that have levels deeper than 50ft
# this may not be a good standard though as states will vary 
# or could just collect say the 20 lowest sites in each state
# but maybe have to some sort of sorting and see how the sites are in proximity to each other
# geography comes into play 

head(site_list)

#edit data to only include lowest sites
nc_gw5 = nc_gw4 %>%
  filter(site_no %in% lowestsites)
nc_gw5

nc_gw6 = nc_gw5 %>%
  group_by(site_no) %>%
  summarize(n())
nc_gw6


# split by site
site_list2 <- split(nc_gw5, nc_gw5$site_no)

predictions_list2 <- lapply(site_list2, predict_site2)
class(predictions_list2)


sapply(predictions_list2, length)
length(site_list2)
print(site_list2)


#put each site as a column 
library(tidyverse)

# Convert the list to a long-format dataframe
long_df <- data.frame(
  site_no = rep(names(predictions_list2), each=60), 
  time_point = rep(1:60, times=length(predictions_list2)),
  water_level = unlist(predictions_list2)
)
head(long_df)

# Convert the long-format dataframe to wide format
wide_df <- long_df %>%
  spread(key = site_no, value = water_level)

head(wide_df)


#graphing only predictions

library(ggplot2)

# Plotting using ggplot2
ggplot(long_df, aes(x=time_point, y=water_level, color=as.factor(site_no))) +
  geom_line() + 
  ylim(max(long_df$water_level), min(long_df$water_level)) +
  theme_minimal() +
  labs(title = "Predicted Water Levels for Sites", 
       x = "Date", 
       y = "Predicted Water Level", 
       color = "Site No.")




# add existing data to long format prediction data

#ncwatertrends = merge(nc_gw5, long_df)

# currently want to merge the datasets but need to have dates rather than time points 


# flip y axis so not confusing
ggplot(ncwatertrends, aes(x=time_point, y=water_level, color=as.factor(site_no))) +
  geom_line() + 
  ylim(max(long_df$water_level), min(long_df$water_level)) +
  theme_minimal() +
  labs(title = "Predicted Water Levels for Sites", 
       x = "Date", 
       y = "Predicted Water Level", 
       color = "Site No.") 





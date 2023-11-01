library(httr)





# Define the generated URL
url <- "https://waterservices.usgs.gov/nwis/gwlevels/?format=waterml,2.0&stateCd=nc&startDT=2000-10-11&endDT=2023-10-11&parameterCd=72019&siteStatus=active"
# Make a GET request to the URL
response <- GET(url)

# Parse the response (assuming it's XML; for JSON, you'd use content(response, "parsed"))
data <- content(waterml, )

# From here, you can process the data as needed
head(data)





library(httr)

# Define the generated URL
url <- "https://waterservices.usgs.gov/nwis/site/?format=rdb&stateCd=nc&startDT=2023-01-01&endDT=2023-10-10&siteType=GW,GW-CR,GW-EX,GW-HZ,GW-IW,GW-MW,GW-TH&siteStatus=active&hasDataTypeCd=gw"

# Make a GET request to the URL
response <- GET(url)

# Check if the request was successful
if(http_status(response)$category == "Success"){
  
  # Convert the response to a text format
  content_text <- content(response, "text", encoding = "UTF-8")
  
  # Identify the row where the actual data starts (after the comment lines)
  start_row <- max(grep("^#", content_text, fixed = TRUE)) + 1
  
  # Read the tab-delimited data, skipping the comment lines
  data <- read.table(text = content_text, header = TRUE, sep = "\t", skip = start_row)
  
  # The data dataframe now contains the RDB data
  print(head(data))
  
} else {
  print(http_status(response)$message)
}



# From here, you can process the data as needed
head(data)



# ignore above ig



?readNWISgwl
groundwater = readNWISgwl()

library(dataRetrieval)
state <- "NC"
start_date <- as.Date("2003-01-01")  # 20 years ago from 2023
end_date <- as.Date("2023-01-01")
parameter_codes <- c("72019", "62610")

gw_data <- readNWISdata(stateCd = state, 
                        parameterCd = parameter_codes, 
                        startDate = start_date, 
                        endDate = end_date, 
                        service = "gwlevels")

head(gw_data)
?gw_data$lev_dateTime

library(dplyr)
gw_data1 = gw_data %>%
  mutate(lev_dt = as.Date(lev_dt),
  ) %>%
  filter(!is.na(lev_dt) & !is.na(lev_va)) %>%
  
  summary(gw_data1)
head(gw_data1)
#maniputate lev_dt to year

#group_by(site_no)
head(gw_data1)
length(unique(gw_data$site_no))

library(lubridate)
avg_meas = gw_data1 %>%
  group_by(site_no, year = year(lev_dt)) %>%
  summarize(
    count = n(),
    year_avg = mean(lev_va, na.rm = TRUE))

head(avg_meas)
mean(avg_meas$n)
summary(avg_meas)

dev.off()
library(ggplot2)

subset_data <- avg_meas %>% 
  filter(site_no %in% sample(unique(site_no), 50)) 

unique_sites <- unique(avg_meas$site_no)
sampled_sites <- sample(unique_sites, 50)

subset_data <- avg_meas %>% 
  filter(site_no %in% sampled_sites)

ggplot(subset_data, aes(x= year, y = year_avg, group = site_no, color = site_no)) +
  geom_line()



sort(gw_data1$lev_va)



#egh

state <- "NC"
start_date <- as.Date("2003-01-01")  # 20 years ago from 2023
end_date <- as.Date("2023-01-01")
parameter_codes <- c("72019")

gw_data3 <- readNWISdata(stateCd = state, 
                         parameterCd = parameter_codes, 
                         startDate = start_date, 
                         endDate = end_date, 
                         service = "gwlevels")

head(gw_data3)
summary(gw_data3)


dir.exists("~/Documents/Projects/Groundwater/")


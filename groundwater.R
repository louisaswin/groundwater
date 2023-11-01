
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
boxplot(avg_meas$n)

dev.off()
library(ggplot2)
ggplot(avg_meas, aes(x= year, y = year_avg, group = site_no, color = site_no)) +
  geom_line()
ggplot(gw_data1, aes(x = lev_dt, y = lev_va, group = site_no, color = site_no)) +
  geom_line()








# data from http://schoolspending.apps.cironline.org/
# Data cleaning
ca_spend <- read.csv("Desktop/california_watch_school_district_spending.csv")

library(dplyr)

# I want to do a county level analysis
ca_spend <- arrange(ca_spend, county)

ca_spend <- ca_spend %>% 
	group_by(county) %>% 
	mutate(mean(expend_per_ada_student))

ca_spend <- ca_spend %>% 
	group_by(county) %>% 
	mutate(mean(api_2010))

ca_spend <- ca_spend %>% 
	group_by(county) %>% 
	mutate(mean(average_daily_attendance))

ca_spend <- select(ca_spend, -district)
ca_spend <- select(ca_spend, -average_daily_attendance)
ca_spend <- select(ca_spend, -expend_per_ada_student)
ca_spend <- select(ca_spend, -api_2010)

ca_spend <- distinct(ca_spend, county)

colnames(ca_spend)[2] <- "county_mean_ada_expenditure"
colnames(ca_spend)[3] <- "county_mean_api_2010"
colnames(ca_spend)[4] <- "county_mean_ada"

ca_spend <- ca_spend %>% 
	group_by() %>%
	arrange(desc(county_mean_ada_expenditure))

# Mapping California counties
library(ggplot2)
library(choroplethr)
library(choroplethrMaps)
data(county.regions)
ca_master <- ca_spend
ca_test <- ca_spend
ca_map <- county.regions
ca_map <- filter(ca_map, state.name == "california")

ca_map <- arrange(ca_map, county.name)
ca_test <- arrange(ca_test, county)
ca_master <- bind_cols(ca_map, ca_test)
ca_master <- ca_master[, c(3, 7, 8, 9, 10, 1, 2, 4, 5, 6)]
ca_master <- select(ca_master, -county)

ca_test <- ca_master
ca_test <- data.frame(ca_test$region)
ca_test["value"] <- ca_master$county_mean_ada_expenditure
colnames(ca_test)[1] <- "region"

county_choropleth(ca_test,
	title      = "Average School District Spending Per Pupil by County",
	legend     = "Spending per pupil ($)",
	num_colors = 9,
	state_zoom = c("california")) + scale_fill_brewer(palette=8)
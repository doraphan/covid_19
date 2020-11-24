library(lubridate)
library(tidyverse)
library(tidyr)
#read csv file
cases <- read_csv("total_cases.csv")
deaths <- read_csv("total_deaths.csv")
country_code <- read_csv("country_code.csv")
long_lat <- read_csv("long_lat.csv")
locations <- read_csv("locations.csv")

#create global cases and deaths
cases_global <- cases %>% select(World)
deaths_global <- deaths %>% select(World)
#counting
cases_global_sum <- sum(cases_global$World,na.rm=TRUE)
deaths_global_sum <- sum(deaths_global$World,na.rm=TRUE)
#Country cases and deaths
##cases
cases[is.na(cases)] <- 0
cases_1<- colSums(cases[,-c(1:2)])

cases_2 <- rbind(cases[,-c(1:2)], cases_1)

cases_final <- tail(cases_2, n = 1)

cases_final_official <- as.data.frame(t(cases_final), col.names = "Count")
cases_final_official$Country <- rownames(t(cases_final))
cases_final_official <- left_join(cases_final_official, country_code,   by ="Country")
cases_final_1 <- left_join(cases_final_official, long_lat, by ="Country")
##deaths
deaths[is.na(deaths)] <- 0
deaths_1<- colSums(deaths[,-c(1:2)])

deaths_2 <- rbind(deaths[,-c(1:2)], deaths_1)

deaths_final <- tail(deaths_2, n = 1)

deaths_final_official <- as.data.frame(t(deaths_final), col.names = "Count")
deaths_final_official$Country <- rownames(t(deaths_final))
deaths_final_official <- left_join(deaths_final_official, country_code,long_lat, by ="Country")
deaths_final_1 <- left_join(deaths_final_official, long_lat, by ="Country")
####continent
#cases
cases_continent <- cases[, -2]
cases_continent_selected <- as.data.frame(t(cases_continent))
cases_name <- as.data.frame(rownames(t(cases_continent)[-1,]))
colnames(cases_name) <- "Country"
cases_df <- data.frame(cases_continent_selected[-1,])
colnames(cases_df) <- cases_continent_selected[1,]
cases_df <- cbind(cases_df, cases_name)
cases_df <- left_join(cases_df, locations, by = "Country")
#death
deaths_continent <- deaths[, -2]
deaths_continent_selected <- as.data.frame(t(deaths_continent))
deaths_name <- as.data.frame(rownames(t(deaths_continent)[-1,]))
colnames(deaths_name) <- "Country"
deaths_df <- data.frame(deaths_continent_selected[-1,])
colnames(deaths_df) <- deaths_continent_selected[1,]
deaths_df <- cbind(deaths_df, cases_name)
deaths_df <- left_join(deaths_df, locations, by = "Country")

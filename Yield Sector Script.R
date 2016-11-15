library(dplyr)
library(tidyr)
library(lubridate)

load("~/SametimeFileTransfers/cubes.saved")

#Yield by Sector
yield.sector.1Q2016 <- closed.cube.F %>%
  filter(date >= ymd("2016-01-01") & date <=ymd("2016-03-30")) %>%
  group_by(Sector) %>%
  summarise(yield = sum(Won)/sum(Won, Not.Won))

yield.sector.2Q2016 <- closed.cube.F %>%
  filter(date >= ymd("2016-04-01") & date <=ymd("2016-06-30")) %>%
  group_by(Sector) %>%
  summarise(yield = sum(Won)/sum(Won, Not.Won))

yield.sector.3Q2016 <- closed.cube.F %>%
  filter(date >= ymd("2016-07-01") & date <=ymd("2016-09-30")) %>%
  group_by(Sector) %>%
  summarise(yield = sum(Won)/sum(Won, Not.Won))

yield.sector.2016 <- closed.cube.F %>%
  filter(date >= ymd("2016-01-01") & date <=ymd("2016-09-30")) %>%
  group_by(Sector) %>%
  summarise(yield = sum(Won)/sum(Won, Not.Won))

write.csv(yield.sector.1Q2016, "yield.sector.1Q2016.csv")
write.csv(yield.sector.2Q2016, "yield.sector.2Q2016.csv")
write.csv(yield.sector.3Q2016, "yield.sector.3Q2016.csv")
write.csv(yield.sector.2016, "yield.sector.2016.csv")


#Yield by NA

yield.na.1Q2016 <- closed.cube.F %>%
  filter(date >= ymd("2016-01-01") & date<=ymd("2016-03-30")) %>%
  summarise(yield = sum(Won)/ sum(Won, Not.Won))

yield.na.2Q2016 <- closed.cube.F %>%
  filter(date >= ymd("2016-04-01") & date<=ymd("2016-06-30")) %>%
  summarise(yield = sum(Won)/ sum(Won, Not.Won))

yield.na.3Q2016 <- closed.cube.F %>%
  filter(date >= ymd("2016-07-01") & date<=ymd("2016-09-30")) %>%
  summarise(yield = sum(Won)/ sum(Won, Not.Won))

yield.na.2016 <- closed.cube.F %>%
  filter(date >= ymd("2016-01-01") & date<=ymd("2016-09-30")) %>%
  summarise(yield = sum(Won)/ sum(Won, Not.Won))

yield.na.1Q3Q2015 <- closed.cube.F %>%
  filter(date >= ymd("2015-01-01") & date<=ymd("2015-09-30")) %>%
  summarise(yield = sum(Won)/ sum(Won, Not.Won))

write.csv(yield.na.1Q2016, "yield.na.1Q2016.csv")
write.csv(yield.na.2Q2016, "yield.na.2Q2016.csv")
write.csv(yield.na.3Q2016, "yield.na.3Q2016.csv")
write.csv(yield.na.2016, "yield.na.2016.csv")

#Yield NA 2014 - 2016

yield.2014.2016.chart <- closed.cube.F %>%
  filter(date >= ymd("2014-01-01") & date <= ymd("2016-09-30")) %>% 
  group_by(date) %>%
  summarise(yield = sum(Won)/ sum(Won, Not.Won))

write.csv(yield.2014.2016.chart, "yield.2014.2016.chart.csv")
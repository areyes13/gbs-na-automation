library(dplyr)
library(tidyr)
library(lubridate)

load()

#Yield by Sector
yield.sector.1Q2016 <- closed.cube.F %>%
  filter(date >= ymd("2016-01-01") & date <=ymd("2016-03-30")) %>%
  group_by(Sector) %>%
  summarise(yield = sum(Won)/sum(Won, Not.Won, Space.Junk))

#Yield by NA

yield.sector.1Q2016 <- closed.cube.F %>%
  filter(Date >= ymd("2016-01-01") & Date<=ymd("2016-03-30")) %>%
  summarise(won = sum("Won"]),
            not.won = sum("Not.Won"),
            junk = sum("Space.Junk"]),
            yield = sum(won)/(won + not.won + junk))

#Yield NA 2014 - 2016

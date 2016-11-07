
#Opening Packages
library(dplyr)
library(tidyr)
library(lubridate)

#OI by Sector
oi.sector.1H2015 <- cube.F %>%
  filter(create.m.yr >= ymd("2015-01-01") & create.m.yr <= ymd("2015-06-30")) %>%
  group_by(Sector) %>%
  summarise(sector.total.oi = sum(Total.TCV))

oi.sector.1H2016 <- cube.F %>%
  filter(create.m.yr >= ymd("2016-01-01") & create.m.yr <= ymd("2016-06-30")) %>%
  group_by(Sector) %>%
  summarise(sector.total.oi = sum(Total.TCV))

#OI NA 2015
oi.na.2015 <- cube.F %>% 
  filter(create.m.yr >= ymd("2015-01-01") & create.m.yr <= ymd("2015-06-30")) %>%
  summarise(na.total.oi = sum(Total.TCV))

#OI NA 2016
oi.na.2015 <- cube.F %>% 
  filter(create.m.yr >= ymd("2016-01-01") & create.m.yr <= ymd("2016-06-30")) %>%
  summarise(na.total.oi = sum(Total.TCV))

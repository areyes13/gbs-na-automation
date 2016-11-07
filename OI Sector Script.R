
#Opening Packages
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

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

#OI NA 2014 - 2016
oi.2014.2016.chart <- cube.F %>%
  filter(create.m.yr >= ymd("2014-01-01") & create.m.yr <= ymd("2016-09-30")) %>% 
  group_by(create.m.yr) %>%
  summarise(na.monthly.tcv = sum(Total.TCV))

#OI NA Chart
ggplot(oi.2014.2016.chart, aes(x = create.m.yr, y = (na.monthly.tcv/1000000000))) + 
  geom_point(size=2, color="black") +
  geom_smooth(method = lm, se = FALSE)
#we may want to use excel for now...
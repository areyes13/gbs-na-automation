
#Opening Packages
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

load("~/SametimeFileTransfers/cube.saved")

#OI by Sector by quarter
oi.sector.2016 <- cube.F %>%
  filter(source != "Space Junk", create.m.yr >= ymd("2016-01-01") & create.m.yr <= ymd("2016-09-30")) %>%
  group_by(Sector) %>%
  summarise(oi.sector.quarter = sum(Total.TCV))

oi.sector.1Q2016 <- cube.F %>%
  filter(source != "Space Junk", create.m.yr >= ymd("2016-01-01") & create.m.yr <= ymd("2016-03-30")) %>%
  group_by(Sector) %>%
  summarise(oi.sector.quarter = sum(Total.TCV))

oi.sector.2Q2016 <- cube.F %>%
  filter(source != "Space Junk", create.m.yr >= ymd("2016-04-01") & create.m.yr <= ymd("2016-06-30")) %>%
  group_by(Sector) %>%
  summarise(oi.sector.quarter = sum(Total.TCV))

oi.sector.3Q2016 <- cube.F %>%
  filter(source != "Space Junk", create.m.yr >= ymd("2016-07-01") & create.m.yr <= ymd("2016-09-30")) %>%
  group_by(Sector) %>%
  summarise(oi.sector.quarter = sum(Total.TCV))


#OI NA total 2016 & Quarter
oi.na.2016 <- created.cube.F %>% 
  filter(source != "Space Junk", create.m.yr >= ymd("2016-01-01") & create.m.yr <= ymd("2016-09-30")) %>%
  summarise(na.total.oi = sum(Total.TCV))

oi.na.1Q2016 <- cube.F %>% 
  filter(source != "Space Junk", create.m.yr >= ymd("2016-01-01") & create.m.yr <= ymd("2016-03-30")) %>%
  summarise(na.total.oi = sum(Total.TCV))

oi.na.2Q2016 <- cube.F %>% 
  filter(source != "Space Junk", create.m.yr >= ymd("2016-04-01") & create.m.yr <= ymd("2016-06-30")) %>%
  summarise(na.total.oi = sum(Total.TCV))

oi.na.3Q2016 <- cube.F %>% 
  filter(source != "Space Junk", create.m.yr >= ymd("2016-07-01") & create.m.yr <= ymd("2016-09-30")) %>%
  summarise(na.total.oi = sum(Total.TCV))

#OI NA 2014 - 2016
oi.2014.2016.chart <- cube.F %>%
  filter(source != "Space Junk", create.m.yr >= ymd("2014-01-01") & create.m.yr <= ymd("2016-09-30")) %>% 
  group_by(create.m.yr) %>%
  summarise(na.monthly.tcv = sum(Total.TCV))

write.csv(oi.2014.2016.chart, "oi 2014 2016.csv")
write.csv(oi.na.1Q2016, "oi.na.1Q2016.csv")
write.csv(oi.na.2Q2016, "oi.na.2Q2016.csv")
write.csv(oi.na.3Q2016, "oi.na.3Q2016.csv")
write.csv(oi.na.2016, "oi.na.2016.csv")
write.csv(oi.sector.1Q2016, "oi.sector.1Q2016.csv")
write.csv(oi.sector.2Q2016, "oi.sector.2Q2016.csv")
write.csv(oi.sector.3Q2016, "oi.sector.3Q2016.csv")
write.csv(oi.sector.2016, "oi.sector.2016.csv")
write.csv(cube.F, "cube data.csv")

#OI NA Chart
ggplot(oi.2014.2016.chart, aes(x = create.m.yr, y = (na.monthly.tcv/1000000000))) + 
  geom_point(size=2, color="black") +
  geom_smooth(method = lm, se = FALSE)
#we may want to use excel for now...
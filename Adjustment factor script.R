# ADJUSTED YIELD CURVES - ALEJANDRO EDITS ---------------------------------
#insert after ABS.YIELD object is created (1175)
load("~/1 - Projects/GBS NA/na gbs objects.saved")
library(tidyr)
library(dplyr)
library(lubridate)

#re-structure closed.cube.F by removing Canada & Federal
adj.tbl <- closed.cube.F %>%
  filter(date >= '2015-01-01',
         !Sector %in% c('Canada', 'US Federal'),
         !is.na(Sector)) %>%
  mutate(year = year(date),
         DPUID = toupper(DPUID))

#split 2015 - calculate yield
tbl15 <- adj.tbl %>%
  filter(year == 2016) %>%
  group_by(Deal.Size) %>%
  summarize(yield15 = sum(Won)/sum(Won, Not.Won))

#split 2016 - calculate yield & then merge in 2015 to get adjustment factor
tbl16 <- adj.tbl %>%
  filter(year == 2016, Sector != '') %>%
  group_by(Deal.Size, Sector) %>%
  summarize(yield16 = sum(Won)/sum(Won, Not.Won)) %>%
  left_join(tbl15, by = 'Deal.Size') %>%
  mutate(adj.f = yield16/yield15)

#placeholder object to map in DPUID details
mapper <- unique(adj.tbl %>% select(DPUID, Size.Type, Sector:Service.Line))

adjusted <- abs.yields %>%
  gather(Month, "yield", `0`:`24`) %>%
  mutate(DPUID.up = toupper(DPUID),
         Month = as.numeric(Month)) %>%
  inner_join(mapper, by = c('DPUID.up'='DPUID')) %>%
  select(-DPUID.up) %>%
  left_join(tbl16 %>% select(-starts_with('yield')), by = c('Deal.Size', 'Sector')) %>%
  mutate(adjusted.yield = adj.f * yield) %>%
  arrange(DPUID, Month)

rm(tbl15, tbl16, adj.tbl, mapper)


# Notes for R -------------------------------------------------------------


#Filter instead of sub-set: filer(data, conditional statement)
#

test <- c(7,3,5,8,2,1)
print(test)
test.2 <- c("a","b","c","d",6,5,2)

#shows when data matches between vectors
test %in% test.2
test.2 %in% test

#mutate creates new variable
#summarise collapses variables to value

#%>% x and then y
#Filter, group, summarize

# Actual Code -------------------------------------------------------------


#Opening Packages
library(dplyr)
library(tidyr)
library(lubridate)

#Sector Yield 2015
sector.yield.2015 <- cube.F %>% 
      gather(Date, tcv, starts_with('20')) %>% #Vert. Dates w gather
      #mutate(space.junk = ifelse(source == "Space Junk", "space.junk", "tcv")) %>% #Adding space junk column
      #spread(space.junk, tcv, fill=0) %>% #casting space junk
      #mutate(won = tcv*.3,
             #lost = tcv*.7) %>%
      filter(Date >= ymd("2015-01-01") & Date<=ymd("2015-06-30")) %>%
      group_by(Sector) %>%
      summarise(won.sector = sum(tcv[source == "Won"]),
               lost.sector = sum(tcv[source == "Not Won"]),
               junk.sector = sum(tcv[source == "Space Junk"]),
                yield.sector = sum(won.sector)/sum(won.sector, lost.sector, junk.sector))

#NA Yield 2015  
na.yield.2015 <- cube.F %>% 
  gather(Date, tcv, starts_with('20')) %>%
  filter(Date >= ymd("2015-01-01") & Date<=ymd("2015-06-30")) %>%
  summarise(won.na = sum(tcv[source == "Won"]),
            lost.na = sum(tcv[source == "Not Won"]),
            junk.na = sum(tcv[source == "Space Junk"]),
             yield.na = won.na/sum(won.na, lost.na, junk.na))

#Sector Yield 2016
sector.yield.2016 <- cube.F %>% 
  gather(Date, tcv, starts_with('20')) %>% #Vert. Dates w gather
  #mutate(space.junk = ifelse(source == "Space Junk", "space.junk", "tcv")) %>% #Adding space junk column
  #spread(space.junk, tcv, fill=0) %>% #casting space junk
  #mutate(won = tcv*.3,
  #lost = tcv*.7) %>%
  filter(Date >= ymd("2016-01-01") & Date<=ymd("2016-06-30")) %>%
  group_by(Sector) %>%
  summarise(won.sector = sum(tcv[source == "Won"]),
            lost.sector = sum(tcv[source == "Not Won"]),
            junk.sector = sum(tcv[source == "Space Junk"]),
             yield = sum(won.sector)/(won.sector+lost.sector+junk.sector))

#NA Yield 2016
na.yield.2016 <- cube.F %>% 
  gather(Date, tcv, starts_with('20')) %>%
  filter(Date >= ymd("2016-01-01") & Date<=ymd("2016-06-30")) %>%
  summarise(won.na = sum(tcv[source == "Won"]),
            lost.na = sum(tcv[source == "Not Won"]),
            junk.na = sum(tcv[source == "Space Junk"]),
            yield.na = won.na/sum(won.na, lost.na, junk.na))

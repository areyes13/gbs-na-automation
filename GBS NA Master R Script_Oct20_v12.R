#Working Directory - SCIP Server
setwd("~/NA GBS Strategic Work/R Content/Outputs")

#Packages to install
#install.packages("data.table")
#install.packages("scales")
#install.packages("reshape2")
#install.packages("stringi",type="win.binary")
#install.packages("lubridate")
#install.packages("zoo")
#install.packages("splitstackshape")
#test

#Closed Pipe-----------------------------------------------------------------------------------
library(data.table)
library(scales)
library(reshape2)
library(stringi)

# OPP LEVEL ROLLUP (2015-2016)
#Read in file from shared
#FIX THIS FREAD SINCE YOU ADDED FOLDERS ON THIS DEVICE
dtl.closed <- fread("~/NA GBS Strategic Work/Data Sources/EIW GBS NA 2012to20161H- First Stage.csv")
years <- as.data.frame(dtl.closed)

new <- gsub("\\s", ".", colnames(years))
setnames(years, colnames(years), new)
rm(new)
detach("package:splitstackshape", unload=TRUE)
detach("package:data.table", unload=TRUE)
library(dplyr)

years$Won.Lost.Year <- as.numeric(years$Won.Lost.Year)
years$Opp.Create.Date <- with(years, as.Date(Opp.Create.Date, format = "%m/%d/%Y"))
years$Opp.Win.Loss.Date <- with(years, as.Date(Opp.Win.Loss.Date, format = "%m/%d/%Y"))
years$First.Stage <- as.character(years$First.Stage)
years$Rpt.Brand.Subgroup <- as.character(years$Rpt.Brand.Subgroup)
years$Ext.Dtl.Rev.Usd <- as.numeric(years$Ext.Dtl.Rev.Usd)
years <- years[order(years$Opp.Win.Loss.Date, na.last = TRUE, decreasing = TRUE),]
years["dupe"] <- duplicated(years$Detail.Key)
years <- subset(years, dupe == FALSE)
years <- subset(years, Edge.Region.Name != 'US Exception')


#use dplyr to rollup data to Opportunity level. Remove opps with tcv of 0 after rolling up
data <- dplyr::tbl_df(years) %>% #specify the data table to summarize
  group_by(Opp.No, Rpt.Brand.Subgroup, Open.Won.Lost, Edge.Region.Name,
           Gbs.Sector.Name, Won.Lost.Year, 
           #Industry, 
           First.Stage) %>%
  summarise(sl.tcv = sum(Ext.Dtl.Rev.Usd),
            created = min(Opp.Create.Date),
            closed = max(Opp.Win.Loss.Date))

data["dupe"] <- duplicated(data$Opp.No)

data <- subset(data, sl.tcv > 0)

#create size categories based on total opp val
data$size <- with(data, factor(ifelse(sl.tcv < 1e6, '<$1M',
                                      ifelse(sl.tcv >= 1e6 & sl.tcv <5e6, '$1M to <$5M',
                                             ifelse(sl.tcv >= 5e6 & sl.tcv < 10e6, '$5M to <$10M',
                                                    ifelse(sl.tcv >= 10e6, '>$10M', NA))))))

#reorder factor levels (originally defaults to alphabetical)
#data$size <- factor(data$size, levels(data$size)[c(1,4,2,3)])

data <- as.data.frame(data)

#Recode Won or Lost statuses as 'Closed'
data$First.Stage <- with(data, ifelse(First.Stage %in% c('Won', 'Lost'),
                                      'Closed', ifelse(First.Stage == 'Noticed', 'Identified',
                                      First.Stage)))


#Recode service lines
data$new.sl <- with(data, ifelse(Rpt.Brand.Subgroup %in% c('iX&M', 'BA&S'), 'Digital',
                            ifelse(Rpt.Brand.Subgroup %in% c('C&SI', 'AD&F', 'AD&I', 'AMS SO'),
                                   'AIC', ifelse(Rpt.Brand.Subgroup == 'GPS', 'BPS',
                                                 Rpt.Brand.Subgroup))))


#fix formatting for date fields
data$created <- with(data, as.Date(created, format = "%m/%d/%Y"))
data$closed <- with(data, as.Date(closed, format = "%m/%d/%Y"))

data$Edge.Region.Name <- with(data, ifelse(Edge.Region.Name == 'US East' |
                                             Edge.Region.Name == 'US West', 'US',
                                           Edge.Region.Name))

data$Sector <- with(data, ifelse(Edge.Region.Name == 'Canada', 'Canada',
                            ifelse(Edge.Region.Name == 'US Federal', 'US Federal',
                              Gbs.Sector.Name)))

#data$Industry <- with(data, ifelse(Industry == 'Aerospace&Defense', 'Aerospace & Defense',
 #                             ifelse(Industry == 'Chemicals&Petroleum', 'Chemicals & Petroleum',
  #                              ifelse(Industry == 'ConsumerPackageGoods', 'Consumer Products',
   #                               ifelse(Industry == 'Media&Entertainment', 'Media & Entertainment',
    #                          ifelse(Industry == 'TravelTransportation', 'Travel & Transportation',
     #                                ifelse(Industry == 'Unassigned', 'UNASSIGNED', Industry)))))))

library(splitstackshape)

data$freq <- with(data,
                  ifelse(Edge.Region.Name == 'US' & Gbs.Sector.Name == "", 5,
                         ifelse(Edge.Region.Name == 'US' & Gbs.Sector.Name == 'Unassigned', 5,
                                ifelse(Edge.Region.Name == 'US' & Gbs.Sector.Name == 'UNASSIGNED', 5,
                                       1))))

data.expanded <- data[rep(row.names(data), data$freq), 1:16]
sectors = c("US COMM", "US DIST", "US FSS", "US IND", "US PUB")

sector.data.1 <- subset(data.expanded, data.expanded$freq == 5)
sector.data.2 <- subset(data.expanded, data.expanded$freq == 1)

sector.data.1$Sector.Man <- rep_len(sectors, length(sector.data.1[,1]))
#New TCV based on sector distribution above
sector.data.1$new.tcv <- with(sector.data.1, ifelse(Sector.Man == 'US COMM', sl.tcv*(285/1461),
                                               ifelse(Sector.Man == 'US DIST', sl.tcv*(353/1461),
                                                ifelse(Sector.Man == 'US FSS', sl.tcv*(282/1461),
                                                 ifelse(Sector.Man == 'US IND', sl.tcv*(395/1461),
                                                  ifelse(Sector.Man == 'US PUB', sl.tcv*(146/1461),
                                                    sl.tcv))))))
sector.data.2$Sector.Man <- with(sector.data.2,
                                 ifelse(Sector == "Communications", "US COMM",
                                        ifelse(Sector == "Distribution", "US DIST",
                                               ifelse(Sector == "Financial Services", "US FSS",
                                                      ifelse(Sector == "Industrial", "US IND",
                                                             ifelse(Sector == "Public", "US PUB", Sector))))))

sector.data.2$new.tcv <- sector.data.2$sl.tcv
closed.pipe <- rbind(sector.data.1, sector.data.2)

# CYCLE TIME CALCULATIONS
#figure out how many weeks it took to go from created to closed
closed.pipe$weeks <- with(closed.pipe, difftime(closed, created, units = 'weeks'))
closed.pipe$weeks <- with(closed.pipe, round(as.numeric(weeks), 0))


#set month bin based on 4 week intervals up to 2 years
closed.pipe$mo.bin <- with(closed.pipe, findInterval(weeks, seq(0,104, by = 4.33))) - 1

#Month and year created to cast data
closed.pipe$create.month <- format(closed.pipe$created, "%m")
closed.pipe$create.year <- format(closed.pipe$created, "%Y")

library(zoo)
closed.pipe$create.m.yr <- as.yearmon(closed.pipe$created)

#DPUID
closed.pipe$DPUID <- with(closed.pipe, paste(First.Stage, size, new.sl, Sector.Man, 
                                             #Industry,
                                             sep = ""))

#Data Type
closed.pipe$Size.Type <- with(closed.pipe, ifelse(size == '>$10M', 'Large', 'Small'))


closed.pipe$Create.Stage <- closed.pipe$First.Stage
closed.pipe$Deal.Size <- closed.pipe$size
closed.pipe$Sector.F <- closed.pipe$Sector.Man
closed.pipe$Service.Line <- closed.pipe$new.sl
closed.pipe$TCV <- closed.pipe$new.tcv
closed.pipe["source"] <- "Closed"

closed.pipe.slim <- subset(closed.pipe, Won.Lost.Year == 2016)


# Open Pipe------------------------------------------------------------------------------------
library(data.table)
library(scales)
library(reshape2)
library(stringi)

# OPP LEVEL ROLLUP

#Adam's fread()
dtl.open <- read.csv("~/NA GBS Strategic Work/Data Sources/Open Pipeline/Heat Map/SMS8021 GBS NA Opportunity Detail - 4Q16 06.10.16.csv")
years.pipe <- as.data.frame(dtl.open)


new <- gsub("\\s", ".", colnames(years.pipe))
setnames(years.pipe, colnames(years.pipe), new)
rm(new)
detach("package:splitstackshape", unload=TRUE)
detach("package:data.table", unload=TRUE)
detach("package:zoo", unload=TRUE)
library(dplyr)


years.pipe$Opp.Create.Date <- with(years.pipe, as.Date(Opp.Create.Date, format = "%m/%d/%Y"))
years.pipe$S.S.Update.Date <- with(years.pipe, as.Date(S.S.Update.Date, format = "%m/%d/%Y"))
years.pipe$create.year <- format(years.pipe$Opp.Create.Date, "%Y")
years.pipe$create.month <- format(years.pipe$Opp.Create.Date, "%m")
years.pipe$tcv <- as.numeric(years.pipe$Rev.Signings.Value...K.)
years.pipe$tcv <- years.pipe$tcv * 1000
years.pipe <- subset(years.pipe, IMT != 'US Exception Mkt')
years.pipe <- subset(years.pipe, SSM.Step.Name != 'Won')

#use dplyr to rollup data to Opportunity level. Remove opps with tcv of 0 after rolling up
data.pipe <- dplyr::tbl_df(years.pipe) %>% #specify the data table to summarize
  group_by(Opp.No, Brand.Sub.Group, SSM.Step.Name, Previous.Sales.Stage,
           IMT, 
           #Industry, 
           GBS.Bus.Unit.Level.2) %>% #specify which records/variables to keep
  summarise(tcv = sum(tcv), #define new variables using functions
            created = min(Opp.Create.Date),
            updated = max(S.S.Update.Date)) %>%
  filter(tcv > 0) #remove 0 or neg tcvs

#create size categories based on total opp val
data.pipe$deal.size <- with(data.pipe,
                            factor(ifelse(tcv < 1000000, '<$1M',
                                     ifelse(tcv >= 1000000 & tcv <5000000, '$1M to <$5M',
                                      ifelse(tcv >= 5000000 & tcv < 10000000, '$5M to <$10M',
                                       ifelse(tcv >= 10000000, '>$10M', NA))))))
#reorder factor levels (originally defaults to alphabetical)
data.pipe$size <- factor(data.pipe$deal.size, levels(data.pipe$deal.size)[c(1,4,2,3)])

data.pipe <- as.data.frame(data.pipe)

#fix formatting for date fields
data.pipe$created <- with(data.pipe, as.Date(created, format = "%m/%d/%Y"))
data.pipe$updated <- with(data.pipe, as.Date(updated, format = "%m/%d/%Y"))
data.pipe$create.month <- format(data.pipe$created, "%m")
data.pipe$create.year <- format(data.pipe$created, "%Y")


data.pipe$Industry <- as.character(data.pipe$Industry)

#data.pipe$Industry <- with(data.pipe, ifelse(Industry == 'Government, Central/Federal',
 #                                            'Government',
  #                                      ifelse(Industry == 'Government, State/Provincial/Local',
   #                                            'Government', Industry)))

library(zoo)
data.pipe$create.m.yr <- as.yearmon(data.pipe$created)

# CYCLE TIME CALCULATIONS
#figure out how many weeks it took to go from created to closed
data.pipe$age.weeks <- difftime(Sys.Date(), data.pipe$created, units = c("weeks"))
data.pipe$age.weeks <- round(as.numeric(data.pipe$age.weeks), 0)

#set month bin based on 4 week intervals
data.pipe$age.month <- with(data.pipe, findInterval(age.weeks, seq(0,104000, by = 4.33))) - 1


opp.data <- data.pipe


#Flag deals that are duplicates across service lines
opp.data["dupe"] <- duplicated(opp.data$Opp.No)

#splitting up dates
opp.data$create.date <- as.Date(opp.data$created)
#opp.data$create.date <- data.frame(date = opp.data$create.date,
#                       year = as.numeric(format(opp.data$create.date, format = "%Y")))


#Changing sl field name
opp.data$new.sl <- opp.data$Brand.Sub.Group

#Combining AIC and AD&F
opp.data$new.sl <- ifelse(opp.data$new.sl == 'AIC' | opp.data$new.sl == 'AD&F',
                    'AIC', ifelse(opp.data$new.sl == 'BPS', 'BPS',
                              ifelse(opp.data$new.sl == 'Digital', 'Digital',
                                 ifelse(opp.data$new.sl == 'EA', 'EA', NA))))


#Collapsing IMTs
opp.data$IMT.roll.up <- ifelse(opp.data$IMT == 'US Communica/CSI Mkt' |
                                 opp.data$IMT == 'US Distribution Mkt' |
                                 opp.data$IMT == 'US Finan Service Mkt' |
                                 opp.data$IMT == 'US Industrial Mkt' |
                                 opp.data$IMT == 'US Public Mkt', 'US',
                               ifelse(opp.data$IMT == 'Canada Mkt', 'Canada',
                                      ifelse(opp.data$IMT == 'US Federal Mkt', 'US Federal', NA)))


#Creating Create.Stage
opp.data$Create.Stage <- with(opp.data,
                              ifelse(Previous.Sales.Stage == '*',
                                 as.character(SSM.Step.Name),
                                ifelse(Previous.Sales.Stage == 1 | Previous.Sales.Stage == 2 |
                                     Previous.Sales.Stage == 3, 'Identified',
                                 ifelse(SSM.Step.Name == 'Identified' &
                                       Previous.Sales.Stage != '*', 'Identified',
                                    ifelse(SSM.Step.Name == 'Validated' &
                                         Previous.Sales.Stage != '*', 'Identified',
                                      ifelse(SSM.Step.Name == 'Qualified' &
                                          Previous.Sales.Stage != '*', 'Identified',
                                         ifelse(SSM.Step.Name == 'Conditional Agreement' &
                                            Previous.Sales.Stage != '*' & Previous.Sales.Stage != 1 &
                                               Previous.Sales.Stage != 2 & Previous.Sales.Stage != 3,
                                                'Validated', "")))))))


opp.data$Create.Stage <- with(opp.data, ifelse(Create.Stage == 'Conditional Agreement',
                                               'Conditional', Create.Stage))
#Field Rename
opp.data$IMT <- opp.data$IMT.roll.up
opp.data$Service.Line <- opp.data$new.sl
opp.data$Sector.IMT <- opp.data$GBS.Bus.Unit.Level.2
opp.data$Sector.IMT <- with(opp.data,
                            ifelse(Sector.IMT == 'US-FSS', 'US FSS',
                             ifelse(Sector.IMT == 'US-DIST', 'US DIST',
                              ifelse(Sector.IMT == 'US-COMM', 'US COMM',
                               ifelse(Sector.IMT == 'US-IND', 'US IND',
                                ifelse(Sector.IMT == 'US-PUB', 'US PUB',
                                 ifelse(Sector.IMT == 'Canada', 'Canada',
                                  ifelse(Sector.IMT == 'US Federal', 'US Federal', ""))))))))

#Smushing
opp.data$size.smush <- ifelse(opp.data$deal.size == '$1M to <$5M' | opp.data$deal.size == '$5M to <$10M',
                              '$1M to <$10M', ifelse(opp.data$deal.size == '<$1M', '<$1M', '>$10M'))
opp.data$size.smush.2 <- ifelse(opp.data$deal.size == '<$1M', '<$1M', '>$1M')

opp.data$sl.smush <- ifelse(opp.data$Service.Line == 'AIC' |
                              opp.data$Service.Line == 'BPS', 'AIC & BPS', 'Digital & EA')

opp.data$sl.smush.2 <- ifelse(opp.data$Service.Line == 'AIC', 'AIC', 'Digital, EA, & BPS')

opp.data$IMT.smush <- ifelse(opp.data$IMT == 'US' | opp.data$IMT == 'US Federal',
                             'US & US Federal', 'Canada')

#Deal Profile Assignment
opp.data$deal.profile <- with(opp.data,
                            ifelse(Service.Line == "AIC" & size.smush == "<$1M" & Create.Stage == "Identified", 1,
                             ifelse(Service.Line == "Digital" & size.smush == "<$1M" & Create.Stage == "Identified", 2,
                              ifelse(Service.Line == "EA" & size.smush == "<$1M" & Create.Stage == "Identified", 3,
                               ifelse(Service.Line == "BPS" & size.smush == "<$1M" & Create.Stage == "Identified", 4,
                                ifelse(Service.Line == "AIC" & size.smush == "$1M to <$10M" & Create.Stage == "Identified", 5,
                           ifelse(Service.Line == "Digital" & size.smush == "$1M to <$10M" & Create.Stage == "Identified", 6,
                            ifelse(Service.Line == "EA" & size.smush == "$1M to <$10M" & Create.Stage == "Identified", 7,
                             ifelse(Service.Line == "BPS" & size.smush == "$1M to <$10M" & Create.Stage == "Identified", 9,
                              ifelse(sl.smush == "AIC & BPS" & size.smush == ">$10M" & Create.Stage == "Identified", 10,
                               ifelse(Service.Line == "Digital" & size.smush == ">$10M" & Create.Stage == "Identified", 13,
                          ifelse(Service.Line == "EA" & size.smush == ">$10M" & Create.Stage == "Identified", 14,
                           ifelse(sl.smush == "AIC & BPS" & size.smush == "<$1M" & Create.Stage == "Validated", 16,
                            ifelse(Service.Line == "Digital" & size.smush == "<$1M" & Create.Stage == "Validated", 19,
                              ifelse(Service.Line == "EA" & size.smush == "<$1M" & Create.Stage == "Validated", 22,
                                ifelse(sl.smush == "AIC & BPS" & size.smush == "$1M to <$10M" & Create.Stage == "Validated", 24,
                          ifelse(Service.Line == "Digital" & size.smush == "$1M to <$10M" & Create.Stage == "Validated", 27,
                           ifelse(Service.Line == "EA" & size.smush == "$1M to <$10M" & Create.Stage == "Validated", 30,
                            ifelse(sl.smush == "AIC & BPS" & size.smush == ">$10M" & Create.Stage == "Validated", 32,
                             ifelse(Service.Line == "Digital" & size.smush == ">$10M" & Create.Stage == "Validated", 33,
                              ifelse(Service.Line == "EA" & size.smush == ">$10M" & Create.Stage == "Validated", 34,
                         ifelse(sl.smush == "AIC & BPS" & size.smush == "<$1M" & Create.Stage == "Qualified", 36,
                          ifelse(Service.Line == "Digital" & size.smush == "<$1M" & Create.Stage == "Qualified", 39,
                           ifelse(Service.Line == "EA" & size.smush == "<$1M" & Create.Stage == "Qualified", 42,
                            ifelse(sl.smush == "AIC & BPS" & size.smush.2 == ">$1M" & Create.Stage == "Qualified", 44,
                             ifelse(sl.smush == "Digital & EA" & size.smush.2 == ">$1M" & Create.Stage == "Qualified", 45,
                          ifelse(Service.Line == "AIC" & size.smush == "<$1M" & Create.Stage == "", 1,
                            ifelse(Service.Line == "Digital" & size.smush == "<$1M" & Create.Stage == "", 2,
                             ifelse(Service.Line == "EA" & size.smush == "<$1M" & Create.Stage == "", 3,
                              ifelse(Service.Line == "BPS" & size.smush == "<$1M" & Create.Stage == "", 4,
                               ifelse(Service.Line == "AIC" & size.smush == "$1M to <$10M" & Create.Stage == "", 5,
                          ifelse(Service.Line == "Digital" & size.smush == "$1M to <$10M" & Create.Stage == "", 6,
                           ifelse(Service.Line == "EA" & size.smush == "$1M to <$10M" & Create.Stage == "", 7,
                            ifelse(Service.Line == "BPS" & size.smush == "$1M to <$10M" & Create.Stage == "", 9,
                             ifelse(sl.smush == "AIC & BPS" & size.smush == ">$10M" & Create.Stage == "", 10,
                              ifelse(Service.Line == "Digital" & size.smush == ">$10M" & Create.Stage == "", 13,
                                 ifelse(Service.Line == "EA" & size.smush == ">$10M" & Create.Stage == "", 14, 0)))))))))))))))))))))))))))))))))))))

opp.data$DPUID <- with(opp.data, paste(Create.Stage, size, Service.Line, Sector.IMT, 
                                       #Industry,
                                       sep = ""))

opp.data$Size.Type <- with(opp.data, ifelse(size == '>$10M', 'Large', 'Small'))


opp.data$Deal.Size <- opp.data$size
opp.data$Sector.F <- opp.data$Sector.IMT
opp.data$TCV <- opp.data$tcv
opp.data["source"] <- "Open"

open.pipe <- opp.data
open.pipe.slim <- open.pipe
open.pipe.slim <- subset(open.pipe.slim, create.year == 2016)

#The Cube----------------------------------------------------------------------------------
closed.cube <- subset(closed.pipe,
                      select=c(Opp.No, DPUID, Create.Stage, Deal.Size, Sector.F, Service.Line,
                                  Size.Type, 
                               #Industry, 
                               TCV, create.m.yr, created, mo.bin, source))

open.cube.prep <- open.pipe
open.cube.prep$mo.bin <- open.cube.prep$age.month

open.cube <- subset(open.cube.prep,
                    select=c(Opp.No, DPUID, Create.Stage, Deal.Size, Sector.F, Service.Line,
                                Size.Type, 
                             #Industry, 
                             TCV, create.m.yr, created, mo.bin, source))


cube.1 <- rbind(closed.cube, open.cube)

cube.2 <- dplyr::tbl_df(cube.1) %>%
  group_by(DPUID, source, create.m.yr, Size.Type) %>%
  filter(Create.Stage != 'Closed') %>%
  summarise(Total.TCV = sum(TCV))

cube.2$unique.ID <- with(cube.2, paste(DPUID, source, Size.Type, sep = ""))

cube.ref <- dplyr::tbl_df(cube.2) %>%
  group_by(DPUID, source, Size.Type, unique.ID) %>%
  summarise(Total.TCV = sum(Total.TCV))

cube.ref <- subset(cube.ref, select = c(DPUID, source, Size.Type, unique.ID))
cube.2.0 <- subset(cube.2, select = c(unique.ID, create.m.yr, Total.TCV))

library(lubridate)

#unique ids
ids <- unique(as.factor(cube.2.0$unique.ID))

#set up time interval of dates (important to set 'from' to be the first of the month)
month.vec <- seq.Date(from = ymd('2009-12-01'),
                      to = floor_date(Sys.Date(), 'month'),
                      by = 'month')

library(data.table)

#create df with combination of both objects
month.expanded <-  CJ(ids, month.vec)

#reset col names
setnames(month.expanded, colnames(month.expanded), c('unique.ID', 'create.m.yr'))
month.expanded$create.m.yr <- as.yearmon(month.expanded$create.m.yr)

month.expanded <- left_join(month.expanded, cube.2.0, by = c('unique.ID', 'create.m.yr'))

month.expanded$Total.TCV[is.na(month.expanded$Total.TCV)] <- 0

cube.2.1 <- month.expanded

cube.2.1$create.m.yr <- with(cube.2.1, as.Date.yearmon(create.m.yr))

cube.2.2 <- inner_join(cube.2.1, cube.ref, by = 'unique.ID')

cube.2.3 <- subset(cube.2.2, source == 'Open')

cube.2.3$sj.date <- as.Date(with(cube.2.3, ifelse(Size.Type == 'Small', create.m.yr - months(13),
                              create.m.yr - months(19))))

cube.2.3.ref <- cube.2.3

cube.2.3$unique.date <- with(cube.2.3, paste(unique.ID, sj.date, sep = "-"))
cube.2.3.ref$unique.date <- with(cube.2.3, paste(unique.ID, create.m.yr, sep = "-"))


cube.2.3$sj.tcv <- cube.2.3.ref$Total.TCV[match(cube.2.3$unique.date, cube.2.3.ref$unique.date)]

cube.2.4 <- subset(cube.2.3, select = c(unique.ID, DPUID, source, Size.Type, sj.date, sj.tcv))
colnames(cube.2.4)[5] <- 'create.m.yr'
colnames(cube.2.4)[6] <- 'Total.TCV'
cube.2.4$source <- 'Space Junk'

cube.2.5 <- rbind(cube.2.4, cube.2.2)

cube.2.5$Total.TCV[is.na(cube.2.5$Total.TCV)] <- 0

cube.3 <- dcast(cube.2.5, unique.ID + DPUID + source + Size.Type
                ~ create.m.yr, value.var = c('Total.TCV'))

cube.3.1 <- cube.3

cube.3.1[is.na(cube.3.1)] <- 0

cube.4 <- as.data.frame(cube.3.1)


#Creating monthly average create rate table--------------------------------------------------
closed.mon.crt <- subset(closed.pipe, create.year == 2016)
closed.limit <- max(closed.mon.crt$create.month)
print(closed.limit)

closed.mon.crt <- subset(closed.mon.crt, select=c(Opp.No, DPUID, Create.Stage, Deal.Size,
                                               Sector.F, Service.Line, 
                                               #Industry, 
                                               Size.Type, TCV))


open.mon.crt <- subset(open.pipe, create.year == 2016 & create.month <= closed.limit)

open.mon.crt <- subset(open.mon.crt, select=c(Opp.No, DPUID, Create.Stage, Deal.Size, Sector.F,
                                      Service.Line, 
                                      #Industry, 
                                      Size.Type, TCV))


comb.data <- rbind(open.mon.crt, closed.mon.crt)


monthly.create.data <- dplyr::tbl_df(comb.data) %>%
  group_by(DPUID, Create.Stage, Deal.Size, Sector.F, Service.Line, 
           #Industry, 
           Size.Type) %>%
  filter(Create.Stage != 'Closed') %>%
  summarise(month.avg.tcv = sum(TCV)/6) #ADAMIANGRUBER


#Absolute Yield Curves----------------------------------------------------------------
#Smushing
#Collapsing Deal Size to <$1M and >$1M
closed.pipe$size.smush <- ifelse(closed.pipe$Deal.Size == '$1M to <$5M'
                                 | closed.pipe$Deal.Size == '$5M to <$10M',
                          '$1M to <$10M', ifelse(closed.pipe$Deal.Size == '<$1M', '<$1M', '>$10M'))

closed.pipe$size.smush.2 <- ifelse(closed.pipe$Deal.Size == '<$1M', '<$1M', '>$1M')

#Collapsing Service Lines
closed.pipe$sl.smush <- ifelse(closed.pipe$Service.Line == 'AIC' | closed.pipe$Service.Line == 'BPS',
                               'AIC & BPS', 'Digital & EA')

closed.pipe$sl.smush.2 <- ifelse(closed.pipe$Service.Line == 'AIC', 'AIC', 'Digital, EA, & BPS')

#Collapsing IMTs
closed.pipe$IMT.smush.1 <- ifelse(closed.pipe$Edge.Region.Name == 'US'
                                  | closed.pipe$Edge.Region.Name == 'US Federal',
                                  'US & US Federal', 'Canada')

#Deal Profile 1: Identified x All IMT x <$1M x AIC-------------------------------------
abs.dp.1 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'AIC',
         Create.Stage == 'Identified', size.smush == '<$1M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = (won.tcv/(total.sum.tcv + 4.201)))

abs.dp.1["Deal.Profile"] <- 1
abs.dp.1["IMT"] <- "All"
abs.dp.1["Service.Line"] <- "AIC"
abs.dp.1["Deal.Size"] <- "<$1M"
abs.dp.1["Create.Stage"] <- "Identified"


#dp1.yields <- dcast(abs.dp.1, Deal.Profile + IMT + Service.Line +
#                      Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 2: Identified x All IMT x <$1M x Digital-------------------------------------
abs.dp.2 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'Digital',
         Create.Stage == 'Identified', size.smush == '<$1M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = won.tcv/(total.sum.tcv + 8.891))

abs.dp.2["Deal.Profile"] <- 2
abs.dp.2["IMT"] <- "All"
abs.dp.2["Service.Line"] <- "Digital"
abs.dp.2["Deal.Size"] <- "<$1M"
abs.dp.2["Create.Stage"] <- "Identified"


#dp2.yields <- dcast(abs.dp.2, Deal.Profile + IMT + Service.Line +
#                      Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 3: Identified x All IMT x <$1M x EA-------------------------------------
abs.dp.3 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'EA',
         Create.Stage == 'Identified', size.smush == '<$1M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = won.tcv/(total.sum.tcv + 2.127))

abs.dp.3["Create.Stage"] <- "Identified"
abs.dp.3["Deal.Profile"] <- 3
abs.dp.3["IMT"] <- "All"
abs.dp.3["Service.Line"] <- "EA"
abs.dp.3["Deal.Size"] <- "<$1M"

#dp3.yields <- dcast(abs.dp.3, Deal.Profile + IMT + Service.Line +
#                      Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 4: Identified x All IMT x <$1M x BPS-------------------------------------
abs.dp.4 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'BPS',
         Create.Stage == 'Identified', size.smush == '<$1M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = won.tcv/(total.sum.tcv + .666))

abs.dp.4["Create.Stage"] <- "Identified"
abs.dp.4["Deal.Profile"] <- 4
abs.dp.4["IMT"] <- "All"
abs.dp.4["Service.Line"] <- "BPS"
abs.dp.4["Deal.Size"] <- "<$1M"

#dp4.yields <- dcast(abs.dp.4, Deal.Profile + IMT + Service.Line +
#                      Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 5: Identified x All IMT x $1M to <$10M x AIC-------------------------------------
abs.dp.5 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'AIC',
         Create.Stage == 'Identified', size.smush == '$1M to <$10M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = won.tcv/(total.sum.tcv + 50.399))

abs.dp.5["Create.Stage"] <- "Identified"
abs.dp.5["Deal.Profile"] <- 5
abs.dp.5["IMT"] <- "All"
abs.dp.5["Service.Line"] <- "AIC"
abs.dp.5["Deal.Size"] <- "$1M to <$10M"

#dp5.yields <- dcast(abs.dp.5, Deal.Profile + IMT + Service.Line +
#                      Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 6: Identified x All IMT x $1M to <$10M x Digital-------------------------------------
abs.dp.6 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'Digital',
         Create.Stage == 'Identified', size.smush == '$1M to <$10M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = won.tcv/(total.sum.tcv + 46.852))

abs.dp.6["Create.Stage"] <- "Identified"
abs.dp.6["Deal.Profile"] <- 6
abs.dp.6["IMT"] <- "All"
abs.dp.6["Service.Line"] <- "Digital"
abs.dp.6["Deal.Size"] <- "$1M to <$10M"

#dp6.yields <- dcast(abs.dp.6, Deal.Profile + IMT + Service.Line +
#                      Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 7: Identified x All IMTs x $1M to <$10M x EA-------------------------------------
abs.dp.7 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'EA',
         Create.Stage == 'Identified', size.smush == '$1M to <$10M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = won.tcv/(total.sum.tcv + 25.833))

abs.dp.7["Create.Stage"] <- "Identified"
abs.dp.7["Deal.Profile"] <- 7
abs.dp.7["IMT"] <- "All"
abs.dp.7["Service.Line"] <- "EA"
abs.dp.7["Deal.Size"] <- "$1M to <$10M"

#dp7.yields <- dcast(abs.dp.7, Deal.Profile + IMT + Service.Line +
#                      Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 9: Identified x All IMT x $1M to <$10M x BPS-------------------------------------
abs.dp.9 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'BPS',
         Create.Stage == 'Identified', size.smush == '$1M to <$10M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = won.tcv/(total.sum.tcv + 18.500))

abs.dp.9["Create.Stage"] <- "Identified"
abs.dp.9["Deal.Profile"] <- 9
abs.dp.9["IMT"] <- "All"
abs.dp.9["Service.Line"] <- "BPS"
abs.dp.9["Deal.Size"] <- "$1M to <$10M"

#dp9.yields <- dcast(abs.dp.9, Deal.Profile + IMT + Service.Line +
#                      Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 10: Identified x All IMTs x >$10M x AIC & BPS-------------------------------------
abs.dp.10 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, sl.smush, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, sl.smush == 'AIC & BPS',
         Create.Stage == 'Identified', size.smush == '>$10M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, sl.smush) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  group_by(size.smush, sl.smush, mo.bin) %>%
  mutate(yield = won.tcv/(total.sum.tcv + 152.000))

abs.dp.10["Create.Stage"] <- "Identified"
abs.dp.10["Deal.Profile"] <- 10
abs.dp.10["IMT"] <- "All"
abs.dp.10["Service.Line"] <- "AIC & BPS"
abs.dp.10["Deal.Size"] <- ">$10M"

#dp10.yields <- dcast(abs.dp.10, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 13: Identified x All IMT x >$10M x Digital-------------------------------------
abs.dp.13 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'Digital',
         Create.Stage == 'Identified', size.smush == '>$10M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = won.tcv/total.sum.tcv)

abs.dp.13["Create.Stage"] <- "Identified"
abs.dp.13["Deal.Profile"] <- 13
abs.dp.13["IMT"] <- "All"
abs.dp.13["Service.Line"] <- "Digital"
abs.dp.13["Deal.Size"] <- ">$10M"

#dp13.yields <- dcast(abs.dp.13, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 14: Identified x All IMT x >$10M x EA-------------------------------------
abs.dp.14 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'EA',
         Create.Stage == 'Identified', size.smush == '>$10M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = won.tcv/total.sum.tcv)

abs.dp.14["Create.Stage"] <- "Identified"
abs.dp.14["Deal.Profile"] <- 14
abs.dp.14["IMT"] <- "All"
abs.dp.14["Service.Line"] <- "EA"
abs.dp.14["Deal.Size"] <- ">$10M"

#dp14.yields <- dcast(abs.dp.14, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 16: Validated x All IMTs x <$1M x AIC & BPS-------------------------------------
abs.dp.16 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, sl.smush, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, sl.smush == 'AIC & BPS',
         Create.Stage == 'Validated', size.smush == '<$1M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, sl.smush) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  group_by(size.smush, sl.smush, mo.bin) %>%
  mutate(yield = won.tcv/(total.sum.tcv + 4.575))

abs.dp.16["Create.Stage"] <- "Validated"
abs.dp.16["Deal.Profile"] <- 16
abs.dp.16["IMT"] <- "All"
abs.dp.16["Service.Line"] <- "AIC & BPS"
abs.dp.16["Deal.Size"] <- "<$1M"

#dp16.yields <- dcast(abs.dp.16, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 19: Validated x All IMTs x <$1M x Digital-------------------------------------
abs.dp.19 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'Digital',
         Create.Stage == 'Validated', size.smush == '<$1M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = won.tcv/(total.sum.tcv + 1.350))

abs.dp.19["Create.Stage"] <- "Validated"
abs.dp.19["Deal.Profile"] <- 19
abs.dp.19["IMT"] <- "All"
abs.dp.19["Service.Line"] <- "Digital"
abs.dp.19["Deal.Size"] <- "<$1M"

#dp19.yields <- dcast(abs.dp.19, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 22: Validated x All IMTs x <$1M x EA-------------------------------------
abs.dp.22 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'EA',
         Create.Stage == 'Validated', size.smush == '<$1M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = won.tcv/(total.sum.tcv + .630))

abs.dp.22["Create.Stage"] <- "Validated"
abs.dp.22["Deal.Profile"] <- 22
abs.dp.22["IMT"] <- "All"
abs.dp.22["Service.Line"] <- "EA"
abs.dp.22["Deal.Size"] <- "<$1M"

#dp22.yields <- dcast(abs.dp.22, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 24: Validated x All IMTs x $1M to <$10M x AIC & BPS-------------------------------------
abs.dp.24 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, sl.smush, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, sl.smush == 'AIC & BPS',
         Create.Stage == 'Validated', size.smush == '$1M to <$10M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, sl.smush) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  group_by(size.smush, sl.smush, mo.bin) %>%
  mutate(yield = won.tcv/(total.sum.tcv + 8.431))

abs.dp.24["Create.Stage"] <- "Validated"
abs.dp.24["Deal.Profile"] <- 24
abs.dp.24["IMT"] <- "All"
abs.dp.24["Service.Line"] <- "AIC & BPS"
abs.dp.24["Deal.Size"] <- "$1M to <$10M"

#dp24.yields <- dcast(abs.dp.24, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 27: Validated x All IMTs x $1M to <$10M x Digital-------------------------------------
abs.dp.27 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'Digital',
         Create.Stage == 'Validated', size.smush == '$1M to <$10M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = won.tcv/(total.sum.tcv + 3.500))

abs.dp.27["Create.Stage"] <- "Validated"
abs.dp.27["Deal.Profile"] <- 27
abs.dp.27["IMT"] <- "All"
abs.dp.27["Service.Line"] <- "Digital"
abs.dp.27["Deal.Size"] <- "$1M to <$10M"

#dp27.yields <- dcast(abs.dp.27, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 30: Validated x All IMTs x $1M to <$10M x EA-------------------------------------
abs.dp.30 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'EA',
         Create.Stage == 'Validated', size.smush == '$1M to <$10M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = won.tcv/(total.sum.tcv + 14.067))

abs.dp.30["Create.Stage"] <- "Validated"
abs.dp.30["Deal.Profile"] <- 30
abs.dp.30["IMT"] <- "All"
abs.dp.30["Service.Line"] <- "EA"
abs.dp.30["Deal.Size"] <- "$1M to <$10M"

#dp30.yields <- dcast(abs.dp.30, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 32: Validated x All IMTs x >$10M x AIC & BPS-------------------------------------
abs.dp.32 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, sl.smush, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, sl.smush == 'AIC & BPS',
         Create.Stage == 'Validated', size.smush == '>$10M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, sl.smush) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  group_by(size.smush, sl.smush, mo.bin) %>%
  mutate(yield = won.tcv/total.sum.tcv)

abs.dp.32["Create.Stage"] <- "Validated"
abs.dp.32["Deal.Profile"] <- 32
abs.dp.32["IMT"] <- "All"
abs.dp.32["Service.Line"] <- "AIC & BPS"
abs.dp.32["Deal.Size"] <- ">$10M"

#dp32.yields <- dcast(abs.dp.32, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 33: Validated x All IMTs x >$10M x Digital-------------------------------------
abs.dp.33 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'Digital',
         Create.Stage == 'Validated', size.smush == '>$10M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = won.tcv/total.sum.tcv)

abs.dp.33["Create.Stage"] <- "Validated"
abs.dp.33["Deal.Profile"] <- 33
abs.dp.33["IMT"] <- "All"
abs.dp.33["Service.Line"] <- "Digital"
abs.dp.33["Deal.Size"] <- ">$10M"

#dp33.yields <- dcast(abs.dp.33, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 34: Validated x All IMTs x >$10M x EA-------------------------------------
abs.dp.34 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'EA',
         Create.Stage == 'Validated', size.smush == '>$10M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = won.tcv/total.sum.tcv)

abs.dp.34["Create.Stage"] <- "Validated"
abs.dp.34["Deal.Profile"] <- 34
abs.dp.34["IMT"] <- "All"
abs.dp.34["Service.Line"] <- "EA"
abs.dp.34["Deal.Size"] <- ">$10M"

#dp34.yields <- dcast(abs.dp.34, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 36: Qualified x All IMTs x <$1M x AIC & BPS-------------------------------------
abs.dp.36 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, sl.smush, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, sl.smush == 'AIC & BPS',
         Create.Stage == 'Qualified', size.smush == '<$1M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, sl.smush) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  group_by(size.smush, sl.smush, mo.bin) %>%
  mutate(yield = won.tcv/(total.sum.tcv + .152))

abs.dp.36["Create.Stage"] <- "Qualified"
abs.dp.36["Deal.Profile"] <- 36
abs.dp.36["IMT"] <- "All"
abs.dp.36["Service.Line"] <- "AIC & BPS"
abs.dp.36["Deal.Size"] <- "<$1M"

#dp36.yields <- dcast(abs.dp.36, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 39: Qualified x All IMTs x <$1M x Digital-------------------------------------
abs.dp.39 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'Digital',
         Create.Stage == 'Qualified', size.smush == '<$1M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = won.tcv/total.sum.tcv)

abs.dp.39["Create.Stage"] <- "Qualified"
abs.dp.39["Deal.Profile"] <- 39
abs.dp.39["IMT"] <- "All"
abs.dp.39["Service.Line"] <- "Digital"
abs.dp.39["Deal.Size"] <- "<$1M"

#dp39.yields <- dcast(abs.dp.39, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 42: Qualified x All IMTs x <$1M x EA-------------------------------------
abs.dp.42 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'EA',
         Create.Stage == 'Qualified', size.smush == '<$1M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = won.tcv/(total.sum.tcv + .667))

abs.dp.42["Create.Stage"] <- "Qualified"
abs.dp.42["Deal.Profile"] <- 42
abs.dp.42["IMT"] <- "All"
abs.dp.42["Service.Line"] <- "EA"
abs.dp.42["Deal.Size"] <- "<$1M"

#dp42.yields <- dcast(abs.dp.42, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 44: Qualified x All IMTs x >$1M x AIC & BPS-------------------------------------
abs.dp.44 <- tbl_df(closed.pipe) %>%
  group_by(size.smush.2, sl.smush, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, sl.smush == 'AIC & BPS',
         Create.Stage == 'Qualified', size.smush.2 == '>$1M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush.2, sl.smush) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  group_by(size.smush.2, sl.smush, mo.bin) %>%
  mutate(yield = won.tcv/total.sum.tcv)

abs.dp.44["Create.Stage"] <- "Qualified"
abs.dp.44["Deal.Profile"] <- 44
abs.dp.44["IMT"] <- "All"
abs.dp.44["Service.Line"] <- "AIC & BPS"
abs.dp.44["Deal.Size"] <- ">$1M"

#dp44.yields <- dcast(abs.dp.44, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 45: Qualified x All IMTs x >$1M x Digital & EA-------------------------------------
abs.dp.45 <- tbl_df(closed.pipe) %>%
  group_by(size.smush.2, sl.smush, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, sl.smush == 'Digital & EA',
         Create.Stage == 'Qualified', size.smush.2 == '>$1M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush.2, sl.smush) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  group_by(size.smush.2, sl.smush, mo.bin) %>%
  mutate(yield = won.tcv/(total.sum.tcv + 7.000))

abs.dp.45["Create.Stage"] <- "Qualified"
abs.dp.45["Deal.Profile"] <- 45
abs.dp.45["IMT"] <- "All"
abs.dp.45["Service.Line"] <- "Digital & EA"
abs.dp.45["Deal.Size"] <- ">$1M"

#dp45.yields <- dcast(abs.dp.45, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')

#Binding Deal Profiles
abs.bind <- rbind(abs.dp.1, abs.dp.2, abs.dp.3, abs.dp.4, abs.dp.5, abs.dp.6, abs.dp.7, abs.dp.9,
                  abs.dp.10, abs.dp.13, abs.dp.14, abs.dp.16, abs.dp.19, abs.dp.22, abs.dp.24,
                  abs.dp.27, abs.dp.30, abs.dp.32, abs.dp.33, abs.dp.34, abs.dp.36, abs.dp.39,
                  abs.dp.42, abs.dp.44, abs.dp.45)



#Expanded Curves to DPUID
dpuid.dp.match <- read.csv("~/NA GBS Strategic Work/Data Sources/dpuid.dp.match.csv")
dpuid.dp.match$DP.mobin <- with(dpuid.dp.match, paste(AGDPID, mo.bin, sep = " "))
abs.bind$DP.mobin <- with(abs.bind, paste(Deal.Profile, mo.bin, sep = " "))

dpuid.dp.match$yield <- abs.bind$yield[match(dpuid.dp.match$DP.mobin, abs.bind$DP.mobin)]
dpuid.dp.match$yield[is.na(dpuid.dp.match$yield)] <- 0

abs.yields <-dcast(dpuid.dp.match, DPUID + AGDPID ~ mo.bin, value.var = c('yield'))


#Curve smoothing
#for(i in 1:length(abs.yields.2$DPUID)){
 # for(j in 5:length(abs.yields.2)-1){
#
 #   if(abs.yields.2[i,j] == 0 & abs.yields.2[i,j-1] != 0){
  #    find(seq(abs.yields.2[i,j] != 0 & abs.yields.2[i,j+1] != 0))

   #   abs.yields.2[i,j] <- (abs.yields.2[i,j+1] + abs.yields.2[i,j-1] + abs.yields.2[i,j]) / 3
    #  abs.yields.2[i,j+1] <- (abs.yields.2[i,j+1] + abs.yields.2[i,j-1] + abs.yields.2[i,j]) / 3
     # abs.yields.2[i,j-1] <- (abs.yields.2[i,j+1] + abs.yields.2[i,j-1] + abs.yields.2[i,j]) / 3

    #}

#  }

#}

write.csv(abs.yields.2, "smoothed yields.csv")

#Relative Won Yield Curves-----------------------------------------------------------------------------
#Deal Profile 1: Identified x All IMT x <$1M x AIC-------------------------------------
rel.w.dp.1 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'AIC',
         Create.Stage == 'Identified', size.smush == '<$1M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = won.tcv/((total.sum.tcv + 4.201) - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.w.dp.1["Deal.Profile"] <- 1
rel.w.dp.1["IMT"] <- "All"
rel.w.dp.1["Service.Line"] <- "AIC"
rel.w.dp.1["Deal.Size"] <- "<$1M"
rel.w.dp.1["Create.Stage"] <- "Identified"


#dp1.yields <- dcast(rel.w.dp.1, Deal.Profile + IMT + Service.Line +
#                      Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 2: Identified x All IMT x <$1M x Digital-------------------------------------
rel.w.dp.2 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'Digital',
         Create.Stage == 'Identified', size.smush == '<$1M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = won.tcv/((total.sum.tcv +8.891) - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.w.dp.2["Deal.Profile"] <- 2
rel.w.dp.2["IMT"] <- "All"
rel.w.dp.2["Service.Line"] <- "Digital"
rel.w.dp.2["Deal.Size"] <- "<$1M"
rel.w.dp.2["Create.Stage"] <- "Identified"


#dp2.yields <- dcast(rel.w.dp.2, Deal.Profile + IMT + Service.Line +
#                      Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 3: Identified x All IMT x <$1M x EA-------------------------------------
rel.w.dp.3 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'EA',
         Create.Stage == 'Identified', size.smush == '<$1M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = won.tcv/((total.sum.tcv + 2.127) - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.w.dp.3["Create.Stage"] <- "Identified"
rel.w.dp.3["Deal.Profile"] <- 3
rel.w.dp.3["IMT"] <- "All"
rel.w.dp.3["Service.Line"] <- "EA"
rel.w.dp.3["Deal.Size"] <- "<$1M"

#dp3.yields <- dcast(rel.w.dp.3, Deal.Profile + IMT + Service.Line +
#                      Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 4: Identified x All IMT x <$1M x BPS-------------------------------------
rel.w.dp.4 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'BPS',
         Create.Stage == 'Identified', size.smush == '<$1M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = won.tcv/((total.sum.tcv + .666) - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.w.dp.4["Create.Stage"] <- "Identified"
rel.w.dp.4["Deal.Profile"] <- 4
rel.w.dp.4["IMT"] <- "All"
rel.w.dp.4["Service.Line"] <- "BPS"
rel.w.dp.4["Deal.Size"] <- "<$1M"

#dp4.yields <- dcast(rel.w.dp.4, Deal.Profile + IMT + Service.Line +
#                      Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 5: Identified x All IMT x $1M to <$10M x AIC-------------------------------------
rel.w.dp.5 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'AIC',
         Create.Stage == 'Identified', size.smush == '$1M to <$10M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = won.tcv/((total.sum.tcv + 50.399) - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.w.dp.5["Create.Stage"] <- "Identified"
rel.w.dp.5["Deal.Profile"] <- 5
rel.w.dp.5["IMT"] <- "All"
rel.w.dp.5["Service.Line"] <- "AIC"
rel.w.dp.5["Deal.Size"] <- "$1M to <$10M"

#dp5.yields <- dcast(rel.w.dp.5, Deal.Profile + IMT + Service.Line +
#                      Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 6: Identified x All IMT x $1M to <$10M x Digital-------------------------------------
rel.w.dp.6 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'Digital',
         Create.Stage == 'Identified', size.smush == '$1M to <$10M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = won.tcv/((total.sum.tcv + 46.852) - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.w.dp.6["Create.Stage"] <- "Identified"
rel.w.dp.6["Deal.Profile"] <- 6
rel.w.dp.6["IMT"] <- "All"
rel.w.dp.6["Service.Line"] <- "Digital"
rel.w.dp.6["Deal.Size"] <- "$1M to <$10M"

#dp6.yields <- dcast(rel.w.dp.6, Deal.Profile + IMT + Service.Line +
#                      Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 7: Identified x All IMTs x $1M to <$10M x EA-------------------------------------
rel.w.dp.7 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'EA',
         Create.Stage == 'Identified', size.smush == '$1M to <$10M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = won.tcv/((total.sum.tcv + 25.833) - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.w.dp.7["Create.Stage"] <- "Identified"
rel.w.dp.7["Deal.Profile"] <- 7
rel.w.dp.7["IMT"] <- "All"
rel.w.dp.7["Service.Line"] <- "EA"
rel.w.dp.7["Deal.Size"] <- "$1M to <$10M"

#dp7.yields <- dcast(rel.w.dp.7, Deal.Profile + IMT + Service.Line +
#                      Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 9: Identified x All IMT x $1M to <$10M x BPS-------------------------------------
rel.w.dp.9 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'BPS',
         Create.Stage == 'Identified', size.smush == '$1M to <$10M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = won.tcv/((total.sum.tcv + 18.500) - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.w.dp.9["Create.Stage"] <- "Identified"
rel.w.dp.9["Deal.Profile"] <- 9
rel.w.dp.9["IMT"] <- "All"
rel.w.dp.9["Service.Line"] <- "BPS"
rel.w.dp.9["Deal.Size"] <- "$1M to <$10M"

#dp9.yields <- dcast(rel.w.dp.9, Deal.Profile + IMT + Service.Line +
#                      Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 10: Identified x All IMTs x >$10M x AIC & BPS-------------------------------------
rel.w.dp.10 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, sl.smush, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, sl.smush == 'AIC & BPS',
         Create.Stage == 'Identified', size.smush == '>$10M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, sl.smush) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, sl.smush, mo.bin) %>%
  mutate(yield = won.tcv/((total.sum.tcv + 152.000) - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.w.dp.10["Create.Stage"] <- "Identified"
rel.w.dp.10["Deal.Profile"] <- 10
rel.w.dp.10["IMT"] <- "All"
rel.w.dp.10["Service.Line"] <- "AIC & BPS"
rel.w.dp.10["Deal.Size"] <- ">$10M"

#dp10.yields <- dcast(rel.w.dp.10, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 13: Identified x All IMT x >$10M x Digital-------------------------------------
rel.w.dp.13 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'Digital',
         Create.Stage == 'Identified', size.smush == '>$10M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = won.tcv/(total.sum.tcv - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.w.dp.13["Create.Stage"] <- "Identified"
rel.w.dp.13["Deal.Profile"] <- 13
rel.w.dp.13["IMT"] <- "All"
rel.w.dp.13["Service.Line"] <- "Digital"
rel.w.dp.13["Deal.Size"] <- ">$10M"

#dp13.yields <- dcast(rel.w.dp.13, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 14: Identified x All IMT x >$10M x EA-------------------------------------
rel.w.dp.14 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'EA',
         Create.Stage == 'Identified', size.smush == '>$10M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = won.tcv/(total.sum.tcv - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.w.dp.14["Create.Stage"] <- "Identified"
rel.w.dp.14["Deal.Profile"] <- 14
rel.w.dp.14["IMT"] <- "All"
rel.w.dp.14["Service.Line"] <- "EA"
rel.w.dp.14["Deal.Size"] <- ">$10M"

#dp14.yields <- dcast(rel.w.dp.14, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 16: Validated x All IMTs x <$1M x AIC & BPS-------------------------------------
rel.w.dp.16 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, sl.smush, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, sl.smush == 'AIC & BPS',
         Create.Stage == 'Validated', size.smush == '<$1M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, sl.smush) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, sl.smush, mo.bin) %>%
  mutate(yield = won.tcv/((total.sum.tcv + 4.575) - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.w.dp.16["Create.Stage"] <- "Validated"
rel.w.dp.16["Deal.Profile"] <- 16
rel.w.dp.16["IMT"] <- "All"
rel.w.dp.16["Service.Line"] <- "AIC & BPS"
rel.w.dp.16["Deal.Size"] <- "<$1M"

#dp16.yields <- dcast(rel.w.dp.16, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 19: Validated x All IMTs x <$1M x Digital-------------------------------------
rel.w.dp.19 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'Digital',
         Create.Stage == 'Validated', size.smush == '<$1M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = won.tcv/((total.sum.tcv + 1.350) - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.w.dp.19["Create.Stage"] <- "Validated"
rel.w.dp.19["Deal.Profile"] <- 19
rel.w.dp.19["IMT"] <- "All"
rel.w.dp.19["Service.Line"] <- "Digital"
rel.w.dp.19["Deal.Size"] <- "<$1M"

#dp19.yields <- dcast(rel.w.dp.19, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 22: Validated x All IMTs x <$1M x EA-------------------------------------
rel.w.dp.22 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'EA',
         Create.Stage == 'Validated', size.smush == '<$1M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = won.tcv/((total.sum.tcv + .630) - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.w.dp.22["Create.Stage"] <- "Validated"
rel.w.dp.22["Deal.Profile"] <- 22
rel.w.dp.22["IMT"] <- "All"
rel.w.dp.22["Service.Line"] <- "EA"
rel.w.dp.22["Deal.Size"] <- "<$1M"

#dp22.yields <- dcast(rel.w.dp.22, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 24: Validated x All IMTs x $1M to <$10M x AIC & BPS-------------------------------------
rel.w.dp.24 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, sl.smush, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, sl.smush == 'AIC & BPS',
         Create.Stage == 'Validated', size.smush == '$1M to <$10M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, sl.smush) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, sl.smush, mo.bin) %>%
  mutate(yield = won.tcv/((total.sum.tcv + 8.431) - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.w.dp.24["Create.Stage"] <- "Validated"
rel.w.dp.24["Deal.Profile"] <- 24
rel.w.dp.24["IMT"] <- "All"
rel.w.dp.24["Service.Line"] <- "AIC & BPS"
rel.w.dp.24["Deal.Size"] <- "$1M to <$10M"

#dp24.yields <- dcast(rel.w.dp.24, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 27: Validated x All IMTs x $1M to <$10M x Digital-------------------------------------
rel.w.dp.27 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'Digital',
         Create.Stage == 'Validated', size.smush == '$1M to <$10M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = won.tcv/((total.sum.tcv + 3.500) - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.w.dp.27["Create.Stage"] <- "Validated"
rel.w.dp.27["Deal.Profile"] <- 27
rel.w.dp.27["IMT"] <- "All"
rel.w.dp.27["Service.Line"] <- "Digital"
rel.w.dp.27["Deal.Size"] <- "$1M to <$10M"

#dp27.yields <- dcast(rel.w.dp.27, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 30: Validated x All IMTs x $1M to <$10M x EA-------------------------------------
rel.w.dp.30 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'EA',
         Create.Stage == 'Validated', size.smush == '$1M to <$10M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = won.tcv/((total.sum.tcv + 14.067) - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.w.dp.30["Create.Stage"] <- "Validated"
rel.w.dp.30["Deal.Profile"] <- 30
rel.w.dp.30["IMT"] <- "All"
rel.w.dp.30["Service.Line"] <- "EA"
rel.w.dp.30["Deal.Size"] <- "$1M to <$10M"

#dp30.yields <- dcast(rel.w.dp.30, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 32: Validated x All IMTs x >$10M x AIC & BPS-------------------------------------
rel.w.dp.32 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, sl.smush, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, sl.smush == 'AIC & BPS',
         Create.Stage == 'Validated', size.smush == '>$10M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, sl.smush) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, sl.smush, mo.bin) %>%
  mutate(yield = won.tcv/(total.sum.tcv - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.w.dp.32["Create.Stage"] <- "Validated"
rel.w.dp.32["Deal.Profile"] <- 32
rel.w.dp.32["IMT"] <- "All"
rel.w.dp.32["Service.Line"] <- "AIC & BPS"
rel.w.dp.32["Deal.Size"] <- ">$10M"

#dp32.yields <- dcast(rel.w.dp.32, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 33: Validated x All IMTs x >$10M x Digital-------------------------------------
rel.w.dp.33 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'Digital',
         Create.Stage == 'Validated', size.smush == '>$10M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = won.tcv/(total.sum.tcv - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.w.dp.33["Create.Stage"] <- "Validated"
rel.w.dp.33["Deal.Profile"] <- 33
rel.w.dp.33["IMT"] <- "All"
rel.w.dp.33["Service.Line"] <- "Digital"
rel.w.dp.33["Deal.Size"] <- ">$10M"

#dp33.yields <- dcast(rel.w.dp.33, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 34: Validated x All IMTs x >$10M x EA-------------------------------------
rel.w.dp.34 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'EA',
         Create.Stage == 'Validated', size.smush == '>$10M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = won.tcv/(total.sum.tcv - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.w.dp.34["Create.Stage"] <- "Validated"
rel.w.dp.34["Deal.Profile"] <- 34
rel.w.dp.34["IMT"] <- "All"
rel.w.dp.34["Service.Line"] <- "EA"
rel.w.dp.34["Deal.Size"] <- ">$10M"

#dp34.yields <- dcast(rel.w.dp.34, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 36: Qualified x All IMTs x <$1M x AIC & BPS-------------------------------------
rel.w.dp.36 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, sl.smush, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, sl.smush == 'AIC & BPS',
         Create.Stage == 'Qualified', size.smush == '<$1M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, sl.smush) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, sl.smush, mo.bin) %>%
  mutate(yield = won.tcv/((total.sum.tcv + .152) - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.w.dp.36["Create.Stage"] <- "Qualified"
rel.w.dp.36["Deal.Profile"] <- 36
rel.w.dp.36["IMT"] <- "All"
rel.w.dp.36["Service.Line"] <- "AIC & BPS"
rel.w.dp.36["Deal.Size"] <- "<$1M"

#dp36.yields <- dcast(rel.w.dp.36, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 39: Qualified x All IMTs x <$1M x Digital-------------------------------------
rel.w.dp.39 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'Digital',
         Create.Stage == 'Qualified', size.smush == '<$1M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = won.tcv/(total.sum.tcv - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.w.dp.39["Create.Stage"] <- "Qualified"
rel.w.dp.39["Deal.Profile"] <- 39
rel.w.dp.39["IMT"] <- "All"
rel.w.dp.39["Service.Line"] <- "Digital"
rel.w.dp.39["Deal.Size"] <- "<$1M"

#dp39.yields <- dcast(rel.w.dp.39, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 42: Qualified x All IMTs x <$1M x EA-------------------------------------
rel.w.dp.42 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'EA',
         Create.Stage == 'Qualified', size.smush == '<$1M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = won.tcv/((total.sum.tcv + .667) - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.w.dp.42["Create.Stage"] <- "Qualified"
rel.w.dp.42["Deal.Profile"] <- 42
rel.w.dp.42["IMT"] <- "All"
rel.w.dp.42["Service.Line"] <- "EA"
rel.w.dp.42["Deal.Size"] <- "<$1M"

#dp42.yields <- dcast(rel.w.dp.42, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 44: Qualified x All IMTs x >$1M x AIC & BPS-------------------------------------
rel.w.dp.44 <- tbl_df(closed.pipe) %>%
  group_by(size.smush.2, sl.smush, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, sl.smush == 'AIC & BPS',
         Create.Stage == 'Qualified', size.smush.2 == '>$1M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush.2, sl.smush) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush.2, sl.smush, mo.bin) %>%
  mutate(yield = won.tcv/(total.sum.tcv - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.w.dp.44["Create.Stage"] <- "Qualified"
rel.w.dp.44["Deal.Profile"] <- 44
rel.w.dp.44["IMT"] <- "All"
rel.w.dp.44["Service.Line"] <- "AIC & BPS"
rel.w.dp.44["Deal.Size"] <- ">$1M"

#dp44.yields <- dcast(rel.w.dp.44, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 45: Qualified x All IMTs x >$1M x Digital & EA-------------------------------------
rel.w.dp.45 <- tbl_df(closed.pipe) %>%
  group_by(size.smush.2, sl.smush, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, sl.smush == 'Digital & EA',
         Create.Stage == 'Qualified', size.smush.2 == '>$1M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush.2, sl.smush) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush.2, sl.smush, mo.bin) %>%
  mutate(yield = won.tcv/((total.sum.tcv + 7.000) - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.w.dp.45["Create.Stage"] <- "Qualified"
rel.w.dp.45["Deal.Profile"] <- 45
rel.w.dp.45["IMT"] <- "All"
rel.w.dp.45["Service.Line"] <- "Digital & EA"
rel.w.dp.45["Deal.Size"] <- ">$1M"

#dp45.yields <- dcast(rel.w.dp.45, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Binding Deal Profiles----------------------------------------------------------------------------
rel.w.yields.df <- matrix(nrow = 0, ncol = 12)
colnames(rel.w.yields.df) <- c("mo.bin", "count", "total.tcv", "won.tcv", "lost.tcv", "total.sum.tcv", "yield",
                         "Deal.Profile", "IMT", "Service.Line", "Deal.Size", "Create.Stage")


rel.w.dplist <- list('rel.w.dp.1', 'rel.w.dp.2', 'rel.w.dp.3', 'rel.w.dp.4', 'rel.w.dp.5',
               'rel.w.dp.6', 'rel.w.dp.7', 'rel.w.dp.9', 'rel.w.dp.10',
               'rel.w.dp.13', 'rel.w.dp.14', 'rel.w.dp.16', 'rel.w.dp.19', 'rel.w.dp.22',
               'rel.w.dp.24', 'rel.w.dp.27', 'rel.w.dp.30', 'rel.w.dp.32',
               'rel.w.dp.33', 'rel.w.dp.34', 'rel.w.dp.36', 'rel.w.dp.39', 'rel.w.dp.42',
               'rel.w.dp.44', 'rel.w.dp.45')


for(i in 1:length(rel.w.dplist)){

  rel.w.dplist.df <- eval(parse(text = rel.w.dplist[i]))

  cols <- colnames(rel.w.dplist.df)
  match('mo.bin', cols)
  rel.w.dplist.df <- dplist.df[match('mo.bin', cols):length(cols)]

  rel.w.dplist.new <- rel.w.dplist.df[c("mo.bin", "count", "total.tcv", "won.tcv", "lost.tcv", "total.sum.tcv",
                            "yield", "Deal.Profile", "IMT", "Service.Line", "Deal.Size",
                            "Create.Stage")]

  rel.w.yields.df <- rbind(rel.w.yields.df, rel.w.dplist.new)

  print(rel.w.dplist.new)
}

rel.won.yields <- dcast(rel.w.yields.df, Deal.Profile + IMT + Service.Line +
                        Deal.Size + Create.Stage ~ mo.bin, value.var = c('yield'))



#Relative Lost Yield Curves---------------------------------------------------------------------
#Deal Profile 1: Identified x All IMT x <$1M x AIC-------------------------------------
rel.l.dp1 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'AIC',
         Create.Stage == 'Identified', size.smush == '<$1M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = lost.tcv/((total.sum.tcv + 4.201) - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.l.dp1["Deal.Profile"] <- 1
rel.l.dp1["IMT"] <- "All"
rel.l.dp1["Service.Line"] <- "AIC"
rel.l.dp1["Deal.Size"] <- "<$1M"
rel.l.dp1["Create.Stage"] <- "Identified"


#dp1.yields <- dcast(rel.l.dp1, Deal.Profile + IMT + Service.Line +
#                      Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 2: Identified x All IMT x <$1M x Digital-------------------------------------
rel.l.dp2 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'Digital',
         Create.Stage == 'Identified', size.smush == '<$1M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = lost.tcv/((total.sum.tcv +8.891) - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.l.dp2["Deal.Profile"] <- 2
rel.l.dp2["IMT"] <- "All"
rel.l.dp2["Service.Line"] <- "Digital"
rel.l.dp2["Deal.Size"] <- "<$1M"
rel.l.dp2["Create.Stage"] <- "Identified"


#dp2.yields <- dcast(rel.l.dp2, Deal.Profile + IMT + Service.Line +
#                      Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 3: Identified x All IMT x <$1M x EA-------------------------------------
rel.l.dp3 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'EA',
         Create.Stage == 'Identified', size.smush == '<$1M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = lost.tcv/((total.sum.tcv + 2.127) - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.l.dp3["Create.Stage"] <- "Identified"
rel.l.dp3["Deal.Profile"] <- 3
rel.l.dp3["IMT"] <- "All"
rel.l.dp3["Service.Line"] <- "EA"
rel.l.dp3["Deal.Size"] <- "<$1M"

#dp3.yields <- dcast(rel.l.dp3, Deal.Profile + IMT + Service.Line +
#                      Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 4: Identified x All IMT x <$1M x BPS-------------------------------------
rel.l.dp4 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'BPS',
         Create.Stage == 'Identified', size.smush == '<$1M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = lost.tcv/((total.sum.tcv + .666) - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.l.dp4["Create.Stage"] <- "Identified"
rel.l.dp4["Deal.Profile"] <- 4
rel.l.dp4["IMT"] <- "All"
rel.l.dp4["Service.Line"] <- "BPS"
rel.l.dp4["Deal.Size"] <- "<$1M"

#dp4.yields <- dcast(rel.l.dp4, Deal.Profile + IMT + Service.Line +
#                      Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 5: Identified x All IMT x $1M to <$10M x AIC-------------------------------------
rel.l.dp5 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'AIC',
         Create.Stage == 'Identified', size.smush == '$1M to <$10M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = lost.tcv/((total.sum.tcv + 50.399) - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.l.dp5["Create.Stage"] <- "Identified"
rel.l.dp5["Deal.Profile"] <- 5
rel.l.dp5["IMT"] <- "All"
rel.l.dp5["Service.Line"] <- "AIC"
rel.l.dp5["Deal.Size"] <- "$1M to <$10M"

#dp5.yields <- dcast(rel.l.dp5, Deal.Profile + IMT + Service.Line +
#                      Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 6: Identified x All IMT x $1M to <$10M x Digital-------------------------------------
rel.l.dp6 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'Digital',
         Create.Stage == 'Identified', size.smush == '$1M to <$10M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = lost.tcv/((total.sum.tcv + 46.852) - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.l.dp6["Create.Stage"] <- "Identified"
rel.l.dp6["Deal.Profile"] <- 6
rel.l.dp6["IMT"] <- "All"
rel.l.dp6["Service.Line"] <- "Digital"
rel.l.dp6["Deal.Size"] <- "$1M to <$10M"

#dp6.yields <- dcast(rel.l.dp6, Deal.Profile + IMT + Service.Line +
#                      Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 7: Identified x All IMTs x $1M to <$10M x EA-------------------------------------
rel.l.dp7 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'EA',
         Create.Stage == 'Identified', size.smush == '$1M to <$10M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = lost.tcv/((total.sum.tcv + 25.833) - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.l.dp7["Create.Stage"] <- "Identified"
rel.l.dp7["Deal.Profile"] <- 7
rel.l.dp7["IMT"] <- "All"
rel.l.dp7["Service.Line"] <- "EA"
rel.l.dp7["Deal.Size"] <- "$1M to <$10M"

#dp7.yields <- dcast(rel.l.dp7, Deal.Profile + IMT + Service.Line +
#                      Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 9: Identified x All IMT x $1M to <$10M x BPS-------------------------------------
rel.l.dp9 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'BPS',
         Create.Stage == 'Identified', size.smush == '$1M to <$10M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = lost.tcv/((total.sum.tcv + 18.500) - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.l.dp9["Create.Stage"] <- "Identified"
rel.l.dp9["Deal.Profile"] <- 9
rel.l.dp9["IMT"] <- "All"
rel.l.dp9["Service.Line"] <- "BPS"
rel.l.dp9["Deal.Size"] <- "$1M to <$10M"

#dp9.yields <- dcast(rel.l.dp9, Deal.Profile + IMT + Service.Line +
#                      Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 10: Identified x All IMTs x >$10M x AIC & BPS-------------------------------------
rel.l.dp10 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, sl.smush, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, sl.smush == 'AIC & BPS',
         Create.Stage == 'Identified', size.smush == '>$10M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, sl.smush) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, sl.smush, mo.bin) %>%
  mutate(yield = lost.tcv/((total.sum.tcv + 152.000) - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.l.dp10["Create.Stage"] <- "Identified"
rel.l.dp10["Deal.Profile"] <- 10
rel.l.dp10["IMT"] <- "All"
rel.l.dp10["Service.Line"] <- "AIC & BPS"
rel.l.dp10["Deal.Size"] <- ">$10M"

#dp10.yields <- dcast(rel.l.dp10, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 13: Identified x All IMT x >$10M x Digital-------------------------------------
rel.l.dp13 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'Digital',
         Create.Stage == 'Identified', size.smush == '>$10M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = lost.tcv/(total.sum.tcv - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.l.dp13["Create.Stage"] <- "Identified"
rel.l.dp13["Deal.Profile"] <- 13
rel.l.dp13["IMT"] <- "All"
rel.l.dp13["Service.Line"] <- "Digital"
rel.l.dp13["Deal.Size"] <- ">$10M"

#dp13.yields <- dcast(rel.l.dp13, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 14: Identified x All IMT x >$10M x EA-------------------------------------
rel.l.dp14 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'EA',
         Create.Stage == 'Identified', size.smush == '>$10M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = lost.tcv/(total.sum.tcv - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.l.dp14["Create.Stage"] <- "Identified"
rel.l.dp14["Deal.Profile"] <- 14
rel.l.dp14["IMT"] <- "All"
rel.l.dp14["Service.Line"] <- "EA"
rel.l.dp14["Deal.Size"] <- ">$10M"

#dp14.yields <- dcast(rel.l.dp14, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 16: Validated x All IMTs x <$1M x AIC & BPS-------------------------------------
rel.l.dp16 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, sl.smush, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, sl.smush == 'AIC & BPS',
         Create.Stage == 'Validated', size.smush == '<$1M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, sl.smush) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, sl.smush, mo.bin) %>%
  mutate(yield = lost.tcv/((total.sum.tcv + 4.575) - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.l.dp16["Create.Stage"] <- "Validated"
rel.l.dp16["Deal.Profile"] <- 16
rel.l.dp16["IMT"] <- "All"
rel.l.dp16["Service.Line"] <- "AIC & BPS"
rel.l.dp16["Deal.Size"] <- "<$1M"

#dp16.yields <- dcast(rel.l.dp16, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 19: Validated x All IMTs x <$1M x Digital-------------------------------------
rel.l.dp19 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'Digital',
         Create.Stage == 'Validated', size.smush == '<$1M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = lost.tcv/((total.sum.tcv + 1.350) - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.l.dp19["Create.Stage"] <- "Validated"
rel.l.dp19["Deal.Profile"] <- 19
rel.l.dp19["IMT"] <- "All"
rel.l.dp19["Service.Line"] <- "Digital"
rel.l.dp19["Deal.Size"] <- "<$1M"

#dp19.yields <- dcast(rel.l.dp19, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 22: Validated x All IMTs x <$1M x EA-------------------------------------
rel.l.dp22 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'EA',
         Create.Stage == 'Validated', size.smush == '<$1M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = lost.tcv/((total.sum.tcv + .630) - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.l.dp22["Create.Stage"] <- "Validated"
rel.l.dp22["Deal.Profile"] <- 22
rel.l.dp22["IMT"] <- "All"
rel.l.dp22["Service.Line"] <- "EA"
rel.l.dp22["Deal.Size"] <- "<$1M"

#dp22.yields <- dcast(rel.l.dp22, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 24: Validated x All IMTs x $1M to <$10M x AIC & BPS-------------------------------------
rel.l.dp24 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, sl.smush, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, sl.smush == 'AIC & BPS',
         Create.Stage == 'Validated', size.smush == '$1M to <$10M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, sl.smush) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, sl.smush, mo.bin) %>%
  mutate(yield = lost.tcv/((total.sum.tcv + 8.431) - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.l.dp24["Create.Stage"] <- "Validated"
rel.l.dp24["Deal.Profile"] <- 24
rel.l.dp24["IMT"] <- "All"
rel.l.dp24["Service.Line"] <- "AIC & BPS"
rel.l.dp24["Deal.Size"] <- "$1M to <$10M"

#dp24.yields <- dcast(rel.l.dp24, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 27: Validated x All IMTs x $1M to <$10M x Digital-------------------------------------
rel.l.dp27 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'Digital',
         Create.Stage == 'Validated', size.smush == '$1M to <$10M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = lost.tcv/((total.sum.tcv + 3.500) - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.l.dp27["Create.Stage"] <- "Validated"
rel.l.dp27["Deal.Profile"] <- 27
rel.l.dp27["IMT"] <- "All"
rel.l.dp27["Service.Line"] <- "Digital"
rel.l.dp27["Deal.Size"] <- "$1M to <$10M"

#dp27.yields <- dcast(rel.l.dp27, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 30: Validated x All IMTs x $1M to <$10M x EA-------------------------------------
rel.l.dp30 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'EA',
         Create.Stage == 'Validated', size.smush == '$1M to <$10M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = lost.tcv/((total.sum.tcv + 14.067) - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.l.dp30["Create.Stage"] <- "Validated"
rel.l.dp30["Deal.Profile"] <- 30
rel.l.dp30["IMT"] <- "All"
rel.l.dp30["Service.Line"] <- "EA"
rel.l.dp30["Deal.Size"] <- "$1M to <$10M"

#dp30.yields <- dcast(rel.l.dp30, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 32: Validated x All IMTs x >$10M x AIC & BPS-------------------------------------
rel.l.dp32 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, sl.smush, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, sl.smush == 'AIC & BPS',
         Create.Stage == 'Validated', size.smush == '>$10M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, sl.smush) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, sl.smush, mo.bin) %>%
  mutate(yield = lost.tcv/(total.sum.tcv - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.l.dp32["Create.Stage"] <- "Validated"
rel.l.dp32["Deal.Profile"] <- 32
rel.l.dp32["IMT"] <- "All"
rel.l.dp32["Service.Line"] <- "AIC & BPS"
rel.l.dp32["Deal.Size"] <- ">$10M"

#dp32.yields <- dcast(rel.l.dp32, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 33: Validated x All IMTs x >$10M x Digital-------------------------------------
rel.l.dp33 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'Digital',
         Create.Stage == 'Validated', size.smush == '>$10M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = lost.tcv/(total.sum.tcv - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.l.dp33["Create.Stage"] <- "Validated"
rel.l.dp33["Deal.Profile"] <- 33
rel.l.dp33["IMT"] <- "All"
rel.l.dp33["Service.Line"] <- "Digital"
rel.l.dp33["Deal.Size"] <- ">$10M"

#dp33.yields <- dcast(rel.l.dp33, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 34: Validated x All IMTs x >$10M x EA-------------------------------------
rel.l.dp34 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'EA',
         Create.Stage == 'Validated', size.smush == '>$10M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = lost.tcv/(total.sum.tcv - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.l.dp34["Create.Stage"] <- "Validated"
rel.l.dp34["Deal.Profile"] <- 34
rel.l.dp34["IMT"] <- "All"
rel.l.dp34["Service.Line"] <- "EA"
rel.l.dp34["Deal.Size"] <- ">$10M"

#dp34.yields <- dcast(rel.l.dp34, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 36: Qualified x All IMTs x <$1M x AIC & BPS-------------------------------------
rel.l.dp36 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, sl.smush, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, sl.smush == 'AIC & BPS',
         Create.Stage == 'Qualified', size.smush == '<$1M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, sl.smush) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, sl.smush, mo.bin) %>%
  mutate(yield = lost.tcv/((total.sum.tcv + .152) - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.l.dp36["Create.Stage"] <- "Qualified"
rel.l.dp36["Deal.Profile"] <- 36
rel.l.dp36["IMT"] <- "All"
rel.l.dp36["Service.Line"] <- "AIC & BPS"
rel.l.dp36["Deal.Size"] <- "<$1M"

#dp36.yields <- dcast(rel.l.dp36, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 39: Qualified x All IMTs x <$1M x Digital-------------------------------------
rel.l.dp39 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'Digital',
         Create.Stage == 'Qualified', size.smush == '<$1M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = lost.tcv/(total.sum.tcv - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.l.dp39["Create.Stage"] <- "Qualified"
rel.l.dp39["Deal.Profile"] <- 39
rel.l.dp39["IMT"] <- "All"
rel.l.dp39["Service.Line"] <- "Digital"
rel.l.dp39["Deal.Size"] <- "<$1M"

#dp39.yields <- dcast(rel.l.dp39, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 42: Qualified x All IMTs x <$1M x EA-------------------------------------
rel.l.dp42 <- tbl_df(closed.pipe) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, Service.Line == 'EA',
         Create.Stage == 'Qualified', size.smush == '<$1M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush, Service.Line) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush, Service.Line, mo.bin) %>%
  mutate(yield = lost.tcv/((total.sum.tcv + .667) - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.l.dp42["Create.Stage"] <- "Qualified"
rel.l.dp42["Deal.Profile"] <- 42
rel.l.dp42["IMT"] <- "All"
rel.l.dp42["Service.Line"] <- "EA"
rel.l.dp42["Deal.Size"] <- "<$1M"

#dp42.yields <- dcast(rel.l.dp42, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 44: Qualified x All IMTs x >$1M x AIC & BPS-------------------------------------
rel.l.dp44 <- tbl_df(closed.pipe) %>%
  group_by(size.smush.2, sl.smush, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, sl.smush == 'AIC & BPS',
         Create.Stage == 'Qualified', size.smush.2 == '>$1M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush.2, sl.smush) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush.2, sl.smush, mo.bin) %>%
  mutate(yield = lost.tcv/(total.sum.tcv - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.l.dp44["Create.Stage"] <- "Qualified"
rel.l.dp44["Deal.Profile"] <- 44
rel.l.dp44["IMT"] <- "All"
rel.l.dp44["Service.Line"] <- "AIC & BPS"
rel.l.dp44["Deal.Size"] <- ">$1M"

#dp44.yields <- dcast(rel.l.dp44, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Deal Profile 45: Qualified x All IMTs x >$1M x Digital & EA-------------------------------------
rel.l.dp45 <- tbl_df(closed.pipe) %>%
  group_by(size.smush.2, sl.smush, mo.bin) %>%
  filter(Won.Lost.Year == 2015 | Won.Lost.Year == 2016, sl.smush == 'Digital & EA',
         Create.Stage == 'Qualified', size.smush.2 == '>$1M') %>%
  summarise(count = length(unique(Opp.No)),
            total.tcv = sum(TCV/1e6, na.rm = T),
            won.tcv = sum(TCV[Open.Won.Lost == 'Won']/1e6, na.rm = T),
            lost.tcv = sum(TCV[Open.Won.Lost == 'Lost']/1e6, na.rm = T)) %>%
  group_by(size.smush.2, sl.smush) %>%
  mutate(total.sum.tcv = sum(total.tcv)) %>%
  mutate(lost.tcv.lag = lag(lost.tcv, n = 1), won.tcv.lag = lag(won.tcv, n = 1),
         lost.tcv.lag = replace(lost.tcv.lag, is.na(lost.tcv.lag), 0),
         won.tcv.lag = replace(won.tcv.lag, is.na(won.tcv.lag), 0),
         lost.tcv.lag.sum = cumsum(lost.tcv.lag), won.tcv.lag.sum = cumsum(won.tcv.lag)) %>%
  group_by(size.smush.2, sl.smush, mo.bin) %>%
  mutate(yield = lost.tcv/((total.sum.tcv + 7.000) - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.l.dp45["Create.Stage"] <- "Qualified"
rel.l.dp45["Deal.Profile"] <- 45
rel.l.dp45["IMT"] <- "All"
rel.l.dp45["Service.Line"] <- "Digital & EA"
rel.l.dp45["Deal.Size"] <- ">$1M"

#dp45.yields <- dcast(rel.l.dp45, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Binding Deal Profiles----------------------------------------------------------------------------
rel.l.yields.df <- matrix(nrow = 0, ncol = 12)
colnames(rel.l.yields.df) <- c("mo.bin", "count", "total.tcv", "won.tcv", "lost.tcv", "total.sum.tcv", "yield",
                         "Deal.Profile", "IMT", "Service.Line", "Deal.Size", "Create.Stage")


rel.l.dplist <- list('rel.l.dp1', 'rel.l.dp2', 'rel.l.dp3', 'rel.l.dp4', 'rel.l.dp5',
               'rel.l.dp6', 'rel.l.dp7', 'rel.l.dp9', 'rel.l.dp10',
               'rel.l.dp13', 'rel.l.dp14', 'rel.l.dp16', 'rel.l.dp19', 'rel.l.dp22',
               'rel.l.dp24', 'rel.l.dp27', 'rel.l.dp30', 'rel.l.dp32',
               'rel.l.dp33', 'rel.l.dp34', 'rel.l.dp36', 'rel.l.dp39', 'rel.l.dp42',
               'rel.l.dp44', 'rel.l.dp45')


for(i in 1:length(rel.l.dplist)){

  rel.l.dplist.df <- eval(parse(text = rel.l.dplist[i]))

  cols <- colnames(rel.l.dplist.df)
  match('mo.bin', cols)
  rel.l.dplist.df <- rel.l.dplist.df[match('mo.bin', cols):length(cols)]

  rel.l.dplist.new <- rel.l.dplist.df[c("mo.bin", "count", "total.tcv", "won.tcv", "lost.tcv", "total.sum.tcv",
                            "yield", "Deal.Profile", "IMT", "Service.Line", "Deal.Size",
                            "Create.Stage")]

  rel.l.yields.df <- rbind(rel.l.yields.df, rel.l.dplist.new)

  print(rel.l.dplist.new)
}

rel.lost.yields <- dcast(rel.l.yields.df, Deal.Profile + IMT + Service.Line +
                        Deal.Size + Create.Stage ~ mo.bin, value.var = c('yield'))



#OI Heat Map------------------------------------------------------------------------------------
setwd("~/NA GBS Strategic Work/Data Sources/Open Pipeline/Heat Map")

OI.files <- list.files(pattern = ".csv")
directory.path <- "~/NA GBS Strategic Work/Data Sources/Open Pipeline/Heat Map"

open.df <- matrix(nrow = 0, ncol = 13)
colnames(open.df) <- c("Opp.No", "Brand.Sub.Group", "IMT", "GBS.Bus.Unit.Level.2", "Industry",
                       "SSM.Step.Name", "Previous.Sales.Stage", "tcv", "created", "source.date",
                       "create.stage", "create.month", "create.year")
library(lubridate)
library(dplyr)

for(i in 1:length(OI.files)){

source.date <- substr(OI.files[i], nchar(OI.files[i]) - 11, nchar(OI.files[i]) - 4)
source.date <- dmy(source.date)
directory <- paste(directory.path, OI.files[i], sep = "/")
single.file <- read.csv(directory)

single.file$Opp.Create.Date <- with(single.file, as.Date(Opp.Create.Date, format = "%m/%d/%Y"))
single.file$Opp.Create.Date <- with(single.file, ymd(Opp.Create.Date))

roll.up <- dplyr::tbl_df(single.file) %>%
  group_by(Opp.No, Brand.Sub.Group, IMT, GBS.Bus.Unit.Level.2, Industry,
           SSM.Step.Name, Previous.Sales.Stage) %>%
  summarise(tcv = sum(Rev.Signings.Value...K.),
            created = min(Opp.Create.Date)) %>%
  filter(source.date-days(7) <= created)

roll.up$source.date <- source.date
roll.up$create.month <- with(roll.up, format(created, "%m"))
roll.up$create.year <- with(roll.up, format(created, "%Y"))


open.df <- rbind(roll.up, open.df)


}

setwd("~/NA GBS Strategic Work/R Content/Outputs")

OI.heat.map <- open.df

OI.heat.map$create.stage <- with(OI.heat.map,
                    ifelse(Previous.Sales.Stage == '*', as.character(SSM.Step.Name),
                      ifelse(Previous.Sales.Stage == 1 | Previous.Sales.Stage == 2 |
                            Previous.Sales.Stage == 3, 'Identified',
                          ifelse(SSM.Step.Name == 'Identified' &
                               Previous.Sales.Stage != '*', 'Identified',
                           ifelse(SSM.Step.Name == 'Validated' &
                                  Previous.Sales.Stage != '*', 'Identified',
                              ifelse(SSM.Step.Name == 'Qualified' &
                                   Previous.Sales.Stage != '*', 'Identified',
                               ifelse(SSM.Step.Name == 'Conditional Agreement' &
                                    Previous.Sales.Stage != '*' & Previous.Sales.Stage != 1 &
                                      Previous.Sales.Stage != 2 & Previous.Sales.Stage != 3, 'Validated', "")))))))

OI.heat.map$create.stage.2 <- with(OI.heat.map, ifelse(create.stage == 'Identified', 'Early Stage',
                                                'Validated or later'))

OI.heat.map <-subset(OI.heat.map, SSM.Step.Name != 'Won')
OI.heat.map <-subset(OI.heat.map, create.stage != 'Won')

OI.heat.map$deal.size <- with(OI.heat.map,
                               factor(ifelse(tcv < 1000, '<$1M',
                                             ifelse(tcv >= 1000 & tcv <5000, '$1M to <$5M',
                                                    ifelse(tcv >= 5000 & tcv < 10000, '$5M to <$10M',
                                                           ifelse(tcv >= 10000, '>$10M', NA))))))

OI.heat.map$service.line <- ifelse(OI.heat.map$Brand.Sub.Group == 'AD&I' | OI.heat.map$Brand.Sub.Group == 'AD&F',
                          'AIC', ifelse(OI.heat.map$Brand.Sub.Group == 'BPS', 'BPS',
                                        ifelse(OI.heat.map$Brand.Sub.Group == 'Digital', 'Digital',
                                               ifelse(OI.heat.map$Brand.Sub.Group == 'EA', 'EA', OI.heat.map$Brand.Sub.Group))))

OI.heat.map$sector <- with(OI.heat.map,
                         ifelse(GBS.Bus.Unit.Level.2 == 'US-FSS', 'US FSS',
                           ifelse(GBS.Bus.Unit.Level.2 == 'US-DIST', 'US DIST',
                             ifelse(GBS.Bus.Unit.Level.2 == 'US-COMM', 'US COMM',
                              ifelse(GBS.Bus.Unit.Level.2 == 'US-IND', 'US IND',
                               ifelse(GBS.Bus.Unit.Level.2 == 'US-PUB', 'US PUB',
                                ifelse(GBS.Bus.Unit.Level.2 == 'Canada', 'Canada',
                                 ifelse(GBS.Bus.Unit.Level.2 == 'US Federal', 'US Federal', ""))))))))

OI.heat.map.2 <- dplyr::tbl_df(OI.heat.map) %>%
  group_by(sector, create.month, create.stage.2) %>%
  summarise(tcv.m = sum(tcv)/1000) %>%
  filter(sector != "")


colnames(OI.heat.map.2)[1] <- "sector"
OI.heat.map.2$create.month <- with(OI.heat.map.2, as.numeric(create.month))
OI.heat.map.3 <- subset(OI.heat.map.2, create.month > 05)

OI.heat.map.NA <- dplyr::tbl_df(OI.heat.map.3) %>%
  group_by(create.month, create.stage) %>%
  summarise(tcv.m = sum(tcv.m))

OI.heat.map.NA$sector <- "NA"
OI.heat.map.NA <- OI.heat.map.NA[,c(4,1,2,3)]
OI.heat.map.NA <- as.data.frame(OI.heat.map.NA)
OI.heat.map.3 <- as.data.frame(OI.heat.map.3)

OI.heat.map.F <- rbind(OI.heat.map.3, OI.heat.map.NA)

#Outputs-------------------------------------------------------------------------------------
#Closed Pipe at Opp Level
write.csv(closed.pipe, "Closed Pipeline_v6.csv")

#Closed Pipe for curves
write.csv(closed.pipe.slim, "Close Pipeline_Jan2015-June2016_v6.csv")

#Open Pipe at Opp Level (open pipe model)
write.csv(open.pipe, "Open Pipeline_10.06.2016_v1.csv")

#Open Pipe at Opp Level for Monthly Create
write.csv(open.pipe.slim, "Open Pipeline Slim_9.29.2016_v1.csv")

#Avg Monthly Create Rate
write.csv(monthly.create.data, 'Monthy Create Rate Tab_09162016_v1.csv')

#Absolute Yield Curves
write.csv(abs.yields, 'Absolute Yield Curves 08-23-16 v3.csv', na = "0")

#Relative Won Yield Curves
write.csv(rel.won.yields, 'Relative Yield Curves 08-29-16 v4.csv', na = "0")

#Relative Lost Yield Curves
write.csv(rel.lost.yields, 'Relative Lost Yield Curves 08-29-16 v2.csv', na = "0")

#Heat Map
write.csv(OI.heat.map.F, 'OI heat map data_10202016_v4.csv')





#Open Pipe for model------------------------------------------------------------------------------------
library(data.table)
library(scales)
library(reshape2)
library(stringi)

#OPP LEVEL ROLLUP

#Adam's fread()
dtl.open <- read.csv("~/NA GBS Strategic Work/Data Sources/Open Pipeline/Heat Map/SMS8021 GBS NA Opportunity Detail - 4Q16 06.10.16.csv")
years.pipe <- as.data.frame(dtl.open)


new <- gsub("\\s", ".", colnames(years.pipe))
setnames(years.pipe, colnames(years.pipe), new)
rm(new)
detach("package:splitstackshape", unload=TRUE)
detach("package:data.table", unload=TRUE)
detach("package:zoo", unload=TRUE)
library(dplyr)


years.pipe$Opp.Create.Date <- with(years.pipe, as.Date(Opp.Create.Date, format = "%m/%d/%Y"))
years.pipe$S.S.Update.Date <- with(years.pipe, as.Date(S.S.Update.Date, format = "%m/%d/%Y"))
years.pipe$create.year <- format(years.pipe$Opp.Create.Date, "%Y")
years.pipe$create.month <- format(years.pipe$Opp.Create.Date, "%m")
years.pipe$tcv <- as.numeric(years.pipe$Rev.Signings.Value...K.)
years.pipe$tcv <- years.pipe$tcv * 1000
years.pipe <- subset(years.pipe, IMT != 'US Exception Mkt')
years.pipe <- subset(years.pipe, SSM.Step.Name != 'Won')

#use dplyr to rollup data to Opportunity level. Remove opps with tcv of 0 after rolling up
data.pipe <- dplyr::tbl_df(years.pipe) %>% #specify the data table to summarize
  group_by(Opp.No, Brand.Sub.Group, SSM.Step.Name, Previous.Sales.Stage,
           IMT, GBS.Bus.Unit.Level.2) %>% #specify which records/variables to keep
  summarise(tcv = sum(tcv), #define new variables using functions
            created = min(Opp.Create.Date),
            updated = max(S.S.Update.Date)) %>%
  filter(tcv > 0) #remove 0 or neg tcvs

#create size categories based on total opp val
data.pipe$deal.size <- with(data.pipe,
                            factor(ifelse(tcv < 1000000, '<$1M',
                                          ifelse(tcv >= 1000000 & tcv <5000000, '$1M to <$5M',
                                                 ifelse(tcv >= 5000000 & tcv < 10000000, '$5M to <$10M',
                                                        ifelse(tcv >= 10000000, '>$10M', NA))))))

#reorder factor levels (originally defaults to alphabetical)
data.pipe$size <- factor(data.pipe$deal.size, levels(data.pipe$deal.size)[c(1,4,2,3)])

data.pipe <- as.data.frame(data.pipe)

#fix formatting for date fields
data.pipe$created <- with(data.pipe, as.Date(created, format = "%m/%d/%Y"))
data.pipe$updated <- with(data.pipe, as.Date(updated, format = "%m/%d/%Y"))
data.pipe$create.month <- format(data.pipe$created, "%m")
data.pipe$create.year <- format(data.pipe$created, "%Y")


library(zoo)
data.pipe$create.m.yr <- as.yearmon(data.pipe$created)

# CYCLE TIME CALCULATIONS
#figure out how many weeks it took to go from created to closed
data.pipe$age.weeks <- difftime(Sys.Date(), data.pipe$created, units = c("weeks"))
data.pipe$age.weeks <- round(as.numeric(data.pipe$age.weeks), 0)

#set month bin based on 4 week intervals
data.pipe$age.month <- with(data.pipe, findInterval(age.weeks, seq(0,104000, by = 4.33))) - 1


opp.data <- data.pipe


#Flag deals that are duplicates across service lines
opp.data["dupe"] <- duplicated(opp.data$Opp.No)

#splitting up dates
opp.data$create.date <- as.Date(opp.data$created)
#opp.data$create.date <- data.frame(date = opp.data$create.date,
#                       year = as.numeric(format(opp.data$create.date, format = "%Y")))


#Changing sl field name
opp.data$new.sl <- opp.data$Brand.Sub.Group

#Combining AIC and AD&F
opp.data$new.sl <- ifelse(opp.data$new.sl == 'AIC' | opp.data$new.sl == 'AD&F',
                          'AIC', ifelse(opp.data$new.sl == 'BPS', 'BPS',
                                        ifelse(opp.data$new.sl == 'Digital', 'Digital',
                                               ifelse(opp.data$new.sl == 'EA', 'EA', NA))))


#Collapsing IMTs
opp.data$IMT.roll.up <- ifelse(opp.data$IMT == 'US Communica/CSI Mkt' |
                                 opp.data$IMT == 'US Distribution Mkt' |
                                 opp.data$IMT == 'US Finan Service Mkt' |
                                 opp.data$IMT == 'US Industrial Mkt' |
                                 opp.data$IMT == 'US Public Mkt', 'US',
                               ifelse(opp.data$IMT == 'Canada Mkt', 'Canada',
                                      ifelse(opp.data$IMT == 'US Federal Mkt', 'US Federal', NA)))


#Creating Create.Stage
opp.data$Create.Stage <- with(opp.data,
                              ifelse(Previous.Sales.Stage == '*',
                                     as.character(SSM.Step.Name),
                                     ifelse(Previous.Sales.Stage == 1 | Previous.Sales.Stage == 2 |
                                              Previous.Sales.Stage == 3, 'Identified',
                                            ifelse(SSM.Step.Name == 'Identified' &
                                                     Previous.Sales.Stage != '*', 'Identified',
                                                   ifelse(SSM.Step.Name == 'Validated' &
                                                            Previous.Sales.Stage != '*', 'Identified',
                                                          ifelse(SSM.Step.Name == 'Qualified' &
                                                                   Previous.Sales.Stage != '*', 'Identified',
                                                                 ifelse(SSM.Step.Name == 'Conditional Agreement' &
                                                                          Previous.Sales.Stage != '*' & Previous.Sales.Stage != 1 &
                                                                          Previous.Sales.Stage != 2 & Previous.Sales.Stage != 3,
                                                                        'Validated', "")))))))


opp.data$Create.Stage <- with(opp.data, ifelse(Create.Stage == 'Conditional Agreement',
                                               'Conditional', Create.Stage))
#Field Rename
opp.data$IMT <- opp.data$IMT.roll.up
opp.data$Service.Line <- opp.data$new.sl
opp.data$Sector.IMT <- opp.data$GBS.Bus.Unit.Level.2
opp.data$Sector.IMT <- with(opp.data,
                            ifelse(Sector.IMT == 'US-FSS', 'US FSS',
                                   ifelse(Sector.IMT == 'US-DIST', 'US DIST',
                                          ifelse(Sector.IMT == 'US-COMM', 'US COMM',
                                                 ifelse(Sector.IMT == 'US-IND', 'US IND',
                                                        ifelse(Sector.IMT == 'US-PUB', 'US PUB',
                                                               ifelse(Sector.IMT == 'Canada', 'Canada',
                                                                      ifelse(Sector.IMT == 'US Federal', 'US Federal', ""))))))))

#Smushing
opp.data$size.smush <- ifelse(opp.data$deal.size == '$1M to <$5M' | opp.data$deal.size == '$5M to <$10M',
                              '$1M to <$10M', ifelse(opp.data$deal.size == '<$1M', '<$1M', '>$10M'))
opp.data$size.smush.2 <- ifelse(opp.data$deal.size == '<$1M', '<$1M', '>$1M')

opp.data$sl.smush <- ifelse(opp.data$Service.Line == 'AIC' |
                              opp.data$Service.Line == 'BPS', 'AIC & BPS', 'Digital & EA')

opp.data$sl.smush.2 <- ifelse(opp.data$Service.Line == 'AIC', 'AIC', 'Digital, EA, & BPS')

opp.data$IMT.smush <- ifelse(opp.data$IMT == 'US' | opp.data$IMT == 'US Federal',
                             'US & US Federal', 'Canada')

#Deal Profile Assignment
opp.data$deal.profile <- with(opp.data,
                   ifelse(Service.Line == "AIC" & size.smush == "<$1M" & Create.Stage == "Identified", 1,
                     ifelse(Service.Line == "Digital" & size.smush == "<$1M" & Create.Stage == "Identified", 2,
                        ifelse(Service.Line == "EA" & size.smush == "<$1M" & Create.Stage == "Identified", 3,
                           ifelse(Service.Line == "BPS" & size.smush == "<$1M" & Create.Stage == "Identified", 4,
                              ifelse(Service.Line == "AIC" & size.smush == "$1M to <$10M" & Create.Stage == "Identified", 5,
                                 ifelse(Service.Line == "Digital" & size.smush == "$1M to <$10M" & Create.Stage == "Identified", 6,
                                    ifelse(Service.Line == "EA" & size.smush == "$1M to <$10M" & Create.Stage == "Identified", 7,
                                       ifelse(Service.Line == "BPS" & size.smush == "$1M to <$10M" & Create.Stage == "Identified", 9,
                      ifelse(sl.smush == "AIC & BPS" & size.smush == ">$10M" & Create.Stage == "Identified", 10,
                       ifelse(Service.Line == "Digital" & size.smush == ">$10M" & Create.Stage == "Identified", 13,
                        ifelse(Service.Line == "EA" & size.smush == ">$10M" & Create.Stage == "Identified", 14,
                         ifelse(sl.smush == "AIC & BPS" & size.smush == "<$1M" & Create.Stage == "Validated", 16,
                          ifelse(Service.Line == "Digital" & size.smush == "<$1M" & Create.Stage == "Validated", 19,
                     ifelse(Service.Line == "EA" & size.smush == "<$1M" & Create.Stage == "Validated", 22,
                      ifelse(sl.smush == "AIC & BPS" & size.smush == "$1M to <$10M" & Create.Stage == "Validated", 24,
                       ifelse(Service.Line == "Digital" & size.smush == "$1M to <$10M" & Create.Stage == "Validated", 27,
                        ifelse(Service.Line == "EA" & size.smush == "$1M to <$10M" & Create.Stage == "Validated", 30,
                         ifelse(sl.smush == "AIC & BPS" & size.smush == ">$10M" & Create.Stage == "Validated", 32,
                          ifelse(Service.Line == "Digital" & size.smush == ">$10M" & Create.Stage == "Validated", 33,
                      ifelse(Service.Line == "EA" & size.smush == ">$10M" & Create.Stage == "Validated", 34,
                      ifelse(sl.smush == "AIC & BPS" & size.smush == "<$1M" & Create.Stage == "Qualified", 36,
                       ifelse(Service.Line == "Digital" & size.smush == "<$1M" & Create.Stage == "Qualified", 39,
                        ifelse(Service.Line == "EA" & size.smush == "<$1M" & Create.Stage == "Qualified", 42,
                         ifelse(sl.smush == "AIC & BPS" & size.smush.2 == ">$1M" & Create.Stage == "Qualified", 44,
                          ifelse(sl.smush == "Digital & EA" & size.smush.2 == ">$1M" & Create.Stage == "Qualified", 45,
                            ifelse(Service.Line == "AIC" & size.smush == "<$1M" & Create.Stage == "", 1,
                         ifelse(Service.Line == "Digital" & size.smush == "<$1M" & Create.Stage == "", 2,
                          ifelse(Service.Line == "EA" & size.smush == "<$1M" & Create.Stage == "", 3,
                           ifelse(Service.Line == "BPS" & size.smush == "<$1M" & Create.Stage == "", 4,
                            ifelse(Service.Line == "AIC" & size.smush == "$1M to <$10M" & Create.Stage == "", 5,
                             ifelse(Service.Line == "Digital" & size.smush == "$1M to <$10M" & Create.Stage == "", 6,
                        ifelse(Service.Line == "EA" & size.smush == "$1M to <$10M" & Create.Stage == "", 7,
                         ifelse(Service.Line == "BPS" & size.smush == "$1M to <$10M" & Create.Stage == "", 9,
                          ifelse(sl.smush == "AIC & BPS" & size.smush == ">$10M" & Create.Stage == "", 10,
                           ifelse(Service.Line == "Digital" & size.smush == ">$10M" & Create.Stage == "", 13,
                            ifelse(Service.Line == "EA" & size.smush == ">$10M" & Create.Stage == "", 14, 0)))))))))))))))))))))))))))))))))))))

opp.data$DPUID <- with(opp.data, paste(Create.Stage, size, Service.Line, Sector.IMT,
                                       sep = ""))

opp.data$Size.Type <- with(opp.data, ifelse(size == '>$10M', 'Large', 'Small'))


opp.data$Deal.Size <- opp.data$size
opp.data$Sector.F <- opp.data$Sector.IMT
opp.data$TCV <- opp.data$tcv
opp.data["source"] <- "Open"

open.pipe <- opp.data
open.pipe.slim <- open.pipe
open.pipe.slim <- subset(open.pipe.slim, create.year == 2016)

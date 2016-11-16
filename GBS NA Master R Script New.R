#Working Directory - SCIP Server
setwd("~/gbs-na-automation")

library(XLConnect)
library(lubridate)
options(java.parameters = "-Xmx8g")

# EXCEL MODEL -------------------------------------------------------------
#load EXCEL INPUT DATA workbook

directory <- paste0(getwd(), "/Input Data - Open")
filename <- list.files(directory)

wb <- loadWorkbook(filename)
getSheets(wb)

#GET DATA FROM WORKBOOK
dtl.open <- readWorksheet(wb, 'rp_GBS_Top_Opportunity_Detail', startRow = 4)

#CLEAN UP DATES 
dtl.open$S.S.Update.Date <- ymd(data$S.S.Update.Date)
dtl.open$Opp.Create.Date <- ymd(data$Opp.Create.Date)

#save as csv
write.csv(dtl.open, file = gsub(filename, pattern = 'xlsx', replacement = 'csv'))



#Working Directory - Adam's Computer
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
dtl.closed <- fread("~/NA GBS Strategic Work/Data Sources/Closed Pipeline/EIW GBS NA 2012to2016Q3- First Stage.csv") #Edit this
years <- as.data.frame(dtl.closed)

new <- gsub("\\s", ".", colnames(years))
setnames(years, colnames(years), new)
rm(new)
detach("package:data.table", unload=TRUE)
library(dplyr)

years$Won.Lost.Year <- as.numeric(years$Won.Lost.Year)
years$Opp.Create.Date <- with(years, as.Date(Opp.Create.Date, format = "%m/%d/%Y"))
years$Opp.Win.Loss.Date <- with(years, as.Date(Opp.Win.Loss.Date, format = "%m/%d/%Y"))
years$First.Stage <- as.character(years$First.Stage)
years$Rpt.Brand.Subgroup <- as.character(years$Rpt.Brand.Subgroup)
years$Ext.Dtl.Rev.Usd <- as.numeric(years$Ext.Dtl.Rev.Usd)
years <- years[order(years$Opp.Win.Loss.Date, na.last = TRUE, decreasing = TRUE),]
years["dupe"] <- duplicated(years$Detail.Key) #assumption
years <- subset(years, dupe == FALSE)
years <- years %>% 
  filter(Edge.Region.Name != 'US Exception', Edge.Region.Name != 'US Exception Mkt') #assumption


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

#Recode Noticed and Identified as same create stage
data$First.Stage <- with(data, ifelse(First.Stage == 'Noticed', 'Identified', First.Stage))


#Recode service lines
data$new.sl <- with(data, ifelse(Rpt.Brand.Subgroup %in% c('iX&M', 'BA&S'), 'Digital',
                            ifelse(Rpt.Brand.Subgroup %in% c('C&SI', 'AD&F', 'AD&I', 'AMS SO'),
                                   'AIC', ifelse(Rpt.Brand.Subgroup == 'GPS', 'BPS', Rpt.Brand.Subgroup))))


#fix formatting for date fields
data$created <- with(data, as.Date(created, format = "%m/%d/%Y"))
data$closed <- with(data, as.Date(closed, format = "%m/%d/%Y"))

data$Edge.Region.Name <- with(data, ifelse(Edge.Region.Name == 'US East' |
                                           Edge.Region.Name == 'US West', 'US', Edge.Region.Name))

data$Sector <- with(data, ifelse(Edge.Region.Name == 'Canada' | Edge.Region.Name == 'Canada Mkt', 'Canada',
                            ifelse(Edge.Region.Name == 'US Federal' | Edge.Region.Name == 'US Federal Mkt', 'US Federal', 'US Commercial')))

#data$Industry <- with(data, ifelse(Industry == 'Aerospace&Defense', 'Aerospace & Defense',
 #                             ifelse(Industry == 'Chemicals&Petroleum', 'Chemicals & Petroleum',
  #                              ifelse(Industry == 'ConsumerPackageGoods', 'Consumer Products',
   #                               ifelse(Industry == 'Media&Entertainment', 'Media & Entertainment',
    #                          ifelse(Industry == 'TravelTransportation', 'Travel & Transportation',
     #                                ifelse(Industry == 'Unassigned', 'UNASSIGNED', Industry)))))))


#Month and year created to cast data
data$create.month <- format(data$created, "%m")
data$create.year <- format(data$created, "%Y")
data$create.year <- as.numeric(data$create.year)

library(splitstackshape)

data$freq <- with(data,
                  ifelse(Sector == 'US Commercial' & create.year < 2016, 5, 1)) #check

data.expanded <- data[rep(row.names(data), data$freq), 1:17]
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
                                                    sl.tcv)))))) #assumption
sector.data.2$Sector.Man <- with(sector.data.2,
                              ifelse(Sector == 'US Commercial' & Gbs.Sector.Name == 'Communications', 'US COMM',
                                ifelse(Sector == 'US Commercial' & Gbs.Sector.Name == 'Distribution', 'US DIST',
                                   ifelse(Sector == 'US Commercial' & Gbs.Sector.Name == 'Financial Services', 'US FSS',
                                      ifelse(Sector == 'US Commercial' & Gbs.Sector.Name == 'Industrial', 'US IND',
                                         ifelse(Sector == 'US Commercial' & Gbs.Sector.Name == 'Public', 'US PUB', 
                                            ifelse(Sector == 'Canada' | Sector == 'Canada Mkt', 'Canada',
                                              ifelse(Sector == 'US Federal' | Sector == 'US Federal Mkt', 'US Federal', "")))))))) #check

sector.data.2$new.tcv <- sector.data.2$sl.tcv
closed.pipe <- rbind(sector.data.1, sector.data.2)

# CYCLE TIME CALCULATIONS
#figure out how many weeks it took to go from created to closed
closed.pipe$weeks <- with(closed.pipe, difftime(closed, created, units = 'weeks'))
closed.pipe$weeks <- with(closed.pipe, round(as.numeric(weeks), 0))


#set month bin based on 4 week intervals up to 2 years
closed.pipe$mo.bin <- with(closed.pipe, findInterval(weeks, seq(0,104, by = 4.33))) - 1

library(lubridate)
closed.pipe$create.m.yr <- floor_date(closed.pipe$created, 'month')

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
closed.pipe$source <- with(closed.pipe, 
                        ifelse(Open.Won.Lost == 'Won', 'Won', 'Not Won'))

closed.pipe.slim <- subset(closed.pipe, Won.Lost.Year == 2016)


#won.create <- closed.pipe %>%
#  filter(create.year > 2014) %>%
#  group_by(Create.Stage, create.year, create.month) %>%
#  summarise(tcv = sum(TCV))

#write.csv(won.create, "tcv created by stage.csv")

# Open Pipe------------------------------------------------------------------------------------
library(data.table)
library(scales)
library(reshape2)
library(stringi)
library(lubridate)

# OPP LEVEL ROLLUP

#Adam's fread()
dtl.open <- read.csv("~/NA GBS Strategic Work/Data Sources/Open Pipeline/Heat Map/SMS8021 GBS NA Opportunity Detail - 3Q16 29.09.16.csv") #Edit
years.pipe <- as.data.frame(dtl.open)


new <- gsub("\\s", ".", colnames(years.pipe))
setnames(years.pipe, colnames(years.pipe), new)
rm(new)
detach("package:splitstackshape", unload=TRUE)
detach("package:data.table", unload=TRUE)


years.pipe$Opp.Create.Date <- with(years.pipe, mdy(Opp.Create.Date))
years.pipe$S.S.Update.Date <- with(years.pipe, mdy(S.S.Update.Date))
years.pipe$create.year <- format(years.pipe$Opp.Create.Date, "%Y")
years.pipe$create.month <- format(years.pipe$Opp.Create.Date, "%m")
years.pipe$tcv <- as.numeric(years.pipe$Rev.Signings.Value...K.)
years.pipe$tcv <- years.pipe$tcv * 1000
years.pipe <- subset(years.pipe, IMT != 'US Exception Mkt') #assumption


library(dplyr)

#use dplyr to rollup data to Opportunity level. Remove opps with tcv of 0 after rolling up
data.pipe <- dplyr::tbl_df(years.pipe) %>% #specify the data table to summarize
  group_by(Opp.No, Brand.Sub.Group, SSM.Step.Name, Previous.Sales.Stage,
           IMT, 
           #Industry, 
           GBS.Bus.Unit.Level.2) %>% #specify which records/variables to keep
  summarise(tcv = sum(tcv), #define new variables using functions
            created = min(Opp.Create.Date),
            #updated = max(S.S.Update.Date)
            s.s.updated = max(S.S.Update.Date)) %>%
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
data.pipe$create.month <- lubridate::month(data.pipe$created)
data.pipe$create.year <- lubridate::year(data.pipe$created)


#data.pipe$Industry <- as.character(data.pipe$Industry)

#data.pipe$Industry <- with(data.pipe, ifelse(Industry == 'Government, Central/Federal',
 #                                            'Government',
  #                                      ifelse(Industry == 'Government, State/Provincial/Local',
   #                                            'Government', Industry)))


data.pipe$create.m.yr <- floor_date(data.pipe$created, 'month')

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
opp.data$new.sl <- ifelse(opp.data$new.sl == 'AIC' | opp.data$new.sl == 'AD&F' | opp.data$new.sl == 'AD&I',
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
                                                'Validated', ""))))))) #assumption


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

open.pipe <- subset(opp.data, Sector.F != "") #assumption
#open.pipe <- subset(open.pipe, SSM.Step.Name != 'Won')
open.pipe.slim <- open.pipe
open.pipe.slim <- subset(open.pipe.slim, create.year == 2016)

#The Cube----------------------------------------------------------------------------------
save(closed.pipe, open.pipe, file = 'open and closed.saved')
load("~/NA GBS Strategic Work/R Content/Outputs/open and closed.saved")

open.pipe$TID <- with(open.pipe, paste(Opp.No, Service.Line, sep = "-"))
open.pipe.trash <- filter(open.pipe, SSM.Step.Name == 'Won' & 
                        s.s.updated <= max(closed.pipe$closed))


trash.ids <- unique(open.pipe.trash$TID)

#check by flagging match to trash id
open.pipe <- open.pipe %>% 
  mutate(trash = TID %in% trash.ids) %>%
  filter(!trash) %>%
  select(-trash)

closed.cube <- closed.pipe %>%
  select(Opp.No, DPUID, Create.Stage, Deal.Size, Sector.F, Service.Line, Size.Type, 
         #Industry, 
         TCV, create.m.yr, created, closed, mo.bin, source)

open.cube.prep <- open.pipe
open.cube.prep$mo.bin <- open.cube.prep$age.month
open.cube.prep$closed <- open.cube.prep$s.s.updated

open.cube <- open.cube.prep %>%
  select(Opp.No, DPUID, Create.Stage, Deal.Size, Sector.F, Service.Line,
         Size.Type, 
         #Industry, 
         TCV, create.m.yr, created, closed, mo.bin, source)


library(lubridate)

cube.1 <- rbind(closed.cube, open.cube)
cube.1$closed.m.yr <- floor_date(cube.1$closed, 'month')

rm(trash.ids, open.pipe.trash, open.cube.prep)



#need to create closed/created divergence here
created.cube <- dplyr::tbl_df(cube.1) %>%
  group_by(DPUID, source, create.m.yr, Size.Type) %>%
 # filter(Create.Stage != 'Closed') %>%
  summarise(Total.TCV = sum(TCV)) %>%
  mutate(unique.ID = paste(DPUID, source, Size.Type, sep = ""))

closed.cube <- dplyr::tbl_df(cube.1) %>%
  group_by(DPUID, source, closed.m.yr, Size.Type) %>%
  # filter(Create.Stage != 'Closed') %>%
  summarise(Total.TCV = sum(TCV)) %>%
  mutate(unique.ID = paste(DPUID, source, Size.Type, sep = ""))


#unique ids - doesn't matter whether you use created.cube or closed.cube
ids <- unique(as.factor(created.cube$unique.ID))

#set up time interval of dates (important to set 'from' to be the first of the month)
month.vec <- seq.Date(from = ymd('2009-12-01'),
                      to = floor_date(Sys.Date(), 'month'),
                      by = 'month')

library(data.table)

#create df with combination of both objects
month.expanded <-  CJ(ids, month.vec)

#reset col names
setnames(month.expanded, colnames(month.expanded), c('unique.ID', 'date'))

created.expanded <- left_join(month.expanded, created.cube, by = c('unique.ID', 'date' = 'create.m.yr'))
closed.expanded <- left_join(month.expanded, closed.cube, by = c('unique.ID', 'date' = 'closed.m.yr'))

created.expanded$Total.TCV[is.na(created.expanded$Total.TCV)] <- 0
closed.expanded$Total.TCV[is.na(closed.expanded$Total.TCV)] <- 0

#delete this eventually
created.saved <- created.expanded 
closed.saved <- closed.expanded

#map in ID details, set up space junk dates
created.cube <- created.expanded %>%
  filter(source == 'Open') %>%
  mutate(sj.date.s = as.character(floor_date(date - months(13), 'month')), 
         sj.date.l = as.character(floor_date(date - months(19), 'month')), 
         sj.date = ifelse(Size.Type == 'Small', sj.date.s, sj.date.l), 
         sj.date = ymd(sj.date),
         unique.date = paste(unique.ID, sj.date, sep = "-")) %>%
  select(-sj.date.l, -sj.date.s)

#left joining tcv for space junk based on sj.date
created.cube <- created.cube %>%
  left_join(created.cube %>%
              select(unique.ID, date, Space.Junk = Total.TCV), by = c("unique.ID", "sj.date" = "date"))

closed.cube <- closed.expanded %>%
  left_join(created.cube %>%
              select(unique.ID, date, Space.Junk), by = c("unique.ID", "date"))

library(tidyr)
 
#created crud
created.crud <- created.cube %>%
  select(Space.Junk, DPUID, date) %>%
  filter(!is.na(DPUID)) %>%
  group_by(DPUID, date) %>%
  summarise(Space.Junk = sum(Space.Junk, na.rm = T))

created.wide <- created.expanded %>%
  mutate(source = gsub(source, pattern = ' ', replacement = '.')) %>%
  group_by(DPUID, Size.Type, date, source) %>%
  filter(!is.na(source)) %>%
  summarise(Total.TCV = sum(Total.TCV, na.rm = T)) %>%
  ungroup() %>%
  spread(source, Total.TCV, fill = 0) %>%
  left_join(created.crud, by = c("DPUID", "date")) %>%
  mutate(Space.Junk = replace(Space.Junk, is.na(Space.Junk), 0),
         Quarter = lubridate::quarter(date, with_year = T))

#closed crud
closed.crud <- closed.cube %>%
  select(Space.Junk, DPUID, date) %>%
  filter(!is.na(DPUID)) %>%
  group_by(DPUID, date) %>%
  summarise(Space.Junk = sum(Space.Junk, na.rm = T))

closed.wide <- closed.expanded %>%
  mutate(source = gsub(source, pattern = ' ', replacement = '.')) %>%
  group_by(DPUID, Size.Type, date, source) %>%
  filter(!is.na(source)) %>%
  summarise(Total.TCV = sum(Total.TCV, na.rm = T)) %>%
  ungroup() %>%
  spread(source, Total.TCV, fill = 0) %>%
  left_join(closed.crud,
            by = c("DPUID", "date")) %>%
  mutate(Space.Junk = replace(Space.Junk, is.na(Space.Junk), 0),
         Quarter = lubridate::quarter(date, with_year = T))

rm(created.crud, closed.crud)


dpuid.map <- read.csv("~/NA GBS Strategic Work/Data Sources/dpuid.sector.match.csv") #Edit

dpuid.map$DPUID <- tolower(dpuid.map$DPUID)
created.wide$DPUID <- tolower(created.wide$DPUID)
closed.wide$DPUID <- tolower(closed.wide$DPUID)

created.wide$Sector <- dpuid.map$Sector[match(created.wide$DPUID, dpuid.map$DPUID)]
created.wide$Deal.Size <- dpuid.map$Deal.Size[match(created.wide$DPUID, dpuid.map$DPUID)]
created.wide$Create.Stage <- dpuid.map$Create.Stage[match(created.wide$DPUID, dpuid.map$DPUID)]
created.wide$Service.Line <- dpuid.map$Service.Line[match(created.wide$DPUID, dpuid.map$DPUID)]

closed.wide$Sector <- dpuid.map$Sector[match(closed.wide$DPUID, dpuid.map$DPUID)]
closed.wide$Deal.Size <- dpuid.map$Deal.Size[match(closed.wide$DPUID, dpuid.map$DPUID)]
closed.wide$Create.Stage <- dpuid.map$Create.Stage[match(closed.wide$DPUID, dpuid.map$DPUID)]
closed.wide$Service.Line <- dpuid.map$Service.Line[match(closed.wide$DPUID, dpuid.map$DPUID)]

created.cube.F <- created.wide
closed.cube.F <- closed.wide

#closed.cube.F2 <- closed.cube.F
#closed.cube.F2$Sector[is.na(closed.cube.F2$Sector)] <- ""
  
#closed.cube.Y <- closed.cube.F2 %>%
 # filter(Sector != "") %>%
  #group_by(Quarter) %>%
  #summarise(Won = sum(Won), 
   #         Not.Won = sum(Not.Won), 
    #        Open = sum(Open), 
     #       Space.Junk = sum(Space.Junk), 
      #      yield = Won / (Won + Not.Won + Space.Junk))

#write.csv(closed.cube.Y, "yield calc.csv")

#Creating monthly average create rate table--------------------------------------------------
closed.mon.crt <- subset(closed.pipe, create.year == 2016)
closed.limit <- max(closed.mon.crt$create.month)
closed.limit <- as.numeric(closed.limit)
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
  #filter(Create.Stage != 'Closed') %>%
  summarise(month.avg.tcv = sum(TCV)/9) #ADAMIANGRUBER


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
  mutate(yield = (won.tcv/(total.sum.tcv)))

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
  mutate(yield = won.tcv/(total.sum.tcv))

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
  mutate(yield = won.tcv/(total.sum.tcv))

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
  mutate(yield = won.tcv/(total.sum.tcv))

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
  mutate(yield = won.tcv/(total.sum.tcv))

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
  mutate(yield = won.tcv/(total.sum.tcv))

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
  mutate(yield = won.tcv/(total.sum.tcv))

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
  mutate(yield = won.tcv/(total.sum.tcv))

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
  mutate(yield = won.tcv/(total.sum.tcv))

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
  mutate(yield = won.tcv/(total.sum.tcv))

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
  mutate(yield = won.tcv/(total.sum.tcv))

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
  mutate(yield = won.tcv/(total.sum.tcv))

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
  mutate(yield = won.tcv/(total.sum.tcv))

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
  mutate(yield = won.tcv/(total.sum.tcv))

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
  mutate(yield = won.tcv/(total.sum.tcv))

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
  mutate(yield = won.tcv/(total.sum.tcv))

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
  mutate(yield = won.tcv/(total.sum.tcv))

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
  mutate(yield = won.tcv/(total.sum.tcv))

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

abs.yields <-dcast(abs.bind, Deal.Profile + IMT + Service.Line + Deal.Size + Create.Stage ~ mo.bin, value.var = c('yield'))
write.csv(abs.yields, "unsmoothed abs yields.csv")

unsmoothed.abs.yields <- read.csv("~/NA GBS Strategic Work/R Content/Outputs/unsmoothed abs yields.csv")

library(tidyr)
#test

abs.melt <- unsmoothed.abs.yields %>%
  gather(MonthBin, yield, X0:X24)

abs.melt$MonthBin <- sapply(strsplit(abs.melt$MonthBin, split='X', fixed=TRUE), function(x) (x[2]))

#Expanded Curves to DPUID
dpuid.dp.match <- read.csv("~/NA GBS Strategic Work/Data Sources/dpuid.dp.match.csv") #Edit
dpuid.dp.match$DP.mobin <- with(dpuid.dp.match, paste(AGDPID, mo.bin, sep = " "))
abs.melt$DP.mobin <- with(abs.melt, paste(Deal.Profile, MonthBin, sep = " "))

dpuid.dp.match$won.tcv <- abs.melt$won.tcv[match(dpuid.dp.match$DP.mobin, abs.melt$DP.mobin)]
dpuid.dp.match$total.sum.tcv <- abs.melt$total.sum.tcv[match(dpuid.dp.match$DP.mobin, abs.melt$DP.mobin)]
dpuid.dp.match$yield <- abs.melt$yield[match(dpuid.dp.match$DP.mobin, abs.melt$DP.mobin)]
dpuid.dp.match$yield[is.na(dpuid.dp.match$yield)] <- 0

#dpuid.dp.match$source <- "Space Junk"
#dpuid.dp.match$sj.anchor <- floor_date(Sys.Date(), unit = "months")
#dpuid.dp.match$sj.id <- with(dpuid.dp.match, paste(DPUID, source, sj.anchor, sept = ""))
#cube.sj <- subset(cube.2.5, source == "Space Junk")
#cube.sj$sj.id <- with(cube.sj, paste(DPUID, source, create.m.yr))
#dpuid.dp.match$space.junk <- cube.sj$Total.TCV[match(dpuid.dp.match$sj.id, cube.sj$sj.id)]
#dpuid.dp.match$yield.sj <- with(dpuid.dp.match, won.tcv/(total.sum.tcv+Total.TCV))

abs.yields.F <-dcast(dpuid.dp.match, DPUID + AGDPID + Sector + Deal.Size ~ mo.bin, value.var = c('yield'))


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
  mutate(yield = won.tcv/((total.sum.tcv) - won.tcv.lag.sum - lost.tcv.lag.sum))

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
  mutate(yield = won.tcv/((total.sum.tcv) - won.tcv.lag.sum - lost.tcv.lag.sum))

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
  mutate(yield = won.tcv/((total.sum.tcv) - won.tcv.lag.sum - lost.tcv.lag.sum))

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
  mutate(yield = won.tcv/((total.sum.tcv) - won.tcv.lag.sum - lost.tcv.lag.sum))

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
  mutate(yield = won.tcv/((total.sum.tcv) - won.tcv.lag.sum - lost.tcv.lag.sum))

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
  mutate(yield = won.tcv/((total.sum.tcv) - won.tcv.lag.sum - lost.tcv.lag.sum))

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
  mutate(yield = won.tcv/((total.sum.tcv) - won.tcv.lag.sum - lost.tcv.lag.sum))

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
  mutate(yield = won.tcv/((total.sum.tcv) - won.tcv.lag.sum - lost.tcv.lag.sum))

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
  mutate(yield = won.tcv/((total.sum.tcv) - won.tcv.lag.sum - lost.tcv.lag.sum))

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
  mutate(yield = won.tcv/((total.sum.tcv) - won.tcv.lag.sum - lost.tcv.lag.sum))

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
  mutate(yield = won.tcv/((total.sum.tcv) - won.tcv.lag.sum - lost.tcv.lag.sum))

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
  mutate(yield = won.tcv/((total.sum.tcv) - won.tcv.lag.sum - lost.tcv.lag.sum))

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
  mutate(yield = won.tcv/((total.sum.tcv) - won.tcv.lag.sum - lost.tcv.lag.sum))

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
  mutate(yield = won.tcv/((total.sum.tcv) - won.tcv.lag.sum - lost.tcv.lag.sum))

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
  mutate(yield = won.tcv/((total.sum.tcv) - won.tcv.lag.sum - lost.tcv.lag.sum))

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
  mutate(yield = won.tcv/((total.sum.tcv) - won.tcv.lag.sum - lost.tcv.lag.sum))

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
  mutate(yield = won.tcv/((total.sum.tcv) - won.tcv.lag.sum - lost.tcv.lag.sum))

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
  mutate(yield = won.tcv/((total.sum.tcv) - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.w.dp.45["Create.Stage"] <- "Qualified"
rel.w.dp.45["Deal.Profile"] <- 45
rel.w.dp.45["IMT"] <- "All"
rel.w.dp.45["Service.Line"] <- "Digital & EA"
rel.w.dp.45["Deal.Size"] <- ">$1M"

#dp45.yields <- dcast(rel.w.dp.45, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Binding Deal Profiles----------------------------------------------------------------------------
rel.w.bind <- rbind(rel.w.dp.1, rel.w.dp.2, rel.w.dp.3, rel.w.dp.4, rel.w.dp.5,
               rel.w.dp.6, rel.w.dp.7, rel.w.dp.9, rel.w.dp.10,
               rel.w.dp.13, rel.w.dp.14, rel.w.dp.16, rel.w.dp.19, rel.w.dp.22,
               rel.w.dp.24, rel.w.dp.27, rel.w.dp.30, rel.w.dp.32,
               rel.w.dp.33, rel.w.dp.34, rel.w.dp.36, rel.w.dp.39, rel.w.dp.42,
               rel.w.dp.44, rel.w.dp.45)


#dpuid.dp.match <- read.csv("~/NA GBS Strategic Work/Data Sources/dpuid.dp.match.csv") #Edit
#dpuid.dp.match$DP.mobin <- with(dpuid.dp.match, paste(AGDPID, mo.bin, sep = " "))
#rel.w.bind$DP.mobin <- with(rel.w.bind, paste(Deal.Profile, mo.bin, sep = " "))

#dpuid.dp.match$won.tcv <- rel.w.bind$won.tcv[match(dpuid.dp.match$DP.mobin, abs.bind$DP.mobin)] #left off here
#dpuid.dp.match$total.sum.tcv <- abs.bind$total.sum.tcv[match(dpuid.dp.match$DP.mobin, abs.bind$DP.mobin)]
#dpuid.dp.match$yield[is.na(dpuid.dp.match$yield)] <- 0

#dpuid.dp.match$source <- "Space Junk"
#dpuid.dp.match$sj.anchor <- floor_date(Sys.Date(), unit = "months")
#dpuid.dp.match$sj.id <- with(dpuid.dp.match, paste(DPUID, source, sj.anchor, sept = ""))
#cube.sj <- subset(cube.2.5, source == "Space Junk")
#cube.sj$sj.id <- with(cube.sj, paste(DPUID, source, create.m.yr))
#dpuid.dp.match$space.junk <- cube.sj$Total.TCV[match(dpuid.dp.match$sj.id, cube.sj$sj.id)]
#dpuid.dp.match$yield.sj <- with(dpuid.dp.match, won.tcv/(total.sum.tcv+Total.TCV))


rel.w.yields <-dcast(rel.w.bind, Deal.Profile + IMT + Service.Line + Deal.Size + Create.Stage ~ mo.bin, value.var = c('yield'))



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
  mutate(yield = lost.tcv/((total.sum.tcv) - won.tcv.lag.sum - lost.tcv.lag.sum))

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
  mutate(yield = lost.tcv/((total.sum.tcv) - won.tcv.lag.sum - lost.tcv.lag.sum))

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
  mutate(yield = lost.tcv/((total.sum.tcv) - won.tcv.lag.sum - lost.tcv.lag.sum))

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
  mutate(yield = lost.tcv/((total.sum.tcv) - won.tcv.lag.sum - lost.tcv.lag.sum))

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
  mutate(yield = lost.tcv/((total.sum.tcv) - won.tcv.lag.sum - lost.tcv.lag.sum))

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
  mutate(yield = lost.tcv/((total.sum.tcv) - won.tcv.lag.sum - lost.tcv.lag.sum))

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
  mutate(yield = lost.tcv/((total.sum.tcv) - won.tcv.lag.sum - lost.tcv.lag.sum))

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
  mutate(yield = lost.tcv/((total.sum.tcv) - won.tcv.lag.sum - lost.tcv.lag.sum))

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
  mutate(yield = lost.tcv/((total.sum.tcv) - won.tcv.lag.sum - lost.tcv.lag.sum))

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
  mutate(yield = lost.tcv/((total.sum.tcv) - won.tcv.lag.sum - lost.tcv.lag.sum))

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
  mutate(yield = lost.tcv/((total.sum.tcv) - won.tcv.lag.sum - lost.tcv.lag.sum))

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
  mutate(yield = lost.tcv/((total.sum.tcv) - won.tcv.lag.sum - lost.tcv.lag.sum))

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
  mutate(yield = lost.tcv/((total.sum.tcv) - won.tcv.lag.sum - lost.tcv.lag.sum))

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
  mutate(yield = lost.tcv/((total.sum.tcv) - won.tcv.lag.sum - lost.tcv.lag.sum))

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
  mutate(yield = lost.tcv/((total.sum.tcv) - won.tcv.lag.sum - lost.tcv.lag.sum))

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
  mutate(yield = lost.tcv/((total.sum.tcv) - won.tcv.lag.sum - lost.tcv.lag.sum))

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
  mutate(yield = lost.tcv/((total.sum.tcv) - won.tcv.lag.sum - lost.tcv.lag.sum))

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
  mutate(yield = lost.tcv/((total.sum.tcv) - won.tcv.lag.sum - lost.tcv.lag.sum))

rel.l.dp45["Create.Stage"] <- "Qualified"
rel.l.dp45["Deal.Profile"] <- 45
rel.l.dp45["IMT"] <- "All"
rel.l.dp45["Service.Line"] <- "Digital & EA"
rel.l.dp45["Deal.Size"] <- ">$1M"

#dp45.yields <- dcast(rel.l.dp45, Deal.Profile + IMT + Service.Line +
#                       Deal.Size + Create.Stage ~ mo.bin, value.var = 'yield')


#Binding Deal Profiles----------------------------------------------------------------------------
rel.l.dplist <- rbind(rel.l.dp1, rel.l.dp2, rel.l.dp3, rel.l.dp4, rel.l.dp5,
               rel.l.dp6, rel.l.dp7, rel.l.dp9, rel.l.dp10,
               rel.l.dp13, rel.l.dp14, rel.l.dp16, rel.l.dp19, rel.l.dp22,
               rel.l.dp24, rel.l.dp27, rel.l.dp30, rel.l.dp32,
               rel.l.dp33, rel.l.dp34, rel.l.dp36, rel.l.dp39, rel.l.dp42,
               rel.l.dp44, rel.l.dp45)

rel.l.yields <-dcast(rel.l.dplist, Deal.Profile + IMT + Service.Line + Deal.Size + Create.Stage ~ mo.bin, value.var = c('yield'))


#Outputs-------------------------------------------------------------------------------------
#Closed Pipe at Opp Level
write.csv(closed.pipe, "Closed Pipeline_v8.csv")

#Closed Pipe for curves
write.csv(closed.pipe.slim, "Close Pipeline_Jan2015-June2016_v6.csv")

#Open Pipe at Opp Level (cube)
write.csv(open.pipe, "Open Pipeline_11.11.2016_v2.csv")

#Open Pipe at Opp Level (open pipe model)
write.csv(open.pipe, "Open Pipeline_09.29.2016_v2.csv")

#Open Pipe at Opp Level for Monthly Create
write.csv(open.pipe.slim, "Open Pipeline Slim_9.29.2016_v1.csv")

#Avg Monthly Create Rate
write.csv(monthly.create.data, 'Monthy Create Rate Tab_11082016_v3.csv')

#Absolute Yield Curves
save(abs.yields.F, rel.w.yields, rel.l.yields, file = "yield curves.saved")
write.csv(abs.yields.F, 'Absolute Yield Curves 11-14-16 v5.csv', na = "0")

#Relative Won Yield Curves
write.csv(rel.w.yields, 'Relative Yield Curves 11-11-16 v5.csv', na = "0")

#Relative Lost Yield Curves
write.csv(rel.l.yields, 'Relative Lost Yield Curves 11-11-16 v3.csv', na = "0")

#Cube
write.csv(closed.cube.F, 'Close Date Cube.csv')
write.csv(created.cube.F, 'Create Date Cube.csv')

#Save Objects
save(closed.pipe, file = "closedpipe.saved")
save(open.pipe, monthly.create.data, abs.yields, rel.w.yields, rel.l.yields, 
     file = "na gbs objects.saved")

save(closed.cube.F, created.cube.F, file = "cubes.saved")

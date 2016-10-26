#Working Directory - SCIP Server
setwd("~/NA GBS Strategic Work/R Content/Outputs")

#Open Pipe for model------------------------------------------------------------------------------------
library(data.table)
library(scales)
library(reshape2)
library(stringi)
library(lubridate)

#OPP LEVEL ROLLUP

#Adam's fread()
dtl.open <- read.csv("~/NA GBS Strategic Work/Data Sources/Open Pipeline/Heat Map/SMS8021 GBS NA Opportunity Detail - 4Q16 20.10.16.csv")
years.pipe <- as.data.frame(dtl.open)


new <- gsub("\\s", ".", colnames(years.pipe))
setnames(years.pipe, colnames(years.pipe), new)
rm(new)
detach("package:splitstackshape", unload=TRUE)
detach("package:data.table", unload=TRUE)
detach("package:zoo", unload=TRUE)


years.pipe$Opp.Create.Date <- with(years.pipe, mdy(Opp.Create.Date))
years.pipe$create.year <- format(years.pipe$Opp.Create.Date, "%Y")
years.pipe$create.month <- format(years.pipe$Opp.Create.Date, "%m")
years.pipe$tcv <- as.numeric(years.pipe$Rev.Signings.Value...K.)
years.pipe$tcv <- years.pipe$tcv * 1000
years.pipe <- subset(years.pipe, IMT != 'US Exception Mkt')
years.pipe <- subset(years.pipe, SSM.Step.Name != 'Won')


library(dplyr)


#use dplyr to rollup data to Opportunity level. Remove opps with tcv of 0 after rolling up
data.pipe <- dplyr::tbl_df(years.pipe) %>% #specify the data table to summarize
  group_by(Opp.No, Brand.Sub.Group, SSM.Step.Name, Previous.Sales.Stage,
           IMT, GBS.Bus.Unit.Level.2) %>% #specify which records/variables to keep
  summarise(tcv = sum(tcv), #define new variables using functions
            created = min(Opp.Create.Date)) %>%
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

detach("package:dplyr", unload=TRUE)

#fix formatting for date fields
data.pipe$create.month <- lubridate::month(data.pipe$created)
data.pipe$create.year <- lubridate::year(data.pipe$created)


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



opp.data$DPUID <- with(opp.data, paste(Create.Stage, size, Service.Line, Sector.IMT, sep = ""))

opp.data$Size.Type <- with(opp.data, ifelse(size == '>$10M', 'Large', 'Small'))


opp.data$Deal.Size <- opp.data$size
opp.data$Sector.F <- opp.data$Sector.IMT
opp.data$TCV <- opp.data$tcv
opp.data["source"] <- "Open"

open.pipe <- subset(opp.data, dupe == FALSE & Sector.F != "")
open.pipe.slim <- open.pipe
open.pipe.slim <- subset(open.pipe.slim, create.year == 2016)

write.csv(open.pipe, "Open Pipeline_10.13.2016_v1.csv")

#Data cleanup----------------------------------------------------------------------------------
#load EXCEL INPUT DATA workbook
setwd("C:/Users/SCIP2/Box Sync/NA GBS Pipeline Final Deliverables/Back-end Input Data")
directory <- paste0(getwd(), "/Reports for Scorecard")

filename <- list.files(directory)

library(readxl)
dtl.open <- read_excel(paste(directory, filename, sep = '/'))

start.row <- which(dtl.open[,1] == 'Opp No')
print(start.row)

#colnames(dtl.open) <- dtl.open[start.row,]
#dtl.open <- dtl.open[start.row+1:length(dtl.open$`Opp No`),]
#dtl.open <- dtl.open[rowSums(is.na(dtl.open)) != ncol(dtl.open),]

dtl.open <- read_excel(paste(directory, filename, sep = '/'),
                       skip = start.row)

#Field Name Cleanup
colnames(dtl.open) <- str_replace_all(colnames(dtl.open), "[^[:alnum:]]", ".")

#CLEAN UP DATES 
dtl.open$S.S.Update.Date <- ymd(dtl.open$S.S.Update.Date)
dtl.open$Opp.Create.Date <- ymd(dtl.open$Opp.Create.Date)

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
  group_by(create.month, create.stage.2) %>%
  summarise(tcv.m = sum(tcv.m))

OI.heat.map.NA$sector <- "NA"
OI.heat.map.NA <- OI.heat.map.NA[,c(4,1,2,3)]
OI.heat.map.NA <- as.data.frame(OI.heat.map.NA)
OI.heat.map.3 <- as.data.frame(OI.heat.map.3)

OI.heat.map.F <- rbind(OI.heat.map.3, OI.heat.map.NA)

#Heat Map
write.csv(OI.heat.map.F, 'OI heat map data_10312016_v7.csv')
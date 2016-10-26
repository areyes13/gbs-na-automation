library(XLConnect)
library(lubridate)
options(java.parameters = "-Xmx8g")

# EXCEL MODEL -------------------------------------------------------------
#load EXCEL INPUT DATA workbook

directory <- paste0(getwd(), "/Input Data")
filename <- list.files(directory)

wb <- loadWorkbook(filename)
getSheets(wb)

#GET DATA FROM WORKBOOK
data <- readWorksheet(wb, 'rp_GBS_Top_Opportunity_Detail', startRow = 4)

#CLEAN UP DATES 
data$S.S.Update.Date <- ymd(data$S.S.Update.Date)
data$Opp.Create.Date <- ymd(data$Opp.Create.Date)

#save as csv
write.csv(data, file = gsub(filename, pattern = 'xlsx', replacement = 'csv'))

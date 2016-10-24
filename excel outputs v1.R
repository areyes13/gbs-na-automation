# include code below after necessary outputs have been generated
load("na gbs objects.saved")

#set JAVA heap size to max
options(java.parameters = "-Xmx8g")
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk1.8.0_102')

library(XLConnect)

#load EXCEL MODEL workbook
wb <- loadWorkbook("GBS Scenario Model - 10 19 2016 - Golden Copy.xls")
getSheets(wb)

#write MONTHLY CREATE to wb
writeWorksheet(wb, 
               monthly.create.data, 
               sheet = "Monthly Create", startRow = 1, startCol = 1,
               header = TRUE)

#hideSheet(wb, sheet = "Data")

#save edits to wb
saveWorkbook(wb, file = paste0('GBS Scenario Model  - ', Sys.Date(), '.xls'))
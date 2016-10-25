# include code below after necessary outputs have been generated
load("na gbs objects.saved")

#set JAVA heap size to max
options(java.parameters = "-Xmx8g")
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk1.8.0_102')

library(XLConnect)

# EXCEL MODEL -------------------------------------------------------------
#load EXCEL MODEL workbook
wb <- loadWorkbook("GBS Scenario Model - template.xls")
getSheets(wb)

#write MONTHLY CREATE to wb
writeWorksheet(wb, 
               monthly.create.data, 
               sheet = "Monthly Create", startRow = 1, startCol = 1,
               header = TRUE)
#hideSheet(wb, sheet = "Data")


#write YIELD CURVES to wb
#may have to modify to create RELATIVE CURVES
writeWorksheet(wb, 
               abs.yields, 
               sheet = "Adjusted Yield", startRow = 1, startCol = 1,
               header = TRUE)

#still missing outputs for PLAN, FV, OPEN PIPE

#save edits to wb
saveWorkbook(wb, file = paste0('GBS Scenario Model  - ', Sys.Date(), '.xls'))



# OPEN PIPE MODEL ---------------------------------------------------------
#load OPEN PIPE workbook
wb <- loadWorkbook("Future Yield Model - template.xlsx")
getSheets(wb)

#write OPEN PIPE DATA to wb
writeWorksheet(wb, 
               open.pipe, 
               sheet = "Pipe.Data", startRow = 1, startCol = 1,
               header = TRUE)

#write RELATIVE YIELDS to wb
rel.l.yields$status <- 'NOT WON'
rel.w.yields$status <- 'WON'
rel.yields <- rbind(rel.w.yields, rel.l.yields)

writeWorksheet(wb, 
               rel.yields, 
               sheet = "Relative Yield Curves", startRow = 1, startCol = 1,
               header = TRUE)

saveWorkbook(wb, file = paste0('Future Yield Model  - ', Sys.Date(), '.xlsx'))

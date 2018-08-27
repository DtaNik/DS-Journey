

install.packages("rJava")
install.packages("XLConnectJars")
install.packages("XLConnect")

library(XLConnectJars)
library(XLConnect)
library(rJava)


library(xlsx) #load the package
install.packages("xlsx")

# we'll assume your spreadsheet will be 20 rows & 8 columns
m <- (matrix('',nrow = 20,ncol = 8)) # '' removes NA from your final spreadsheet
# place values in specific cells 
m[10,2] <- c("B10") # as B10 is in row 10 and column 2
m[20,8] <- c("H20")
# export as xlsx
write.xlsx(x = m, file = "your.excelfile.xlsx",sheetName = "test", row.names = FALSE, col.names=FALSE)


getwd()
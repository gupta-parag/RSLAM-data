library(dplyr)
library(data.table)
library(stringr)

# choice <- "Rt33"

# selectionExclude <- cbind(taz_data[["MODEL_TAZ"]], taz_data[[choice]])


selectionExclude <- as.data.frame(cbind(exComputed[["MODEL_TAZ"]], exComputed[["Rt33"]]))

 #geographyFile <- read.csv('C:\\Projects_2023\\land-use-model\\scenario\\TazTag.csv')
 excludeFile <- geographyFile[,c(1,2,8:44,109)]

 for( i in 40){ #:39
   total_jurs <- unique(excludeFile$Coun_Name)
   exclude_jurs <- unique(excludeFile$Coun_Name[ excludeFile[,i] == 1])
   x <- excludeFile[,i]
   x[ excludeFile[,i] == 1 ] <- 0 
   x[(excludeFile$Coun_Name %in% exclude_jurs) & excludeFile[,i] ==0]  <- 1
   x[!(excludeFile$Coun_Name %in% exclude_jurs)]  <-0
   x[(excludeFile$Coun_Name=='Ashland')]  <-0
   excludeFile[,i] <- x
 }
# 
 
write.csv(excludeFile,"excludedComputed.csv", row.names = F)

fileName <- read.csv(str_c(base_dir_input,"/",alt_name, "/Input/","TAZx.csv"))







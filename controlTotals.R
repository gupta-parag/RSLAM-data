library(foreign)
library(dplyr)


alt_name <- "alt_secondary"


input_control <- read.csv(str_c("C:\\R_SLAM_2\\BaseLU\\",alt_name,
                                            "\\Input\\control.csv"))
input_f <- input_control[,c(1,5,4)]


input_ff <- pivot_wider(input_f, names_from = "LU_NAME", values_from = CNTL_TOT) %>%
              select(1,5,2,3,4)


tdm_file <- read.dbf(str_c("C:\\R_SLAM_2\\BaseLU\\",alt_name,
                           "\\Output\\RSlam_output_",alt_name,".dbf"))

output <- tdm_file %>% group_by(JUR) %>%
  summarise(POP_HH = sum(POP_HH),
            HH = sum(HH) , 
            RET_EMP = sum(RET_EMP),
            NON_RET_EMP = sum(NON_EMP),
            TOT_EMP = sum(TOT_EMP))

output$POP_HH[output$JUR == "Hanover"] <- output$POP_HH[output$JUR == "Hanover"] + output$POP_HH[output$JUR == "Ashland"]
output$HH[output$JUR == "Hanover"] <- output$HH[output$JUR == "Hanover"] + output$HH[output$JUR == "Ashland"]
output$RET_EMP[output$JUR == "Hanover"] <- output$RET_EMP[output$JUR == "Hanover"] + output$RET_EMP[output$JUR == "Ashland"]
output$NON_RET_EMP[output$JUR == "Hanover"] <- output$NON_RET_EMP[output$JUR == "Hanover"] + 
                                      output$NON_RET_EMP[output$JUR == "Ashland"]

colnames(input_ff)[1] <- "JUR"
ctl_file <- merge(input_ff, output, 
                      by =  "JUR", 
                      suffixes = c("_In", "_Out"))

write.csv(ctl_file, "Secondary.csv", row.names = F)



library(foreign)
library(stringr)

alt_name <- "alt_secondary"
alt_name2 <- "alt_primary"

secon_base <- read.dbf(str_c("C:\\R_SLAM_2\\BaseLU\\",alt_name,
                                "\\Input\\RTC_LANDUSE_2045.dbf"))
geographyFile <- read.csv('C:\\Projects_2023\\land-use-model\\scenario\\TazTag.csv')
outerLoopTAZs <- geographyFile$Model_TAZ[geographyFile$OuterLpTAZ == 1]

primaryOutput <- read.dbf(str_c("C:\\R_SLAM_2\\BaseLU\\",alt_name2,
                       "\\Output\\RSlam_output_",alt_name2,".dbf"))

primaryZtaZ <- primaryOutput[(primaryOutput$N %in% outerLoopTAZs),]

secon_base_f <- secon_base[!(secon_base$N %in% primaryZtaZ$N),]
final_file <- rbind(secon_base_f,primaryZtaZ )

write.dbf(final_file, "RTC_LANDUSE_2045.dbf")

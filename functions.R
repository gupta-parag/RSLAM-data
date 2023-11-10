
############################################## CALLING LIBRARIES #################################
library(htmltools)
library(shinyFiles)
library(RColorBrewer)
library(shinyBS)
library(cartography)
library(shinyLP)
library(shinycssloaders)
library(fs)
library(readxl)
library(purrr)
library(sf)
library(crosstalk)
library(stringr)
library(shiny)
library(DT)
library(stringr)
library(shinyWidgets)
library(shinythemes)
library(leaflet)
# library(runslam)
library(tidyr)
library(plyr)
library(dplyr)
#library(Matrix.utils)
library(RColorBrewer)
library(ggplot2)
library(maptools)
library(rgdal)
library(RColorBrewer)
library(httpuv)
library(reshape2)
library(kableExtra)
library(scales)
library(foreign)
library(xlsx)
library(data.table)
library(rstudioapi)
library(reactable)
library(foreign)
library(leaflet.extras)

##################################################################################################

# Function 1
read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

# Function 2
runslam <- function (basedir, alt) 
{

message("****** Hello from runslam ********************")

# Assigning base directory and alternative name
# and getting all the input and output directories
# basedir <- 'C:\\R_SLAM_2\\BaseLU'
# alt <- 'alternative_urban'

message("***** ", basedir, "  ***\n")
message("***** ", alt, "  ***\n")
gc(full = TRUE)
savwd <- getwd()
setwd(basedir)
altdir <- paste0(basedir, "/", alt, "/")
indir <- paste0(altdir, "Input/")
outdir <- paste0(altdir, "Output/")
message("** indir ", indir, "\n")
message("** outdir ", outdir, "\n")  # printing input and output directory paths

# if no output directory then create output directory
if (!dir.exists(outdir)) 
  dir.create(outdir) # creating output directory


# creating the name of fonal TDM output file through RSLAM, and reading, 
#creating file paths for TAZx files
# only file read here is the params.csv file

p2 <- "RSlam_output_" # starting name of the output of the file
outf <- paste0(outdir, p2, alt, ".xlsx") # full name with path
message("** outf ", outf, "\n") # path of output directory
calF <- paste0(basedir, "/calibration/params.csv") # reading calibrated parameters
message("** calibration file ", calF, "\n") # printing out path of calibration file
cal.DF <- read.csv(calF, fileEncoding = "UTF-8-BOM", 
                   row.names = "COEF") # reading calibration file
scoreFB <- paste0(basedir, "/calibration/TAZx.dbf") # path of calibrated scores TAZx file
scoreF <- paste0(indir, "TAZx.csv") # path of scenario input scores
scoreFO <- paste0(outdir, "TAZx.dbf") # path of output scenario scores
message("** TAZx.dbf ", scoreFO, "\n") # priting path of output scenario


# Reading and Renaming area type data frame and column names
atypeF <- paste0(indir, "Areatype_network.csv") # making path of scenario input areatype network file
message("** Area Types ", atypeF, "\n") # printing path of area type : input scenario
atype <- read.csv(atypeF)  # reading area type file
colnames(atype) <- c("TAZ", "MTP_AT") # renaming column names


#READING 2045 TDM data and Making the name of RSLAM TDM Output file
rtcF <- paste0(indir, "RTC_LANDUSE_2045.DBF") # making 2045 file path TDM input datya
rtc.df <- read.dbf(rtcF, as.is = TRUE) # reading the dbf file 2045 TDM file
luoutF <- paste0(outdir, p2, alt, ".dbf") # saving the name and path of RSLAM output file


# READING AND RETURNING DENSITIES 
lookF <- paste0(indir, "densities.csv") # path of input densities
lutab <- read.csv(file = lookF) # reading the densities
lutab$key <- 10 * lutab$JURNUM + lutab$MTP_AT # ccreating a unique key to get densities
row.names(lutab) <- lutab$key

# FUNCTION TO RETURN THE DENSITY
getden <- function(jurnum, at, lutype) {
  lukey <- as.character(jurnum * 10 + at)
  density <- lutab[lukey, lutype] # using row name to get the rows
  return(density)
}

# Reading base year 2017 TAZ Data and replacing it TAZ field with N zone number
baseF <- paste0(indir, "RTC_LANDUSE_2017.dbf")
base.df <- read.dbf(baseF, as.is = TRUE)
base.df$TAZ <- base.df$N

# Merging both base(2017) and horizon(2045) year data
tpoGRO <- merge(base.df, rtc.df, by = "ZONE", suffixes = c(".b", 
                                                           ".f"))
tpoGRO <- tpoGRO[, c("ZONE", "HH.f", "RET_EMP.f", 
                     "NON_EMP.f", "AUTO.f", "K12_ENROLL.f", 
                     "POP_GQ.f", "POP_HH.f", "U_ENROLL.f")]
colnames(tpoGRO) <- c("ZONE", "HH", "RET_EMP", 
                      "NON_EMP", "AUTO", "K12_ENROLL", "POP_GQ", 
                      "POP_HH", "U_ENROLL")

# Replacing Ashland with Hanover as it is a part of it- in base year TDM file
base.df$JUR_b <- base.df$JUR
base.df$JURNUM_b <- base.df$JURNUM
base.df$JUR <- ifelse(base.df$JUR == "Ashland", "Hanover", 
                      base.df$JUR)
base.df$JURNUM <- ifelse(base.df$JURNUM == 14, 6, base.df$JURNUM)


# reading scenario control total file as den file
ctlF <- paste0(indir, "control.csv")
den <- NULL
den <- read.csv(ctlF, stringsAsFactors = FALSE)
den$key <- paste(den$LU_NAME, den$JURNUM) # adding control totals key

pvars <- den$LU_NAME # getting all the land use fields in control total file
jurs <- sort(unique(den$JURNUM)) # sort jurisdiction numbers

# reading calibrated scores from scoreFB (this contains 2017 HH,RET_EMP AND NON_RET_EMP), 
#and reading input alternative scores
score.df <- NULL
score.df <- read.dbf(scoreFB, as.is = TRUE) # scoreFB <- paste0(basedir, "/calibration/TAZx.dbf") 
alt.df <- read.csv(scoreF) # scoreF <- paste0(indir, "TAZx.csv")

#Replacing Ashland with Hanover // PG added this to avoid Ashland Problem
alt.df$JUR <- ifelse(alt.df$JUR == "Ashland", "Hanover", 
                     alt.df$JUR)
alt.df$JURNUM <- ifelse(alt.df$JURNUM == 14, 6, alt.df$JURNUM)

score.df$JUR <- ifelse(score.df$JUR == "Ashland", "Hanover", 
                       score.df$JUR)
score.df$JURNUM <- ifelse(score.df$JURNUM == 14, 6, score.df$JURNUM)


# Computing excluded TAZs
excluded <- alt.df[, c("MODEL_TAZ", "JURNUM", 
                       "EXCLUDE")]
excluded <- merge(excluded, tpoGRO, by.x = "MODEL_TAZ", 
                  by.y = "ZONE")

# Getting Included and Excluded TAZs and their data fromtpoGRO file
included <- excluded[excluded$EXCLUDE == 0, ]
excluded <- excluded[excluded$EXCLUDE > 0, ]

# getting included and excluded TAZs
xvec <- excluded$MODEL_TAZ
ivec <- included$MODEL_TAZ

# getting as is TAZs and their data from 2045\2050 landuse file
tpo_pass_through <- rtc.df[rtc.df$ZONE %in% xvec, ]
exFO <- paste0(outdir, "Exclude.csv")
message("** Excluded TAZs ", exFO)
write.csv(excluded, file = exFO) # with tpoGRO data(fields in control total file) 
                                  #and three columns from TAZx file, 
                                 # MODEL_TAZ,JURNUM,EXCLUDE, there is Ashland in this coming from TAZx
                                  # file

# Getting excluded control totals and substracting from the input scenario control total
xGRO <- excluded %>% 
  group_by(JURNUM) %>% 
  summarize(HH = sum(HH), RET_EMP = sum(RET_EMP), NON_EMP = sum(NON_EMP), 
                  AUTO = sum(AUTO), K12_ENROLL = sum(K12_ENROLL), 
                     POP_GQ = sum(POP_GQ), POP_HH = sum(POP_HH), 
                    U_ENROLL = sum(U_ENROLL)) # wide data frame


xGR2 <- xGRO %>% gather(variable, value, -c(JURNUM)) # to long data frame
colnames(xGR2) <- c("JURNUM", "LU_NAME", "CNTL_TOT") # rename columns to control total file
xGR2$key <- paste(xGR2$LU_NAME, xGR2$JURNUM) # creating a key

# merging input control totals and exclude control totals
denM <- merge(den, xGR2, by = "key", all = TRUE, suffixes = c("", ".e"))

# computing the final control totals to allocate
denM$CNTL_TOT <- ifelse(!is.na(denM$CNTL_TOT.e), denM$CNTL_TOT - 
                          denM$CNTL_TOT.e, denM$CNTL_TOT)

# getting the original control total file from the merged file
den <- subset(denM, select = -c(CNTL_TOT.e, JURNUM.e, LU_NAME.e, 
                                key))
den <- den[with(den, order(Jname, LU_NUM)), ] # final control total file to allocate


# Working on TAZx score of input scenario and calibration TAZx file that has 2017, HH and RET ,NONRET data

#alt.df <- read.csv(scoreF) # scoreF <- paste0(indir, "TAZx.csv")
alt.df <- dplyr::select(alt.df, -EXCLUDE) # take out exclude from input TAZx file
altflds <- colnames(alt.df) # get the column names
alim <- length(altflds) # putting the length of column names vector in alim

#score.df <- read.dbf(scoreFB, as.is = TRUE) # scoreFB <- paste0(basedir, "/calibration/TAZx.dbf") 
scoflds <- colnames(score.df) # get the column names from calibration TAZx file


# Merging calibrated and input scenario TAZx files
msco <- merge(score.df, alt.df, by = "MODEL_TAZ", all.x = TRUE, 
              all.y = FALSE, sort = TRUE, suffixes = c("", "_"))

msco$MODEL_TAZ_ <- msco$MODEL_TAZ # Creating a copy of TAZ column

# replacing all the calibration TAZx scores with the input scenario scores
for (i in 4:alim) { # from dev_acr till v_idx_NRE
  f1 <- msco[[altflds[i]]]  # msco['dev_acr']
  f2 <- msco[[paste0(altflds[i], "_")]] #  msco['dev_acr_']
  msco[[altflds[i]]] <- ifelse(!is.na(f2), f2, f1) # msco['dev_acr'] <- f2 ,from dev_acr till v_idx_NRE
}

# this file now contains 2017 HH,RET_EMP,NON_RET_EMP values with input scenario (2050) scores
score.df <- msco[, (names(msco) %in% scoflds)] # and has jurisdiction and jurisdiction number of ashland

### writing out final scores
message("** edited scores ", scoreFO)
write.dbf(file = scoreFO, score.df)



# writing now the accompanying shapefile for the final scores dbf
suf <- c("cpg", "shp", "shp.xml", "prj", 
         "shx")
slim <- length(suf)
for (i in 1:slim) {
  tin <- paste0(basedir, "/calibration/tazx.", suf[i])
  message("** shape components: ", tin, " ", 
          outdir)
  file.copy(tin, outdir, overwrite = TRUE)
}

# replacting the jurisdiction and jurisdictoion number of Ashland 
# and adding the area type of the TAZs
score.df$JUR_b <- score.df$JUR
score.df$JURNUM_b <- score.df$JURNUM
score.df$JUR <- ifelse(score.df$JUR == "Ashland", "Hanover", 
                       score.df$JUR)
score.df$JURNUM <- ifelse(score.df$JURNUM == 14, 6, score.df$JURNUM)
score.df$TAZ <- score.df$MODEL_TAZ
score.df <- join(score.df, atype, by = "TAZ", type = "left", 
                 match = "first")

# Storing file names and file paths
FileType <- c("Base", "Control", "Scores")
FileName <- c(baseF, ctlF, scoreF) # input files with paths like 2017.dbf , control.csv , TAZx.csv
files <- data.frame(FileType, FileName)

# Saving NAICS type 
naics <- c("NAICS_11", "NAICS_21", "NAICS_22", 
           "NAICS_23", "NAICS_3133", "NAICS_42", 
           "NAICS_4445", "NAICS_4849", "NAICS_51", 
           "NAICS_52", "NAICS_53", "NAICS_54", 
           "NAICS_55", "NAICS_56", "NAICS_61", 
           "NAICS_62", "NAICS_71", "NAICS_72", 
           "NAICS_81", "NAICS_92", "NAICS_OTH")
retV <- c("NAICS_4445", "NAICS_72", "NAICS_81")
nonV <- c("NAICS_11", "NAICS_21", "NAICS_22", 
          "NAICS_23", "NAICS_3133", "NAICS_42", 
          "NAICS_4849", "NAICS_51", "NAICS_52", 
          "NAICS_53", "NAICS_54", "NAICS_55", 
          "NAICS_56", "NAICS_61", "NAICS_62", 
          "NAICS_71", "NAICS_92", "NAICS_OTH")

#Getting sum of retail NAICS individually by region total and then calculating fraction of NAICS 
# to retail total on a regional level
vRetail <- vector()
for (i in 1:length(retV)) {
  vRetail <- c(vRetail, sum(base.df[, retV[i]],na.rm = T))
}
vRetail <- cbind(retV, vRetail)
RetFrac <- data.frame(vRetail)
rm(vRetail)
RetFrac$vRetail <- as.numeric(RetFrac$vRetail)
RetFrac$frac <- RetFrac$vRetail/sum(RetFrac$vRetail)



#Getting sum of non-retail NAICS individually by region total and then calculating fraction of NAICS 
# to non-retail total on a regional level
vNRetail <- vector()
for (i in 1:length(nonV)) {
  print(nonV[i])
  vNRetail <- c(vNRetail, sum(base.df[, nonV[i]],na.rm=T))
}
vNRetail <- cbind(nonV, vNRetail)
NonFrac <- data.frame(vNRetail)
rm(vNRetail)
NonFrac$vNRetail <- as.numeric(NonFrac$vNRetail)
NonFrac$frac <- NonFrac$vNRetail/sum(NonFrac$vNRetail)

# creating a work data frame that consosts of all score.df data
work <- score.df[order(score.df$MODEL_TAZ), ]

# Adding all the density information from density file
work$xHH_DEN <- mapply(getden, work$JURNUM, work$MTP_AT, 
                       "HH_DEN")
work$xRET_DEN <- mapply(getden, work$JURNUM, work$MTP_AT, 
                        "RET_DEN")
work$xNON_DEN <- mapply(getden, work$JURNUM, work$MTP_AT, 
                        "NON_DEN")
# replacing HH_DEN and adding two extra columns 
#RET AND NON_RET employment density,
# to compute a column that consists maximum of both
# existing (2017) and scenario input density file : 

work$HH_DEN <- ifelse(work$HH_DEN > work$xHH_DEN, work$HH_DEN, 
                      work$xHH_DEN)
work$RET_EMP_DEN <- ifelse(work$RET_DEN > work$xRET_DEN, 
                           work$RET_DEN, work$xRET_DEN)
work$NON_EMP_DEN <- ifelse(work$NON_DEN > work$xNON_DEN, 
                           work$NON_DEN, work$xNON_DEN)

# getting the maximum possible development based on 
#  ideal densities and developable land open for development
vtemhh <- work$HH_DEN * work$DevLand_Ar
vtemret <- work$RET_DEN * work$DevLand_Ar # possible mistake here as this does not contain the maximum
                                            # of land uses to calculate utility
vtemnre <- work$NON_DEN * work$DevLand_Ar

# Capping the capcity to 2000 new HHs, 
#retail employment and non-retail employment
work$cap_hh <- ifelse(vtemhh <= 2000, vtemhh, 2000)
work$cap_ret <- ifelse(vtemret <= 2000, vtemret, 2000)
work$cap_nre <- ifelse(vtemnre <= 2000, vtemnre, 2000)
rm(vtemhh, vtemret, vtemnre)

# creating more clear columns 
work$RET_EMP_COMPi <- work$RET_COMPi
work$NON_EMP_COMPi <- work$NON_COMPi
work$HH_MKTSC <- work$P_Index
work$RET_EMP_MKTSC <- work$RE_Index
work$NON_EMP_MKTSC <- work$NRE_Index
work$HH_aci <- work$HHaci
work$RET_EMP_aci <- work$EMPaci
work$NON_EMP_aci <- work$EMPaci
work$VAC_ACRE <- work$DevLand_Ar


# Adding utility function here to calculate final scores
util <- function(df, params) {
  df <- work
  params <- cal.DF
  jnames <- sort(unique(df$JUR))
  uhh <- vector(mode = "numeric", length = nrow(df))
  uret <- uhh
  unre <- uhh
  for (JUR in jnames) {
    jx <- gsub(" ", ".", JUR)
    c1 <- cal.DF["P_Index", jx]
    c2 <- cal.DF["v_indexHH", jx]
    c3 <- cal.DF["cap_hh", jx]
    uhh <- ifelse(df$JUR == JUR, c1 * df$P_Index + c2 * 
                    df$v_idx_HH + c3 * df$cap_hh, uhh)
    
    uret <- ifelse(df$JUR == JUR, cal.DF["RE_Index", jx] * df$RE_Index + 
                     cal.DF["EMPaci_RET", jx] * df$EMPaci + 
                     cal.DF["RET_COMPi", jx] *  df$RET_COMPi + 
                     cal.DF["v_indexHHforRET", jx] * df$v_idx_HH + #possible mistake here
                     cal.DF["cap_ret", jx] *  df$cap_ret, uret)
    
    unre <- ifelse(df$JUR == JUR, cal.DF["NRE_Index", jx] * df$NRE_Index + 
                     cal.DF["EMPaci_NRE", jx] * df$EMPaci + 
                     cal.DF["v_indexNRE", jx] * df$v_idx_NRE + 
                     cal.DF["cap_nre", jx] * df$cap_nre, unre)
  }
  
  df$HH_score <- ifelse(df$P_Index > 0, exp(uhh), 0)
  df$RET_EMP_score <- ifelse(df$RE_Index > 0, exp(uret),  0)
  df$NON_EMP_score <- ifelse(df$NRE_Index > 0, exp(unre), 0)
  df$uhh <- uhh
  df$uret <- uret
  df$unre <- unre
  debugfile <- paste0(outdir, "debug.csv")
  message("** debug: ", debugfile, "\n")
  write.csv(df, file = debugfile)
  rm(uhh, uret, unre)
  return(df)
}


work <- util(work, cal.DF)
work$VAC_ACRE <- work$VAC_ACRE + work$vertHH + work$vertRE + work$vertNRE
xden <- den
xbase.df <- base.df[base.df$ZONE %in% ivec, ]
xwork <- work[work$TAZ %in% ivec, ]

# Creating skeleton of Output Files to be written 
oden <- data.frame() # output density data
owork <- data.frame() # output working data
ozdat <- data.frame() #Output TAZ data - not sure
ogdat <- data.frame() # Output growth data - not sure
k_iters <- vector(mode = "numeric", length = length(8))
k_jurnum <- vector(mode = "numeric", length = length(8))
k_area <- 0

# revising jurisdictions to get only those that are included
jurs <- unique(sort(included$JURNUM))

for (k in jurs) {
  k_area <- k_area + 1
  den <- xden[xden$JURNUM == k, ] # left over control total of richmond ; K == 1
  work <- xwork[xwork$JURNUM == k, ] # working ddataframe that contians all the scores
  base.df <- xbase.df[xbase.df$JURNUM == k, ] # base year data for richmond ; k == 1
  
  for (i in 1:nrow(den)) {
    den$basetot[i] <- sum(base.df[[den$LU_NAME[i]]], na.rm = T)
  }
  
  den$GROW <- den$CNTL_TOT - den$basetot # this means left over control total - base control file 2017 to calculate growth
                                           # calculating growth total for included TAZs
  pri <- den[order(den$PRIORITY), ]
  pri <- pri[pri$PRIORITY > 0, ]
  GROW_pri <- as.vector(pri$GROW) # control totals vector of a jurisdiction retail,non-retail and HH
  work$VAC_LEFT <- work$VAC_ACRE
  v0 <- vector(mode = "numeric", length = length(work$VAC_LEFT))
  nc <- 1
  ik <- 0
  
  # beginning of while loop
  while (nc) {
    ik <- ik + 1                                    # keeping track of number of iterations
    for (i in 1:nrow(pri)) {                        # total three rows
      dname <- paste0(pri$LU_NAME[i], "_DEN")       # RET_EMP_DEN, NON_EMP_DEN, HH_DEN
      sname <- paste0(pri$LU_NAME[i], "_score")     # RET_EMP_score, NON_EMP_score, HH_score
      snam2 <- paste0(pri$LU_NAME[i], "_score2")    # RET_EMP_score2, NON_EMP_score2, HH_score2,
      gname <- paste0(pri$LU_NAME[i], "_g")         # RET_EMP_g, NON_EMP_g, HH_g,
      uname <- paste0(pri$LU_NAME[i], "_acres")     # RET_EMP_acres, NON_EMP_acres, HH_acres,
      work[[dname]] <- as.numeric(work[[dname]])    # converting density to numeric, RET_EMP_DEN, NON_EMP_DEN, HH_DEN
      if (ik == 1) {                                # if it is first iteration
        work[[gname]] <- v0                         # RET_EMP_g, NON_EMP_g, HH_g are all zero
        work[[uname]] <- v0                         # # RET_EMP_acres, NON_EMP_acres, HH_acres acres is zero
      }
      
      # RET_EMP_score, NON_EMP_score, HH_score
      work[[sname]] <- ifelse(work[[dname]] > 0, work[[sname]], # if density > 0, score  else zero 
                              0)                                # RET_EMP_DEN, NON_EMP_DEN, HH_DEN
      # RET_EMP_score2, NON_EMP_score2, HH_score2,
        work[[snam2]] <- ifelse(work$VAC_LEFT > 0, work[[sname]], # if vacant land > 0, score2 is score
                              0)                                  # else zero 
      tot <- sum(work[[snam2]])                                   # total score 2 of jurisdiction
      if (tot > 0) {                                              # if total of score2 > 0
        work[[snam2]] <- work[[snam2]]/tot                        # calculate score fraction of each TAZ
      } else{                                                     # RET_EMP_score2, NON_EMP_score2, HH_score2,
         work[[snam2]] <- v0 
      }                                                           # if total is zero then score is zero,
                                                              # RET_EMP_score2, NON_EMP_score2, HH_score2
      t_alloc <- work[[snam2]] * GROW_pri[i]                    ### Score 2 fraction * control total, allocation
      t_consumed <- ifelse(work[[dname]] > 0, t_alloc/work[[dname]], # jobs/density = area used
                           0)                                     # Caluclating area consumed
      t_consumed <- ifelse(t_consumed > work$VAC_LEFT,            # failsafe, if consumed more than available
                           work$VAC_LEFT, t_consumed)             # then use available
      t_left <- work$VAC_LEFT - t_consumed                        #  vacant land left
      t_alloc <- ifelse(t_left > 0, t_alloc, work$VAC_LEFT *      # if more land left, then original allocation
                          work[[dname]])                          # if not, then density * developable land
      t_left <- ifelse(t_left > 0, t_left, 0)                     # if land left more than zero than t_left else 0
      work[[gname]] <- t_alloc + work[[gname]]                    # allocation + itself RET_EMP_g, HH_G etc
      
      # uname is acres, so RET_EMP_acres, 
      work[[uname]] <- ifelse(work[[dname]] > 0, work[[gname]]/work[[dname]], 
                              work[[uname]])                    # if RET_EMP_DEN > 0, growth/density to get area  
      work$VAC_LEFT <- t_left                                   #  updating vacant left column,   
      
      # An error message for controls, when allocated is much much more than the control total
      if (GROW_pri[i] - sum(t_alloc) < -1) {
        mess <- paste0("negative, LU= ", i, ",  jur= ", 
                       k, ",  iter= ", ik, " GROW_pri=", 
                       GROW_pri[i], " sum(t_alloc)=", sum(t_alloc), 
                       "\n")
        cat(mess, "\n")
      }
      
      GROW_pri[i] <- GROW_pri[i] - sum(t_alloc)                   # revise the control total
    }
    
    if (sum(GROW_pri, na.rm = T) <= 1)                            # close the while loop when 
      nc <- 0                                                     # control total is less than or equal to 1
    if (ik > 20) 
      nc <- 0                                                    # If iterations are more than 20
    k_iters[k_area] <- ik                                        # cancel while  loo[]
  }
  
  
  
  
  
  GHH <- sum(work$HH_g)                                      # sum of all households growth
  pvarsX <- c("POP_HH", "POP_GQ", "AUTO")                    # Population and Auto variables
  for (i in 1:length(pvarsX)) {
    j <- match(pvarsX[i], pvars)                             # get index of POP_HH (4),POP_GQ
    luT <- den[j, "GROW"]                                    # den[4,"GROW"]
    gvar <- paste0(pvarsX[i], "_g")                          # allocating population and auto growth
    work[, gvar] <- luT * work$HH_g/GHH                      # growth control total using household 
  }                                                          # growth proportions
  k12j <- match("K12_ENROLL", pvars)                         # get index of K12_enroll (8)
  k12b <- den[k12j, "basetot"]
  k12f <- den[k12j, "CNTL_TOT"]
  if (k12b != 0) {
    k12fac <- k12f/k12b                                      # getting growth factor from future/base
  } else {
    k12fac <- 1                                              # if base year zero, then factor 1
  }
  uj <- match("U_ENROLL", pvars)                             # same way increasing university enrollment
  ub <- den[uj, "basetot"]
  uf <- den[uj, "CNTL_TOT"]                                  #base year total and future year total ratio
  if (ub != 0) {
    ufac <- uf/ub
  }else {
    ufac <- 1
  }
  zdat <- join(base.df, work, by = "TAZ")                    # zdata file is base.df (2017) with work.df
  naics <- c("NAICS_11", "NAICS_21", "NAICS_22", 
             "NAICS_23", "NAICS_3133", "NAICS_42", 
             "NAICS_4445", "NAICS_4849", "NAICS_51", 
             "NAICS_52", "NAICS_53", "NAICS_54", 
             "NAICS_55", "NAICS_56", "NAICS_61", 
             "NAICS_62", "NAICS_71", "NAICS_72", 
             "NAICS_81", "NAICS_92", "NAICS_OTH")
  for (i in 1:length(nonV)) {
    fld <- paste0(nonV[i], "_f") # NAICS_11_f
    fldg <- paste0(nonV[i], "_g") # NAICS_11_g
    zdat[, fld] <- ifelse(zdat$NON_EMP > 0, zdat[, nonV[i]] *                # base year non-retail > 0
                            (zdat$NON_EMP_g + zdat$NON_EMP)/zdat$NON_EMP,  # non-retail factor increase 
                          (zdat$NON_EMP_g + zdat$NON_EMP) * NonFrac[i,3])   # using NAICS in non-retail
    zdat[, fldg] <- zdat[, fld] - zdat[, nonV[i]]    # future minus base    # or use regiional fraction
  }
  for (i in 1:length(retV)) {                
    fld <- paste0(retV[i], "_f")
    fldg <- paste0(retV[i], "_g")                                         # same thing for retail 
    zdat[, fld] <- ifelse(zdat$RET_EMP > 0, zdat[, retV[i]] *             # NAICS
                            (zdat$RET_EMP_g + zdat$RET_EMP)/zdat$RET_EMP, 
                          (zdat$RET_EMP_g + zdat$RET_EMP) * RetFrac[i, 3])
    zdat[, fldg] <- zdat[, fld] - zdat[, retV[i]]
  }
  gdat <- zdat
  zdat$TOT_POP_f <- zdat$TOT_POP + zdat$POP_HH_g + zdat$POP_GQ_g     # getting all the future totals
  zdat$POP_HH_f <- zdat$POP_HH + zdat$POP_HH_g
  zdat$POP_GQ_f <- zdat$POP_GQ + zdat$POP_GQ_g
  zdat$HH_f <- zdat$HH + zdat$HH_g
  zdat$K12_ENROLL_f <- zdat$K12_ENROLL * k12fac
  zdat$K12_ENROLL_g <- zdat$K12_ENROLL_f - zdat$K12_ENROLL
  zdat$U_ENROLL_f <- zdat$U_ENROLL * ufac
  zdat$U_ENROLL_g <- zdat$U_ENROLL_f - zdat$U_ENROLL
  zdat$AUTO_f <- zdat$AUTO + zdat$AUTO_g
  zdat$TOT_EMP_f <- zdat$TOT_EMP + zdat$RET_EMP_g + zdat$NON_EMP_g
  zdat$RET_EMP_f <- zdat$RET_EMP + zdat$RET_EMP_g
  zdat$NON_EMP_f <- zdat$NON_EMP + zdat$NON_EMP_g
  zdat <- zdat %>% mutate_if(is.numeric, round)            # rounding them to zero
  zdat$ACRES <- base.df$ACRES
  zdat$AVGAUTO <- ifelse(zdat$HH_f > 0, zdat$AUTO_f/zdat$HH_f, 0) # updating AVG AUTO column
  of1 <- c("N", "ZONE", "PDC", "JUR", 
           "JURNUM", "JURCODE", "TOT_POP_f", 
           "POP_HH_f", "POP_GQ_f", "HH_f", 
           "K12_ENROLL_f", "U_ENROLL_f", "AUTO_f",            # two set of column names one for future
           "TOT_EMP_f", "RET_EMP_f", "NON_EMP_f", 
           "NAICS_11_f", "NAICS_21_f", "NAICS_22_f", 
           "NAICS_23_f", "NAICS_3133_f", "NAICS_42_f", 
           "NAICS_4445_f", "NAICS_4849_f", "NAICS_51_f", 
           "NAICS_52_f", "NAICS_53_f", "NAICS_54_f", 
           "NAICS_55_f", "NAICS_56_f", "NAICS_61_f", 
           "NAICS_62_f", "NAICS_71_f", "NAICS_72_f", 
           "NAICS_81_f", "NAICS_92_f", "NAICS_OTH_f", 
           "AVGAUTO", "ACRES", "JUR_b", "JURNUM_b")
  of2 <- c("N", "ZONE", "PDC", "JUR", 
           "JURNUM", "JURCODE", "TOT_POP", 
           "POP_HH", "POP_GQ", "HH", "K12_ENROLL", 
           "U_ENROLL", "AUTO", "TOT_EMP", 
           "RET_EMP", "NON_EMP", "NAICS_11", 
           "NAICS_21", "NAICS_22", "NAICS_23", 
           "NAICS_3133", "NAICS_42", "NAICS_4445",           # and other generic column names
           "NAICS_4849", "NAICS_51", "NAICS_52", 
           "NAICS_53", "NAICS_54", "NAICS_55", 
           "NAICS_56", "NAICS_61", "NAICS_62", 
           "NAICS_71", "NAICS_72", "NAICS_81", 
           "NAICS_92", "NAICS_OTH", "AVGAUTO", 
           "ACRES", "JUR_b", "JURNUM_b")
  zdat$AVGAUTO <- round(zdat$AVGAUTO, 2)                    # rounding average to two places
  zdat <- zdat[, of1]                                       # subsetting of1 column names
  colnames(zdat) <- of2                                     # renaming of2 column names
  gdat$USED <- gdat$VAC_ACRE - gdat$VAC_LEFT                 # gdat is growth data
  gdat <- gdat[, c("TAZ", "JURNUM", "HH_g",                 # this encapsulates growth summary
                   "HH_acres", "RET_EMP_g", "RET_EMP_acres", 
                   "NON_EMP_g", "NON_EMP_acres", "USED", 
                   "VAC_LEFT", "VAC_ACRE")]
  gdat <- round(gdat, 2)                         # rounding to 2 digits after decimal places
  owork <- rbind(owork, work)
  ozdat <- rbind(ozdat, zdat)
  ogdat <- rbind(ogdat, gdat)
  oden <- rbind(oden, den)                       # getting all jurisdictions files by TAZs
  gc()
}


lusum <- function(myzdat) {
  ctab <- myzdat %>% group_by(JUR) %>% summarize(JURNUM = max(JURNUM), 
                                                 N_Zones = n(), TOT_POP = sum(TOT_POP), POP_HH = sum(POP_HH), 
                                                 POP_GQ = sum(POP_GQ), HH = sum(HH), AUTO = sum(AUTO), 
                                                 K12_ENROLL = sum(K12_ENROLL), U_ENROLL = sum(U_ENROLL), 
                                                 RET_EMP = sum(RET_EMP), NON_EMP = sum(NON_EMP), TOT_EMP = sum(TOT_EMP))
  ctab <- replace(ctab, is.na(ctab), 0)
  ctab <- as.data.frame(ctab)
  return(ctab)
}

 # growth summary by jurisdiction
gsum <- ogdat %>% 
  group_by(JURNUM) %>% 
  summarize(N_Zones = n(), 
           HH_g = sum(HH_g), HH_acres = sum(HH_acres), RET_EMP_g = sum(RET_EMP_g), 
           RET_EMP_acres = sum(RET_EMP_acres), NON_EMP_g = sum(NON_EMP_g), 
           NON_EMP_acres = sum(NON_EMP_acres), USED = sum(USED), 
           VAC_LEFT = sum(VAC_LEFT), VAC_ACRE = sum(VAC_ACRE))
gsum <- cbind(gsum, k_iters)
gsum <- replace(gsum, is.na(gsum), 0)
gsum <- as.data.frame(gsum)

county_tab <- lusum(ozdat)

base_tab <- lusum(xbase.df[xbase.df$JURNUM %in% jurs, ])

gnames <- county_tab[order(county_tab$JURNUM), c("JUR", "JURNUM")]
gsum$JUR <- gnames$JUR
gsum <- gsum[order(gsum$JUR), ]
gsum$ReqDen <- round((gsum$HH_g + gsum$RET_EMP_g + gsum$NON_EMP_g)/gsum$VAC_ACRE, 2)
gsum$HH_Den <- round(gsum$HH_g/gsum$HH_acres, 2)
gsum$Ret_Den <- round(gsum$RET_EMP_g/gsum$RET_EMP_acres, 2)
gsum$Non_Den <- round(gsum$NON_EMP_g/gsum$NON_EMP_acres, 2)
owork <- owork[order(owork$MODEL_TAZ), ]
ozdat <- ozdat[order(ozdat$N), ]
ogdat <- ogdat[order(ogdat$TAZ), ]
of2x <- c("N", "ZONE", "PDC", "JUR", 
          "JURNUM", "JURCODE", "TOT_POP", "POP_HH", 
          "POP_GQ", "HH", "K12_ENROLL", "U_ENROLL", 
          "AUTO", "TOT_EMP", "RET_EMP", "NON_EMP", 
          "NAICS_11", "NAICS_21", "NAICS_22", 
          "NAICS_23", "NAICS_3133", "NAICS_42", 
          "NAICS_4445", "NAICS_4849", "NAICS_51", 
          "NAICS_52", "NAICS_53", "NAICS_54", 
          "NAICS_55", "NAICS_56", "NAICS_61", 
          "NAICS_62", "NAICS_71", "NAICS_72", 
          "NAICS_81", "NAICS_92", "NAICS_OTH", 
          "AVGAUTO", "ACRES")
ozdat$JUR <- ozdat$JUR_b
ozdat$JURNUM <- ozdat$JURNUM_b
ozdat <- ozdat[, of2x]
colnames(ozdat) <- of2x
message("** create output EXCEL ", outf)
gc()
write.xlsx2(files, file = outf, sheetName = "Input Files", 
            append = FALSE, row.names = FALSE)
message(" finished ", "Input Files sheet")
write.xlsx2(county_tab, file = outf, sheetName = "Output Summary", 
            append = TRUE, row.names = FALSE)
write.xlsx2(base_tab, file = outf, sheetName = "Input Summary", 
            append = TRUE, row.names = FALSE)
write.xlsx2(ozdat, file = outf, sheetName = "New Zonal Data", 
            append = TRUE, row.names = FALSE)
gc()
write.xlsx2(ogdat, file = outf, sheetName = "Growth Allocation", 
            append = TRUE, row.names = FALSE)
write.xlsx2(gsum, file = outf, sheetName = "Growth Summary", 
            append = TRUE, row.names = FALSE)
gc()
write.xlsx2(score.df, file = outf, sheetName = "Input scores", 
            append = TRUE, row.names = FALSE)
gc()
write.xlsx2(oden, file = outf, sheetName = "Input Controls", 
            append = TRUE, row.names = FALSE)
write.xlsx2(owork, file = outf, sheetName = "Working Data", 
            append = TRUE, row.names = FALSE)
gc()
write.xlsx2(xbase.df, file = outf, sheetName = "ZDATA_in", 
            append = TRUE, row.names = FALSE)

# Creating the output travel demand model file
oz2 <- ozdat # geting data for TAZs that are open for allocation
nonTPO <- rtc.df[rtc.df$ZONE > 955, ] # TAZs outside the MPO region
outzdatx <- rbind(oz2, nonTPO) # combining the newly allocated with 2045 nonTPO 
outzdata <- rbind(outzdatx, tpo_pass_through) # combining the above with excluded
outzdata <- outzdata[order(outzdata$ZONE), ] # ordering them by zone

message("** create Zonal data output ", luoutF) 
last2 <- outzdata[, c("AVGAUTO", "ACRES")]
outzdata <- select(outzdata, c(-AVGAUTO, -ACRES))
outzdata[is.na(outzdata)] = 0
is.num <- sapply(outzdata, is.numeric) # What is this ??
outzdata[is.num] <- lapply(outzdata[is.num], as.integer,  0)
outzdata <- cbind(outzdata, last2)
write.dbf(file = luoutF, outzdata) # writing the travel demand model file

############################################ LAND USE OUTPUT ####################################

# taz_2050 <- read.dbf(paste0(outdir, p2, alt, ".dbf")) # read this from RSLAM output
taz_by_lu <- read.csv(paste0(indir, "TAZ_By_LU.csv")) #default input so copy this from alternative A
message( " ** read the taz by lu file **")
taz_by_lu$TAZ <- as.numeric(as.character(taz_by_lu$TAZ))
model_output <- read.dbf(paste0(outdir, p2, alt, ".dbf"))# read this from rslam output

message(" ** read the model output file")
model_output <- model_output[model_output$ZONE %in% taz_by_lu$TAZ, ]

file_name <- taz_by_lu
file_name <- left_join(file_name, 
                       model_output[ , c("N",'JUR', "HH","TOT_EMP", "TOT_POP")], 
                       by = c( "TAZ" = "N"))

#residential land uses
file_name$res_total <- file_name$L_D_RE_Ar + file_name$M_D_RE_SF_Ar + 
  file_name$M_D_RE_MF_Ar + file_name$H_D_RE_Ar + file_name$MU_Ar
file_name$res_total[file_name$res_total == 0] <- 1

file_name$LDRE_prop <- file_name$L_D_RE_Ar / file_name$res_total
file_name$MDRESF_prop <- file_name$M_D_RE_SF_Ar / file_name$res_total
file_name$MDREMF_prop <- file_name$M_D_RE_MF_Ar / file_name$res_total
file_name$HDRE_prop <- file_name$H_D_RE_Ar / file_name$res_total
file_name$MUHH_prop <- file_name$MU_Ar / file_name$res_total

file_name$LDHH <- round(file_name$LDRE_prop * file_name$HH ,0)
file_name$MDSFHH <- round(file_name$MDRESF_prop * file_name$HH, 0)
file_name$MDMFHH <- round(file_name$MDREMF_prop * file_name$HH,0)
file_name$HDHH <- round(file_name$HDRE_prop* file_name$HH,0)
file_name$MUHH <- round(file_name$MUHH_prop * file_name$HH,0)
file_name$TSFHH <- file_name$LDHH + file_name$MDSFHH
file_name$TMFHH <- file_name$MDSFHH + file_name$HDHH


#calculating employment proportions
file_name$Other <- as.numeric(as.character(file_name$Other))
file_name$Other[is.na(file_name$Other)] <- 0

file_name$AG <- as.numeric(as.character(file_name$AG))
file_name$AG[is.na(file_name$AG)] <- 0

file_name$T_EMP_AR <- file_name$COM_Ar + file_name$INS_Ar + file_name$OF_Ar + file_name$IND_Ar +
  file_name$Other_Ar + file_name$AG_Ar + file_name$MU_Ar
file_name$T_EMP_AR[file_name$T_EMP_AR == 0 ] <- 1

file_name$COM_prop <- file_name$COM_Ar / file_name$T_EMP_AR
file_name$INS_prop <- file_name$INS_Ar / file_name$T_EMP_AR
file_name$OF_prop <- file_name$OF_Ar / file_name$T_EMP_AR
file_name$IND_prop <- file_name$IND_Ar / file_name$T_EMP_AR
file_name$Other_prop <- file_name$Other_Ar / file_name$T_EMP_AR
file_name$AG_prop <- file_name$AG_Ar / file_name$T_EMP_AR
file_name$MUEMP_prop <- file_name$MU_Ar / file_name$T_EMP_AR
# 
# #calculating final numbers
file_name$COM_EMP <-  round(file_name$COM_prop * file_name$TOT_EMP,0)
file_name$INS_EMP<- round(file_name$INS_prop * file_name$TOT_EMP,0)
file_name$OF_EMP <- round(file_name$OF_prop * file_name$TOT_EMP,0)
file_name$IND_EMP  <- round(file_name$IND_prop * file_name$TOT_EMP,0)
file_name$Other_EMP <- round(file_name$Other_prop * file_name$TOT_EMP,0)
file_name$AG_EMP <- round(file_name$AG_prop * file_name$TOT_EMP,0)
file_name$MU_EMP <- round(file_name$MUEMP_prop * file_name$TOT_EMP,0)

#write.csv(file_name, paste0(outdir, p2,"detailed_", alt, ".csv"), row.names = F)

detail_out <- file_name
ken_summ <- read_excel_allsheets(paste0(outdir, p2,alt, ".xlsx"))
growth_allo <- ken_summ[["Growth Allocation"]]
growth_allo_f <- growth_allo[ , c(1,4,6,8)]
growth_allo_f$HH_acres[growth_allo_f$HH_acres < 0 ] <- 0
growth_allo_f$RET_EMP_acres[growth_allo_f$RET_EMP_acres < 0] <- 0
growth_allo_f$NON_EMP_acres[growth_allo_f$NON_EMP_acres < 0] <- 0



filter_output <-  left_join(detail_out, growth_allo_f, by= "TAZ" )
filter_output$L_D_RE_Ar_new <- filter_output$L_D_RE_Ar + filter_output$LDRE_prop * filter_output$HH_acres
filter_output$M_D_RE_SF_Ar_new <- filter_output$M_D_RE_SF_Ar + filter_output$MDRESF_prop * filter_output$HH_acres
filter_output$M_D_RE_MF_Ar_new <- filter_output$M_D_RE_MF_Ar + filter_output$MDREMF_prop * filter_output$HH_acres
filter_output$H_D_RE_Ar_new <- filter_output$H_D_RE_Ar + filter_output$HDRE_prop * filter_output$HH_acres
filter_output$MU_Ar_new <- filter_output$MU_Ar + filter_output$MUHH_prop * filter_output$HH_acres
filter_output$res_total_new <- filter_output$res_total + filter_output$HH_acres

filter_output$EMP_acres <- filter_output$RET_EMP_acres + filter_output$NON_EMP_acres

filter_output$COM_Ar_new <- filter_output$COM_Ar + filter_output$COM_prop * filter_output$EMP_acres
filter_output$INS_Ar_new <- filter_output$INS_Ar + filter_output$INS_prop * filter_output$EMP_acres
filter_output$OF_Ar_new <- filter_output$OF_Ar + filter_output$OF_prop * filter_output$EMP_acres
filter_output$IND_Ar_new <- filter_output$IND_Ar + filter_output$IND_prop * filter_output$EMP_acres
filter_output$Other_Ar_new <- filter_output$Other_Ar + filter_output$Other_prop * filter_output$EMP_acres
filter_output$AG_Ar_new <- filter_output$AG_Ar + filter_output$AG_prop * filter_output$EMP_acres
filter_output$MU_Ar_new <- filter_output$MU_Ar + filter_output$MUEMP_prop * filter_output$EMP_acres
filter_output$T_EMP_AR_new <- filter_output$T_EMP_AR + filter_output$EMP_acres

final_names <- c('COUN_NAME','MPO','TAZ','L_D_RE_Ar_new','M_D_RE_SF_Ar_new','M_D_RE_MF_Ar_new',
                 'H_D_RE_Ar_new','MU_Ar_new','COM_Ar_new',	'INS_Ar_new',	'OF_Ar_new','IND_Ar_new',	
                 'Other_Ar_new','AG_Ar_new','FO_Ar','PA_Ar','LDHH','MDSFHH','MDMFHH','HDHH','MUHH',
                 'TSFHH','TMFHH','COM_EMP','INS_EMP','OF_EMP','IND_EMP','Other_EMP','AG_EMP','MU_EMP',
                  'JUR','HH','TOT_EMP', 'res_total_new',	'Parcel_ACR_Ar',	'ACRES_Ar',	'T_EMP_AR_new',	
                 'TOT_POP')
#c('JUR','HH','TOT_EMP', 'res_total_Ar',	'Parcel_ACR_Ar',	'TAZ_ACRES_Ar',	'T_EMP_AR_Ar',	'TOT_POP')

# getting the land-use file in the right format
filter_output_f <- filter_output[,final_names]
colnames(filter_output_f) <- gsub(x = colnames(filter_output_f), 
                                  pattern="_new", replacement = "")
colnames(filter_output_f)[34:37] <- c('res_total_Ar', "Parcel_ACR_Ar", "TAZ_ACRES_Ar", "T_EMP_AR_Ar")

filter_output_f <- filter_output_f[filter_output_f$TAZ %in% included$MODEL_TAZ, ] # taking out excluded TAZs


# getting the excluded TAZs
excluded_output <- read.csv('C:\\R_SLAM_2\\BaseLU\\Alt_A\\input_2050_targets.csv')
excluded_output_f <- excluded_output[excluded_output$TAZ %in% excluded$MODEL_TAZ, ]

excluded_output_f <- excluded_output_f %>% 
  mutate(across(17:30, round, 0))

landUseComputed <-  rbind(filter_output_f, excluded_output_f)

write.csv(landUseComputed, paste0(outdir, p2,"revisedAr_", alt, ".csv"), row.names = F)

##############################################################################################


setwd(savwd)
return(owork)
}


















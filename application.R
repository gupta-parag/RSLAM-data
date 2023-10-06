
###########################################################
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



read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

######################################## RSLAM ###########################################


runslam <- function (basedir, alt) 
{message("****** Hello from runslam ********************")
  message("***** ", basedir, "  ***\n")
  message("***** ", alt, "  ***\n")
  gc(full = TRUE)
  savwd <- getwd()
  setwd(basedir)
  altdir <- paste0(basedir, "/", alt, "/")
  indir <- paste0(altdir, "Input/")
  outdir <- paste0(altdir, "Output/")
  message("** indir ", indir, "\n")
  message("** outdir ", outdir, "\n")
  if (!dir.exists(outdir)) 
    dir.create(outdir)
  p2 <- "RSlam_output_"
  outf <- paste0(outdir, p2, alt, ".xlsx")
  message("** outf ", outf, "\n")
  calF <- paste0(basedir, "/calibration/params.csv")
  message("** calibration file ", calF, "\n")
  cal.DF <- read.csv(calF, fileEncoding = "UTF-8-BOM", 
                     row.names = "COEF")
  scoreFB <- paste0(basedir, "/calibration/TAZx.dbf")
  scoreF <- paste0(indir, "TAZx.csv")
  scoreFO <- paste0(outdir, "TAZx.dbf")
  message("** TAZx.dbf ", scoreFO, "\n")
  atypeF <- paste0(indir, "Areatype_network.csv")
  message("** Area Types ", atypeF, "\n")
  atype <- read.csv(atypeF)
  colnames(atype) <- c("TAZ", "MTP_AT")
  rtcF <- paste0(indir, "RTC_LANDUSE_2045.DBF")
  rtc.df <- read.dbf(rtcF, as.is = TRUE)
  luoutF <- paste0(outdir, p2, alt, ".dbf")
  lookF <- paste0(indir, "densities.csv")
  lutab <- read.csv(file = lookF)
  lutab$key <- 10 * lutab$JURNUM + lutab$MTP_AT
  row.names(lutab) <- lutab$key
  getden <- function(jurnum, at, lutype) {
    lukey <- as.character(jurnum * 10 + at)
    density <- lutab[lukey, lutype]
    return(density)
  }
  baseF <- paste0(indir, "RTC_LANDUSE_2017.dbf")
  base.df <- read.dbf(baseF, as.is = TRUE)
  base.df$TAZ <- base.df$N
  tpoGRO <- merge(base.df, rtc.df, by = "ZONE", suffixes = c(".b", 
                                                             ".f"))
  tpoGRO <- tpoGRO[, c("ZONE", "HH.f", "RET_EMP.f", 
                       "NON_EMP.f", "AUTO.f", "K12_ENROLL.f", 
                       "POP_GQ.f", "POP_HH.f", "U_ENROLL.f")]
  colnames(tpoGRO) <- c("ZONE", "HH", "RET_EMP", 
                        "NON_EMP", "AUTO", "K12_ENROLL", "POP_GQ", 
                        "POP_HH", "U_ENROLL")
  base.df$JUR_b <- base.df$JUR
  base.df$JURNUM_b <- base.df$JURNUM
  base.df$JUR <- ifelse(base.df$JUR == "Ashland", "Hanover", 
                        base.df$JUR)
  base.df$JURNUM <- ifelse(base.df$JURNUM == 14, 6, base.df$JURNUM)
  ctlF <- paste0(indir, "control.csv")
  den <- NULL
  den <- read.csv(ctlF, stringsAsFactors = FALSE)
  den$key <- paste(den$LU_NAME, den$JURNUM)
  pvars <- den$LU_NAME
  jurs <- sort(unique(den$JURNUM))
  score.df <- NULL
  score.df <- read.dbf(scoreFB, as.is = TRUE)
  alt.df <- read.csv(scoreF)
  excluded <- alt.df[, c("MODEL_TAZ", "JURNUM", 
                         "EXCLUDE")]
  excluded <- merge(excluded, tpoGRO, by.x = "MODEL_TAZ", 
                    by.y = "ZONE")
  included <- excluded[excluded$EXCLUDE == 0, ]
  excluded <- excluded[excluded$EXCLUDE > 0, ]
  xvec <- excluded$MODEL_TAZ
  ivec <- included$MODEL_TAZ
  tpo_pass_through <- rtc.df[rtc.df$ZONE %in% xvec, ]
  exFO <- paste0(outdir, "Exclude.csv")
  message("** Excluded TAZs ", exFO)
  write.csv(excluded, file = exFO)
  xGRO <- excluded %>% group_by(JURNUM) %>% summarize(HH = sum(HH), 
                                                      RET_EMP = sum(RET_EMP), NON_EMP = sum(NON_EMP), AUTO = sum(AUTO), 
                                                      K12_ENROLL = sum(K12_ENROLL), POP_GQ = sum(POP_GQ), POP_HH = sum(POP_HH), 
                                                      U_ENROLL = sum(U_ENROLL))
  xGR2 <- xGRO %>% gather(variable, value, -c(JURNUM))
  colnames(xGR2) <- c("JURNUM", "LU_NAME", "CNTL_TOT")
  xGR2$key <- paste(xGR2$LU_NAME, xGR2$JURNUM)
  denM <- merge(den, xGR2, by = "key", all = TRUE, suffixes = c("", 
                                                                ".e"))
  denM$CNTL_TOT <- ifelse(!is.na(denM$CNTL_TOT.e), denM$CNTL_TOT - 
                            denM$CNTL_TOT.e, denM$CNTL_TOT)
  den <- subset(denM, select = -c(CNTL_TOT.e, JURNUM.e, LU_NAME.e, 
                                  key))
  den <- den[with(den, order(Jname, LU_NUM)), ]
  alt.df <- dplyr::select(alt.df, -EXCLUDE)
  altflds <- colnames(alt.df)
  alim <- length(altflds)
  scoflds <- colnames(score.df)
  msco <- merge(score.df, alt.df, by = "MODEL_TAZ", all.x = TRUE, 
                all.y = FALSE, sort = TRUE, suffixes = c("", "_"))
  msco$MODEL_TAZ_ <- msco$MODEL_TAZ
  for (i in 4:alim) {
    f1 <- msco[[altflds[i]]]
    f2 <- msco[[paste0(altflds[i], "_")]]
    msco[[altflds[i]]] <- ifelse(!is.na(f2), f2, f1)
  }
  score.df <- msco[, (names(msco) %in% scoflds)]
  message("** edited scores ", scoreFO)
  write.dbf(file = scoreFO, score.df)
  suf <- c("cpg", "shp", "shp.xml", "prj", 
           "shx")
  slim <- length(suf)
  for (i in 1:slim) {
    tin <- paste0(basedir, "/calibration/tazx.", suf[i])
    message("** shape components: ", tin, " ", 
            outdir)
    file.copy(tin, outdir, overwrite = TRUE)
  }
  score.df$JUR_b <- score.df$JUR
  score.df$JURNUM_b <- score.df$JURNUM
  score.df$JUR <- ifelse(score.df$JUR == "Ashland", "Hanover", 
                         score.df$JUR)
  score.df$JURNUM <- ifelse(score.df$JURNUM == 14, 6, score.df$JURNUM)
  score.df$TAZ <- score.df$MODEL_TAZ
  score.df <- join(score.df, atype, by = "TAZ", type = "left", 
                   match = "first")
  FileType <- c("Base", "Control", "Scores")
  FileName <- c(baseF, ctlF, scoreF)
  files <- data.frame(FileType, FileName)
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
  vRetail <- vector()
  for (i in 1:length(retV)) {
    vRetail <- c(vRetail, sum(base.df[, retV[i]]))
  }
  vRetail <- cbind(retV, vRetail)
  RetFrac <- data.frame(vRetail)
  rm(vRetail)
  RetFrac$vRetail <- as.numeric(RetFrac$vRetail)
  RetFrac$frac <- RetFrac$vRetail/sum(RetFrac$vRetail)
  vNRetail <- vector()
  for (i in 1:length(nonV)) {
    vNRetail <- c(vNRetail, sum(base.df[, nonV[i]]))
  }
  vNRetail <- cbind(nonV, vNRetail)
  NonFrac <- data.frame(vNRetail)
  rm(vNRetail)
  NonFrac$vNRetail <- as.numeric(NonFrac$vNRetail)
  NonFrac$frac <- NonFrac$vNRetail/sum(NonFrac$vNRetail)
  work <- score.df[order(score.df$MODEL_TAZ), ]
  work$xHH_DEN <- mapply(getden, work$JURNUM, work$MTP_AT, 
                         "HH_DEN")
  work$xRET_DEN <- mapply(getden, work$JURNUM, work$MTP_AT, 
                          "RET_DEN")
  work$xNON_DEN <- mapply(getden, work$JURNUM, work$MTP_AT, 
                          "NON_DEN")
  work$HH_DEN <- ifelse(work$HH_DEN > work$xHH_DEN, work$HH_DEN, 
                        work$xHH_DEN)
  work$RET_EMP_DEN <- ifelse(work$RET_DEN > work$xRET_DEN, 
                             work$RET_DEN, work$xRET_DEN)
  work$NON_EMP_DEN <- ifelse(work$NON_DEN > work$xNON_DEN, 
                             work$NON_DEN, work$xNON_DEN)
  vtemhh <- work$HH_DEN * work$DevLand_Ar
  vtemret <- work$RET_DEN * work$DevLand_Ar
  vtemnre <- work$NON_DEN * work$DevLand_Ar
  work$cap_hh <- ifelse(vtemhh <= 2000, vtemhh, 2000)
  work$cap_ret <- ifelse(vtemret <= 2000, vtemret, 2000)
  work$cap_nre <- ifelse(vtemnre <= 2000, vtemnre, 2000)
  rm(vtemhh, vtemret, vtemnre)
  work$RET_EMP_COMPi <- work$RET_COMPi
  work$NON_EMP_COMPi <- work$NON_COMPi
  work$HH_MKTSC <- work$P_Index
  work$RET_EMP_MKTSC <- work$RE_Index
  work$NON_EMP_MKTSC <- work$NRE_Index
  work$HH_aci <- work$HHaci
  work$RET_EMP_aci <- work$EMPaci
  work$NON_EMP_aci <- work$EMPaci
  work$VAC_ACRE <- work$DevLand_Ar
  util <- function(df, params) {
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
      uret <- ifelse(df$JUR == JUR, cal.DF["RE_Index", 
                                           jx] * df$RE_Index + cal.DF["EMPaci_RET", 
                                                                      jx] * df$EMPaci + cal.DF["RET_COMPi", jx] * 
                       df$RET_COMPi + cal.DF["v_indexHHforRET", 
                                             jx] * df$v_idx_HH + cal.DF["cap_ret", jx] * 
                       df$cap_ret, uret)
      unre <- ifelse(df$JUR == JUR, cal.DF["NRE_Index", 
                                           jx] * df$NRE_Index + cal.DF["EMPaci_NRE", 
                                                                       jx] * df$EMPaci + cal.DF["v_indexNRE", 
                                                                                                jx] * df$v_idx_NRE + cal.DF["cap_nre", 
                                                                                                                            jx] * df$cap_nre, unre)
    }
    df$HH_score <- ifelse(df$P_Index > 0, exp(uhh), 0)
    df$RET_EMP_score <- ifelse(df$RE_Index > 0, exp(uret), 
                               0)
    df$NON_EMP_score <- ifelse(df$NRE_Index > 0, exp(unre), 
                               0)
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
  work$VAC_ACRE <- work$VAC_ACRE + work$vertHH + work$vertRE + 
    work$vertNRE
  xden <- den
  xbase.df <- base.df[base.df$ZONE %in% ivec, ]
  xwork <- work[work$TAZ %in% ivec, ]
  oden <- data.frame()
  owork <- data.frame()
  ozdat <- data.frame()
  ogdat <- data.frame()
  k_iters <- vector(mode = "numeric", length = length(8))
  k_jurnum <- vector(mode = "numeric", length = length(8))
  k_area <- 0
  
  
  
  for (k in jurs) {
    k_area <- k_area + 1
    den <- xden[xden$JURNUM == k, ]
    work <- xwork[xwork$JURNUM == k, ]
    base.df <- xbase.df[xbase.df$JURNUM == k, ]
    for (i in 1:nrow(den)) {
      den$basetot[i] <- sum(base.df[[den$LU_NAME[i]]])
    }
    den$GROW <- den$CNTL_TOT - den$basetot
    pri <- den[order(den$PRIORITY), ]
    pri <- pri[pri$PRIORITY > 0, ]
    GROW_pri <- as.vector(pri$GROW)
    work$VAC_LEFT <- work$VAC_ACRE
    v0 <- vector(mode = "numeric", length = length(work$VAC_LEFT))
    nc <- 1
    ik <- 0
    while (nc) {
      ik <- ik + 1
      for (i in 1:nrow(pri)) {
        dname <- paste0(pri$LU_NAME[i], "_DEN")
        sname <- paste0(pri$LU_NAME[i], "_score")
        snam2 <- paste0(pri$LU_NAME[i], "_score2")
        gname <- paste0(pri$LU_NAME[i], "_g")
        uname <- paste0(pri$LU_NAME[i], "_acres")
        work[[dname]] <- as.numeric(work[[dname]])
        if (ik == 1) {
          work[[gname]] <- v0
          work[[uname]] <- v0
        }
        work[[sname]] <- ifelse(work[[dname]] > 0, work[[sname]], 
                                0)
        work[[snam2]] <- ifelse(work$VAC_LEFT > 0, work[[sname]], 
                                0)
        tot <- sum(work[[snam2]])
        if (tot > 0) {
          work[[snam2]] <- work[[snam2]]/tot
        }
        else {
          work[[snam2]] <- v0
        }
        t_alloc <- work[[snam2]] * GROW_pri[i]
        t_consumed <- ifelse(work[[dname]] > 0, t_alloc/work[[dname]], 
                             0)
        t_consumed <- ifelse(t_consumed > work$VAC_LEFT, 
                             work$VAC_LEFT, t_consumed)
        t_left <- work$VAC_LEFT - t_consumed
        t_alloc <- ifelse(t_left > 0, t_alloc, work$VAC_LEFT * 
                            work[[dname]])
        t_left <- ifelse(t_left > 0, t_left, 0)
        work[[gname]] <- t_alloc + work[[gname]]
        work[[uname]] <- ifelse(work[[dname]] > 0, work[[gname]]/work[[dname]], 
                                work[[uname]])
        work$VAC_LEFT <- t_left
        if (GROW_pri[i] - sum(t_alloc) < -1) {
          mess <- paste0("negative, LU= ", i, ",  jur= ", 
                         k, ",  iter= ", ik, " GROW_pri=", 
                         GROW_pri[i], " sum(t_alloc)=", sum(t_alloc), 
                         "\n")
          cat(mess, "\n")
        }
        GROW_pri[i] <- GROW_pri[i] - sum(t_alloc)
      }
      if (sum(GROW_pri) <= 1) 
        nc <- 0
      if (ik > 20) 
        nc <- 0
      k_iters[k_area] <- ik
    }
    GHH <- sum(work$HH_g)
    pvarsX <- c("POP_HH", "POP_GQ", "AUTO")
    for (i in 1:length(pvarsX)) {
      j <- match(pvarsX[i], pvars)
      luT <- den[j, "GROW"]
      gvar <- paste0(pvarsX[i], "_g")
      work[, gvar] <- luT * work$HH_g/GHH
    }
    k12j <- match("K12_ENROLL", pvars)
    k12b <- den[k12j, "basetot"]
    k12f <- den[k12j, "CNTL_TOT"]
    if (k12b != 0) {
      k12fac <- k12f/k12b
    }
    else {
      k12fac <- 1
    }
    uj <- match("U_ENROLL", pvars)
    ub <- den[uj, "basetot"]
    uf <- den[uj, "CNTL_TOT"]
    if (ub != 0) {
      ufac <- uf/ub
    }
    else {
      ufac <- 1
    }
    zdat <- join(base.df, work, by = "TAZ")
    naics <- c("NAICS_11", "NAICS_21", "NAICS_22", 
               "NAICS_23", "NAICS_3133", "NAICS_42", 
               "NAICS_4445", "NAICS_4849", "NAICS_51", 
               "NAICS_52", "NAICS_53", "NAICS_54", 
               "NAICS_55", "NAICS_56", "NAICS_61", 
               "NAICS_62", "NAICS_71", "NAICS_72", 
               "NAICS_81", "NAICS_92", "NAICS_OTH")
    for (i in 1:length(nonV)) {
      fld <- paste0(nonV[i], "_f")
      fldg <- paste0(nonV[i], "_g")
      zdat[, fld] <- ifelse(zdat$NON_EMP > 0, zdat[, nonV[i]] * 
                              (zdat$NON_EMP_g + zdat$NON_EMP)/zdat$NON_EMP, 
                            (zdat$NON_EMP_g + zdat$NON_EMP) * NonFrac[i, 
                                                                      3])
      zdat[, fldg] <- zdat[, fld] - zdat[, nonV[i]]
    }
    for (i in 1:length(retV)) {
      fld <- paste0(retV[i], "_f")
      fldg <- paste0(retV[i], "_g")
      zdat[, fld] <- ifelse(zdat$RET_EMP > 0, zdat[, retV[i]] * 
                              (zdat$RET_EMP_g + zdat$RET_EMP)/zdat$RET_EMP, 
                            (zdat$RET_EMP_g + zdat$RET_EMP) * RetFrac[i, 
                                                                      3])
      zdat[, fldg] <- zdat[, fld] - zdat[, retV[i]]
    }
    gdat <- zdat
    zdat$TOT_POP_f <- zdat$TOT_POP + zdat$POP_HH_g + zdat$POP_GQ_g
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
    zdat <- zdat %>% mutate_if(is.numeric, round)
    zdat$ACRES <- base.df$ACRES
    zdat$AVGAUTO <- ifelse(zdat$HH_f > 0, zdat$AUTO_f/zdat$HH_f, 
                           0)
    of1 <- c("N", "ZONE", "PDC", "JUR", 
             "JURNUM", "JURCODE", "TOT_POP_f", 
             "POP_HH_f", "POP_GQ_f", "HH_f", 
             "K12_ENROLL_f", "U_ENROLL_f", "AUTO_f", 
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
             "NAICS_3133", "NAICS_42", "NAICS_4445", 
             "NAICS_4849", "NAICS_51", "NAICS_52", 
             "NAICS_53", "NAICS_54", "NAICS_55", 
             "NAICS_56", "NAICS_61", "NAICS_62", 
             "NAICS_71", "NAICS_72", "NAICS_81", 
             "NAICS_92", "NAICS_OTH", "AVGAUTO", 
             "ACRES", "JUR_b", "JURNUM_b")
    zdat$AVGAUTO <- round(zdat$AVGAUTO, 2)
    zdat <- zdat[, of1]
    colnames(zdat) <- of2
    gdat$USED <- gdat$VAC_ACRE - gdat$VAC_LEFT
    gdat <- gdat[, c("TAZ", "JURNUM", "HH_g", 
                     "HH_acres", "RET_EMP_g", "RET_EMP_acres", 
                     "NON_EMP_g", "NON_EMP_acres", "USED", 
                     "VAC_LEFT", "VAC_ACRE")]
    gdat <- round(gdat, 2)
    owork <- rbind(owork, work)
    ozdat <- rbind(ozdat, zdat)
    ogdat <- rbind(ogdat, gdat)
    oden <- rbind(oden, den)
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
  gsum <- ogdat %>% group_by(JURNUM) %>% summarize(N_Zones = n(), 
                                                   HH_g = sum(HH_g), HH_acres = sum(HH_acres), RET_EMP_g = sum(RET_EMP_g), 
                                                   RET_EMP_acres = sum(RET_EMP_acres), NON_EMP_g = sum(NON_EMP_g), 
                                                   NON_EMP_acres = sum(NON_EMP_acres), USED = sum(USED), 
                                                   VAC_LEFT = sum(VAC_LEFT), VAC_ACRE = sum(VAC_ACRE))
  gsum <- cbind(gsum, k_iters)
  gsum <- replace(gsum, is.na(gsum), 0)
  gsum <- as.data.frame(gsum)
  county_tab <- lusum(ozdat)
  base_tab <- lusum(xbase.df[xbase.df$JURNUM %in% jurs, ])
  gnames <- county_tab[order(county_tab$JURNUM), c("JUR", 
                                                   "JURNUM")]
  gsum$JUR <- gnames$JUR
  gsum <- gsum[order(gsum$JUR), ]
  gsum$ReqDen <- round((gsum$HH_g + gsum$RET_EMP_g + gsum$NON_EMP_g)/gsum$VAC_ACRE, 
                       2)
  gsum$HH_Den <- round(gsum$HH_g/gsum$HH_acres, 2)
  gsum$Ret_Den <- round(gsum$RET_EMP_g/gsum$RET_EMP_acres, 
                        2)
  gsum$Non_Den <- round(gsum$NON_EMP_g/gsum$NON_EMP_acres, 
                        2)
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
  oz2 <- ozdat
  nonTPO <- rtc.df[rtc.df$ZONE > 955, ]
  outzdatx <- rbind(oz2, nonTPO)
  outzdata <- rbind(outzdatx, tpo_pass_through)
  outzdata <- outzdata[order(outzdata$ZONE), ]
  message("** create Zonal data output ", luoutF)
  last2 <- outzdata[, c("AVGAUTO", "ACRES")]
  outzdata <- select(outzdata, c(-AVGAUTO, -ACRES))
  outzdata[is.na(outzdata)] = 0
  is.num <- sapply(outzdata, is.numeric)
  outzdata[is.num] <- lapply(outzdata[is.num], as.integer, 
                             0)
  outzdata <- cbind(outzdata, last2)
  write.dbf(file = luoutF, outzdata)
  
  
  
  # taz_2050 <- read.dbf(paste0(outdir, p2, alt, ".dbf")) # read this from RSLAM output
  taz_by_lu <- read.csv(paste0(indir, "TAZ_By_LU.csv")) #default input so copy this from alternative A
  message( " ** read the taz by lu file **")
  taz_by_lu$TAZ <- as.numeric(as.character(taz_by_lu$TAZ))
  model_output <- read.dbf(paste0(outdir, p2, alt, ".dbf"))# read this from rslam output
  
  message(" ** read the model output file")
  model_output <- model_output[model_output$ZONE %in% taz_by_lu$TAZ, ]
  
  file_name <- taz_by_lu
  file_name <- left_join(file_name, model_output[ , c("N","HH","TOT_EMP")], by = c( "TAZ" = "N"))
  
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
  
  write.csv(file_name, paste0(outdir, p2,"detailed_", alt, ".csv"), row.names = F)
  
  read.csv(paste0(indir, "TAZ_By_LU.csv"))
  detail_out <- read.csv(paste0(outdir, p2,"detailed_", alt, ".csv"))
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
  
  
  write.csv(filter_output, paste0(outdir, p2,"revisedAr_", alt, ".csv"), row.names = F)
  
  setwd(savwd)
  return(owork)
}


##########################################################################################



##### I need to read control and tazx file to run the program: for choices and display 

basedir <- "C:/R_SLAM_2/BaseLU"
taztags <- read.csv(str_c(basedir,"/Alt_A/TazTags.csv"))
exComputed <- read.csv(str_c(basedir,"/Alt_A/excludedComputed.csv"))
setwd(basedir)
control<-read.csv('Alt_A/input/control.csv')
control_default <- control
taz <- read.csv('Alt_A/input/TAZx.csv')
taz_default <- taz
rate_of_consumption <- read.csv('Alt_A/input/densities.csv')
rate_of_consumption_default <- rate_of_consumption
taz_shapefile <- st_read('Shapefiles', layer = "taz_to_display")
taz_data <- taz_shapefile
st_geometry(taz_data) <- NULL
taz_data <- left_join(taz_data,taztags[,c(1,8:45)], by = c("MODEL_TAZ" = "Model_TAZ"))

labels_of_taz <-  c("Developable Acreage","Household Compatibility Score","Retail Compatibility Score",
                    "Non-Retail Compatibility Score",
                    "Household Accessibility Score","Employment Accessibility Score","Developable Acreage",
                    "Population Index","Retail Index","Non-retail Index",
                    "Vertical HH developable acreage beyond the available area",
                    "Vertical Retail developable acreage beyond the available area",
                    "Vertical Non-Retail developable acreage beyond the available area",
                    "Household Vertical Index","Retail Vertical Index" ,
                    "Non-Retail Vertical Index","Exculde from the Model")

final_labels_of_taz <- stringr::str_c(labels_of_taz," (",colnames(taz)[4:length(taz)],")")



############# functions to do things

all_ids <- c("j_name", rep(c("hh", "ret", "non_ret","pop"), times = 2), 
             "model_taz", final_labels_of_taz, "area_type")
all_ids[2:8] <- c(str_c(all_ids[2:5], "_value"), str_c(all_ids[6:8], "_den"))


running_model <- function(base_dir_input, alt_name){
  print(getwd())
  if(base_dir_input == ""){
    base_dir_input <- basedir
  }
  
  if(alt_name == ""){
    alt_name <- "Alt_A"
  }
  
  runslam(base_dir_input,alt_name)
  message("*** RTC Land Use Model Complete ***\n") 
}
creating_alternative <- function(base_dir_input, alt_name ){
  if(base_dir_input == ""){
    base_dir_input <- "C:/R_SLAM_2/BaseLU"
  }
  
  if(alt_name == ""){
    base_dir_input <- "C:/R_SLAM_2/BaseLU"
  }else {
    dir.create(str_c("C:/R_SLAM_2/BaseLU/", alt_name))
    dir.create(str_c("C:/R_SLAM_2/BaseLU/",alt_name, "/", "Input"))
    dir.create(str_c("C:/R_SLAM_2/BaseLU/", alt_name,"/", "Output"))
  }
  
  #reading files from the parent folder
  parent_files_path <- list.files(paste0(base_dir_input, '/Alt_A/input'), full.names = T)
  child_files_path <- paste0(base_dir_input,"/" ,alt_name, "/input")
  file.copy(parent_files_path, child_files_path )
  
}
changing_ui <- function(file_choice){
  
  creating_ti_taz <- function(){
    # labels_of_taz <-  c("Developed Acreage","Household Compatibility Score","Retail Compatibility Score","Non-Retail Compatibility Score",
    #                     "Household Accessibility Score","Employment Accessibility Score","Developable Acreage",
    #                     "Population Index","Retail Index","Non-retail Index","Vertical value Households in acres","Vertical value Retail in acres","Vertical value Non-Retail in acres",
    #                     "Household Vertical Index","Retail Vertical Index" ,"Non-Retail Vertical Index","Exculde from the Model")
    # final_labels_of_taz <- stringr::str_c(labels_of_taz," (", name_columns[4:length(taz)],")")
    
    inputs_taz <- lapply(final_labels_of_taz, function(x){
      textInput(inputId = x, label = x, placeholder = "Enter value to not use defaults")
    })
    return(inputs_taz)
  }
  
  jurisdiction <- pickerInput("j_name", "Select Jurisdiction", choices = unique(control$Jname), selected = "Richmond",
                              options = list(title = paste0( unique(control$Jname), collapse = ", ")))
  buttons <- list( actionButton("change_input", "Change Input"),
                   actionButton("csv", "Write  CSV"))
  if(file_choice == 1){
    list(
      jurisdiction,
      textInput("pop_value", "Enter Total population", placeholder = "Enter value to not use defaults"),
      textInput("hh_value", "Enter the number of Households", placeholder = "Enter value to not use defaults"),
      textInput("ret_value", "Enter the number of Retail Jobs", placeholder = "Enter value to not use defaults"),
      textInput("non_ret_value", "Enter the number of Non-retail Jobs", placeholder = "Enter value to not use defaults"), 
      buttons)
  } else if(file_choice == 2){
    list(
      jurisdiction,
      pickerInput("area_type", "Select Area type", choices = c("Urban" = 1, "Semi-Urban" = 2, "Rural" = 3), 
                  options = list(title =  paste0(c("Urban", "Semi-Urban", "Rural"), collapse = ", "))),
      textInput("hh_den", "Enter the density of HH per acre", placeholder = "Enter value to not use defaults"),
      textInput("ret_den", "Enter the density of jobs per Acre (Retail Employment)", placeholder = "Enter value to not use defaults"),
      textInput("non_ret_den", "Enter the density of jobs per Acre (Non-retail Employment)", placeholder = "Enter value to not use defaults"), 
      buttons)
    
  } else  if(file_choice == 3){
    list(
      jurisdiction,
      pickerInput("model_taz", "Select TAZ", choices = c(), options = list(title = "1, 2, 3, 4, 5 ..."), multiple = T),
      creating_ti_taz(),
      buttons)
    
    
  }
}

changing_columns <- function(name_df, col_to_change, data_to_replace, row_number, choice = 1){
  if(choice == 1){
    j <- 1
    for( i in col_to_change){ #directly using the name of the column in i 
      print(i)
      name_df[[i]][row_number] <- as.numeric(data_to_replace[j]) 
      #print(x[j])
      j <- j + 1
    }
    return(name_df)
  } else if(choice == 2) {
    for( i in col_to_change){ #directly using the name of the column in i 
      print(i)
      name_df[[i]][row_number] <- as.numeric(data_to_replace[[i]][row_number]) 
    }
    return(name_df)
  }
}


changing_input_file <- function(data_by_juris, file_choice, taz_group_input = ""){
  
  file_choice <- as.numeric(file_choice)
  x <- unlist(strsplit(data_by_juris, split = ",")) # "JNAME", "HH","RET_JOBS", "NON_RET_JOBS"
  
  if(file_choice == 1) {
    
    indices <- which(control$Jname == x[1])
    rows_to_change <- indices[1:4]
    jur_name <- x[1]
    
    if(x[2] == "" & x[5] !="" ){
      x[2] <- round(as.numeric(x[5]) / as.numeric(control_default$Ratio[rows_to_change[1]]))
      control[rows_to_change[4], "CNTL_TOT"] <<- as.numeric(x[5]) - as.numeric(control$CNTL_TOT[control$Jname == jur_name][5])
    } else if( x[2] != "" & x[5] !="" ) {
      control[rows_to_change[4], "CNTL_TOT"] <<- as.numeric(x[5]) - as.numeric(control$CNTL_TOT[control$Jname == jur_name][5])
    }
    
    x <- x[-1]
    
    if(x[4] != ""){
      x <- x[-4]
      rows_to_change <- rows_to_change[1:3]
    }
    
    empty_indices <- grepl(pattern = "\\S", x )  #T, F, T
    
    control$CNTL_TOT[ rows_to_change[empty_indices] ] <<- as.numeric(x[empty_indices])
    control$CNTL_TOT[ rows_to_change[!empty_indices] ] <<- as.numeric(control_default$CNTL_TOT[rows_to_change[!empty_indices]])
    return(control)
    
  } else if (file_choice == 3) {
    taz_to_change <- as.numeric(taz_group_input)
    index <- unlist(lapply(taz_to_change, function(x){
      which(taz$MODEL_TAZ == x)
    }))
    empty_indices <- grepl(pattern = "\\S", x ) #### columns to change from user input using it on data by jurisdiction
    columns_to_change <- colnames(taz)[4:length(taz)]
    columns_to_change_from_user_input <- columns_to_change[empty_indices] #### columns to change from user input
    columns_to_change_from_default <- columns_to_change[!empty_indices] #### columns to change from default
    
    taz <<- changing_columns(taz, columns_to_change_from_user_input, x[empty_indices], index)
    taz <<- changing_columns(taz, columns_to_change_from_default, data_to_replace = taz_default,index, choice = 2)
    return(taz)
    as
  } else if (file_choice == 2) {
    x <- unlist(strsplit(data_by_juris, split = ",")) # "JNAME", "HH","RET_JOBS", "NON_RET_JOBS"
    filtered_by_juris <- which(rate_of_consumption$JUR == x[1] & rate_of_consumption$MTP_AT == as.numeric(x[5]))
    x <- x[ -c(1,5)]
    empty_indices <- grepl(pattern = "\\S", x )  #T, F, T
    columns_to_change <-     colnames(rate_of_consumption)[4:6]
    cols_to_change_user_input <- columns_to_change[empty_indices]
    cols_to_change_default <- columns_to_change[!empty_indices]
    
    rate_of_consumption <<- changing_columns(name_df = rate_of_consumption, 
                                             col_to_change =  cols_to_change_user_input, 
                                             data_to_replace = x[empty_indices], row_number = filtered_by_juris)
    rate_of_consumption <<- changing_columns(rate_of_consumption, cols_to_change_default,
                                             row_number = filtered_by_juris, choice = 2,
                                             data_to_replace = rate_of_consumption_default)
    return(rate_of_consumption)
  }
}



#writing_file(input$m_file, input$base_dir, input$alt_name)
writing_file <- function(choice, base_dir_input, alt_name){
  if(base_dir_input == ""){
    base_dir_input <- basedir
  }
  if(alt_name == ""){
    alt_name <- ""
  }
  
  if(choice == 1) {
    return(str_c(base_dir_input,"/",alt_name,"/Input/","control.csv"))
  } else if(choice == 2){
    return(str_c(base_dir_input,"/",alt_name,"/Input/","densities.csv"))
  } else if(choice == 3){
    return(str_c(base_dir_input,"/",alt_name, "/Input/","TAZx.csv"))
  }
}
reading_file <- function(choice, base_dir_input, alt_name){
  if(base_dir_input == ""){
    base_dir_input <- basedir
  }
  if(alt_name == ""){
    alt_name <- ""
  }
  
  if(choice == 1) {
    a <- read.csv(str_c(base_dir_input,"/",alt_name,"/Input/","control.csv"))
    return(a)
  } else if(choice == 2){
    b <- read.csv(str_c(base_dir_input,"/",alt_name,"/Input/","densities.csv"))
    return(b)
  } else if(choice == 3){
    c <- read.csv(str_c(base_dir_input,"/",alt_name, "/Input/","TAZx.csv"))
    return(c)
  }
}
reading_output_of_alternative <- function(base_dir_input, alt_name){
  if(base_dir_input == ""){
    base_dir_input <- basedir
  }
  
  if(alt_name == ""){
    message("No Alternative name")
  }
  output_file_path <- list.files(str_c(base_dir_input,"/",alt_name,"/Output"), pattern = ".dbf$", full.names = T)
  main_output <- read.dbf(output_file_path)
  return(main_output)
  
}
reading_output_file <- function(base_dir_input, alt_name, name_column){
  if(base_dir_input == ""){
    base_dir_input <- basedir
  }
  
  if(alt_name == ""){
    message("No Alternative name")
  }
  output_file_path <- list.files(str_c(base_dir_input,"/",alt_name,"/Output"), pattern = ".xlsx$", full.names = T) 
  
  output_data <- output_file_path %>%
    excel_sheets() %>%
    set_names() %>%
    map(read_excel,
        path = output_file_path)
  
  return(output_data[[name_column]])
}
reading_taz_output <- function(base_dir_input, alt_name, type = "model"){
  if(base_dir_input == ""){
    base_dir_input <- basedir
  }
  if(alt_name == ""){
    alt_name <- "Alt_A"
  }
  taz_visualize <- taz_shapefile[,1]
  #base_data <- list.files(str_c(basedir,"/",alt_name,"/Input"), pattern = ".dbf$", full.names = T)
  if(type == "model") {
    
    output_file_path <- list.files(str_c(basedir,"/",alt_name,"/Output"), pattern = ".dbf$", full.names = T)
    main_output <- read.dbf(output_file_path)
    final_data <- left_join(taz_visualize, main_output, by = c("MODEL_TAZ" = "ZONE"))
  } else if( type == "base") {
  
    base_file<- read.dbf(str_c(basedir,"/",alt_name,"/Input/RTC_LANDUSE_2017.dbf"))
    final_data <- left_join(taz_visualize, base_file, by = c("MODEL_TAZ" = "ZONE"))
  }  else if( type == "horizon") {
    horizon_file <- read.dbf(str_c(basedir,"/",alt_name,"/Input/RTC_LANDUSE_2045.dbf"))
    final_data <- left_join(taz_visualize, horizon_file, by = c("MODEL_TAZ" = "ZONE"))
  }
  return(final_data)
}

write_exclude <- function(choice, base_dir_input, alt_name){
 
  message(choice)
  message(alt_name)
   if(base_dir_input == ""){
    base_dir_input <- basedir
  }
  if(alt_name == ""){
    alt_name <- ""
  }
  
  fileName <- read.csv(str_c(base_dir_input,"/",alt_name, "/Input/TAZx.csv"))
  
  selectionExclude <- as.data.frame(cbind(exComputed[["Model_TAZ"]], exComputed[[choice]]))
  colnames(selectionExclude) <- c("MODEL_TAZ", "ExcludeThis")
  message('I failed on message 2')
  x <- left_join(fileName, selectionExclude, by ="MODEL_TAZ")
  x$EXCLUDE <- x$ExcludeThis
  
  write.csv(x[,1:20], 
            str_c(base_dir_input,"/",alt_name, "/Input/","TAZx.csv"),
            row.names = F)
  
  
}
create_scen_map <- function(base_dir_input, alt_name, type=1){
  
  if(base_dir_input == ""){
    base_dir_input <- basedir
  }
  if(alt_name == ""){
    alt_name <- "Alt_A"
  }
  
  message(base_dir_input)
  message(alt_name)
  taz_sf <- st_read(str_c(basedir,"/",alt_name, "/Output"),
                    layer = "tazx")
  exclude_tazs <- read.csv(str_c(basedir,"/",alt_name,
                                 "/Output/Exclude.csv"))
  tdm_file <- read.dbf(str_c(basedir,"/",alt_name,
                             "/Output/RSlam_output_",alt_name,".dbf"))
  
  base_file <- read.dbf(str_c(basedir,"/",alt_name,
                              "/Input/RTC_LANDUSE_2045.dbf"))
  
  x <- tdm_file[tdm_file$JURNUM %in% exclude_tazs$JURNUM,] # tazs of 3 jurisdictions that are excluded
  y <- x[!(x$N %in% exclude_tazs$MODEL_TAZ), ] # TAZs of Jurisdiction,
  # that have rt 33
  
  y_baseline <- left_join(y[,c(1,4,7:10)], 
                          base_file[,c(1,7:10)], 
                          by = "N")
  
  y_baseline$POP_Delta <- y_baseline$TOT_POP.x - y_baseline$TOT_POP.y
  y_baseline$HH_Delta <- y_baseline$HH.x - y_baseline$HH.y
  
  
  z <- left_join(taz_sf[taz_sf$MODEL_TAZ %in% y_baseline$N,1], 
                 y_baseline, by = c("MODEL_TAZ" = "N") )
 
}


###############################################

ui <- navbarPage(theme = shinytheme("flatly"),title = "Land-Allocation Model", id = "nav", 
                 tabPanel("About", div(class = "rvalogo",img(src='TPO_Logo_road.png',height="20%", width="20%", align = "right")), 
                          tags$style(".rvalogo{margin-bottom:-20px; padding-top : 15px; padding-right: 50px}"),
        jumbotron("RSLAM", "Richmond Simplified Land Use Allocation Model (RSLAM) was developed to support the modeling and 
                  planning effort, and to facilitate scenario planning. It is a data-driven growth allocation model that creates 
                  a zonal data file suitable for input to and use by the RRTPO regional travel demand model. The model begins with
                  a zonal data file from the base year, and regional future year control totals for each zonal data item, and 
                  allocates the growth specified in the control totals to individual zones, thereby creating a future year zonal 
                  data file. It handles the eight jurisdictions in the region separately. It uses local data to guide the way that 
                  growth is allocated and was customized for Richmond. It is a fast R application, and has been calibrated using 
                  local data to replicate current forecasts. Besides the future year zonal data file, the model produces tables and 
                  maps to illustrate model assumptions and results and is controlled by a graphical user interface.",
                 buttonLabel = "Watch Video"),
fluidRow( column(6, 
                 panel_div(class_type = "primary", panel_title = "Directions",
                content = HTML(" <a href='https://docs.google.com/document/d/14geGbfDAZhPOGPn5x2zHBvEsyPADUkvMELttNEHTm_4/edit?usp=sharing'>How to use the app</a>"))),   
          column(6, panel_div("success", "Application Maintainers",
                HTML("Email: <a href='mailto:SAryal@planrva.org?Subject=Shiny%20Help' target='_top'> Sulabh Aryal</a>")))
         ),  # end of fluidRow
fluidRow(
  column(6, panel_div("info", "App Status", "RSLAM version 1.0")),
  column(6, panel_div("danger", "Security and License", "Copyright 2020")), # End of Fluidrow
  
  #### FAVICON TAGS SECTION ####
  tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
  bsModal("modalExample", "Instructional Video", "tabBut", size = "large" ,
          p("Please watch the full video to learn how to run the Model"),
          iframe(width = "560", height = "315", url_link = "https://www.youtube.com/watch?v=30oPT43Y4-I"))
       ), # end of Fluidrow

fluidRow(
  column(6, panel_div( class_type = "primary",panel_title = "Created by :  ",
                  content = list( div(class = "creators", 
                          h5(HTML("<a href = 'https://www.linkedin.com/in/ken-kaltenbach-a699149/'>Ken Kaltenbach </a>, 
                                   <a href = 'https://www.linkedin.com/in/parag-gupta-29413214a/'>Parag Gupta </a>, 
                                  <a href = 'https://www.linkedin.com/in/srinivas-srin-varanasi-9a538a2a/'>Srinivas Varanasi</a>"))),
                                              tags$style(".creators{color:; margin-bottom:-5px; margin-top:-12px}"),
                                              tags$style(".corradino{margin-bottom:-8px; margin-top:px;}"),
                                              div(class = "corradino",img(src='logo1.png', height = "47%", width = "47%"))
                                              
                              ))))), # end of tabPanel

tabPanel("Input", useSweetAlert(),
         #titlePanel("Input Modifiication"),
         sidebarLayout(
           sidebarPanel(
             titlePanel("Input Modifiication"),
             #textOutput("value"),
             textInput("base_dir", label = "Type the path of selected directory", placeholder = basedir),
             textInput("alt_name", label = "Specify the name of the Alternative", 
                       placeholder =  "Type the name of Alternative"),
             actionButton("create_folder", label = "Create Alternative"), 
             shinyDirButton('folder', 'Existing Alternative', 'Please select a folder', FALSE),
             actionButton("changeName", label = "Read Data"),
             pickerInput("m_file", "Which file you want to modify?",
                         choices = c("Control Total" = 1, "Density (Rate of Consumption of land)" = 2, 
                                     "TAZ" = 3),
                         selected = 1, options = list(title = "Select from the choices")),
           uiOutput("more_controls"),
           br(),
           actionButton("run_model", "Run Model"),
         ),
         mainPanel(
           tabsetPanel( tabPanel("Baseline", reactableOutput("current_table") %>% withSpinner()),
                        tabPanel("Scenario specific", reactableOutput("new_table") %>% withSpinner()),
                         tabPanel("TAZ Map", withSpinner(
                         leafletOutput("taz_choice_map", width="100%", height= 900)) ,
                                absolutePanel( class = "panel panel-default",
                                               top = 150, right = 30, width = 350, fixed=TRUE,
                                               draggable = TRUE, height = "auto",
                                               style = "background-color: white;
                                              padding: 10px 0px 10px 10px;
                                              cursor: move;
                                            /* Fade out while not hovering */
                                              opacity: 0.75;
                                              zoom: 0.9;
                                          transition: opacity 500ms 1s;",
                                         
                                          pickerInput("taz_attribute", 
                                                "Select TAZ attribute to see the information of :",
                                            choices = colnames(taz_default)[4:length(taz_default)]),
                                          pickerInput("scenario", 
                                                      "Select any Alternative Geography :",
                                                      choices = colnames(taz_data)[25:57]),
                                          # reactableOutput("taz_choice_table"),
                                          actionButton("writeExclude", "Write Exclude")
                                          
                                ))
           ) # end of tabset panel
         ) )), # end of Input Panel
tabPanel("Output",
         tabsetPanel(tabPanel("Map",
                              style = "position: fixed; top: 125px;left: 0; right: 0;
                                       bottom: 0; overflow: hidden; padding: 0;" ,
                      leafletOutput("mymap", width="100%", height= 985) %>% withSpinner(),
                      absolutePanel( class = "panel panel-default",
                                     top = 150, right = 50, width = 230, fixed=TRUE,
                                     draggable = TRUE, height = "auto",
                                     style = "background-color: white;
                                              padding: 10px 0px 10px 10px;
                                              cursor: move;
                                            /* Fade out while not hovering */
                                              opacity: 0.8;
                                              zoom: 0.9;
                                          transition: opacity 500ms 1s;",
                                     selectInput("taz_col_name", label = "Select Variable to Visualize", 
                                                 choices = c("Total Population" = "TOT_POP", 
                                                             "Households" = "HH", 
                                                             "Reatil Jobs" = "RET_EMP",
                                                             "Non Retail Jobs" = "NON_EMP"),               
                                                 selected = "Total Population", width = '95%'),
                                     selectInput("no_of_class", label = "Number of classes", 
                                                 choices =  c("3","4","5","6","7","8"), selected = "5",
                                                 width = '95%'),
                                     selectInput("breaks", "Method to Categorize:", width = '95%', 
                                                 choices = c("Quantile" = "quantile", 
                                                             "Fisher" = "fisher",
                                                             "Jenks" = "jenks",
                                                             "K Means clustering" = "kmeans"),
                                                 selected = "jenks"),
                                     selectInput("color_palette", "Select Color Options", 
                                                 choices = row.names(brewer.pal.info), 
                                                 selected = row.names(brewer.pal.info)[35]),
                                     # actionButton("read_taz_output", "Read TAZ"),
                                     actionButton("plot", "Create Map")
                      )),
                     tabPanel("Table", selectInput("column_output", "Select table to choose", 
                            choices = c("Output Summary","Input Summary","New Zonal Data","Growth Allocation","Growth Summary",  
                                  "Input scores","Input Controls" ,"Working Data", "ZDATA_in")),
                              actionButton("read_data", "Output Data"),
                              DT::dataTableOutput("alt_output") %>% withSpinner()),
                     tabPanel("Main Output", actionButton("read_model_data", "Read Data"),
                              DT::dataTableOutput("model_file") %>% withSpinner()),
                     tabPanel("Scenario Map", actionButton("read_Scen_data", "Read Scenario Data"),
                              leafletOutput("scenMap", width="100%", height = "75vh") %>% withSpinner())
                     
         )))

################################################################################

server <- function(input, output, session) {

   shinyDirChoose(input, 'folder', roots=c(wd='C:/'), filetypes=c('', 'txt'))
  
  # # file <- reactive(parseFilePaths(roots, input$file))
  # output$file <- renderPrint(input$folder[[1]][[2]])
  folder_name <- eventReactive(input$changeName,{
    input$folder[[1]][[length(input$folder[[1]])]]
  })
  
  observeEvent(input$changeName,{
    updateTextInput(session, "alt_name", value = folder_name())
  })
   
  
  #creating new alternative 
  observeEvent(input$create_folder, { 
    do_this <- function(){
      creating_alternative(input$base_dir, input$alt_name)
      sendSweetAlert(
        session = session,
        title = "Success !!",
        text = "All in order",
        type = "success"
      )}
    do_this()
  })
  
  #run model 
  observeEvent(input$run_model, 
               ask_confirmation(
                 inputId = "myconfirmation",
                 title = "Want to confirm ?"
               ))
  
  
  observeEvent(input$myconfirmation, {
    if (isTRUE(input$myconfirmation)) {
      
      showModal(modalDialog(
        title = "RSLAM Model",
        "RSLAM model in progress....",
        footer = NULL
      ))
      running_model(input$base_dir, input$alt_name)
      removeModal()
      sendSweetAlert(
        session = session,
        title = "Success !!",
        text = "All in order",
        type = "success"
      )
      
    } else {
    }
  }, ignoreNULL = TRUE)
  
  #dynamic UI
  output$more_controls <- renderUI(
    changing_ui(input$m_file)
  )
  
  ###updating taz choices
  observeEvent(input$j_name, {
    updatePickerInput(session, "model_taz", choices = taz$MODEL_TAZ[taz$JUR %in% input$j_name])
  })
  
  #displaying the table to modify
  current_table_display <- eventReactive(input$m_file, {
    if(input$m_file == 1){
      return(control_default)
    }else if(input$m_file == 2){
      return(rate_of_consumption_default)
    } else if(input$m_file == 3){
      return(taz_default)
    }
  })
  output$current_table <- renderReactable(
                              reactable(current_table_display(),  
                              compact = T, filterable = T, defaultPageSize = 50, 
                              bordered = T, highlight = T,
                              showPageSizeOptions = TRUE, height = 730,
                              defaultColDef = colDef(align = "center")))
  
  output$value <- renderText(string_to_display())
  
  group_taz_input <- eventReactive(input$change_input, 
                        switch(input$m_file, "3" = input$model_taz ))  
  string_to_display <- eventReactive(input$change_input,
                                     #switch(input$m_file, str_c(past0(input[[all_ids[1:3]]], collapse = ","), ","),"bye","bye from 3")
                                     switch(input$m_file, 
                                            "1" = str_c(paste0(unlist(lapply(all_ids[1:5], 
                                                        function(x) input[[x]])), collapse = ","), ","),
                                            "2" = str_c(paste0(unlist(lapply(all_ids[c(1,6:8, length(all_ids))], 
                                                        function(x) input[[x]])), collapse = ","), ","),
                                            "3" = str_c(paste0(unlist(lapply(all_ids[12:length(all_ids) - 1], 
                                                        function(x) input[[x]])), collapse = ","), ",")
                                     ))
  
  
  
  #observeEvent(input$csv, changing_input_file(string_to_display()))
  observeEvent(input$csv, { 
                                write.csv(changing_input_file(string_to_display(),
                                file_choice = input$m_file, 
                                taz_group_input = group_taz_input()), 
                                file = writing_file(input$m_file, input$base_dir, input$alt_name), 
                                row.names =  F)
  })
  
  new_file <- eventReactive(input$csv, {
    changing_input_file(string_to_display(), 
                      file_choice = input$m_file, 
                      taz_group_input = group_taz_input())})
  new_file <- eventReactive(input$changeName, {
    reading_file( choice = input$m_file, 
                         basedir, 
                  folder_name())
    })
  
  output$new_table <- renderReactable(
    reactable(new_file(),  compact = T, filterable = T, 
              defaultPageSize = 50, bordered = T, highlight = T,
                  showPageSizeOptions = TRUE, height = 730,
                 defaultColDef = colDef(align = "center")) )

  
  ##### TAZ selection

  
  # output$taz_choice_table <- renderReactable(
  #   reactable(taz_data[,c(1:3,25:57)], selection = "multiple", onClick = "select",
  #             compact = T, filterable = T, defaultPageSize = 10,
  #             defaultSelected = 1:10))
  
  selected <- eventReactive(input$scenario, { taz_data[[input$scenario]] == 1})
  
  observeEvent(input$writeExclude,{
    write_exclude(choice = input$scenario, basedir, input$alt_name)
    sendSweetAlert(
      session = session,
      title = "Success !!",
      text = "All in order",
      type = "success"
    )
  })
  
  putting_labels <- reactive( sprintf(
    "TAZ ID : %s<br/>%s : %g",
    taz_data$MODEL_TAZ[selected()], input$taz_attribute,
    taz_default[[input$taz_attribute]][selected()]) %>% lapply(htmltools::HTML))
  
  output$taz_choice_map <- renderLeaflet(
    leaflet() %>% 
      addProviderTiles(providers[[110]]) %>%
      # addDrawToolbar(
      #   targetGroup = "draw",
      #   editOptions = editToolbarOptions(
      #     selectedPathOptions = selectedPathOptions())) %>%
      # addProviderTiles(providers$Esri.WorldImagery ,  options = providerTileOptions(noWrap = TRUE)) %>%  
      # setView(lng = -77.43 , lat = 37.57, zoom = 10) %>% 
      addPolygons(data = st_transform(taz_shapefile[selected(),], crs = '+proj=longlat +datum=WGS84'), 
                  label = putting_labels(),
                  labelOptions = labelOptions( textsize = "14px"), 
                  fillColor = "#A52A2A", weight = 2,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.3,
                  highlightOptions = highlightOptions(color = "black",
                                                      weight = 2,
                                                      bringToFront = TRUE)))
  
  #######Output
  
  data_to_display <- eventReactive(input$read_data, {
    reading_output_file(input$base_dir, input$alt_name, input$column_output)}) 
  
  output$alt_output <- DT::renderDataTable(data_to_display())
  main_model_output <-  eventReactive(input$read_model_data, 
                         reading_output_of_alternative(input$base_dir, 
                                                       input$alt_name))
  
  
  output$model_file <- DT::renderDataTable(main_model_output())
  
  tazs <- eventReactive(input$plot, 
                        reading_taz_output(input$base_dir, 
                            input$alt_name, type = "model"))
  
  base_tazs <- eventReactive(input$plot, 
                             reading_taz_output(input$base_dir, 
                             input$alt_name, type = "base"))
  
  future_tazs <- eventReactive(input$plot, 
                               reading_taz_output(input$base_dir, 
                              input$alt_name, type = "horizon"))
  
  scen_tazs <- eventReactive(input$read_Scen_data, 
                             create_scen_map(input$base_dir, 
                                  input$alt_name))
  
  
  output$scenMap <- renderLeaflet({
    leaflet() %>% 
      clearShapes() %>%
      clearControls() %>%
      addProviderTiles(providers[[110]]) %>%
      setView(lng = -77.43 , lat = 37.54, zoom = 13)
  })
  
  observeEvent(input$read_Scen_data,{
    labels <- sprintf(
      "<strong>MODEL TAZ</strong> : %s<br/><strong>County </strong>: %s<br/>
  <strong>Scenario TotPop </strong>: %s<br/>
  <strong>Baseline Pop</strong> : %s<br/><strong>Scenario HH </strong>: %s<br/>
  <strong>Baseline HH</strong> : %s<br/>",
  scen_tazs()$MODEL_TAZ, scen_tazs()$JUR,scen_tazs()$TOT_POP.x, scen_tazs()$TOT_POP.y, scen_tazs()$HH.x, scen_tazs()$HH.y) %>%
      lapply(htmltools::HTML)
    
    # getting breaks and coloring
    
    bks <- getBreaks(v = scen_tazs()$TOT_POP.x ,
                     nclass = 5,
                     method = "jenks")
    
    
    
    pal <- colorBin("YlOrRd", domain = scen_tazs()$TOT_POP.x, bins = bks)
    
    leafletProxy("scenMap") %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data = st_transform(scen_tazs(),4326),
                  weight = 2 , 
                  label = labels,
                  fillColor =  ~pal(scen_tazs()$TOT_POP.x),
                  fillOpacity = 0.7,
                  color = "black",
                  dashArray = "3",
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.4,
                    bringToFront = TRUE)) %>%
      addLegend(pal = pal, values =scen_tazs()$TOT_POP.x,
                opacity = 0.7, title = NULL, 
                position = "topright") %>%
      addScaleBar()
  })

  
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      clearShapes() %>%
      clearControls() %>%
      addProviderTiles(providers[[110]]) %>%
       setView(lng = -77.43 , lat = 37.54, zoom = 13) #%>%
      # addPolygons(data = st_transform(tazs(), crs = '+proj=longlat +datum=WGS84'),weight = 3, dashArray = 6,
      #             color = "white",
      #             fillColor = "black",
      #             fillOpacity = 0.2,  
      #             highlightOptions = highlightOptions(color = "black", weight = 2,bringToFront = TRUE))
         
  })
  
  bks2 <- eventReactive(input$plot, {
    as.numeric(getBreaks(v = switch(input$taz_col_name,
                                    "TOT_POP" = as.numeric(tazs()$TOT_POP),
                                    "HH" = as.numeric(tazs()$HH),
                                    "RET_EMP" = as.numeric(tazs()$RET_EMP),
                                    "NON_EMP" = as.numeric(tazs()$NON_EMP)) ,
                         nclass = as.numeric(input$no_of_class),
                         method = input$breaks))
    
  })
  
  bks <- eventReactive(input$plot, {
    as.numeric(getBreaks(v = switch(input$taz_col_name,
                                    "TOT_POP" = as.numeric(base_tazs()$TOT_POP ),
                                    "HH" = as.numeric(base_tazs()$HH),
                                    "RET_EMP" = as.numeric(base_tazs()$RET_EMP),
                                    "NON_EMP" = as.numeric(base_tazs()$NON_EMP)) ,
                         nclass = as.numeric(input$no_of_class),
                         method = input$breaks))
    
  })
  
  bks_horizon <- eventReactive(input$plot, {
    as.numeric(getBreaks(v = switch(input$taz_col_name,
                                    "TOT_POP" = as.numeric(future_tazs()$TOT_POP ),
                                    "HH" = as.numeric(future_tazs()$HH),
                                    "RET_EMP" = as.numeric(future_tazs()$RET_EMP),
                                    "NON_EMP" = as.numeric(future_tazs()$NON_EMP)) ,
                         nclass = as.numeric(input$no_of_class),
                         method = input$breaks))
    
  })
  
  
  observeEvent(input$plot , {
    pal <- colorBin(input$color_palette, domain = as.numeric(tazs()[[input$taz_col_name]]), bins = as.numeric(bks2()))
    pal_base <- colorBin(input$color_palette, domain = as.numeric(base_tazs()[[input$taz_col_name]]), bins = as.numeric(bks()))
    pal_horizon <- colorBin(input$color_palette, domain = as.numeric(future_tazs()[[input$taz_col_name]]), bins = as.numeric(bks_horizon()))
    labels = sprintf(
      "<strong>TAZ ID : %s</strong><br>%s : %g",
      tazs()$N, input$taz_col_name, tazs()[[input$taz_col_name]]) %>% lapply(htmltools::HTML)
    label_base = sprintf(
      "<strong>TAZ ID : %s</strong><br>%s : %g",
      base_tazs()$N, input$taz_col_name, base_tazs()[[input$taz_col_name]]) %>% lapply(htmltools::HTML)
    label_horizon = sprintf(
      "<strong>TAZ ID : %s</strong><br>%s : %g",
      future_tazs()$N, input$taz_col_name, future_tazs()[[input$taz_col_name]]) %>% lapply(htmltools::HTML)
    
    
    leafletProxy("mymap") %>%
      clearControls() %>%
      clearShapes() %>%
      addPolygons( data = st_transform(tazs(), crs = '+proj=longlat +datum=WGS84') ,
                   weight = 1, dashArray = 3,
                   fillColor = ~pal(tazs()[[input$taz_col_name]]),
                   fillOpacity = 0.7,
                   label = labels,
                   highlight = highlightOptions(
                     weight = 2, color = "green",fillOpacity = 0.5,bringToFront = TRUE, dashArray = ""),
                   labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                               textsize = "15px",direction = "auto"), group = "Model") %>%
      addLegend(pal = pal, values = tazs()[[input$taz_col_name]], 
                opacity = 0.7, title = NULL, position = "topleft", group = "Model") %>%
      addPolygons(data = st_transform(base_tazs(), crs = '+proj=longlat +datum=WGS84') ,
                  weight = 1, dashArray = 3,
                  fillColor = ~pal_base(base_tazs()[[input$taz_col_name]]),
                  fillOpacity = 0.7,
                  label = label_base,
                  highlight = highlightOptions(weight = 2, color = "green",fillOpacity = 0.5,bringToFront = TRUE, dashArray = ""),
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                              textsize = "15px",direction = "auto"), group = "2017") %>% 
      addLegend(pal = pal_base, values = base_tazs()[[input$taz_col_name]], 
                opacity = 0.7, title = NULL, position = "topleft", group = "2017") %>% 
      addPolygons( data = st_transform(future_tazs(), crs = '+proj=longlat +datum=WGS84') ,
                   weight = 1, dashArray = 3,
                   fillColor = ~pal_horizon(future_tazs()[[input$taz_col_name]]),
                   fillOpacity = 0.7,
                   label = label_horizon,
                   highlight = highlightOptions(
                     weight = 2, color = "green",fillOpacity = 0.5,bringToFront = TRUE, dashArray = ""),
                   labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                               textsize = "15px",direction = "auto"), group = "2050") %>%
      addLegend(pal = pal_horizon, values = future_tazs()[[input$taz_col_name]], 
                opacity = 0.7, title = NULL, position = "topleft", group = "2050") %>% 
      addLayersControl(  overlayGroups = c("2017","2050", "Model"), 
                         options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("2017","2050")) %>%
      showGroup("Model")
  })
}

shinyApp(ui, server)
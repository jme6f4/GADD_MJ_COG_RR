# HEADER ##############################################################
#     Program:    21_Phenotyp_DATErr.R
#     Project:    GADMJCOG
#     Tasks:       
#                 A) Batch Model Characteristics
#                 B) createModels
#                 C) runModels
#                 D) readModels/summaries
#                 E) readModels/estimates
#                 F) Create Unstandardized Table
#                 G) Create Standardized Table
#                 H) Export Tables
# DATASETS # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#     Libraries:  GADMJCOG
#     Source:     NA
#     Produced:   INSERT
#                 INSERT
# HISTORY # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#     Jarrod Ellingson    18-08-28
# DEPENDENTS # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
     source("Work/01_Functions.R", echo = F)
# COMMENTS # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#     Model Overview: 
#       Outcome = Cognitive Measures
#       Predictor = Cannabis Use 
#       Covariats = age, gender
#     For Revision -- run models on subset of data
#       i)  only participants 25 & under at Wave 1
#       ii) only siblings within 10 years of age
#     Re-running analyses to confirm/match results published in MS
#     Corresponds to Table 2
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# A) Batch Model Characteristics ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
project <- "GADMJCOG"
Model <- "21_Phenotyp"
date <- "190618rr"



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # B) createModels ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
createModels(templatefile=paste0("Work/Documentation/githubRR/", Model, "_", date, ".inp"))
createModels(templatefile=paste0("Work/Documentation/githubRR/", Model, "_Gap_", date, ".inp"))



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # C) runModels ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
runModels(target = paste0(getwd(),"/Work/Documentation/githubRR/",Model), filefilter = date,
          recursive=TRUE,
          showOutput=TRUE,
          replaceOutfile="always"
          )



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # D) readModels/summaries ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
ReadModels21rr <- readModels(target = paste0(getwd(),"/Work/Documentation/githubRR/",Model),
                           filefilter = c("_1_19|_2_19"))
library(plyr)
Summary21rr <- as_tibble(do.call("rbind.fill", sapply(ReadModels21rr, "[", "summaries")))
detach("package:plyr", unload=TRUE)
FitProblems_21rr <- Summary21rr %>% dplyr::filter(is.na(AIC))
ifelse(dim(FitProblems_21rr)[1]!=0, "CHECK MODELS", "ok")



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# E) readModels/summaries ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
ci.unstandardized21rr <- sapply(sapply(ReadModels21rr, 
                                  "[", "parameters"), 
                           "[", "ci.unstandardized")
lapply(names(ci.unstandardized21rr), function(element) {
  ci.unstandardized21rr[[element]]$filename <<- element
  })
Estimates_21rr_UnstCI <- as_tibble(do.call("rbind", ci.unstandardized21rr)) %>%
  dplyr::select(-filename)

se.unstandardized21rr <- sapply(sapply(ReadModels21rr, 
                                  "[", "parameters"), 
                           "[", "unstandardized")
lapply(names(se.unstandardized21rr), function(element) {
  se.unstandardized21rr[[element]]$filename <<- element
  })
Estimates_21rr_UnstSE <- as_tibble(do.call("rbind", se.unstandardized21rr))



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# F) Create UnstandardizedTables ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
unstandardized21rr <- list(Estimates_21rr_UnstCI, Estimates_21rr_UnstSE) %>%
  purrr::reduce(full_join)

Estimates_21rr_Unst <- unstandardized21rr  %>%
  mutate(Column = ifelse(param == "CANEVR", "2a) WI Mj Ever Used",
                  ifelse(param == "CANONS", "2b) WI Mj Onset",
                  ifelse(param == "CANFLF", "2c) WI Mj Lifetime Freq.",
                  ifelse(param == "CANF6M", "2d) WI Mj 6-Month Freq.",
                  
                  ifelse(param == "CANEVRBW", "1a) BW Mj Ever Used",
                  ifelse(param == "CANONSBW", "1b) BW Mj Onset",
                  ifelse(param == "CANFLFBW", "1c) BW Mj Lifetime Freq.",
                  ifelse(param == "CANF6MBW", "1d) BW Mj 6-Month Freq.",
                  
                  ifelse(param == "ALCEVR", "2e) WI Alc Ever Used",                                                                                    
                  ifelse(param == "ALCONS", "2f) WI Alc Onset",
                  ifelse(param == "ALCFLF", "2g) WI Alc Lifetime Freq.",
                  ifelse(param == "ALCF6M", "2h) WI Alc 6-Month Freq.",
                  
                  ifelse(param == "ALCEVRBW", "1e) BW Alc Ever Used",
                  ifelse(param == "ALCONSBW", "1f) BW Alc Onset",
                  ifelse(param == "ALCFLFBW", "1g) BW Alc Lifetime Freq.",
                  ifelse(param == "ALCF6MBW", "1h) BW Alc 6-Month Freq.",
                         param)))))))))))))))), 
         Row = ifelse(grepl("SRPWCR", toupper(filename)) & grepl("_1_", filename), "1a) Stroop Word (Wave 1)",
               
               ifelse(grepl("WASBLK", toupper(filename)) & grepl("_1_", filename), "1b) Block Design (Wave 1)",
               ifelse(grepl("WASDGT", toupper(filename)) & grepl("_1_", filename), "1c) Digit Span (Wave 1)",
               ifelse(grepl("WASVCB", toupper(filename)) & grepl("_1_", filename), "1d) Vocabulary (Wave 1)",
               ifelse(grepl("WSISUM", toupper(filename)) & grepl("_1_", filename), "1e) IQ (Wave 1)",      
                      
               ifelse(grepl("TRLATS", toupper(filename)) & grepl("_1_", filename), "1f) Trails A (Wave 1)",
               ifelse(grepl("TRLBTS", toupper(filename)) & grepl("_1_", filename), "1g) Trails B (Wave 1)",
                      
               ifelse(grepl("CVLALF", toupper(filename)) & grepl("_1_", filename), "1h) CVLT Long Delay Free (Wave 1)",
               ifelse(grepl("CVLCOM", toupper(filename)) & grepl("_1_", filename), "1i) CVLT Composite (Wave 1)",
                      
               ifelse(grepl("SRPWCR", toupper(filename)) & grepl("_2_", filename), "2a) Stroop Word (Wave 2)",
                      
               ifelse(grepl("WASBLK", toupper(filename)) & grepl("_2_", filename), "2b) Block Design (Wave 2)",
               ifelse(grepl("WASDGT", toupper(filename)) & grepl("_2_", filename), "2c) Digit Span (Wave 2)",
               
               ifelse(grepl("TRLATS", toupper(filename)) & grepl("_2_", filename), "2f) Trails A (Wave 2)",
               ifelse(grepl("TRLBTS", toupper(filename)) & grepl("_2_", filename), "2g) Trails B (Wave 2)",
                      
               ifelse(grepl("CVLALF", toupper(filename)) & grepl("_2_", filename), "2h) CVLT Long Delay Free (Wave 2)",
               ifelse(grepl("CVLCOM", toupper(filename)) & grepl("_2_", filename), "2i) CVLT Composite (Wave 2)",
               NA))))))))))))))))) %>%
  filter(grepl("ON", paramHeader)) %>% filter(!grepl("WITH", paramHeader)) %>%
  mutate(Wave = ifelse(grepl("Wave 1", Row), 1, 
                             ifelse(grepl("Wave 2", Row), 2, NA))) %>%
  dplyr::filter(grepl("Stroop|Block|Digit|Vocab|IQ|Trails|CVLT Long Delay Free|CVLT Composite", Row)) %>%
  dplyr::filter(!grepl("DEM", Column)) %>%
  group_by(Wave, Column) %>%
  dplyr::mutate(pAdj  = as.numeric(p.adjust(p = pval, method = "hochberg"))) %>%
  ungroup() %>% 
  mutate(nDisplay = 2, 
         estRnd = roundn(est, nDisplay),
         lciRnd = roundn(low2.5, nDisplay),
         uciRnd = roundn(up2.5, nDisplay),
         estChar = nchar(estRnd),
         lciChar = nchar(lciRnd),
         uciChar = nchar(uciRnd),

         estTemp = ifelse(estRnd == 0.00, "0.00",
                   ifelse(estRnd < 0 & nchar(estRnd) == 2, paste0(substr(estRnd, 1, nchar(estRnd)), ".00"), 
                   ifelse(estRnd > 0 & nchar(estRnd) == 1, paste0(substr(estRnd, 1, nchar(estRnd)), ".00"),        
                   ifelse(estRnd < 0 & nchar(estRnd) == nDisplay+1, paste0(substr(estRnd, 1, nchar(estRnd)),"00"), 
                   ifelse(estRnd > 0 & nchar(estRnd) == nDisplay+1, paste0(substr(estRnd, 1, nchar(estRnd)),"0"), 
                   ifelse(estRnd < 0 & nchar(estRnd) == nDisplay+2, paste0(substr(estRnd, 1, nchar(estRnd)),"0"), 
                   ifelse(estRnd < -10 & nchar(estRnd) == nDisplay+4, paste0(substr(estRnd, 1, nDisplay+4)),
                   ifelse(estRnd < -10 & nchar(estRnd) == nDisplay+3, paste0(substr(estRnd, 1, nDisplay+3), "0"),
                   ifelse(estRnd > 10 & nchar(estRnd) == nDisplay+3, paste0(substr(estRnd, 1, nDisplay+3)),
                   ifelse(estRnd > 10 & nchar(estRnd) == nDisplay+2, paste0(substr(estRnd, 1, nDisplay+2), "0"),
                          paste0(estRnd))))))))))),
         
         lciTemp = ifelse(lciRnd == 0.00, "0.00",
                   ifelse(lciRnd < 0 & nchar(lciRnd) == 2, paste0(substr(lciRnd, 1, nchar(lciRnd)), ".00"), 
                   ifelse(lciRnd > 0 & nchar(lciRnd) == 1, paste0(substr(lciRnd, 1, nchar(lciRnd)), ".00"),        
                   ifelse(lciRnd < 0 & nchar(lciRnd) == nDisplay+1, paste0(substr(lciRnd, 1, nchar(lciRnd)),"00"), 
                   ifelse(lciRnd > 0 & nchar(lciRnd) == nDisplay+1, paste0(substr(lciRnd, 1, nchar(lciRnd)),"0"), 
                   ifelse(lciRnd < 0 & nchar(lciRnd) == nDisplay+2, paste0(substr(lciRnd, 1, nchar(lciRnd)),"0"), 
                   ifelse(lciRnd < -10 & nchar(lciRnd) == nDisplay+4, paste0(substr(lciRnd, 1, nDisplay+4)),
                   ifelse(lciRnd < -10 & nchar(lciRnd) == nDisplay+3, paste0(substr(lciRnd, 1, nDisplay+3), "0"),
                   ifelse(lciRnd > 10 & nchar(lciRnd) == nDisplay+3, paste0(substr(lciRnd, 1, nDisplay+3)),
                   ifelse(lciRnd > 10 & nchar(lciRnd) == nDisplay+2, paste0(substr(lciRnd, 1, nDisplay+2), "0"),
                          paste0(lciRnd))))))))))),

         uciTemp = ifelse(uciRnd == 0.00, "0.00",
                   ifelse(uciRnd < 0 & nchar(uciRnd) == 2, paste0(substr(uciRnd, 1, nchar(uciRnd)), ".00"), 
                   ifelse(uciRnd > 0 & nchar(uciRnd) == 1, paste0(substr(uciRnd, 1, nchar(uciRnd)), ".00"),        
                   ifelse(uciRnd < 0 & nchar(uciRnd) == nDisplay+1, paste0(substr(uciRnd, 1, nchar(uciRnd)),"00"), 
                   ifelse(uciRnd > 0 & nchar(uciRnd) == nDisplay+1, paste0(substr(uciRnd, 1, nchar(uciRnd)),"0"), 
                   ifelse(uciRnd < 0 & nchar(uciRnd) == nDisplay+2, paste0(substr(uciRnd, 1, nchar(uciRnd)),"0"), 
                   ifelse(uciRnd < -10 & nchar(uciRnd) == nDisplay+4, paste0(substr(uciRnd, 1, nDisplay+4)),
                   ifelse(uciRnd < -10 & nchar(uciRnd) == nDisplay+3, paste0(substr(uciRnd, 1, nDisplay+3), "0"),
                   ifelse(uciRnd > 10 & nchar(uciRnd) == nDisplay+3, paste0(substr(uciRnd, 1, nDisplay+3)),
                   ifelse(uciRnd > 10 & nchar(uciRnd) == nDisplay+2, paste0(substr(uciRnd, 1, nDisplay+2), "0"),
                          paste0(uciRnd))))))))))),

         
         DisplayFull = ifelse(pAdj < .001, paste0("'",estTemp," (",lciTemp,",",uciTemp,")***"),
                               ifelse(pAdj < .01, paste0("'",estTemp," (",lciTemp,",",uciTemp,")**"),
                                      ifelse(pAdj < .05, paste0("'",estTemp," (",lciTemp,",",uciTemp,")*"),
                                             paste0("'",estTemp," (",lciTemp,",",uciTemp,")")))),
         EstP = ifelse(pAdj < .001, paste0("'",estTemp,"***"),
                               ifelse(pAdj < .01, paste0("'",estTemp,"**"),
                                      ifelse(pAdj < .05, paste0("'",estTemp,"*"),
                                             paste0("'",estTemp," ")))),
         DisplayUnadj = ifelse(pval < .001, paste0("'",estTemp," (",lciTemp,",",uciTemp,")***"),
                               ifelse(pval < .01, paste0("'",estTemp," (",lciTemp,",",uciTemp,")**"),
                                      ifelse(pval < .025, paste0("'",estTemp," (",lciTemp,",",uciTemp,")*"),
                                             ifelse(pval < .05, paste0("'",estTemp," (",lciTemp,",",uciTemp,")."),
                                                    paste0("'",estTemp," (",lciTemp,",",uciTemp,")"))))),
         EstPunadj = ifelse(pAdj < .001, paste0("'",estTemp,"***"),
                               ifelse(pAdj < .01, paste0("'",estTemp,"**"),
                                      ifelse(pAdj < .025, paste0("'",estTemp,"*"),
                                             ifelse(pAdj < .05, paste0("'",estTemp,"."),
                                                    paste0("'",estTemp," ")))))) %>%
  dplyr::select(Column, Row, 
                DisplayFull, DisplayUnadj, 
                estChar, est,  estRnd, estTemp, 
                lciChar, low2.5, lciRnd, lciTemp, 
                uciChar, up2.5, uciRnd, uciTemp, 
                pval, pAdj, EstPunadj) %>%
  filter(!is.na(Row))

Estimates_21rr_Tab_Unst  <- Estimates_21rr_Unst %>%
  dplyr::select(Column, Row, 
                DisplayFull, DisplayUnadj, pAdj) %>%
  mutate(EstP = as.numeric(pAdj)) %>%
  group_by(Row, Column) %>%
  dplyr::mutate(EstP_M = mean(EstP, na.rm=T)) %>%
  ungroup() %>%
  dplyr::mutate(EstP_D = abs(EstP-EstP_M)) %>%
  group_by(Row, Column) %>%
  dplyr::mutate(EstP_D_Min = min(EstP_D, na.rm=T),
                EstP_D_Rank = rank(EstP_D_Min, ties.method = "random")) %>%
  ungroup() %>%
  filter(EstP_D_Rank == 1) 

TableEstimates_21rr_Unst <- Estimates_21rr_Tab_Unst %>%
  dplyr::select(Column, Row, DisplayFull)  %>%
  filter(grepl(")",Column)) %>%
  spread(key = Column, value = DisplayFull) 

TableEstimates_21rr_Unadj <- Estimates_21rr_Tab_Unst %>%
  dplyr::select(Column, Row, DisplayUnadj)  %>%
  filter(grepl(")",Column)) %>%
  spread(key = Column, value = DisplayUnadj) 

View(Estimates_21rr_Unst)
View(TableEstimates_21rr_Unadj)
View(TableEstimates_21rr_Unst)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# H) Export Tables ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
project <- "GADMJCOG"
Model <- "21_Phenotyp"
date <- "190618rr"
write_csv(x = TableEstimates_21rr_Unst,
          na = "",
          path = paste0(getwd(), "/Work/Results/", Model, "_", date,".csv"))

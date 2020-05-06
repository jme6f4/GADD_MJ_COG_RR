# HEADER ###############################################################
#      Program:   03_Clean.R
#      Project:   GADMJCOG
#      Tasks:      
                # A) Demographics
                # B) CogNeuro
                # C) SAM Supp (Drug Use)
                # D) Merge
                # E) Export
# DATASETS  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
     # Libraries: GADMJCOG
     # Source:    
                # A) Demographics:  
                #                   GADD_CogNeuro
                #                   GADD_Supp_W1
                #                   GADD_Supp_W2
                # B) CogNeuro:      
                #                   GADD_CogNeuro
                # C) SAM Supp:      
                #                   GADD_Supp_W1
                #                   GADD_Supp_W2
                # D) GWAS:          
                #                   GADD_GWAS
      # Derived:   
                # E) Merge:         
# HISTORY  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
     # Jarrod Ellingson    DATE
# DEPENDENTS # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
        source("Work/01_Functions.R", echo = T)
        source("Work/02_Load.R", echo = T)
# COMMENTS # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#      Correction from correspondence with R. Corley
#         FIX: 45-D12020321 
#         w1wstott (WISC/WASI sum): 47  <- should be 97 
#         w1estiq: 97
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   A) Demographics #####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#   1) Wave 1 #####
GADD_Demo_Wave1a <- GADD_CogNeuro %>%
  dplyr::select(NIDAID,
         project, afamily, id, nage1, old_age1, nsex1, testyr1, testtype1, wave1,
         exclude1, clinical1, havedna1, comments1, hispanic1, racecat1) %>% 
  dplyr::rename(project1 = project, 
                afamily1 = afamily, 
                id1 = id, 
                sex1 = nsex1) 
names(GADD_Demo_Wave1a) <- c(names(GADD_Demo_Wave1a)[1],
                            substring(names(GADD_Demo_Wave1a[2:dim(GADD_Demo_Wave1a)[2]]), 
                                      1, 
                                      nchar(names(GADD_Demo_Wave1a[2:dim(GADD_Demo_Wave1a)[2]]))-1))

GADD_Demo_Wave1 <- GADD_Demo_Wave1a %>%
  dplyr::mutate(MyNIDAID = trim(NIDAID), 
         testyr = as.numeric(testyr), 
         Wave = 1,
         male = ifelse(sex == 1, 1, 
                       ifelse(sex == 0, 0, NA))) %>%
  dplyr::select(-sex, -NIDAID, -wave)

# Drop Duplicates
GADD_Supp_ForDemoMerge_W1 <- GADD_Supp_W1 %>%
  filter(!grepl("catch", tolower(TESTTYPE))) %>%
  dplyr::mutate(MyNIDAID = trim(NIDAID),
         testyr = TESTYR) %>%
  dplyr::select(MyNIDAID, testyr)
GADD_Supp_ForDemoMerge_W1_Duplicates <- GADD_Supp_ForDemoMerge_W1 %>% 
  group_by(MyNIDAID) %>%
  filter(n()>2)
GADD_Demo_Wave1_ForMerge <- list(GADD_Supp_ForDemoMerge_W1, GADD_Demo_Wave1) %>%
  reduce(left_join) %>%
  filter(!is.na(Wave))

#   2) Wave 2 #####
GADD_Demo_Wave2a <- GADD_CogNeuro %>%
  dplyr::select(NIDAID,
         project2, afamily2, id2, nage2, old_age2, nsex2, testyr2, testtype2, wave2,
         exclude2, clinical2, havedna2, comments2, hispanic2, racecat2)
names(GADD_Demo_Wave2a) <- c(names(GADD_Demo_Wave2a)[1],
                            substring(names(GADD_Demo_Wave2a[2:dim(GADD_Demo_Wave2a)[2]]), 
                                      1, 
                                      nchar(names(GADD_Demo_Wave2a[2:dim(GADD_Demo_Wave2a)[2]]))-1))

GADD_Demo_Wave2 <- GADD_Demo_Wave2a %>%
  dplyr::mutate(MyNIDAID = trim(NIDAID), 
         testyr = as.numeric(testyr), 
         Wave = 2,
         male = ifelse(nsex == 1, 1, 
                       ifelse(nsex == 0, 0, NA))) %>%
  dplyr::select(-nsex, -NIDAID, -wave)

# Drop Duplicates
GADD_Supp_ForDemoMerge_W2 <- GADD_Supp_W2 %>%
  filter(!grepl("catch", tolower(TESTTYPE))) %>%
  dplyr::mutate(MyNIDAID = trim(NIDAID),
         testyr = TESTYR) %>%
  dplyr::select(MyNIDAID, testyr)
GADD_Supp_ForDemoMerge_W2_Duplicates <- GADD_Supp_ForDemoMerge_W2 %>% 
  group_by(MyNIDAID) %>%
  filter(n()>2)
GADD_Demo_Wave2_ForMerge <- list(GADD_Supp_ForDemoMerge_W2, GADD_Demo_Wave2) %>%
  reduce(left_join) %>%
  filter(!is.na(Wave))

#   3) Merge Waves #####         
GADD_Demo_BothWaves <- list(GADD_Demo_Wave1_ForMerge,
                            GADD_Demo_Wave2_ForMerge) %>%
  reduce(full_join) %>%
  dplyr::select(MyNIDAID, Wave, testyr, male, 
         project:racecat)

# Includ Pre-Morbid IQ
GADD_Demo_PreIQ <- GADD_CogNeuro %>%
  dplyr::select(NIDAID, w1rpiatr) %>% 
  dplyr::mutate(MyNIDAID = trim(NIDAID),
                PRE_FX = as.numeric(miss999(w1rpiatr))) %>%
  dplyr::select(MyNIDAID:PRE_FX) %>%
  filter(!is.na(PRE_FX))
GADD_Demo_PreIQ_ForMerge <- list(GADD_Supp_ForDemoMerge_W1, GADD_Demo_PreIQ) %>%
  reduce(inner_join) %>%
  dplyr::select(-testyr)

GADD_Demo_ForMerge <- list(GADD_Demo_BothWaves,
                           GADD_Demo_PreIQ_ForMerge) %>%
  reduce(left_join)

# Check Duplicates/Triplicates: n = 0
# GADD_Demo_Duplicates <- GADD_Demo_ForMerge %>%
#   group_by(MyNIDAID) %>%
#   filter(n()>2)
# dim(GADD_Demo_Duplicates)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   B) CogNeuro #####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#   1) Wave 1 #####
GADD_Neuro_Wave1 <- GADD_CogNeuro %>%
  dplyr::mutate(MyNIDAID = trim(NIDAID), 
         testyr = as.numeric(testyr1), 
         Wave = 1, 
         Neuro = "Neuro1",
         # Stroop
         SRPBLK = as.numeric(miss999(w1strpwdts)), # Stroop Black
         SRPCLR = as.numeric(miss999(w1strpcots)), # Stroop Color
         SRPWCR = as.numeric(miss999(w1strpcwts))-SRPCLR, # Stroop Word/Color
         # WAIS/WASI/etc
         WASVCB = as.numeric(ifelse(!is.na(w1wsvoss), miss999(w1wsvoss), #WASI
                              ifelse(!is.na(w1wavoss), 
                                     rescale(x = miss999(w1wavoss),
                                             mean = mean(miss999(w1wsvoss), na.rm=T), 
                                             sd = sd(miss999(w1wsvoss), na.rm=T), 
                                             df = F), #wAIS
                              ifelse(!is.na(w1wivoss), 
                                     rescale(x = miss999(w1wivoss),
                                             mean = mean(miss999(w1wsvoss), na.rm=T), 
                                             sd = sd(miss999(w1wsvoss), na.rm=T), 
                                             df = F), #WISC
                                     NA)))), #VOCAB
         WASBLK = as.numeric(ifelse(!is.na(w1wsblss), miss999(w1wsblss), #WASI
                              ifelse(!is.na(w1wablss), 
                                     rescale(x = miss999(w1wablss),
                                             mean = mean(miss999(w1wsblss), na.rm=T), 
                                             sd = sd(miss999(w1wsblss), na.rm=T), 
                                             df = F), #wAIS
                              ifelse(!is.na(w1wiblss), 
                                     rescale(x = miss999(w1wiblss),
                                             mean = mean(miss999(w1wsblss), na.rm=T), 
                                             sd = sd(miss999(w1wsblss), na.rm=T), 
                                             df = F), #WISC
                                     NA)))), #Block
         WASDGT = as.numeric(miss999(w1wadiss)), #### DIGIT ###
         WSISUM = as.numeric(miss999(w1wstott)), #### SUM ###
         #### TRAILS ###
         TRLATS = as.numeric(miss999(w1trlats)), #### A ###
         TRLBTS = as.numeric(miss999(w1trlbts)), #### B ###
         #### CVLT ###
         CVLASF = as.numeric(miss999(w1cvlt10)),
         CVLASC = as.numeric(miss999(w1cvlt12)),
         CVLALF = as.numeric(miss999(w1cvlt14)),
         CVLALC = as.numeric(miss999(w1cvlt16))) %>%
  dplyr::select(MyNIDAID, Wave, testyr, Neuro, SRPBLK:CVLALC) %>%
  dplyr::mutate(WSISUM = ifelse(MyNIDAID == "45-D12020321", 97, WSISUM))
# FIX: 45-D12020321 # w1wstott (WISC/WASI sum): 47 # w1estiq: 97

# Drop Duplicates
GADD_Neuro_Wave1_ForMerge <- list(GADD_Supp_ForDemoMerge_W1,
                                  GADD_Neuro_Wave1) %>%
  reduce(left_join) %>%
  filter(!is.na(Wave)) %>%
  dplyr::mutate(SRPWCRz = as.numeric(rescale(SRPWCR, mean=0, sd=1, df = F)), 
                
                WASVCBz = as.numeric(rescale(WASVCB, mean=0, sd=1, df = F)), 
                WSISUMz = as.numeric(rescale(WSISUM, mean=0, sd=1, df = F)),  
                
                WASBLKz = as.numeric(rescale(WASBLK, mean=0, sd=1, df = F)), 
                WASDGTz = as.numeric(rescale(WASDGT, mean=0, sd=1, df = F)), 
                
                TRLATSz = as.numeric(rescale(TRLATS, mean=0, sd=1, df = F)),  
                TRLBTSz = as.numeric(rescale(TRLBTS, mean=0, sd=1, df = F)),
                
                CVLASFz = as.numeric(rescale(CVLASF, mean=0, sd=1, df = F)),
                CVLASCz = as.numeric(rescale(CVLASC, mean=0, sd=1, df = F)),
                CVLALFz = as.numeric(rescale(CVLALF, mean=0, sd=1, df = F)),
                CVLALCz = as.numeric(rescale(CVLALC, mean=0, sd=1, df = F))) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(MISS = sum(is.na(SRPWCRz), is.na(WASBLKz), is.na(WASDGTz), 
                           is.na(TRLATSz), is.na(TRLBTSz)),
                CVLCOM = mean(x = c(CVLASFz, CVLASCz, CVLALFz, CVLALCz), na.rm = F)) %>%
  dplyr::ungroup() %>%
  dplyr::select(MyNIDAID, Wave, testyr, Neuro, 
         SRPBLK:CVLALC, 
         CVLCOM)

#   2) Wave 2 #####
GADD_Neuro_Wave2 <- GADD_CogNeuro %>%
  dplyr::mutate(MyNIDAID = trim(NIDAID), 
         testyr = as.numeric(testyr2), 
         Wave = 2, 
         Neuro = "Neuro2",
          # STROOP
         SRPBLK = as.numeric(miss999(w2strpwdts)),
         SRPCLR = as.numeric(miss999(w2strpcots)),
         SRPWCR = as.numeric(miss999(w2strpcwts))-SRPCLR,
          # WAIS
         WASBLK = as.numeric(miss999(w2wsblss)),
         WASDGT = as.numeric(miss999(w2wadiss)),

         TRLATS = as.numeric(miss999(w2trlats)),
         TRLBTS = as.numeric(miss999(w2trlbts)),
         
         CVLASF = as.numeric(miss999(w2cvlt27)),
         CVLASC = as.numeric(miss999(w2cvlt28)),
         CVLALF = as.numeric(miss999(w2cvlt29)),
         CVLALC = as.numeric(miss999(w2cvlt30))) %>%
  dplyr::select(MyNIDAID, Wave, testyr, Neuro, SRPBLK:CVLALC)

# Drop Duplicates
GADD_Neuro_Wave2_ForMerge <- list(GADD_Supp_ForDemoMerge_W2, GADD_Neuro_Wave2) %>%
  reduce(left_join) %>%
  filter(!is.na(Wave)) %>%
  dplyr::mutate(SRPWCRz = as.numeric(rescale(SRPWCR, mean=0, sd=1, df = F)), 
                
                WASBLKz = as.numeric(rescale(WASBLK, mean=0, sd=1, df = F)), 
                WASDGTz = as.numeric(rescale(WASDGT, mean=0, sd=1, df = F)), 
                
                TRLATSz = as.numeric(rescale(TRLATS, mean=0, sd=1, df = F)),  
                TRLBTSz = as.numeric(rescale(TRLBTS, mean=0, sd=1, df = F)),
                
                CVLASFz = as.numeric(rescale(CVLASF, mean=0, sd=1, df = F)),
                CVLASCz = as.numeric(rescale(CVLASC, mean=0, sd=1, df = F)),
                CVLALFz = as.numeric(rescale(CVLALF, mean=0, sd=1, df = F)),
                CVLALCz = as.numeric(rescale(CVLALC, mean=0, sd=1, df = F))) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(MISS = sum(is.na(SRPWCRz), is.na(WASBLKz), is.na(WASDGTz), 
                           is.na(TRLATSz), is.na(TRLBTSz)),
                CVLCOM = mean(x = c(CVLASFz, CVLASCz, CVLALFz, CVLALCz), na.rm = F)) %>%
  dplyr::ungroup() %>%
  dplyr::select(MyNIDAID, Wave, testyr, Neuro, 
         SRPBLK:CVLALC, 
         CVLCOM)

#   3) Merge Waves #####
GADD_Neuro_ForMerge <- list(GADD_Neuro_Wave1_ForMerge, 
                            GADD_Neuro_Wave2_ForMerge) %>%
  reduce(full_join) %>%
  dplyr::select(-testyr)

# Check Duplicates/Triplicates: n = 0
# GADD_Neuro_Duplicates <- GADD_Neuro_ForMerge %>% 
#   group_by(MyNIDAID) %>% 
#   filter(n()>2)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   C) SAM Supplement #####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#   1) Wave 1 #####
GADD_SuppW1_ForMerge <- GADD_Supp_W1 %>%
  filter(!grepl("catch", tolower(TESTTYPE))) %>%
  dplyr::mutate(MyNIDAID = trim(NIDAID), 
         Wave = 1, 
         SAM = "SAM1",
         # Cannabis
         SAMX3A = as.character(SAMX3A),
         SAMX3C = as.numeric(SAMX3C),
         SAMX3D = as.character(SAMX3D), 
         SAMX3E = as.character(SAMX3E), 
         SAMX3F = as.numeric(SAMX3F),
         # Alcohol
         SAMX2A = as.character(SAMX2A), 
         SAMX2C = as.numeric(SAMX2C), 
         SAMX2D = as.character(SAMX2D), 
         SAMX2E = as.character(SAMX2E), 
         SAMX2F = as.numeric(SAMX2F)) %>%
  dplyr::select(MyNIDAID, Wave, SAM, 
         # Cannabis
         SAMX3A, SAMX3C, 
         SAMX3D, SAMX3E, SAMX3F,
         # Alcohol
         SAMX2A, SAMX2C, 
         SAMX2D, SAMX2E, SAMX2F) 

#   2) Wave 2 #####
GADD_SuppW2_ForMerge <- GADD_Supp_W2 %>%
  filter(!grepl("catch", tolower(TESTTYPE))) %>%
  dplyr::mutate(MyNIDAID = trim(NIDAID), 
         Wave = 2, 
         SAM = "SAM2",
         # Cannabis
         SAMX3A = as.character(SAMX3A), 
         SAMX3C = as.numeric(SAMX3C),
         SAMX3D = as.character(SAMX3D), 
         SAMX3E = as.character(SAMX3E), 
         SAMX3F = as.numeric(SAMX3F),
         # Alcohol
         SAMX2A = as.character(SAMX2A), 
         SAMX2C = as.numeric(SAMX2C),
         SAMX2D = as.character(SAMX2D), 
         SAMX2E = as.character(SAMX2E), 
         SAMX2F = as.numeric(SAMX2F)) %>%
  dplyr::select(MyNIDAID, Wave, SAM, 
         # Cannabis
         SAMX3A, SAMX3C, 
         SAMX3D, SAMX3E, SAMX3F,
         # Alcohol
         SAMX2A, SAMX2C, 
         SAMX2D, SAMX2E, SAMX2F
         ) 

#   3) Merge Waves #####
GADD_SAMSUP_MergedWaves <- list(GADD_SuppW1_ForMerge, 
                                GADD_SuppW2_ForMerge) %>%
  reduce(full_join)  %>%
  dplyr::mutate(CANEVR = ifelse(grepl("Yes", SAMX3A), 1,
                         ifelse(grepl("No", SAMX3A), 0, 
                         NA)),
                CANONS = ifelse(SAMX3C >= 99, NA, 
                                ifelse(SAMX3C < 9, 9, SAMX3C)), 
                CANFLF = ifelse(CANEVR == 0 | SAMX3D == "0" | grepl("never",tolower(SAMX3D)), 0,
                         ifelse(grepl("1-2", SAMX3D), 1.5,
                         ifelse(grepl("3-5", SAMX3D), 4,
                         ifelse(grepl("6-9", SAMX3D), 7.5,
                         ifelse(grepl("10-19", SAMX3D), 15,
                         ifelse(grepl("20-39", SAMX3D), 30,
                         ifelse(grepl("40 or more", SAMX3D), 60,
                         NA))))))),
                CANFTY = ifelse(CANEVR == 0 | SAMX3E == "0" | grepl("never",tolower(SAMX3E)), 0,
                         ifelse(grepl("less than once a month", tolower(SAMX3E)), 0.5,
                         ifelse(grepl("once a month", tolower(SAMX3E)), 1.0,
                         ifelse(grepl("2 or more times a month", tolower(SAMX3E)), 3.0,
                         ifelse(grepl("once a week", tolower(SAMX3E)), round(52/12,2),
                         ifelse(grepl("2 or more times a week", tolower(SAMX3E)), round(52/12*2,2),
                         ifelse(grepl("once a day", tolower(SAMX3E)), round(52/12*7,2),
                                NA))))))), 
                CANF6M = round(ifelse(!is.na(SAMX3F), SAMX3F, 
                               ifelse((grepl("no", tolower(SAMX3A)) | 
                                       grepl("never",tolower(SAMX3D)) |
                                       SAMX3D == "0"), 0,
                                      SAMX3F))/6,2),
                ALCEVR = ifelse(grepl("Yes", SAMX2A), 1,
                         ifelse(grepl("No", SAMX2A), 0, 
                         NA)),
                ALCONS = ifelse(SAMX2C >= 99, NA, 
                                ifelse(SAMX2C < 9, 9, SAMX2C)), 
                ALCFLF = ifelse(ALCEVR == 0 | SAMX2D == "0" | grepl("never",tolower(SAMX2D)), 0,
                         ifelse(grepl("1-2", SAMX2D), 1.5,
                         ifelse(grepl("3-5", SAMX2D), 4,
                         ifelse(grepl("6-9", SAMX2D), 7.5,
                         ifelse(grepl("10-19", SAMX2D), 15,
                         ifelse(grepl("20-39", SAMX2D), 30,
                         ifelse(grepl("40 or more", SAMX2D), 60,
                         NA))))))),
                ALCFTY = ifelse(SAMX2E == "0" | grepl("never",tolower(SAMX2E)), NA,
                         ifelse(SAMX2E == "Less than once a month", 0.5,
                         ifelse(SAMX2E == " Once a month", 1.0,
                         ifelse(SAMX2E == "2 or more times a month", 3.0,
                         ifelse(SAMX2E == "Once a week", round(52/12,2),
                         ifelse(SAMX2E == "2 or more times a week", round(52/12*2,2),
                         ifelse(grepl("once a day", tolower(SAMX2E)), round(52/12*7,2),
                         NA))))))), 
                ALCF6M = round(ifelse(!is.na(SAMX2F), SAMX2F, 
                               ifelse((grepl("no", tolower(SAMX2A)) | 
                                       grepl("never",tolower(SAMX2D)) |
                                       SAMX2D == "0"), 0,
                                      SAMX2F))/6,2)) %>%
  dplyr::select(MyNIDAID, Wave, SAM,
                CANEVR, CANONS, CANFLF, CANFTY, CANF6M, SAMX3E,
                ALCEVR, ALCONS, ALCFLF, ALCFTY, ALCF6M, SAMX2E)

GADD_SAMSUP_MergedWaves_20021 <- GADD_SAMSUP_MergedWaves %>% 
  dplyr::filter(grepl("44-S00020021", MyNIDAID)) 

GADD_SAMSUP_AccumulatedUse <- GADD_SAMSUP_MergedWaves %>%
  dplyr::group_by(MyNIDAID) %>%
  dplyr::mutate(CANAC6 = mean(CANF6M, na.rm=F), 
                ALCAC6 = mean(ALCF6M, na.rm=F)) %>%
  dplyr::ungroup() %>%
  dplyr::select(MyNIDAID, Wave, 
                CANAC6:ALCAC6)

GADD_SAMSUP_ForMerge <- list(GADD_SAMSUP_MergedWaves, 
                             GADD_SAMSUP_AccumulatedUse) %>%
  purrr::reduce(full_join) %>%
  dplyr::select(MyNIDAID, Wave, SAM, 
                CANEVR, CANONS, 
                CANFLF, CANFTY, CANF6M, CANAC6, 
                ALCEVR, ALCONS, 
                ALCFLF, ALCFTY, ALCF6M, ALCAC6)


# Check Duplicates/Triplicates
# GADD_SAM_Duplicates <- GADD_SAMSUP_ForMerge %>% 
#   group_by(MyNIDAID) %>% 
#   filter(n()>2)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   D) Merge Scales #####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

GADD_Merged <- list(GADD_Demo_ForMerge, 
                    GADD_Neuro_ForMerge,
                    GADD_SAMSUP_ForMerge) %>%
  reduce(full_join)

GADD_Clean <- GADD_Merged %>%
  filter(substr(MyNIDAID,1,2) != "43") %>%
  dplyr::mutate(FILTER44 = ifelse(substr(MyNIDAID,1,2) == "44", 1, 0), 
                FILTER45 = ifelse(substr(MyNIDAID,1,2) == "45", 1, 0), 
                FILTER = ifelse(FILTER44 == 1 | FILTER45 == 1, 1, 0)) %>%
  dplyr::mutate(DEMYER = testyr, 
                DEMMAL = male, 
                DEMPRJ = project, 
                DEMFAM = afamily, 
                DEMAGE = round(nage, 2),
                DEMHSP = hispanic, 
                DEMRAC = racecat,
                NeuroW = ifelse(Neuro == "Neuro1", 1, 
                         ifelse(Neuro == "Neuro2", 2, NA)),
                SAMW = ifelse(SAM == "SAM1", 1, 
                       ifelse(SAM == "SAM2", 2, NA)),
                ID_Cty = ifelse(FILTER == 1, 
                                substr(MyNIDAID,4,4), 
                                substr(MyNIDAID,nchar(MyNIDAID),nchar(MyNIDAID))),
                ID_Fam = ifelse(FILTER == 1, 
                                as.numeric(substr(MyNIDAID,5,8)),
                                as.numeric(substr(MyNIDAID,2,4))),
                ID_Rel = ifelse(FILTER == 1, 
                                substr(MyNIDAID,9,10),
                                substr(MyNIDAID,5,6)),
                ID_MPL = ifelse(FILTER == 1, 
                                as.numeric(substr(MyNIDAID,5,nchar(MyNIDAID))),
                                as.numeric(substr(MyNIDAID,2,nchar(MyNIDAID)-1))),
                CANYRS = ifelse(CANEVR == 0, 0, DEMAGE - CANONS), 
                ALCYRS = ifelse(ALCEVR == 0, 0, DEMAGE - ALCONS)) %>%
  dplyr::select(ID_MPL, ID_Fam, ID_Rel, ID_Cty, 
                Wave, NeuroW, SAMW, 
                DEMYER:DEMRAC,
                SRPWCR, 
                WASVCB, WASBLK, WASDGT, WSISUM, 
                TRLATS, TRLBTS, 
                CVLASF, CVLASC, CVLALF, CVLALC, CVLCOM,
                CANEVR, CANONS, CANFLF, CANFTY, CANF6M, CANAC6, CANYRS, 
                ALCEVR, ALCONS, ALCFLF, ALCFTY, ALCF6M, ALCAC6, ALCYRS,
                MyNIDAID, FILTER)

GADD_Sibs <- GADD_Clean %>%
  dplyr::filter(FILTER == 1) %>%
  dplyr::filter(ID_Rel == "00" |
                  ID_Rel == "03" |
                  ID_Rel == "04") %>%
  dplyr::select(ID_MPL, ID_Fam, ID_Rel, ID_Cty, Wave, NeuroW, SAMW, 
                DEMYER, DEMMAL, DEMPRJ, DEMFAM, DEMAGE, DEMHSP, DEMRAC, 
                SRPWCR, WASVCB, WASBLK, WASDGT, WSISUM, TRLATS, TRLBTS, 
                CVLASF, CVLASC, CVLALF, CVLALC, CVLCOM, 
                CANEVR, CANONS, CANFLF, CANFTY, CANF6M, CANAC6, CANYRS, 
                ALCEVR, ALCONS, ALCFLF, ALCFTY, ALCF6M, ALCAC6, ALCYRS, 
                MyNIDAID, FILTER)

GADD_CrossSectional_1 <- GADD_Sibs %>%
  dplyr::filter(Wave == 1) %>%
  dplyr::select(ID_MPL, DEMAGE, 
                CANEVR:ALCYRS) 
names(GADD_CrossSectional_1) <- c(names(GADD_CrossSectional_1)[1],
                                  paste0(names(GADD_CrossSectional_1)[2:length(names(GADD_CrossSectional_1))],"01"))
GADD_CrossSectional_2 <- GADD_Sibs %>%
  dplyr::filter(Wave == 2) %>%
  dplyr::select(ID_MPL, DEMAGE, 
                CANEVR:ALCYRS) 
names(GADD_CrossSectional_2) <- c(names(GADD_CrossSectional_2)[1],
                                  paste0(names(GADD_CrossSectional_2)[2:length(names(GADD_CrossSectional_2))],"02"))
  
GADD_CrossSectional <- list(GADD_CrossSectional_1, 
                            GADD_CrossSectional_2) %>%
  purrr::reduce(full_join) %>%
  dplyr::mutate(CANONS01 = ifelse(is.na(DEMAGE01) | is.na(CANEVR01), CANONS01, 
                                  ifelse(CANEVR01 == 1 & DEMAGE01 < CANONS01, DEMAGE01, CANONS01)),
                CANONS02 = ifelse(is.na(DEMAGE01) | is.na(CANEVR01) | is.na(CANONS01), CANONS02, 
                                  ifelse(DEMAGE01 < CANONS02 & CANEVR01 == 1, CANONS01, 
                                         ifelse(is.na(CANONS02), CANONS01, 
                                                CANONS02))),
                CANEVR02 = ifelse(!is.na(CANEVR01) & CANEVR01>CANEVR02, CANEVR01, 
                                  ifelse(is.na(CANEVR02) & CANEVR01 == 1, CANEVR01, 
                                         CANEVR02)),
                CANFLF02 = ifelse(!is.na(CANFLF01) & CANFLF01>CANFLF02, CANFLF01, 
                                   ifelse(is.na(CANFLF02), CANFLF01, 
                                          CANFLF02)),
                
                ALCONS01 = ifelse(is.na(DEMAGE01) | is.na(ALCEVR01), ALCONS01, 
                                  ifelse(ALCEVR01 == 1 & DEMAGE01 < ALCONS01, DEMAGE01, ALCONS01)),
                ALCONS02 = ifelse(is.na(DEMAGE01) | is.na(ALCEVR01) | is.na(ALCONS01), ALCONS02, 
                                  ifelse(DEMAGE01 < ALCONS02 & ALCEVR01 == 1, ALCONS01, 
                                         ifelse(is.na(ALCONS02), ALCONS01, 
                                                ALCONS02))),
                ALCEVR02 = ifelse(!is.na(ALCEVR01) & ALCEVR01>ALCEVR02, ALCEVR01, 
                                  ifelse(is.na(ALCEVR02) & ALCEVR01 == 1, ALCEVR01, 
                                         ALCEVR02)),
                ALCFLF02 = ifelse(!is.na(ALCFLF01) & ALCFLF01>ALCFLF02, ALCFLF01,
                                  ifelse(is.na(ALCFLF02), ALCFLF01, ALCFLF02))) %>%
  dplyr::select(ID_MPL,CANFLF01,
                CANONS01, CANONS02, CANEVR02, CANFLF02,
                ALCONS01, ALCONS02, ALCEVR02, ALCFLF02)

GADD_SibsCross <- list(GADD_Sibs, 
                       GADD_CrossSectional) %>%
  purrr::reduce(full_join) 

GADD_Long_Full <- GADD_SibsCross %>%
  dplyr::mutate(CANONS = ifelse(Wave == 1, CANONS01, CANONS),
                CANONS = ifelse(Wave == 2, CANONS02, CANONS),
                CANEVR = ifelse(Wave == 2, CANEVR02, CANEVR),
                CANFLF = ifelse(Wave == 2, CANFLF02, CANFLF),
                ALCONS = ifelse(Wave == 1, ALCONS01, ALCONS),
                ALCONS = ifelse(Wave == 2, ALCONS02, ALCONS),
                ALCEVR = ifelse(Wave == 2, ALCEVR02, ALCEVR),
                ALCFLF = ifelse(Wave == 2, ALCFLF02, ALCFLF)) %>%
  dplyr::select(names(GADD_Sibs))

GADD_PreMplus <- GADD_Long_Full

GADD_Proband <- GADD_PreMplus %>%
  filter(ID_Rel == "00") %>%
  dplyr::select(ID_Fam, Wave, MyNIDAID, ID_MPL, 
                DEMMAL,DEMAGE,DEMHSP,DEMRAC, 
                SRPWCR:ALCYRS)
names(GADD_Proband)  <- c(names(GADD_Proband)[1:2], 
                          paste0(names(GADD_Proband)[3:length(names(GADD_Proband))],"Pr"))

GADD_Sibling <-  GADD_PreMplus %>%
  filter(ID_Rel == "03" | ID_Rel == "04") %>%
  dplyr::select(ID_Fam, Wave, MyNIDAID, ID_MPL, 
                DEMMAL,DEMAGE,DEMHSP,DEMRAC, 
                SRPWCR:ALCYRS)
names(GADD_Sibling)  <- c(names(GADD_Sibling)[1:2], 
                           paste0(names(GADD_Sibling)[3:length(names(GADD_Sibling))],"Sb"))

GADD_Pairs_A <- list(GADD_Proband,
                     GADD_Sibling) %>%
  reduce(full_join)

GADD_Pairs <- GADD_Pairs_A %>%
dplyr::select(ID_Fam, Wave, 
              MyNIDAIDPr, MyNIDAIDSb, 
                ID_MPLPr, ID_MPLSb, 
                DEMMALPr:DEMRACPr,
                DEMMALSb:DEMRACSb, 
                SRPWCRPr:ALCYRSPr,
                SRPWCRSb:ALCYRSSb)

GADD_SibPairs <- GADD_Pairs %>% 
  dplyr::select(ID_Fam, Wave, 
                MyNIDAIDPr, MyNIDAIDSb, 
                DEMMALPr:DEMRACPr,
                DEMMALSb:DEMRACSb) 

# write_csv(x = GADD_Pairs, 
#           path = "Work/Datasets/GADMJCOG_SibPairs_190618.csv", 
#           na = ".", col_names = T)

GADD_Descriptive <- GADD_PreMplus %>%
  dplyr::mutate(SRPWCRD = ifelse(!is.na(SRPWCR), 1, 0),
                WASVCBD = ifelse(!is.na(WASVCB), 1, 0),
                WASBLKD = ifelse(!is.na(WASBLK), 1, 0),
                WASDGTD = ifelse(!is.na(WASDGT), 1, 0),
                WSISUMD = ifelse(!is.na(WSISUM), 1, 0),
                TRLATSD = ifelse(!is.na(TRLATS), 1, 0),
                TRLBTSD = ifelse(!is.na(TRLBTS), 1, 0),
                CVLASFD = ifelse(!is.na(CVLASF), 1, 0),
                CVLASCD = ifelse(!is.na(CVLASC), 1, 0),
                CVLALFD = ifelse(!is.na(CVLALF), 1, 0),
                CVLALCD = ifelse(!is.na(CVLALC), 1, 0),
                CVLCOMD = ifelse(!is.na(CVLCOM), 1, 0)
                ) %>%
  dplyr::group_by(ID_MPL) %>%
  dplyr::mutate(SRPWCRDw = sum(SRPWCRD, na.rm=T),
                WASVCBDw = sum(WASVCBD, na.rm=T),
                WASBLKDw = sum(WASBLKD, na.rm=T),
                WASDGTDw = sum(WASDGTD, na.rm=T),
                WSISUMDw = sum(WSISUMD, na.rm=T),
                TRLATSDw = sum(TRLATSD, na.rm=T),
                TRLBTSDw = sum(TRLBTSD, na.rm=T),
                CVLASFDw = sum(CVLASFD, na.rm=T),
                CVLASCDw = sum(CVLASCD, na.rm=T),
                CVLALFDw = sum(CVLALFD, na.rm=T),
                CVLALCDw = sum(CVLALCD, na.rm=T),
                CVLCOMDw = sum(CVLCOMD, na.rm=T)
                ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(SRPWCRDt = ifelse(SRPWCRDw > 0, 1, 0),
                WASVCBDt = ifelse(WASVCBDw > 0, 1, 0),
                WASBLKDt = ifelse(WASBLKDw > 0, 1, 0),
                WASDGTDt = ifelse(WASDGTDw > 0, 1, 0),
                WSISUMDt = ifelse(WSISUMDw > 0, 1, 0),
                TRLATSDt = ifelse(TRLATSDw > 0, 1, 0),
                TRLBTSDt = ifelse(TRLBTSDw > 0, 1, 0),
                CVLASFDt = ifelse(CVLASFDw > 0, 1, 0),
                CVLASCDt = ifelse(CVLASCDw > 0, 1, 0),
                CVLALFDt = ifelse(CVLALFDw > 0, 1, 0),
                CVLALCDt = ifelse(CVLALCDw > 0, 1, 0),
                CVLCOMDt = ifelse(CVLCOMDw > 0, 1, 0)
                ) %>% 
  dplyr::group_by(ID_MPL) %>%
  dplyr::mutate(CogDataPoints = sum(x=c(SRPWCRDt,WASVCBDt,WASBLKDt,WASDGTDt,WSISUMDt, 
                                        TRLATSDt,TRLBTSDt,
                                        CVLASFDt,CVLASCDt,CVLALFDt,CVLALCDt), na.rm=T)) %>%
  dplyr::ungroup() %>% 
  dplyr::filter(CogDataPoints > 0)

GADD_Mplus <- GADD_Descriptive %>%
  dplyr::select(ID_Fam, Wave, ID_MPL,
                DEMMAL:DEMRAC,
                SRPWCR:CVLCOM,
                CANEVR:ALCYRS,
                SRPWCRDt:CVLCOMDt)

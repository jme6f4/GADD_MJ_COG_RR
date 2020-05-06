# HEADER ################################################################
# PURPOSE
#      Program:   04_Descriptives.R
#      Project:   GADMJCOG
#      Tasks:      
#                 A) Conduct analyses
#                 B) Build Tables & Figures
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# DATASETS
#      Libraries: GADMJCOG
#      Source:    INSERT
#      Derived:   INSERT
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# REVISION HISTORY
#      Jarrod Ellingson    DATE
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# COMMENTS
        source("Work/20_Clean_RR.R")
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# REFERENCES
#       INSERT
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


GADD_MplusRR_fordesc <- GADD_MplusRR %>% dplyr::select(ID_MPL, Wave)
GADD_DescriptiveRR <- list(GADD_Descriptive, 
                           GADD_MplusRR_fordesc) %>%
  purrr::reduce(inner_join)
GADD_DescriptiveRR_W1 <- GADD_DescriptiveRR %>% dplyr::filter(Wave == 1)
GADD_DescriptiveRR_W2 <- GADD_DescriptiveRR %>% dplyr::filter(Wave == 2)

GADD_DescriptiveRR_W1_Den <- GADD_DescriptiveRR_W1 %>% dplyr::filter(ID_Cty == "D")
GADD_DescriptiveRR_W2_Den <- GADD_DescriptiveRR_W2 %>% dplyr::filter(ID_Cty == "D")

GADD_DescriptiveRR_W1_San <- GADD_DescriptiveRR_W1 %>% dplyr::filter(ID_Cty == "S")
GADD_DescriptiveRR_W2_San <- GADD_DescriptiveRR_W2 %>% dplyr::filter(ID_Cty == "S")

#   1) Particpants ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
GADDn <- dim(table(GADD_DescriptiveRR$ID_MPL))
GADDn_fam <- dim(table(GADD_DescriptiveRR$ID_Fam))

GADD_Descriptive_W1 <- GADD_DescriptiveRR %>% filter(Wave == 1)                   
GADD_Descriptive_W2 <- GADD_DescriptiveRR %>% filter(Wave == 2)

GADD_Descriptive_W1_D <- GADD_DescriptiveRR %>% filter(Wave == 1 & ID_Cty == "D")                   
GADD_Descriptive_W1_S <- GADD_DescriptiveRR %>% filter(Wave == 1 & ID_Cty == "S")
GADD_Descriptive_W2_D <- GADD_DescriptiveRR %>% filter(Wave == 2 & ID_Cty == "D")                   
GADD_Descriptive_W2_S <- GADD_DescriptiveRR %>% filter(Wave == 2 & ID_Cty == "S")

#### Wave # (n=) ####
GADDn_W1 <- dim(table(GADD_Descriptive_W1$ID_MPL))
GADDn_W2 <- dim(table(GADD_Descriptive_W2$ID_MPL))

GADDn_W1_D <- dim(table(GADD_Descriptive_W1_D$ID_MPL))
GADDn_W1_S <- dim(table(GADD_Descriptive_W1_S$ID_MPL))
GADDn_W2_D <- dim(table(GADD_Descriptive_W2_D$ID_MPL))
GADDn_W2_S <- dim(table(GADD_Descriptive_W2_S$ID_MPL))

GADDn_W1
GADDn_W2

GADDn_W1_D
GADDn_W1_S
GADDn_W2_D
GADDn_W2_S

#### Denver/San Diego ####
table(GADD_Descriptive_W1$ID_Cty)
table(GADD_Descriptive_W2$ID_Cty)

#### Probands/Siblings ####
table(GADD_Descriptive_W1$ID_Rel)
# 596 Proband
# 596 Sibling
table(GADD_Descriptive_W2$ID_Rel)
# 431 Proband
# 444 Sibling

#### City x Proband Status ####
table(GADD_Descriptive_W1_D$ID_Rel == "00")
table(GADD_Descriptive_W1_D$ID_Rel %in% c("03", "04"))
table(GADD_Descriptive_W1_S$ID_Rel == "00")
table(GADD_Descriptive_W1_S$ID_Rel %in% c("03", "04"))
table(GADD_Descriptive_W2_D$ID_Rel == "00")
table(GADD_Descriptive_W2_D$ID_Rel %in% c("03", "04"))
table(GADD_Descriptive_W2_S$ID_Rel == "00")
table(GADD_Descriptive_W2_S$ID_Rel %in% c("03", "04"))

table(GADD_Descriptive_W1$ID_Cty, GADD_Descriptive_W1$ID_Rel)
# Denver: 351 Proband, 359 Sibling
# San Diego: 245 Proband, 237 Sibling

table(GADD_Descriptive_W2$ID_Cty, GADD_Descriptive_W2$ID_Rel)
# Denver: 225 Proband, 234 Sibling
# San Diego: 206 Proband, 210 Sibling

#### Age ####
w1AgeM <- mean(GADD_Descriptive_W1$DEMAGE, na.rm=T)
w2AgeM <- mean(GADD_Descriptive_W2$DEMAGE, na.rm=T)

w1AgeSD <- sd(GADD_Descriptive_W1$DEMAGE, na.rm=T)
w2AgeSD <- sd(GADD_Descriptive_W2$DEMAGE, na.rm=T)

w1AgeMd <- mean(GADD_Descriptive_W1_D$DEMAGE, na.rm=T)
w2AgeMd <- mean(GADD_Descriptive_W2_D$DEMAGE, na.rm=T)
w1AgeMs <- mean(GADD_Descriptive_W1_S$DEMAGE, na.rm=T)
w2AgeMs <- mean(GADD_Descriptive_W2_S$DEMAGE, na.rm=T)

w1AgeSDd <- sd(GADD_Descriptive_W1_D$DEMAGE, na.rm=T)
w2AgeSDd <- sd(GADD_Descriptive_W2_D$DEMAGE, na.rm=T)
w1AgeSDs <- sd(GADD_Descriptive_W1_S$DEMAGE, na.rm=T)
w2AgeSDs <- sd(GADD_Descriptive_W2_S$DEMAGE, na.rm=T)

w1AgeM
w1AgeSD

w2AgeM
w2AgeSD

w1AgeMd 
w1AgeSDd

w1AgeMs 
w1AgeSDs

w2AgeMd 
w2AgeSDd

w2AgeMs 
w2AgeSDs

#### Female/Male ####
w1MAL <- table(GADD_Descriptive_W1$DEMMAL)[2]
w2MAL <- table(GADD_Descriptive_W2$DEMMAL)[2]

w1MALd <- table(GADD_Descriptive_W1_D$DEMMAL)[2]
w1MALs <- table(GADD_Descriptive_W1_S$DEMMAL)[2]
w2MALd <- table(GADD_Descriptive_W2_D$DEMMAL)[2]
w2MALs <- table(GADD_Descriptive_W2_S$DEMMAL)[2]

w1MALd/GADDn_W1_D
w1MALs/GADDn_W1_S
w2MALd/GADDn_W2_D
w2MALs/GADDn_W2_S

##### Race/Hispanic ####
w1HSP <- table(GADD_Descriptive_W1$DEMHSP)[2]
w2HSP <- table(GADD_Descriptive_W2$DEMHSP)[2]

w1HSPd <- table(GADD_Descriptive_W1_D$DEMHSP=="Y")[2]
w1HSPs <- table(GADD_Descriptive_W1_S$DEMHSP=="Y")[2]
w2HSPd <- table(GADD_Descriptive_W2_D$DEMHSP=="Y")[2]
w2HSPs <- table(GADD_Descriptive_W2_S$DEMHSP=="Y")[2]

# w1RAC5/GADDn_W1 #44.9=White (Non Hispanic)
w1WHT <- table(GADD_Descriptive_W1$DEMHSP=="N", GADD_Descriptive_W1$DEMRAC=="5")[2,2]

w1WHTd <- table(GADD_Descriptive_W1_D$DEMHSP=="N", GADD_Descriptive_W1_D$DEMRAC=="5")[2,2]
w1WHTs <- table(GADD_Descriptive_W1_S$DEMHSP=="N", GADD_Descriptive_W1_S$DEMRAC=="5")[2,2]
w2WHTd <- table(GADD_Descriptive_W2_D$DEMHSP=="N", GADD_Descriptive_W2_D$DEMRAC=="5")[2,2]
w2WHTs <- table(GADD_Descriptive_W2_S$DEMHSP=="N", GADD_Descriptive_W2_S$DEMRAC=="5")[2,2]
# w1RAC4/GADDn_W1 #9.3 = Black
w1BLKd <- table(GADD_Descriptive_W1_D$DEMRAC=="4")[2]
w1BLKs <- table(GADD_Descriptive_W1_S$DEMRAC=="4")[2]
w2BLKd <- table(GADD_Descriptive_W2_D$DEMRAC=="4")[2]
w2BLKs <- table(GADD_Descriptive_W2_S$DEMRAC=="4")[2]
# w1RAC1/GADDn_W1 #1.5 = American Indian
w1AINd <- table(GADD_Descriptive_W1_D$DEMRAC=="1")[2]
w1AINs <- table(GADD_Descriptive_W1_S$DEMRAC=="1")[2]
w2AINd <- table(GADD_Descriptive_W2_D$DEMRAC=="1")[2]
w2AINs <- table(GADD_Descriptive_W2_S$DEMRAC=="1")[2]
# w1RAC2/GADDn_W1 #0.7 = Asian
# w1RAC3/GADDn_W1 #0.8 = Pacific Islander
w1APId <- table(GADD_Descriptive_W1_D$DEMRAC %in% c("2","3"))[2]
w1APIs <- table(GADD_Descriptive_W1_S$DEMRAC %in% c("2","3"))[2]
w2APId <- table(GADD_Descriptive_W2_D$DEMRAC %in% c("2","3"))[2]
w2APIs <- table(GADD_Descriptive_W2_S$DEMRAC %in% c("2","3"))[2]
# w1RAC6/GADDn_W1 #11.3 = Multiracial
w1MLTd <- table(GADD_Descriptive_W1_D$DEMRAC=="6")[2]
w1MLTs <- table(GADD_Descriptive_W1_S$DEMRAC=="6")[2]
w2MLTd <- table(GADD_Descriptive_W2_D$DEMRAC=="6")[2]
w2MLTs <- table(GADD_Descriptive_W2_S$DEMRAC=="6")[2]
# w1RAC9/GADDn_W1 #Other (Non-Hispanic)
w1OTHd <- table(GADD_Descriptive_W1_D$DEMHSP == "N", GADD_Descriptive_W1_D$DEMRAC == "9")[2,2]
w1OTHs <- table(GADD_Descriptive_W1_S$DEMHSP == "N", GADD_Descriptive_W1_S$DEMRAC == "9")[2,2]
w2OTHd <- table(GADD_Descriptive_W2_D$DEMHSP == "N", GADD_Descriptive_W2_D$DEMRAC == "9")[2,2]
w2OTHs <- table(GADD_Descriptive_W2_S$DEMHSP == "N", GADD_Descriptive_W2_S$DEMRAC == "9")[2,2]

# Hispanic
round(w1HSPd/GADDn_W1_D, 3)*100
round(w1HSPs/GADDn_W1_S, 3)*100
round(w2HSPd/GADDn_W2_D, 3)*100
round(w2HSPs/GADDn_W2_S, 3)*100
# White Non-Hisp.
round(w1WHTd/GADDn_W1_D, 3)*100
round(w1WHTs/GADDn_W1_S, 3)*100
round(w2WHTd/GADDn_W2_D, 3)*100
round(w2WHTs/GADDn_W2_S, 3)*100
# Black/AA
round(w1BLKd/GADDn_W1_D, 3)*100
round(w1BLKs/GADDn_W1_S, 3)*100
round(w2BLKd/GADDn_W2_D, 3)*100
round(w2BLKs/GADDn_W2_S, 3)*100
# American Indian
round(w1AINd/GADDn_W1_D, 3)*100
round(w1AINs/GADDn_W1_S, 3)*100
round(w2AINd/GADDn_W2_D, 3)*100
round(w2AINs/GADDn_W2_S, 3)*100
# Asian/Pacific
round(w1APId/GADDn_W1_D, 3)*100
round(w1APIs/GADDn_W1_S, 3)*100
round(w2APId/GADDn_W2_D, 3)*100
round(w2APIs/GADDn_W2_S, 3)*100
# Multiracial
round(w1MLTd/GADDn_W1_D, 3)*100
round(w1MLTs/GADDn_W1_S, 3)*100
round(w2MLTd/GADDn_W2_D, 3)*100
round(w2MLTs/GADDn_W2_S, 3)*100
# Other
round(w1OTHd/GADDn_W1_D, 3)*100
round(w1OTHs/GADDn_W1_S, 3)*100
round(w2OTHd/GADDn_W2_D, 3)*100
round(w2OTHs/GADDn_W2_S, 3)*100


w1RAC1 <- table(GADD_Descriptive_W1$DEMRAC)[1]
w1RAC2 <- table(GADD_Descriptive_W1$DEMRAC)[2]
w1RAC3 <- table(GADD_Descriptive_W1$DEMRAC)[3]
w1RAC4 <- table(GADD_Descriptive_W1$DEMRAC)[4]
w1RAC5 <- table(GADD_Descriptive_W1$DEMRAC)[5]
w1RAC6 <- table(GADD_Descriptive_W1$DEMRAC)[6]
w1RAC9 <- table(GADD_Descriptive_W1$DEMRAC)[7]

w2RAC1 <- table(GADD_Descriptive_W2$DEMRAC)[1]
w2RAC2 <- table(GADD_Descriptive_W2$DEMRAC)[2]
w2RAC3 <- table(GADD_Descriptive_W2$DEMRAC)[3]
w2RAC4 <- table(GADD_Descriptive_W2$DEMRAC)[4]
w2RAC5 <- table(GADD_Descriptive_W2$DEMRAC)[5]
w2RAC6 <- table(GADD_Descriptive_W2$DEMRAC)[6]
w2RAC9 <- table(GADD_Descriptive_W2$DEMRAC)[7]


w2RAC1/GADDn_W2
w2RAC2/GADDn_W2
w2RAC3/GADDn_W2
w2RAC4/GADDn_W2
w2RAC5/GADDn_W2
w2RAC6/GADDn_W2
w2RAC9/GADDn_W2



table(GADD_Descriptive_W1$DEMHSP, GADD_Descriptive_W1$DEMRAC)

#### Cannabis Ever ####
w1EVR <- table(GADD_Descriptive_W1$CANEVR)[2]
w2EVR <- table(GADD_Descriptive_W2$CANEVR)[2]

w1EVRd <- table(GADD_Descriptive_W1_D$CANEVR)[2]
w1EVRs <- table(GADD_Descriptive_W1_S$CANEVR)[2]
w2EVRd <- table(GADD_Descriptive_W2_D$CANEVR)[2]
w2EVRs <- table(GADD_Descriptive_W2_S$CANEVR)[2]

w1EVR/GADDn_W1
w2EVR/GADDn_W2

w1EVRd/GADDn_W1_D
w1EVRs/GADDn_W1_S
w2EVRd/GADDn_W2_D
w2EVRs/GADDn_W2_S

##### Onset ####
w1CANONS_M <- mean(GADD_Descriptive_W1$CANONS, na.rm=T)
w2CANONS_M <- mean(GADD_Descriptive_W2$CANONS, na.rm=T)

w1CANONS_SD <- sd(GADD_Descriptive_W1$CANONS, na.rm=T)
w2CANONS_SD <- sd(GADD_Descriptive_W2$CANONS, na.rm=T)


w1CANONS_Md <- mean(GADD_Descriptive_W1_D$CANONS, na.rm=T)
w1CANONS_Ms <- mean(GADD_Descriptive_W1_S$CANONS, na.rm=T)
w2CANONS_Md <- mean(GADD_Descriptive_W2_D$CANONS, na.rm=T)
w2CANONS_Ms <- mean(GADD_Descriptive_W2_S$CANONS, na.rm=T)

w1CANONS_SDd <- sd(GADD_Descriptive_W1_D$CANONS, na.rm=T)
w1CANONS_SDs <- sd(GADD_Descriptive_W1_S$CANONS, na.rm=T)
w2CANONS_SDs <- sd(GADD_Descriptive_W2_D$CANONS, na.rm=T)
w2CANONS_SDs <- sd(GADD_Descriptive_W2_S$CANONS, na.rm=T)

w1CANONS_M
w1CANONS_SD
w2CANONS_M
w2CANONS_SD

w1CANONS_Md 
w1CANONS_SDd

w1CANONS_Ms 
w1CANONS_SDs

w2CANONS_Md 
w2CANONS_SDs

w2CANONS_Ms 
w2CANONS_SDs

min(GADD_Descriptive_W1_D$CANONS, na.rm=T)
min(GADD_Descriptive_W1_S$CANONS, na.rm=T)
min(GADD_Descriptive_W2_D$CANONS, na.rm=T)
min(GADD_Descriptive_W2_S$CANONS, na.rm=T)

max(GADD_Descriptive_W1_D$CANONS, na.rm=T)
max(GADD_Descriptive_W1_S$CANONS, na.rm=T)
max(GADD_Descriptive_W2_D$CANONS, na.rm=T)
max(GADD_Descriptive_W2_S$CANONS, na.rm=T)



##### FTY ####
table(GADD_Descriptive_W1$CANFTY)
sum(table(GADD_Descriptive_W1$CANFTY))

w1CANFTY_M <- mean(GADD_Descriptive_W1$CANFTY, na.rm=T)
w1CANFTY_SD <- sd(GADD_Descriptive_W1$CANFTY, na.rm=T)

w1CANFTY_Md <- mean(GADD_Descriptive_W1_D$CANFTY, na.rm=T)
w1CANFTY_SDd <- sd(GADD_Descriptive_W1_D$CANFTY, na.rm=T)

w1CANFTY_Ms <- mean(GADD_Descriptive_W1_S$CANFTY, na.rm=T)
w1CANFTY_SDs <- sd(GADD_Descriptive_W1_S$CANFTY, na.rm=T)

w1CANFTY_M
w1CANFTY_SD

w1CANFTY_Md
w1CANFTY_SDd
w1CANFTY_Ms
w1CANFTY_SDs


##### F6M ####
w1CANF6M_M <- mean(GADD_Descriptive_W1$CANF6M, na.rm=T)
w2CANF6M_M <- mean(GADD_Descriptive_W2$CANF6M, na.rm=T)

w1CANF6M_SD <- sd(GADD_Descriptive_W1$CANF6M, na.rm=T)
w2CANF6M_SD <- sd(GADD_Descriptive_W2$CANF6M, na.rm=T)

w1CANF6M_Md <- mean(GADD_Descriptive_W1_D$CANF6M, na.rm=T)
w1CANF6M_Ms <- mean(GADD_Descriptive_W1_S$CANF6M, na.rm=T)
w2CANF6M_Md <- mean(GADD_Descriptive_W2_D$CANF6M, na.rm=T)
w2CANF6M_Ms <- mean(GADD_Descriptive_W2_S$CANF6M, na.rm=T)

w1CANF6M_SDd <- sd(GADD_Descriptive_W1_D$CANF6M, na.rm=T)
w1CANF6M_SDs <- sd(GADD_Descriptive_W1_S$CANF6M, na.rm=T)
w2CANF6M_SDd <- sd(GADD_Descriptive_W2_D$CANF6M, na.rm=T)
w2CANF6M_SDs <- sd(GADD_Descriptive_W2_S$CANF6M, na.rm=T)

w1CANF6M_M
w1CANF6M_SD

w2CANF6M_M
w2CANF6M_SD

w1CANF6M_Md 
w1CANF6M_SDd

w1CANF6M_Ms 
w1CANF6M_SDs

w2CANF6M_Md 
w2CANF6M_SDs

w2CANF6M_Ms 
w2CANF6M_SDs

##### FLF ####
w1CANFLF_M <- mean(GADD_Descriptive_W1$CANFLF, na.rm=T)
w2CANFLF_M <- mean(GADD_Descriptive_W2$CANFLF, na.rm=T)

w1CANFLF_SD <- sd(GADD_Descriptive_W1$CANFLF, na.rm=T)
w2CANFLF_SD <- sd(GADD_Descriptive_W2$CANFLF, na.rm=T)

w1CANFLF_Md <- mean(GADD_Descriptive_W1_D$CANFLF, na.rm=T)
w1CANFLF_Ms <- mean(GADD_Descriptive_W1_S$CANFLF, na.rm=T)
w2CANFLF_Md <- mean(GADD_Descriptive_W2_D$CANFLF, na.rm=T)
w2CANFLF_Ms <- mean(GADD_Descriptive_W2_S$CANFLF, na.rm=T)

w1CANFLF_SDd <- sd(GADD_Descriptive_W1_D$CANFLF, na.rm=T)
w1CANFLF_SDs <- sd(GADD_Descriptive_W1_S$CANFLF, na.rm=T)
w2CANFLF_SDd <- sd(GADD_Descriptive_W2_D$CANFLF, na.rm=T)
w2CANFLF_SDs <- sd(GADD_Descriptive_W2_S$CANFLF, na.rm=T)

w1CANFLF_M
w1CANFLF_SD

w2CANFLF_M
w2CANFLF_SD

w1CANFLF_Md 
w1CANFLF_SDd 

w1CANFLF_Ms 
w1CANFLF_SDs 

w2CANFLF_Md 
w2CANFLF_SDd 

w2CANFLF_Ms 
w2CANFLF_SDs 

#### Alcohol Ever ####
w1ALCEVR <- table(GADD_Descriptive_W1$ALCEVR)[2]
w2ALCEVR <- table(GADD_Descriptive_W2$ALCEVR)[2]

w1ALCEVRd <- table(GADD_Descriptive_W1_D$ALCEVR)[2]
w1ALCEVRs <- table(GADD_Descriptive_W1_S$ALCEVR)[2]
w2ALCEVRd <- table(GADD_Descriptive_W2_D$ALCEVR)[2]
w2ALCEVRs <- table(GADD_Descriptive_W2_S$ALCEVR)[2]

w1ALCEVR/GADDn_W1
w2ALCEVR/GADDn_W2

w1ALCEVRd/GADDn_W1_D
w1ALCEVRs/GADDn_W1_S
w2ALCEVRd/GADDn_W2_D
w2ALCEVRs/GADDn_W2_S

##### Alcohol Onset ####
w1ALCONS_M <- mean(GADD_Descriptive_W1$ALCONS, na.rm=T)
w2ALCONS_M <- mean(GADD_Descriptive_W2$ALCONS, na.rm=T)

w1ALCONS_SD <- sd(GADD_Descriptive_W1$ALCONS, na.rm=T)
w2ALCONS_SD <- sd(GADD_Descriptive_W2$ALCONS, na.rm=T)


w1ALCONS_Md <- mean(GADD_Descriptive_W1_D$ALCONS, na.rm=T)
w1ALCONS_Ms <- mean(GADD_Descriptive_W1_S$ALCONS, na.rm=T)
w2ALCONS_Md <- mean(GADD_Descriptive_W2_D$ALCONS, na.rm=T)
w2ALCONS_Ms <- mean(GADD_Descriptive_W2_S$ALCONS, na.rm=T)

w1ALCONS_SDd <- sd(GADD_Descriptive_W1_D$ALCONS, na.rm=T)
w1ALCONS_SDs <- sd(GADD_Descriptive_W1_S$ALCONS, na.rm=T)
w2ALCONS_SDs <- sd(GADD_Descriptive_W2_D$ALCONS, na.rm=T)
w2ALCONS_SDs <- sd(GADD_Descriptive_W2_S$ALCONS, na.rm=T)

w1ALCONS_M
w1ALCONS_SD
w2ALCONS_M
w2ALCONS_SD

w1ALCONS_Md 
w1ALCONS_SDd

w1ALCONS_Ms 
w1ALCONS_SDs

w2ALCONS_Md 
w2ALCONS_SDs

w2ALCONS_Ms 
w2ALCONS_SDs

min(GADD_Descriptive_W1_D$ALCONS, na.rm=T)
min(GADD_Descriptive_W1_S$ALCONS, na.rm=T)
min(GADD_Descriptive_W2_D$ALCONS, na.rm=T)
min(GADD_Descriptive_W2_S$ALCONS, na.rm=T)

max(GADD_Descriptive_W1_D$ALCONS, na.rm=T)
max(GADD_Descriptive_W1_S$ALCONS, na.rm=T)
max(GADD_Descriptive_W2_D$ALCONS, na.rm=T)
max(GADD_Descriptive_W2_S$ALCONS, na.rm=T)

##### Alcohol F6M ####
w1ALCF6M_M <- mean(GADD_Descriptive_W1$ALCF6M, na.rm=T)
w2ALCF6M_M <- mean(GADD_Descriptive_W2$ALCF6M, na.rm=T)

w1ALCF6M_SD <- sd(GADD_Descriptive_W1$ALCF6M, na.rm=T)
w2ALCF6M_SD <- sd(GADD_Descriptive_W2$ALCF6M, na.rm=T)

w1ALCF6M_Md <- mean(GADD_Descriptive_W1_D$ALCF6M, na.rm=T)
w1ALCF6M_Ms <- mean(GADD_Descriptive_W1_S$ALCF6M, na.rm=T)
w2ALCF6M_Md <- mean(GADD_Descriptive_W2_D$ALCF6M, na.rm=T)
w2ALCF6M_Ms <- mean(GADD_Descriptive_W2_S$ALCF6M, na.rm=T)

w1ALCF6M_SDd <- sd(GADD_Descriptive_W1_D$ALCF6M, na.rm=T)
w1ALCF6M_SDs <- sd(GADD_Descriptive_W1_S$ALCF6M, na.rm=T)
w2ALCF6M_SDd <- sd(GADD_Descriptive_W2_D$ALCF6M, na.rm=T)
w2ALCF6M_SDs <- sd(GADD_Descriptive_W2_S$ALCF6M, na.rm=T)

w1ALCF6M_M
w1ALCF6M_SD

w2ALCF6M_M
w2ALCF6M_SD

w1ALCF6M_Md 
w1ALCF6M_SDd

w1ALCF6M_Ms 
w1ALCF6M_SDs

w2ALCF6M_Md 
w2ALCF6M_SDs

w2ALCF6M_Ms 
w2ALCF6M_SDs


##### FLF ####
w1ALCFLF_M <- mean(GADD_Descriptive_W1$ALCFLF, na.rm=T)
w2ALCFLF_M <- mean(GADD_Descriptive_W2$ALCFLF, na.rm=T)

w1ALCFLF_SD <- sd(GADD_Descriptive_W1$ALCFLF, na.rm=T)
w2ALCFLF_SD <- sd(GADD_Descriptive_W2$ALCFLF, na.rm=T)

w1ALCFLF_Md <- mean(GADD_Descriptive_W1_D$ALCFLF, na.rm=T)
w1ALCFLF_Ms <- mean(GADD_Descriptive_W1_S$ALCFLF, na.rm=T)
w2ALCFLF_Md <- mean(GADD_Descriptive_W2_D$ALCFLF, na.rm=T)
w2ALCFLF_Ms <- mean(GADD_Descriptive_W2_S$ALCFLF, na.rm=T)

w1ALCFLF_SDd <- sd(GADD_Descriptive_W1_D$ALCFLF, na.rm=T)
w1ALCFLF_SDs <- sd(GADD_Descriptive_W1_S$ALCFLF, na.rm=T)
w2ALCFLF_SDd <- sd(GADD_Descriptive_W2_D$ALCFLF, na.rm=T)
w2ALCFLF_SDs <- sd(GADD_Descriptive_W2_S$ALCFLF, na.rm=T)

w1ALCFLF_M
w1ALCFLF_SD

w2ALCFLF_M
w2ALCFLF_SD

w1ALCFLF_Md 
w1ALCFLF_SDd 

w1ALCFLF_Ms 
w1ALCFLF_SDs 

w2ALCFLF_Md 
w2ALCFLF_SDd 

w2ALCFLF_Ms 
w2ALCFLF_SDs 


w1ALCFLF_M
w1ALCFLF_SD

w2ALCFLF_M
w2ALCFLF_SD

# IQ ####
w1WSISUM_Md <- mean(GADD_Descriptive_W1_D$WSISUM, na.rm=T)
w1WSISUM_SDd <- sd(GADD_Descriptive_W1_D$WSISUM, na.rm=T)

w1WSISUM_Ms <- mean(GADD_Descriptive_W1_S$WSISUM, na.rm=T)
w1WSISUM_SDs <- sd(GADD_Descriptive_W1_S$WSISUM, na.rm=T)

round(w1WSISUM_Md,1)
round(w1WSISUM_SDd,1)

round(w1WSISUM_Ms,1)
round(w1WSISUM_SDs,1)

# GADD_Year_W1 <- c(min(GADD_DescriptiveRR$testyr.lyons2, na.rm = T), 
#                  max(GADD_DescriptiveRR$testyr.lyons2, na.rm = T))
# GADD_Year_W2 <- c(min(GADD_DescriptiveRR$testyr.lyons3, na.rm = T), 
#                  max(GADD_DescriptiveRR$testyr.lyons3, na.rm = T))

GADD_Male_Percent <- round(100*mean(as.numeric(GADD_DescriptiveRR$DEMMAL), na.rm = T),2)

GADD_Age_W1 <- c(min(as.numeric(GADD_Descriptive_W1$age), na.rm = T), 
                    max(as.numeric(GADD_Descriptive_W1$age), na.rm = T))
GADD_Age_W1_Mean <- round(mean(as.numeric(GADD_Descriptive_W1$age), na.rm = T),2)
GADD_Age_W1_SD <- round(sd(as.numeric(GADD_Descriptive_W1$age), na.rm = T),2)

GADD_Age_W2 <- c(min(as.numeric(GADD_Descriptive_W2$age), na.rm = T), 
                    max(as.numeric(GADD_Descriptive_W2$age), na.rm = T))
GADD_Age_W2_Mean <- round(mean(as.numeric(GADD_Descriptive_W2$age), na.rm = T),2)
GADD_Age_W2_SD <- round(sd(as.numeric(GADD_Descriptive_W2$age), na.rm = T),2)


#   2) Measures  ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
GADD_Descriptive_W1_Users <- GADD_Descriptive_W1 %>% dplyr::filter(CANEVR ==1)
CANEVER_W1 <- round(100*mean(as.numeric(GADD_Descriptive_W1$CANEVR), na.rm = T),2)
CANEVER_W2 <- round(100*mean(as.numeric(GADD_Descriptive_W2$CANEVR), na.rm = T),2)

CANONS_W1 <- round(mean(as.numeric(GADD_Descriptive_W1$CANONS), na.rm = T),2)
CANONS_W2 <- round(mean(as.numeric(GADD_Descriptive_W2$CANONS), na.rm = T),2)

CANF6M_W1m <- round(mean(as.numeric(GADD_Descriptive_W1$CANF6M), na.rm = T),2)
CANF6M_W1sd <- round(sd(as.numeric(GADD_Descriptive_W1$CANF6M), na.rm = T),2)
CANF6M_W2 <- round(mean(as.numeric(GADD_Descriptive_W2$CANF6M), na.rm = T),2)

CANFTY_W1m <- round(mean(as.numeric(GADD_Descriptive_W1$CANFTY), na.rm = T),2)
CANFTY_W1sd <- round(sd(as.numeric(GADD_Descriptive_W1$CANFTY), na.rm = T),2)
CANFTY_W2 <- round(mean(as.numeric(GADD_Descriptive_W2$CANFTY), na.rm = T),2)

CANFLF_W1m <- round(mean(as.numeric(GADD_Descriptive_W1$CANFLF), na.rm = T),2)
CANFLF_W2m <- round(mean(as.numeric(GADD_Descriptive_W2$CANFLF), na.rm = T),2)
CANFLF_W1sd <- round(sd(as.numeric(GADD_Descriptive_W1$CANFLF), na.rm = T),2)
CANFLF_W2sd <- round(sd(as.numeric(GADD_Descriptive_W2$CANFLF), na.rm = T),2)

CANAC6_W2 <- round(mean(as.numeric(GADD_Descriptive_W2$CANAC6), na.rm = T),2)


#   3) Sib-Pair  ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
GADD_Descriptive_SibAvgs <- GADD_DescriptiveRR %>%
  dplyr::group_by(ID_Fam, Wave) %>%
  dplyr::mutate(CANEVRm = round(mean(CANEVR, na.rm = T),2), 
                CANONSm = round(mean(CANONS, na.rm = T),2), 
                CANFLFm = round(mean(CANFLF, na.rm = T),2), 
                CANF6Mm = round(mean(CANF6M, na.rm = T),2), 
                CANAC6m = round(mean(CANAC6, na.rm = T),2), 
                CVLCOMm = round(mean(CVLCOM, na.rm = T),2)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(CANEVRd = CANEVR - CANEVRm, 
                CANONSd = CANONS - CANONSm, 
                CANFLFd = CANFLF - CANFLFm, 
                CANF6Md = CANF6M - CANF6Mm, 
                CANAC6d = CANAC6 - CANAC6m, 
                CVLCOMd = CVLCOM - CVLCOMm) %>%
  dplyr::mutate(CANEVRanyD = CANEVRd > 0, 
                CANONSanyD = CANONSd < 0, 
                CANFLFanyD = CANFLFd > 0, 
                CANF6ManyD = CANF6Md > 0, 
                CANAC6anyD = CANAC6d > 0,
                CVLCOManyD = CVLCOMd > 0) 

GADD_Descriptive_SibAvgs_W1 <- GADD_Descriptive_SibAvgs %>% dplyr::filter(Wave == 1)
GADD_Descriptive_SibAvgs_W2 <- GADD_Descriptive_SibAvgs %>% dplyr::filter(Wave == 2)

GADD_Descriptive_Greater_EVR <- GADD_Descriptive_SibAvgs %>%
  dplyr::filter(CANEVRd > 0)
GADD_Descriptive_Greater_ONS <- GADD_Descriptive_SibAvgs %>%
  dplyr::filter(CANONSd < 0)
GADD_Descriptive_Greater_FLF <- GADD_Descriptive_SibAvgs %>%
  dplyr::filter(CANFLFd > 0)
GADD_Descriptive_Greater_F6M <- GADD_Descriptive_SibAvgs %>%
  dplyr::filter(CANF6Md > 0)
GADD_Descriptive_Greater_AC6 <- GADD_Descriptive_SibAvgs %>%
  dplyr::filter(CANAC6d > 0)

GADD_Descriptive_Greater_ONS_W1 <- GADD_Descriptive_Greater_ONS %>% dplyr::filter(Wave == 1)
GADD_Descriptive_Greater_ONS_W2 <- GADD_Descriptive_Greater_ONS %>% dplyr::filter(Wave == 2)

GADD_Descriptive_Greater_FLF_W1 <- GADD_Descriptive_Greater_FLF %>% dplyr::filter(Wave == 1)
GADD_Descriptive_Greater_FLF_W2 <- GADD_Descriptive_Greater_FLF %>% dplyr::filter(Wave == 2)

GADD_Descriptive_Greater_F6M_W1 <- GADD_Descriptive_Greater_F6M %>% dplyr::filter(Wave == 1)
GADD_Descriptive_Greater_F6M_W2 <- GADD_Descriptive_Greater_F6M %>% dplyr::filter(Wave == 2)

GADD_Descriptive_Greater_AC6_W1 <- GADD_Descriptive_Greater_AC6 %>% dplyr::filter(Wave == 1)
GADD_Descriptive_Greater_AC6_W2 <- GADD_Descriptive_Greater_AC6 %>% dplyr::filter(Wave == 2)

dim(table(GADD_Descriptive_SibAvgs_W1$ID_Fam))
dim(table(GADD_Descriptive_SibAvgs_W2$ID_Fam))
table(GADD_Descriptive_Greater_EVR$CANEVRanyD, GADD_Descriptive_Greater_EVR$Wave)
table(GADD_Descriptive_Greater_ONS$CANONSanyD, GADD_Descriptive_Greater_ONS$Wave)
table(GADD_Descriptive_Greater_FLF$CANFLFanyD, GADD_Descriptive_Greater_FLF$Wave)
table(GADD_Descriptive_Greater_F6M$CANF6ManyD, GADD_Descriptive_Greater_F6M$Wave)
table(GADD_Descriptive_Greater_AC6$CANAC6anyD, GADD_Descriptive_Greater_AC6$Wave)

table(GADD_Descriptive_SibAvgs$CANEVRm, GADD_Descriptive_SibAvgs$Wave)
Discordant_CANEVER_Wave1 <- round(100*((4+21+170+5+72+20)/(4+21+170+5+72+20+10+912)), 2)
Discordant_CANEVER_Wave2 <- round(100*((6+70+24+4)/(6+70+24+4+10+779)), 2)
Discordant_CANEVER_Wave1
Discordant_CANEVER_Wave2

dim(GADD_Descriptive_Greater_ONS_W1)
dim(GADD_Descriptive_Greater_ONS_W2)
mean(GADD_Descriptive_Greater_ONS_W1$CANONSd, na.rm=T)
mean(GADD_Descriptive_Greater_ONS_W2$CANONSd, na.rm=T)

dim(GADD_Descriptive_Greater_FLF_W1)
dim(GADD_Descriptive_Greater_FLF_W2)
mean(GADD_Descriptive_Greater_FLF_W1$CANFLFd, na.rm=T)
mean(GADD_Descriptive_Greater_FLF_W2$CANFLFd, na.rm=T)

dim(GADD_Descriptive_Greater_F6M_W1)
dim(GADD_Descriptive_Greater_F6M_W2)
mean(GADD_Descriptive_Greater_F6M_W1$CANF6Md, na.rm=T)
mean(GADD_Descriptive_Greater_F6M_W2$CANF6Md, na.rm=T)

dim(GADD_Descriptive_Greater_AC6_W2)
mean(GADD_Descriptive_Greater_AC6_W2$CANAC6d, na.rm=T)

table(!is.na(GADD_Descriptive_Greater_AC6_W2$CVLCOM))      

scatterHist(GADD_Descriptive_Greater_AC6_W2$CANAC6d, GADD_Descriptive_Greater_AC6_W2$CVLCOM)
scatterHist(GADD_Descriptive_Greater_ONS_W2$CANONSd, GADD_Descriptive_Greater_ONS_W2$CVLCOM)
scatterHist(GADD_Descriptive_Greater_ONS_W2$CANONSd, GADD_Descriptive_Greater_ONS_W2$COGPGS)


# PGS  ####
GADD_SibPairs_pi50 <- GADD_Pairs %>% filter(PI_HAT > .38 & 
                                              Wave == 1 & 
                                              PC1Pr < 0 & PC1Sb)
dim(GADD_SibPairs_pi50)
scatter.hist(GADD_SibPairs_pi50$COGPGSPr, GADD_SibPairs_pi50$COGPGSSb)


# COG  ####
mean(GADD_Descriptive_W1_D$WSISUM, na.rm=T)
sd(GADD_Descriptive_W1_D$WSISUM, na.rm=T)

mean(GADD_Descriptive_W1_S$WSISUM, na.rm=T)
sd(GADD_Descriptive_W1_S$WSISUM, na.rm=T)



mean(GADD_Descriptive_W1_D$SRPWCR, na.rm=T)
sd(GADD_Descriptive_W1_D$SRPWCR, na.rm=T)

mean(GADD_Descriptive_W1_S$SRPWCR, na.rm=T)
sd(GADD_Descriptive_W1_S$SRPWCR, na.rm=T)

mean(GADD_Descriptive_W2_D$SRPWCR, na.rm=T)
sd(GADD_Descriptive_W2_D$SRPWCR, na.rm=T)

mean(GADD_Descriptive_W2_S$SRPWCR, na.rm=T)
sd(GADD_Descriptive_W2_S$SRPWCR, na.rm=T)


CVLw1 <- GADD_DescriptiveRR %>% filter(Wave == 1) %>% select(CVLALC, CVLALF, 
                                                           CVLASC, CVLASF)
  
CVLw2 <- GADD_DescriptiveRR %>% filter(Wave == 2) %>% select(CVLALC, CVLALF, 
                                                           CVLASC, CVLASF)
  
cor(CVLw1, use = "pairwise.complete.obs")
cor(CVLw2, use = "pairwise.complete.obs")

GADD_Mpus_Wave1 <- GADD_Mplus %>% filter(Wave == 1)
GADD_Mpus_Wave2 <- GADD_Mplus %>% filter(Wave == 2)

scatterHist(GADD_Mpus_Wave1$COGPGS, GADD_Mpus_Wave1$CANAC6)
scatterHist(GADD_Mpus_Wave1$COGPGS, GADD_Mpus_Wave1$CANONS)
scatterHist(GADD_Mpus_Wave1$COGPGS, GADD_Mpus_Wave1$CANFLF)
scatterHist(GADD_Mpus_Wave1$COGPGS, GADD_Mpus_Wave1$CANFTY)
scatterHist(GADD_Mpus_Wave1$COGPGS, GADD_Mpus_Wave1$CANF6M)

scatterHist(GADD_Mpus_Wave2$COGPGS, GADD_Mpus_Wave2$CANAC6)
scatterHist(GADD_Mpus_Wave2$COGPGS, GADD_Mpus_Wave2$CANONS)
scatterHist(GADD_Mpus_Wave2$COGPGS, GADD_Mpus_Wave2$CANFLF)
scatterHist(GADD_Mpus_Wave2$COGPGS, GADD_Mpus_Wave2$CANFTY)
scatterHist(GADD_Mpus_Wave2$COGPGS, GADD_Mpus_Wave2$CANF6M)

# RR ####
# CANONS #
min(GADD_Descriptive_W1_D$CANONS, na.rm=T)
max(GADD_Descriptive_W1_D$CANONS, na.rm=T)

min(GADD_Descriptive_W1_S$CANONS, na.rm=T)
max(GADD_Descriptive_W1_S$CANONS, na.rm=T)

min(GADD_Descriptive_W2_D$CANONS, na.rm=T)
max(GADD_Descriptive_W2_D$CANONS, na.rm=T)

min(GADD_Descriptive_W2_S$CANONS, na.rm=T)
max(GADD_Descriptive_W2_S$CANONS, na.rm=T)

# CANF6M #
min(GADD_Descriptive_W1_D$CANF6M, na.rm=T)
max(GADD_Descriptive_W1_D$CANF6M, na.rm=T)

min(GADD_Descriptive_W1_S$CANF6M, na.rm=T)
max(GADD_Descriptive_W1_S$CANF6M, na.rm=T)

min(GADD_Descriptive_W2_D$CANF6M, na.rm=T)
max(GADD_Descriptive_W2_D$CANF6M, na.rm=T)

min(GADD_Descriptive_W2_S$CANF6M, na.rm=T)
max(GADD_Descriptive_W2_S$CANF6M, na.rm=T)

# CANFLF #
min(GADD_Descriptive_W1_D$CANFLF, na.rm=T)
max(GADD_Descriptive_W1_D$CANFLF, na.rm=T)

min(GADD_Descriptive_W1_S$CANFLF, na.rm=T)
max(GADD_Descriptive_W1_S$CANFLF, na.rm=T)

min(GADD_Descriptive_W2_D$CANFLF, na.rm=T)
max(GADD_Descriptive_W2_D$CANFLF, na.rm=T)

min(GADD_Descriptive_W2_S$CANFLF, na.rm=T)
max(GADD_Descriptive_W2_S$CANFLF, na.rm=T)


# ALCONS #
min(GADD_Descriptive_W1_D$ALCONS, na.rm=T)
max(GADD_Descriptive_W1_D$ALCONS, na.rm=T)

min(GADD_Descriptive_W1_S$ALCONS, na.rm=T)
max(GADD_Descriptive_W1_S$ALCONS, na.rm=T)

min(GADD_Descriptive_W2_D$ALCONS, na.rm=T)
max(GADD_Descriptive_W2_D$ALCONS, na.rm=T)

min(GADD_Descriptive_W2_S$ALCONS, na.rm=T)
max(GADD_Descriptive_W2_S$ALCONS, na.rm=T)

# ALCF6M #
min(GADD_Descriptive_W1_D$ALCF6M, na.rm=T)
max(GADD_Descriptive_W1_D$ALCF6M, na.rm=T)

min(GADD_Descriptive_W1_S$ALCF6M, na.rm=T)
max(GADD_Descriptive_W1_S$ALCF6M, na.rm=T)

min(GADD_Descriptive_W2_D$ALCF6M, na.rm=T)
max(GADD_Descriptive_W2_D$ALCF6M, na.rm=T)

min(GADD_Descriptive_W2_S$ALCF6M, na.rm=T)
max(GADD_Descriptive_W2_S$ALCF6M, na.rm=T)

# ALCFLF #
min(GADD_Descriptive_W1_D$ALCFLF, na.rm=T)
max(GADD_Descriptive_W1_D$ALCFLF, na.rm=T)

min(GADD_Descriptive_W1_S$ALCFLF, na.rm=T)
max(GADD_Descriptive_W1_S$ALCFLF, na.rm=T)

min(GADD_Descriptive_W2_D$ALCFLF, na.rm=T)
max(GADD_Descriptive_W2_D$ALCFLF, na.rm=T)

min(GADD_Descriptive_W2_S$ALCFLF, na.rm=T)
max(GADD_Descriptive_W2_S$ALCFLF, na.rm=T)

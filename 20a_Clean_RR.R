# HEADER ##############################################################
#     Program:    22_Clean_RR.R
#     Project:    GADMJCOG
#     Tasks:       
#                 A) Subset Data: only siblings within 10 years of age
# DATASETS # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#     Libraries:  GADMJCOG
#     Source:     GD190618.csv (original analyzed data)
#     Produced:   GD190618rr.csv (& GADD_MplusRR -- for models on subset)
# HISTORY # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#     Jarrod Ellingson    20-04-20
# DEPENDENTS # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
      source("Work/01_Functions.R", echo = F)
      source("Work/03_Clean.R")
# COMMENTS # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#     INSERT
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# Import data analyzed for original submission ####
GADD_Mplus_Original <- GADD_Mplus

# check sibling difference in age ####
GADD_SibDiff_Check_W1 <- GADD_Mplus_Original %>% 
  dplyr::filter(Wave == 1) %>%
  group_by(ID_Fam) %>%
  dplyr::mutate(minAge = min(DEMAGE, na.rm=T), 
                maxAge = max(DEMAGE, na.rm=T)) %>%
  ungroup() %>%
  dplyr::mutate(DEMAGEmin = DEMAGE-minAge,
                DEMAGEmax = maxAge-DEMAGE) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(DEMAGEabs = max(c(abs(DEMAGEmin), abs(DEMAGEmax)), na.rm=T),
                DEMAGEsd = round(sd(c(abs(DEMAGEmin), abs(DEMAGEmax)), na.rm=T),2)) %>%
  dplyr::ungroup() %>% 
  dplyr::group_by(ID_Fam) %>%
  dplyr::mutate(DEMAGEMinFam = min(DEMAGEabs, na.rm=T), 
                DEMAGEMaxFam = max(DEMAGEabs, na.rm=T), 
                DEMAGEsdMinF = min(DEMAGEsd, na.rm=T),
                DEMAGEsdMaxF = max(DEMAGEsd, na.rm=T)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(ID_Fam) %>%
  dplyr::mutate(nFam = n()) %>%
  dplyr::ungroup()

# Save Wave 1 IDs for singletons ####
GADD_Singletons_W1 <- GADD_SibDiff_Check_W1 %>% 
  dplyr::filter(nFam == 1) %>%
  dplyr::select(ID_MPL)
dim(GADD_Singletons_W1)

# Look at age differences among sibling pairs/sets ####
GADD_SibSets_W1 <- GADD_SibDiff_Check_W1  %>%
  dplyr::filter(nFam > 1)%>%
  dplyr::select(ID_Fam, nFam, Wave, ID_MPL, 
                DEMAGE, minAge:DEMAGEsdMaxF) 
# Save Wave 1 IDs for youngest sibling in each fam ####
GADD_SibDiff_Youngest <- GADD_SibSets_W1 %>%
  dplyr::filter(DEMAGEmin == 0 | is.na(DEMAGE)) 
GADD_SibDiff_Youngest_W1 <- GADD_SibDiff_Youngest %>%
  dplyr::select(ID_MPL)

# Look only at sibs who aren't the youngest in the fam  ####
GADD_SibDiff_Older_W1 <- GADD_SibSets_W1 %>%
  dplyr::filter(DEMAGEmin != 0)

mean(GADD_SibDiff_Older_W1$DEMAGEmin, na.rm=T)
sd(GADD_SibDiff_Older_W1$DEMAGEmin, na.rm=T)
table(GADD_SibDiff_Older_W1$DEMAGEmin>15) # 1 sib pair 15+ years apart
table(GADD_SibDiff_Older_W1$DEMAGEmin>10) # 10 sib pair 10+ years apart
table(GADD_SibDiff_Older_W1$DEMAGEmin>5) # 105 sib pair 5+ years apart
table(GADD_SibDiff_Older_W1$DEMAGEmin<=1) # 105 sib pair 5+ years apart
# outliers at 10+ years apart
hist(GADD_SibDiff_Older_W1$DEMAGEmin, na.rm=T, 
     main = "Full Sample\nAge Difference from Youngest Sibling", xlab = "Years", 
     probability = T, ylim = c(0,0.35), breaks=seq(0,17,1))
# all older sibs 10+ years apart are age 25+
table(GADD_SibDiff_Older_W1$DEMAGEmin>10, round(GADD_SibDiff_Older_W1$DEMAGE))
# Save Wave 1 IDs for older siblings under 25 and within 10 years of youngest sibling ####
GADD_SibDiff_SibSets_u25 <- GADD_SibDiff_Older_W1 %>%
  dplyr::filter(DEMAGE <= 25) 
GADD_SibDiff_SibSets_CloseAge <- GADD_SibDiff_SibSets_u25 %>%
  dplyr::filter(DEMAGEmin < 10) 
GADD_SibDiff_SibSets_CloseAge_W1 <- GADD_SibDiff_SibSets_CloseAge %>%  
  dplyr::select(ID_MPL)

table(GADD_SibDiff_Older_W1$DEMAGE > 25, useNA = "always") # n = 23 over age 25
table(GADD_SibDiff_SibSets_u25$DEMAGEmin >= 10, useNA = "always") # n = 1 more than 10 years older than youngest sib

# Among the sample included for re-analysis, a majority were within three years of age
table(GADD_SibDiff_SibSets_u25$DEMAGEmin<=1)[2]/sum(table(GADD_SibDiff_SibSets_u25$DEMAGEmin<=1))
table(GADD_SibDiff_SibSets_u25$DEMAGEmin<=3)[2]/sum(table(GADD_SibDiff_SibSets_u25$DEMAGEmin<=3))
median(GADD_SibDiff_SibSets_u25$DEMAGEmin, na.rm=T)

mean(GADD_SibDiff_SibSets_u25$DEMAGEmin, na.rm=T)


hist(GADD_SibDiff_SibSets_u25$DEMAGEmin, na.rm=T, 
     main = "Reanalysis Sample/nAge Difference from Youngest Sibling", xlab = "Years", 
     probability = T, ylim = c(0,0.35))

GADD_SibDiff_SibSets_Outliers <- GADD_SibDiff_Older_W1 %>%
  dplyr::filter(DEMAGE > 25 | DEMAGEmin >= 10) %>%
  dplyr::select(ID_MPL)

GADD_Singletons_CloseIDs_W1 <- list(GADD_Singletons_W1, 
                                    GADD_SibDiff_Youngest_W1, 
                                    GADD_SibDiff_SibSets_CloseAge_W1) %>%
  purrr::reduce(full_join)

GADD_OtherIDs_W2 <- GADD_Mplus_Original %>%
  dplyr::filter(Wave == 2) %>%
  dplyr::filter(ID_MPL %notin% GADD_Singletons_CloseIDs_W1$ID_MPL) %>% 
  dplyr::filter(ID_MPL %notin% GADD_SibDiff_SibSets_Outliers$ID_MPL) %>%
  dplyr::select(ID_MPL)

GADD_Singletons_CloseIDs_W1_OtherW2 <- list(GADD_Singletons_CloseIDs_W1, 
                                            GADD_OtherIDs_W2) %>%
  purrr::reduce(full_join)

GADD_MplusRR <- list(GADD_Mplus_Original, 
                     GADD_Singletons_CloseIDs_W1_OtherW2) %>%
  purrr::reduce(inner_join) %>% dplyr::select(-DEMPRJ, -DEMFAM)
dim(GADD_MplusRR)
dim(table(GADD_Mplus_Original$ID_Fam))
dim(table(GADD_MplusRR$ID_Fam))
dim(table(GADD_MplusRR$ID_MPL))

GADD_MplusRR_W1 <- GADD_MplusRR %>% filter(Wave == 1)
mean(GADD_MplusRR_W1$DEMAGE, na.rm=T)
max(GADD_MplusRR_W1$DEMAGE, na.rm=T)

table(table(GADD_MplusRR$Wave, GADD_MplusRR$ID_Fam)>1)

# write.table(x = GADD_Mplus_Original, 
#             file = "Work/Models/21_Phenotyp/GD190618.csv", sep=",",
#             na = ".", col.names = F, row.names = F)
# write.table(x = GADD_Mplus_Original, 
#             file = "Work/Models/22_MultiLev/GD190618.csv", sep=",",
#             na = ".", col.names = F, row.names = F)
# write.table(x = GADD_Mplus_Original, 
#             file = "Work/Models/23_RRMlvAlc/GD190618.csv", sep=",",
#             na = ".", col.names = F, row.names = F)
# write.table(x = GADD_Mplus_Original, 
#             file = "Work/Models/24_RRMlvPGS/GD190618.csv", sep=",",
#             na = ".", col.names = F, row.names = F)
# 
# 
# write.table(x = GADD_MplusRR,
#             file = "Work/Models/21_Phenotyp/CloseAge/GD190618rr.csv", sep=",",
#             na = ".", col.names = F, row.names = F)
# write.table(x = GADD_MplusRR,
#             file = "Work/Models/22_MultiLev/CloseAge/GD190618rr.csv", sep=",",
#             na = ".", col.names = F, row.names = F)
# write.table(x = GADD_MplusRR,
#             file = "Work/Models/23_RRMlvAlc/CloseAge/GD190618rr.csv", sep=",",
#             na = ".", col.names = F, row.names = F)

# Confirm Replication for github ####
write.table(x = GADD_MplusRR,
            file = "Work/Documentation/githubRR/21_Phenotyp/GD190618rr.csv", sep=",",
            na = ".", col.names = F, row.names = F)
write.table(x = GADD_MplusRR,
            file = "Work/Documentation/githubRR/22_MultiLev/GD190618rr.csv", sep=",",
            na = ".", col.names = F, row.names = F)
write.table(x = GADD_MplusRR,
            file = "Work/Documentation/githubRR/23_RRMlvAlc/GD190618rr.csv", sep=",",
            na = ".", col.names = F, row.names = F)
write.table(x = GADD_MplusRR,
            file = "Work/Documentation/githubRR/25_PostHoc/GD190618rr.csv", sep=",",
            na = ".", col.names = F, row.names = F)

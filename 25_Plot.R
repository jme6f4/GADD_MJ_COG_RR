# HEADER ##############################################################
#     Program:    25_Plot
#     Project:    GADMJCOG
#     Tasks:       
#                 Create Figure 1
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
#       Outcome = CVLT Commposite
#       Predictor = Cannabise Use (within family)
#     For Revision -- run models on subset of data
#       i)  only participants 25 & under at Wave 1
#       ii) only siblings within 10 years of age
#     Re-running analyses to confirm/match results published in MS
#     Corresponds to Figure 1
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# #   3) Sib-Pair  ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
library(tidyverse)
GADD_Mplus_SibAvgs <- GADD_MplusRR %>%
  dplyr::group_by(ID_Fam, Wave)  %>%
  dplyr::mutate(nFam = rank(ID_Fam, ties.method = "random")) %>%
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

GADD_Mplus_SibAvgs_W1 <- GADD_Mplus_SibAvgs %>% dplyr::filter(Wave == 1)
GADD_Mplus_SibAvgs_W2 <- GADD_Mplus_SibAvgs %>% dplyr::filter(Wave == 2)

GADD_Mplus_Greater_EVR <- GADD_Mplus_SibAvgs %>%
  dplyr::filter(CANEVRd > 0)
GADD_Mplus_Greater_FLF <- GADD_Mplus_SibAvgs %>%
  dplyr::filter(CANFLFd > 0)
GADD_Mplus_Greater_AC6 <- GADD_Mplus_SibAvgs %>%
  dplyr::filter(CANAC6d > 0)
GADD_Mplus_Greater_ONS <- GADD_Mplus_SibAvgs %>%
  dplyr::filter(CANONSd < 0)
GADD_Mplus_Greater_F6M <- GADD_Mplus_SibAvgs %>%
  dplyr::filter(CANF6Md > 0) 

GADD_Mplus_Greater_ONS_view <- GADD_Mplus_SibAvgs_W1 %>% dplyr::filter(Wave == 1) %>% dplyr::select(ID_Fam, Wave, CANONS, CANONSm, CANONSd)
GADD_Mplus_Greater_ONS_W1 <- GADD_Mplus_Greater_ONS %>% dplyr::filter(Wave == 1) 
GADD_Mplus_Greater_ONS_W2 <- GADD_Mplus_Greater_ONS %>% dplyr::filter(Wave == 2)
GADD_Mplus_Greater_AC6_W2 <- GADD_Mplus_Greater_AC6 %>% dplyr::filter(Wave == 2)
GADD_Mplus_Greater_F6M_W2 <- GADD_Mplus_Greater_F6M %>% dplyr::filter(Wave == 2)
GADD_Mplus_Greater_F6M_W2_view <- GADD_Mplus_SibAvgs_W2 %>% dplyr::filter(Wave == 2) %>% dplyr::select(ID_Fam, Wave, CANF6M, CANF6Mm, CANF6Md)
GADD_Mplus_Greater_F6Md_view <- GADD_Mplus_Greater_F6M %>% dplyr::filter(Wave == 2) %>% dplyr::select(ID_Fam, Wave, CANF6M, CANF6Mm, CANF6Md)

# there were sibling differences in the age of onset in 262 families (44%).  
# That is, 262 families contributed to the within-family effect estimates for age of onset of regular use. 
dim(table(GADD_Mplus_Greater_ONS_W1$ID_Fam))
dim(table(GADD_Mplus_Greater_ONS_W1$ID_Fam)) / dim(table(GADD_Mplus_SibAvgs_W1$ID_Fam))
# Among families in which siblings differed in age of onset, most (57%) began using regularly within one to two years of age of each other (M = 2.8, SD = 1.9). 
table(GADD_Mplus_Greater_ONS_W1$CANONSd*2)
table(GADD_Mplus_Greater_ONS_W1$CANONSd*2 >= -2 & GADD_Mplus_Greater_ONS_W1$CANONSd*2 <= -1)
mean(GADD_Mplus_Greater_ONS_W1$CANONSd*2, na.rm=T)
sd(GADD_Mplus_Greater_ONS_W1$CANONSd*2, na.rm=T)
# Further, at Wave 2, there were sibling differences in the past-six-month frequency of use in 258 families (54%). 
dim(table(GADD_Mplus_Greater_F6M_W2$ID_Fam))
dim(table(GADD_Mplus_Greater_F6M_W2$ID_Fam)) / dim(table(GADD_Mplus_SibAvgs_W2$ID_Fam))
# Among families with Wave 2 sibling differences in frequency of use, siblings differed in use by, on average, two days per week (M = 2.0, SD = 1.8). 
hist(round(GADD_Mplus_Greater_F6M_W2$CANF6Md*2/7, 1))
table(round(GADD_Mplus_Greater_F6M_W2$CANF6Md*2/7, 1))
table(round(GADD_Mplus_Greater_F6M_W2$CANF6Md*2/7 < 1, 1))
mean(GADD_Mplus_Greater_F6M_W2$CANF6Md*2/7, na.rm=T)
sd(GADD_Mplus_Greater_F6M_W2$CANF6Md*2/7, na.rm=T)


psych::scatterHist(GADD_Mplus_Greater_ONS_W1$CANONSd, GADD_Mplus_Greater_ONS_W1$CANONSm)
psych::scatterHist(GADD_Mplus_Greater_F6M_W2$CANF6Md, GADD_Mplus_Greater_F6M_W2$CANF6Mm)


# AC6
GADD_Mplus_Greater_AC6_W2_Graph <- GADD_Mplus_Greater_AC6_W2 %>%
  dplyr::mutate(CANAC6week = as.factor(ifelse(CANAC6d <= 4, "< 1 More Day/Week", 
                                       ifelse(CANAC6d <= 10, "1-2 More Days/Week", 
                                              "2+ Days/Week"))))

GADD_Mplus_Greater_AC6_W2_Graph_ForBar <- GADD_Mplus_Greater_AC6_W2_Graph %>% 
  dplyr::select(ID_MPL, CANAC6week, CANAC6d, CVLCOMd)  %>%
  dplyr::filter(!is.na(CVLCOMd))

tgc_CANAC62 <- summarySE(GADD_Mplus_Greater_AC6_W2_Graph_ForBar, 
                        measurevar="CVLCOMd", 
                        groupvars=c("CANAC6week"))

table(!is.na(GADD_Mplus_Greater_AC6_W2_Graph_ForBar$CVLCOMd), 
      !is.na(GADD_Mplus_Greater_AC6_W2_Graph_ForBar$CANAC6d))

scatterHist(GADD_Mplus_Greater_AC6_W2_Graph_ForBar$CVLCOMd, 
            GADD_Mplus_Greater_AC6_W2_Graph_ForBar$CANAC6d)

CVLCOM_CANAC62 <- ggplot(tgc_CANAC62, 
                         aes(x=CANAC6week, 
                             y=CVLCOMd)) + 
  geom_point(size=5) +
  geom_errorbar(aes(ymin=CVLCOMd-se, ymax=CVLCOMd+se),
                width=.50,                    # Width of the error bars
                position=position_dodge(.9)) + 
  labs(x = "Sibling Difference: Persistent, Weekly Use", 
       y = "Sibling Difference: Delayed Verbal Memory") +
  ylim(-0.55,0.15) + 
  theme_minimal() +
  theme_bw() +
  geom_hline(yintercept=0) +
  theme(
    title = element_text(face="bold", colour="#000000", size=16),
    plot.title = element_text(vjust=0, hjust=0.5),
    axis.title.x = element_text(face="bold", colour="#000000", size=15),
    axis.title.y = element_text(face="bold", colour="#000000", size=15),
    axis.text.x  = element_text(colour="#000000", size=12),
    axis.text.y  = element_text(colour="#000000", size=12),
    legend.title = element_text(size=14),
    legend.text = element_text(size=12)
    )
CVLCOM_CANAC62
ggsave(filename = "Figure1B_AggregateUse_200505.pdf", plot = CVLCOM_CANAC62, device = "pdf", 
       path = "Work/TablesFigures/", width = 8.5, height = 4.75)





# ONS
GADD_Mplus_Greater_ONS_W2_Graph <- GADD_Mplus_Greater_ONS_W2 %>%
  dplyr::mutate(CANONSyear = ifelse(CANONSd > -1, "<1 year earlier", 
                              ifelse(CANONSd >= -2, "1-2 years earlier", 
                              "2+ years earlier")))

GADD_Mplus_Greater_ONS_W2_Graph_ForBar <- GADD_Mplus_Greater_ONS_W2_Graph %>% 
  dplyr::select(ID_MPL, CANONSyear, CANONSd, CVLCOMd)  %>%
  dplyr::filter(!is.na(CVLCOMd))

table(!is.na(GADD_Mplus_Greater_ONS_W2_Graph_ForBar$CVLCOMd), 
      !is.na(GADD_Mplus_Greater_ONS_W2_Graph_ForBar$CANONSd))

scatterHist(GADD_Mplus_Greater_ONS_W2_Graph_ForBar$CVLCOMd, 
            GADD_Mplus_Greater_ONS_W2_Graph_ForBar$CANONSd)

tgc_CANONS2 <- summarySE(GADD_Mplus_Greater_ONS_W2_Graph_ForBar, 
                        measurevar="CVLCOMd", 
                        groupvars=c("CANONSyear"))

CVLCOM_CANONS2 <- ggplot(tgc_CANONS2, 
                         aes(x=CANONSyear, 
                             y=CVLCOMd)) + 
  geom_point(size=5) +
  geom_errorbar(aes(ymin=CVLCOMd-se, ymax=CVLCOMd+se),
                width=.50,                    # Width of the error bars
                position=position_dodge(.9)) + 
  labs(x = "Sibling Difference: Onset of Monthly Use", 
       y = "Sibling Difference: Delayed Verbal Memory") +
  ylim(-0.55,0.15) + 
  theme_minimal() +
  theme_bw() +
  geom_hline(yintercept=0) +
  theme(
    title = element_text(face="bold", colour="#000000", size=16),
    plot.title = element_text(vjust=0, hjust=0.5),
    axis.title.x = element_text(face="bold", colour="#000000", size=15),
    axis.title.y = element_text(face="bold", colour="#000000", size=15),
    axis.text.x  = element_text(colour="#000000", size=12),
    axis.text.y  = element_text(colour="#000000", size=12),
    legend.title = element_text(size=14),
    legend.text = element_text(size=12)
    )
CVLCOM_CANONS2
ggsave(filename = "Figure1A_Onset_200505.pdf", plot = CVLCOM_CANONS2, device = "pdf", 
       path = "Work/TablesFigures/", width = 8.5, height = 4.75)


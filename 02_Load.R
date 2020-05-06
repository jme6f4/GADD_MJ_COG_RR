# HEADER ###############################################################
#      Program:   02_Load.R
#      Project:   GADMJCOG
#      Tasks:      
                # A) Load Original Data
                # B) Load Plink-Generated PGS Data
# DATASETS  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
     # Libraries: GADMJCOG
     # Source:    
                # 2) CogNeuro:  GADD neuropsych measures.sav
                #               neurocog data GADD.sav
                # 3) SAM Supp:  GADD1_supplements wave 1.por
                #               SAMsupps2 GADD wave 2.por
                # 4) GWAS:      GADD_GWAS.xlsx
     # Derived:   
                # 2) CogNeuro:  GADD_Neuro
                #               GADD_CogNeuro
                # 3) SAM Supp:  GADD_Supp_W1
                #               GADD_Supp_W2
                # 4) GWAS:      GADD_GWAS
# HISTORY  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
     # Jarrod Ellingson    DATE
# DEPENDENTS # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
       source("Work/01_Functions.R", echo = T)
# COMMENTS # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#      INSERT
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
### A) Load Original Data #####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#   1) Not Followed #####
GADD_NotFollowed <- read_sav("Work/Datasets/Source/notfollowed.sav")
GADD_Drop <- GADD_NotFollowed$nidaid

#   2) CogNeuro #####
GADD_Neuro <- read_sav("Work/Datasets/Source/GADD neuropsych measures.sav") %>%
  dplyr::rename(NIDAID = nidaid)
GADD_Neuro <- as_tibble(sapply(GADD_Neuro, miss999))
dim(GADD_Neuro)

GADD_CogNeuro <- read_sav("Work/Datasets/Source/neurocog data GADD.sav") %>%
  dplyr::rename(NIDAID = nidaid)

#   3) SAM Supp. #####
GADD_Supp_W1 <- as_tibble(spss.get("Work/Datasets/Source/GADD1_supplements wave 1.por")) 
GADD_Supp_W2 <- as_tibble(spss.get("Work/Datasets/Source/SAMsupps2 GADD wave 2.por"))  


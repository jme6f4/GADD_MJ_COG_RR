# HEADER ###############################################################
#      Program:   01_Funtions.R
#      Project:   GADMJCOG
#      Tasks:      
                # A) Load Packages
                # B) Define Functions
# DATASETS  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
     # Libraries: NA
     # Source:    
     # Derived:   
# HISTORY  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
     # Jarrod Ellingson    DATE
# DEPENDENTS # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#      INSERT
# COMMENTS # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#      INSERT
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



Sys.setenv(TZ="America/Denver")
Sys.getenv("TZ")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#  A) Load Packages ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
wants <- c("haven", "Hmisc", "readxl", 
           "psych",  
           "tidyverse", 
           "MplusAutomation"
           )
# has   <- wants %in% rownames(installed.packages())
# if(any(!has)) install.packages(wants[!has])
library("haven")
library("Hmisc")
library("readxl")
library("psych")

library("tidyverse")

library("MplusAutomation")



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#   B) Definite Functions ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
trim <- function(x) {
  gsub("^\\s+|\\s+$", "", x)
  }

`%notin%` <- Negate(`%in%`)

est_round <- function(Est) {
  Est_Rd <- round2(Est, 2)
  Est_3 <- ifelse(test = substr(x = Est_Rd, 1, 1) == "." | substr(x = Est_Rd, 1, 1) == "-",
                  yes = 
                    ifelse(Est_Rd < 0 & nchar(Est_Rd) == 4, paste0("'",substr(Est_Rd, 1, nchar(Est_Rd)),"0"),
                           ifelse(Est_Rd < 0 & nchar(Est_Rd) == 5, paste0("'",substr(Est_Rd, 1, nchar(Est_Rd))),
                                  ifelse(Est_Rd >= 0 & nchar(Est_Rd) == 4, paste0("'",substr(Est_Rd, 1, nchar(Est_Rd))),
                                         ifelse(Est_Rd < 0 & nchar(Est_Rd) == 4, paste0("'",substr(Est_Rd, 1, nchar(Est_Rd))),
                                                ifelse(Est_Rd >= 0 & nchar(Est_Rd) == 3, paste0("'",substr(Est_Rd, 1, nchar(Est_Rd)),"0"),
                                                       ifelse(Est_Rd == .00, "'0.00",
                                                              NA)))))),
                  no = 
                    ifelse(Est_Rd < 0 & nchar(Est_Rd) == 6, paste0("'",substr(Est_Rd, 1, nchar(Est_Rd))),
                           ifelse(Est_Rd >= 0 & nchar(Est_Rd) == 5, paste0("'",substr(Est_Rd, 1, nchar(Est_Rd))),
                                  ifelse(Est_Rd < 0 & nchar(Est_Rd) == 5, paste0("'",substr(Est_Rd, 1, nchar(Est_Rd))),
                                         ifelse(Est_Rd >= 0 & nchar(Est_Rd) == 4, paste0("'",substr(Est_Rd, 1, nchar(Est_Rd))),
                                                ifelse(Est_Rd >= 0 & nchar(Est_Rd) == 3, paste0("'",substr(Est_Rd, 1, nchar(Est_Rd)),"0"),
                                                       ifelse(Est_Rd == 0.00, "'0.00",
                                                              NA)))))))
  }


#  x)  p-value asterisks
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
p_sig <- function(p_value) {
  ifelse(p_value < .001, "***",
         ifelse(p_value < .01, "**",
                ifelse(p_value < .05, "*",
                       ifelse(test = p_value < .10, yes = " .", no = ""))))
  }

#  xi)  display est, se, p
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
est_se_p_num <- function(Est, St_Er, p_value) {
  ifelse(p_value < .001, paste0("'",Est," (",St_Er, ")***"),
         ifelse(p_value < .01, paste0("'",Est," (",St_Er, ")**"),
                ifelse(p_value < .05, paste0("'",Est," (",St_Er, ")*"),
                       ifelse(p_value < .10, paste0("'",Est," (",St_Er, ")."),
                              paste0("'",Est," (",St_Er, ")")))))
}

est_p_se_num <- function(Est, p_value, St_Er) {
  Est_vec <- Est
  Pval_vec <- p_value
  StEr_vec <- St_Er
  
  EPS <- as_tibble(as.matrix(cbind(Est_vec, Pval_vec, StEr_vec)), 
                   ncol = length(Est_vec), nrow = length(Est_vec))
}

roundn = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg}

getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

miss999 <- function(x) {
   x <- ifelse(x == "999", NA, 
               ifelse(x == 999, NA, 
                      x))
}

## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
    library(plyr)

    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }

    # This does the summary. For each group's data frame, return a vector with
    # N, mean, and sd
    datac <- ddply(data, groupvars, .drop=.drop,
      .fun = function(xx, col) {
        c(N    = length2(xx[[col]], na.rm=na.rm),
          mean = mean   (xx[[col]], na.rm=na.rm),
          sd   = sd     (xx[[col]], na.rm=na.rm)
        )
      },
      measurevar
    )

    # Rename the "mean" column    
    datac <- rename(datac, c("mean" = measurevar))

    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult

    return(datac)
}



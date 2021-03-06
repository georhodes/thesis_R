#Full script

#Install and load appropriate packages
# install.packages("haven")
library("haven")
# install.packages("dplyr")
library("dplyr")
# install.packages("readr")
library(readr)
# install.packages("MASS") #negative binomrial glm.nb()
library("MASS")
# install.packages("pscl")
library("pscl")
library("gmodels")
library("foreign")

#set working directory
setwd("~/Documents/School/UW/Thesis/thesis_R")

#Loading data from 1999, 2000, 2001 in stata
NHSDA_1999 <- read_dta("~/Documents/School/UW/Thesis/thesis_R/DATA/NHSDA-1999-DS0001-data-stata.dta")

NHSDA_2000 <- read_dta("~/Documents/School/UW/Thesis/thesis_R/DATA/NHSDA-2000-DS0001-data-stata.dta")

NHSDA_2001 <- read_dta("~/Documents/School/UW/Thesis/thesis_R/DATA/NHSDA-2001-DS0001-data-stata.dta")

#loading data from 2009,2010,2011, 2015 in Stata
NSDUH_2009 <- read_dta("~/Documents/School/UW/Thesis/thesis_R/DATA/NSDUH-2009-DS0001-data-stata.dta")

NSDUH_2010 <- read_dta("~/Documents/School/UW/Thesis/thesis_R/DATA/NSDUH-2010-DS0001-data-stata.dta")

NSDUH_2011 <- read_dta("~/Documents/School/UW/Thesis/thesis_R/DATA/NSDUH-2011-DS0001-data-stata.dta")

#NHSDA_1999
#change all variable names to all lowercase for ease of calling.
names(NHSDA_1999) <- toupper(names(NHSDA_1999))

#subset to relevant variables. 
subset_NHSDA_1999 <- NHSDA_1999 %>%
  dplyr::select(TXEVER, TXYREVER,IRFAMIN3, IRPINC3, 
         IREDUC2, IRSEX, AGE2, HEALTH, IRMARIT, NRCH17_2, EMPSTATY, JBSTATR2, ALCEVER,
         ALCTRY, MJEVER, COCEVER, ALCDAYS, NODR30A, DR5DAY)

  #removed: TXADGREC, TXLTMAI2, TXLTALC, TXOUTC2, TRTNEED2, JOBSTAT2, LAMTALC, SNFDALC,\
#           SNFDDRNK,  SNFAALDY, SNFAALDV,
names(subset_NHSDA_1999) <- tolower(names(subset_NHSDA_1999))

#add year marker 1999 = 1
subset_NHSDA_1999 <- subset_NHSDA_1999 %>%
  mutate(year = 1)
  

#rename columns
# colnames(subset_NHSDA_1999) <- c("txever", "tx12months",
#                                  "famincome", "income", "education", 
#                                 "sex", "age", "health", "marital", "children", "employment", 
#                                 "jobstatus", "firstuse", "cocaineever", 
#                                 "feeldrivebuzzed", "friendsdrunkweekly", "alcdays",
#                                 "drinksperday", "bingedays")    
#NHSDA_2000
#change all variable names to all lowercase for ease of calling.
names(NHSDA_2000) <- toupper(names(NHSDA_2000))

#subset to relevant variables. 
subset_NHSDA_2000 <- NHSDA_2000 %>%
  dplyr::select(TXEVER, TXYREVER, IRFAMIN3, IRPINC3, 
         IREDUC2, IRSEX, AGE2, HEALTH, IRMARIT, NRCH17_2, EMPSTATY, JBSTATR2, ALCEVER,
         ALCTRY, MJEVER, COCEVER, ALCDAYS, NODR30A, DR5DAY)
names(subset_NHSDA_2000) <- tolower(names(subset_NHSDA_2000))
#add year marker 2000=2
subset_NHSDA_2000 <- subset_NHSDA_2000 %>%
  mutate(year = 2)


#NHSDA_2001
#change all variable names to all lowercase for ease of calling.
names(NHSDA_2001) <- toupper(names(NHSDA_2001))

#subset to relevant variables. 
subset_NHSDA_2001 <- NHSDA_2001 %>%
  dplyr::select(TXEVER, TXYREVER,IRFAMIN3, IRPINC3, 
         IREDUC2, IRSEX, AGE2, HEALTH, IRMARIT, NRCH17_2, EMPSTATY,JBSTATR2, ALCEVER,
         ALCTRY, MJEVER, COCEVER, ALCDAYS, NODR30A, DR5DAY)
names(subset_NHSDA_2001) <- tolower(names(subset_NHSDA_2001))
#add year marker 2001 = 3
subset_NHSDA_2001 <- subset_NHSDA_2001 %>%
  mutate(year = 3)


#2009
names(NSDUH_2009) <- toupper(names(NSDUH_2009))

subset_NSDUH_2009 <- NSDUH_2009 %>%
  dplyr::select(TXEVER, TXYREVER,IRFAMIN3, IRPINC3, 
         IREDUC2, IRSEX, AGE2, HEALTH, IRMARIT, NRCH17_2, EMPSTATY, JBSTATR2, ALCEVER,
         ALCTRY, MJEVER, COCEVER, ALCDAYS, NODR30A, DR5DAY)

names(subset_NSDUH_2009) <- tolower(names(subset_NSDUH_2009))

#add year marker 2009 = 4
subset_NSDUH_2009 <- subset_NSDUH_2009 %>%
  mutate(year = 4)

#2010
names(NSDUH_2010) <- toupper(names(NSDUH_2010))

subset_NSDUH_2010 <- NSDUH_2010 %>%
  dplyr::select(TXEVER, TXYREVER,IRFAMIN3, IRPINC3, 
         IREDUC2, IRSEX, AGE2, HEALTH, IRMARIT, NRCH17_2, EMPSTATY, JBSTATR2, ALCEVER,
         ALCTRY, MJEVER, COCEVER, ALCDAYS, NODR30A, DR5DAY)

names(subset_NSDUH_2010) <- tolower(names(subset_NSDUH_2010))

#add year marker 2010 = 5
subset_NSDUH_2010 <- subset_NSDUH_2010 %>%
  mutate(year = 5)

#2011
names(NSDUH_2011) <- toupper(names(NSDUH_2011))

subset_NSDUH_2011 <- NSDUH_2011 %>%
  dplyr::select(TXEVER, TXYREVER,IRFAMIN3, IRPINC3, 
         IREDUC2, IRSEX, AGE2, HEALTH, IRMARIT, NRCH17_2, EMPSTATY, JBSTATR2, ALCEVER,
         ALCTRY, MJEVER, COCEVER, ALCDAYS, NODR30A, DR5DAY)

names(subset_NSDUH_2011) <- tolower(names(subset_NSDUH_2011))

#add year marker 2011 = 6
subset_NSDUH_2011 <- subset_NSDUH_2011 %>%
  mutate(year = 6)

#stack 1999, 2000, and 2001 subsets into one data frame stack99_01
stack1 <- rbind(subset_NHSDA_1999, subset_NHSDA_2000)
stack99_01 <-rbind(stack1, subset_NHSDA_2001)

#stack 2009, 2010, 2011 into stack09_11
stack2 <- rbind(subset_NSDUH_2009, subset_NSDUH_2010)
stack09_11 <- rbind(stack2, subset_NSDUH_2011)

#stack all into stack_total
stack_total <- rbind(stack99_01, stack09_11)


#Compress education level
stack_total <- stack_total %>%
  mutate(edu4cat = (ifelse(ireduc2 == 11, 4,
                           ifelse(ireduc2 == 10 | ireduc2 == 9, 3,
                                  ifelse(ireduc2 == 8, 2,
                                         1)))))

stack_total$edu4cat <- factor(stack_total$edu4cat, labels = c("<HS", "HS", "Some C", "C Grad"))

#label edu level as factor
# data_clean$ireduc2 <- factor(data_clean$ireduc2, labels =
#                                c("<=5th", "6th", "7th", "8th", "9th", "10th", "11th", "12th", "freshman",
#                                  "soph/Jun", "senior+"))

#Filter for those over 25
data_clean <- stack_total %>%
  filter(age2 >= 13)

#filtering out days of binge drinking. Ignoring missing data. 
data_clean <-data_clean %>%
  filter(dr5day <= 30 | dr5day == 80 | dr5day == 91 | dr5day == 93)

#assign logically to 0
data_clean$dr5day[data_clean$dr5day == 80] <- 0  #log assign
data_clean$dr5day[data_clean$dr5day == 91] <- NA #never used alcohol
data_clean$dr5day[data_clean$dr5day == 93] <- 0  #did not use in past 30

#Mutate catagories from dr5day to binge2cat
data_clean <- data_clean %>%
  mutate(binge2cat = (ifelse(dr5day == 0, 0,
                             1)))

#mutate catagories from dr5day to binge5cat
data_clean <- data_clean %>%
  mutate(binge5cat = (ifelse(dr5day == 0, 0,
                             ifelse(dr5day == 1 | dr5day == 2, 1,
                                    ifelse(dr5day >= 3 & dr5day <= 5, 2,
                                           ifelse( dr5day >= 6 & dr5day <= 9, 3,
                                                   4))))))
data_clean$binge5cat <- factor(data_clean$binge5cat, labels = c(" 0", " 1-2", " 3-5", " 6-9", " 10+"))

#mutate bingenorm: dr4day <= 4.
data_clean <- data_clean %>%
  mutate(bingenorm = (ifelse(dr5day <= 4, 0,
                      1)))

#assign income midpoints to income_mid
data_clean <- data_clean %>%
  mutate(income_mid = (ifelse(irpinc3 == 1, .5,
                              ifelse(irpinc3 == 2, 1.5,
                                     ifelse(irpinc3 == 3, 2.5,
                                            ifelse(irpinc3 == 4, 3.5,
                                                   ifelse(irpinc3 ==5, 4.5,
                                                          ifelse(irpinc3 == 6, 5.75,
                                                                10))))))))
#Assign income to income_fac
data_clean <- data_clean %>%
  mutate(income_fac = (ifelse(irpinc3 == 1, 1,
                              ifelse(irpinc3 == 2, 2,
                                     ifelse(irpinc3 == 3, 3,
                                            ifelse(irpinc3 == 4, 4,
                                                   ifelse(irpinc3 ==5, 5,
                                                          ifelse(irpinc3 == 6, 6,
                                                                 7))))))))
#making income_fac into factors
data_clean$income_fac <-factor(data_clean$income_fac, labels =
                              c("0-10k", "10-20k", "20-30k", "30-40k", "40-50k", "50-75k", "75k+"))

#making income3cat
data_clean <- data_clean %>%
  mutate(income3cat = (ifelse(irpinc3 == 1 | irpinc3 == 2 | irpinc3 == 3 |irpinc3 == 4, 1,
                              ifelse(irpinc3 == 5 | irpinc3 == 6, 2,
                                     3))))
data_clean$income3cat <- factor(data_clean$income3cat, labels = c("0-40k", "40-75k", "75k+"))
                                
#assign family income midpoints to faminc_mid
data_clean <- data_clean %>%
  mutate(faminc_mid = (ifelse(irfamin3 == 1, .5,
                              ifelse(irfamin3 == 2, 1.5,
                                     ifelse(irfamin3 == 3, 2.5,
                                            ifelse(irfamin3 == 4, 3.5,
                                                   ifelse(irfamin3 ==5, 4.5,
                                                          ifelse(irfamin3 == 6, 5.75,
                                                                 10))))))))
#Assign income to income_fac
data_clean <- data_clean %>%
  mutate(faminc_fac = (ifelse(irfamin3 == 1, 1,
                              ifelse(irfamin3 == 2, 2,
                                     ifelse(irfamin3 == 3, 3,
                                            ifelse(irfamin3 == 4, 4,
                                                   ifelse(irfamin3 ==5, 5,
                                                          ifelse(irfamin3 == 6, 6,
                                                                 7))))))))
#making income_fac into factors
data_clean$faminc_fac <-factor(data_clean$faminc_fac, labels =
                                 c("0-10k", "10-20k", "20-30k", "20-40k", "40-50k", "50-75k", "75k+"))

# create data_treated with column treated
data_clean <- data_clean %>%
  mutate(treated = ifelse((txyrever == 2) | (txyrever == 4), 1,
                                   ifelse((txyrever == 1) | (txyrever == 3), 2,
                                          ifelse((txyrever == 81) | (txyrever == 91) | (txyrever ==99), 0,
                                                 NA))))

data_clean <- data_clean %>%
  mutate(treatedyn = ifelse(treated == 0, 0,
                            1))

#make treated factor with labels
data_clean$treated <- factor(data_clean$treated, labels = 
                               c("None", "Distant", "Recent"))

data_clean$treatedyn <- factor(data_clean$treatedyn, labels = 
                               c("No", "Yes"))

# #dummy variables for treated
# treatment_distant <- as.numeric(data_clean$txyrever == 2) | as.numeric(data_clean$txyrever == 4)
# #d.txyrever2 has received  treatment recent (in last 12 months)
# treatment_recent <- as.numeric(data_clean$txyrever == 1) | as.numeric(data_clean$txyrever == 3)
# #d.txyrever3 has not recieved treatment
# treatment_never <- as.numeric(data_clean$txyrever == 81) | as.numeric(data_clean$txyrever == 91) |
#   as.numeric(data_clean$txyrever == 99)
# #d.txyrever4 "missing"
# treatment_missing <- as.numeric(data_clean$txyrever == 85) |  as.numeric(data_clean$txyrever == 94) |
#   as.numeric(data_clean$txyrever == 97) | as.numeric(data_clean$txyrever == 98)


#Set up dummy for never used drugs or alcohol
#alc ever - Switch order to compare to no alcever
data_clean <- data_clean %>%
  mutate(everdrank = ifelse(alcever ==1, 2,
                            ifelse(alcever == 2, 1,
                                   NA)))
data_clean$everdrank <- factor(data_clean$everdrank, labels = 
                                 c("no", "yes"), exclude =NA)

#mjever
data_clean <- data_clean %>%
  mutate(evermj = ifelse(mjever ==1, 2,
                         ifelse(mjever == 2, 1,
                                NA)))
data_clean$evermj <- factor(data_clean$evermj, labels = 
                              c("no", "yes"))

#cocever
data_clean <- data_clean %>%
  mutate(evercoc = ifelse(cocever ==1, 2,
                          ifelse(cocever == 2, 1,
                                 NA)))
data_clean$evercoc <- factor(data_clean$evercoc, labels = 
                               c("no", "yes"))

#Age of first use alctry 1-70
data_clean$alctry[data_clean$alctry == 985] <- NA  
data_clean$alctry[data_clean$alctry == 991] <- NA  #Never used / everdrank = no
data_clean$alctry[data_clean$alctry == 994] <- NA  
data_clean$alctry[data_clean$alctry == 997] <- NA   
data_clean$alctry[data_clean$alctry == 998] <- NA 



#demographics IRSEX, AGE2, HEALTH, IRMARIT, NRCH17_2
#create variable sex from irsex
data_clean <- data_clean %>%
  mutate(sex = ifelse(irsex == 1, 1,
                      2))
#make factors, m is referent
data_clean$sex <- factor(data_clean$sex, labels = c(" m", " f"))

#number of children under 18 in household
data_clean <- data_clean %>%
  mutate(dependents = (ifelse(nrch17_2 == 0, 0,
                              ifelse(nrch17_2 == 1, 1,
                                     ifelse(nrch17_2 == 2, 2,
                                            ifelse(nrch17_2 ==3, 3,
                                                   NA))))))
#making children into factors
data_clean$dependents <-factor(data_clean$dependents, labels =
                          c(" 0", "1", "2", "3+"))

#marital status from # irmarit
data_clean <- data_clean %>%
  mutate(marital = (ifelse(irmarit == 1, 1,
                           ifelse(irmarit == 2, 2,
                                  ifelse(irmarit ==3, 3, 
                                         ifelse(irmarit == 4, 4,
                                                NA))))))
#making age into factors
data_clean$marital <-factor(data_clean$marital, labels =
                                 c(" married", " widowed", " divorced", " never"))

#age 
data_clean <- data_clean %>%
  mutate(age = (ifelse(age2 == 13, 13,
                              ifelse(age2 == 14, 14,
                                            ifelse(age2 == 15, 15,
                                                   ifelse(age2 == 16, 16,
                                                                 17))))))
#making age into factors
data_clean$age <-factor(data_clean$age, labels =
                                 c(" 26-29", " 30-34", " 35-49", " 50-64", " 65+"))



data_clean2 <- data_clean %>%
  mutate(insample = ifelse( is.na(dr5day) | is.na(binge2cat) | is.na(bingenorm) | is.na(treated) | 
                              is.na(alctry) | is.na(evermj) | is.na(evercoc) | is.na(dependents) , 0,
                            1))
data_clean <- data_clean2 %>%
  filter(insample == 1)

#Seperate Data_clean into era_a and era_b
data_clean <- data_clean %>%
  mutate(era = (ifelse( year <= 3, 1,
                        2)))

era_1 <- data_clean %>%
  filter(data_clean$era == 1)

era_2 <- data_clean %>%
  filter(data_clean$era == 2)

### Probability: binge vs. Income_mid
### Moderators: income_mid

#regressions
#USEE THIS ONE
lm_prob_mid <- lm(binge2cat ~ income_mid + treated + alctry + evermj + evercoc + 
                    edu4cat + age + sex + marital + dependents + income_mid*treated, 
                  data = data_clean, na.action = na.omit)
summary(lm_prob_mid)

lm_prob_mid2 <- lm(binge2cat ~ income_mid + treated + alctry + evermj + evercoc + 
                     edu4cat + age + sex + marital + dependents + income_mid*treated + 
                     edu4cat*treated, data = data_clean, na.action = na.omit)
summary(lm_prob_mid2)

lm_prob_fam_mid <- lm(binge2cat ~ faminc_mid + treated + alctry + evermj + evercoc + 
                        edu4cat + age + sex + marital + dependents + faminc_mid*treated,
                      data = data_clean, na.action = na.omit)
summary(lm_prob_fam_mid)

lm_prob_fam_mid2 <- lm(binge2cat ~ faminc_mid + treated + alctry + evermj + evercoc + 
                         edu4cat + age + sex + marital + dependents + faminc_mid*treated + 
                         edu4cat*treated, data = data_clean, na.action = na.omit)
summary(lm_prob_fam_mid2)

lm_prob_fac <- lm(binge2cat ~ income_fac + treated + alctry + evermj + evercoc + 
                    edu4cat + age + sex + marital + dependents + income_fac*treated,
                  data = data_clean, na.action = na.omit)
summary(lm_prob_fac)

lm_prob_fac2 <- lm(binge2cat ~ income_fac + treated + alctry + evermj + evercoc + 
                     edu4cat + age + sex + marital + dependents + income_fac*treated + 
                     edu4cat*treated, data = data_clean, na.action = na.omit)
summary(lm_prob_fac2)

lm_prob_edu <- lm(binge2cat ~ treated + income_mid + alctry + evermj + evercoc + 
                    edu4cat + age + sex + marital + dependents + 
                    edu4cat*treated, data = data_clean, na.action = na.omit)
summary(lm_prob_edu)

lm_prob_fac <- lm(binge2cat ~ income_fac + treated + alctry + evermj + evercoc +
                    edu4cat + age + sex + marital + dependents + income_fac*treated,
                  data = data_clean, na.action = na.omit)
summary(lm_prob_fac)

lm_prob_fac3 <- lm(binge2cat ~ income3cat + treated + alctry + evermj + evercoc +
                     edu4cat + age + sex + marital + dependents + income3cat*treated,
                   data = data_clean, na.action = na.omit)
summary(lm_prob_fac3)


lm_prob_fac2 <- lm(binge2cat ~ income_fac + treated + alctry + evermj + evercoc +
                     edu4cat + age + sex + marital + dependents + income_fac*treated +
                     edu4cat*treated, data = data_clean, na.action = na.omit)
summary(lm_prob_fac2)

lm_prob_edu2 <- lm(binge2cat ~ treated + alctry + evermj + evercoc + 
                     edu4cat + age + sex + marital + dependents + 
                     edu4cat*treated + income_mid*treated, data = data_clean, na.action = na.omit)
summary(lm_prob_edu2)

#comparing different models

lm_prob_mid2 <- lm(binge2cat ~ income_mid + treated + alctry + evermj + evercoc + 
                     edu4cat + age + sex + marital + dependents + income_mid*treated + 
                     edu4cat*treated, data = data_clean, na.action = na.omit)
summary(lm_prob_mid2)


#bingenorm
lm_bingenorm <- lm(bingenorm ~ income_mid + treated + alctry + evermj + evercoc +
                     edu4cat + age + sex + marital + dependents + income_mid*treated +
                     edu4cat*treated, data = data_clean, na.action = na.omit)
summary(lm_bingenorm)

#negative binomial
nb_trial <- glm.nb(dr5day ~ income_mid + treated + alctry + evermj + evercoc +
                     edu4cat + age + sex + marital + dependents + income_mid*treated +
                     edu4cat*treated, data = data_clean, na.action = na.omit)
summary(nb_trial)
odTest(nb_trial)

#Poisson
p_trial <- glm(dr5day ~ income_mid + treated + alctry + evermj + evercoc +
                 edu4cat + age + sex + marital + dependents + income_mid*treated +
                 edu4cat*treated, family = "poisson", data = data_clean, na.action = na.omit)
summary(p_trial)
install.packages("AER")
library(AER)
dispersiontest(p_trial)


p_income3cat <-glm(dr5day ~ income3cat + treated + alctry + evermj + evercoc +
                     edu4cat + age + sex + marital + dependents + income3cat*treated +
                     edu4cat*treated, family = "poisson", data = data_clean, na.action = na.omit)
summary(p_income3cat)
  
p_income_fac <-glm(dr5day ~ income_fac + treated + alctry + evermj + evercoc +
                     edu4cat + age + sex + marital + dependents + income_fac*treated +
                     edu4cat*treated, family = "poisson", data = data_clean, na.action = na.omit)
summary(p_income_fac)

#zero-inflated poisson regression PROBLEM
zip_trial <- zeroinfl(dr5day ~ income_mid + treated + alctry + evermj + evercoc +
  edu4cat + age + sex + marital + dependents + income_mid*treated +
  edu4cat*treated, data = data_clean, subset = NULL, na.action = na.omit,
  distribution = "poisson")
summary(zip_trial)

#Logistic regression
logit_trial <- glm(binge2cat ~ income_mid + treated + alctry + evermj + evercoc +
                     edu4cat + age + sex + marital + dependents + income_mid*treated +
                     edu4cat*treated, data = data_clean, family = "binomial", na.action = na.omit)
summary(logit_trial)

#Logistic regression with Normal Binging (0-4 per month)
logit_norm_trial <- glm(bingenorm ~ income_mid + treated + alctry + evermj + evercoc +
                     edu4cat + age + sex + marital + dependents + income_mid*treated +
                     edu4cat*treated, data = data_clean, family = "binomial", na.action = na.omit)
summary(logit_norm_trial)


CrossTable(data_clean2$insample)

nb_trial <- glm.nb(dr5day ~ income_mid + treated + alctry + evermj + evercoc +
                     edu4cat + age + sex + marital + dependents  + era + income_mid*treated +
                     edu4cat*treated, data = data_clean, na.action = na.omit)
summary(nb_trial)

nb_faminc <- glm.nb(dr5day ~ faminc_mid + treated + alctry + evermj + evercoc +
                                 edu4cat + age + sex + marital + dependents + era + faminc_mid*treated +
                                 edu4cat*treated, data = data_clean, na.action = na.omit)
summary(nb_faminc)

?predict.glm
table(data_clean$dr5day)

#Visualization Work below
#creating data fixed for visualizations
data_fixed <- data.frame(income_mid = mean(data_clean$income_mid), treated = 0:2, alctry = mean(data_clean$alctry),
                         evermj = 2, evercoc = 2, edu4cat = 3, age = 15, sex = 1, marital = 1, 
                         dependents = 0, era = 1 )
data_fixed_fac <- data_fixed

                         
#convert all to data_fixed factors. 
#make treated factor with labels later
data_fixed_fac$treated <- factor(data_fixed$treated, labels = 
                                                        c("None", "Distant", "Recent"))

data_fixed_fac$evermj <- factor(data_fixed$evermj, labels = 
                              c("yes"))  

data_fixed_fac$evercoc <- factor(data_fixed$evercoc, labels = 
                               c("yes"))

data_fixed_fac$edu4cat <- factor(data_fixed$edu4cat, labels = c("C Grad"))

data_fixed_fac$age <-factor(data_fixed$age, labels =
                          c(" 35-49"))

data_fixed_fac$sex <- factor(data_fixed$sex, labels = c(" m"))

data_fixed_fac$marital <-factor(data_fixed$marital, labels =
                              c(" married"))

data_fixed_fac$dependents <-factor(data_fixed$dependents, labels =
                                 c(" 0"))



#exporting data_clean to STATA
write.dta(data_clean, "~/Documents/School/UW/Thesis/thesis_R/DATA/data_clean.dta")

#Final Models!
lm_prob_fam_yn <- lm(binge2cat ~ faminc_mid + treatedyn + alctry + evermj + evercoc + 
                       edu4cat + age + sex + marital + dependents + era+ faminc_mid*treatedyn + 
                       edu4cat*treatedyn, data = data_clean, na.action = na.omit)
summary(lm_prob_fam_yn)

#negative binomial
nb_famincyn <- glm.nb(dr5day ~ faminc_mid + treatedyn + alctry + evermj + evercoc +
                        edu4cat + age + sex + marital + dependents + era + faminc_mid*treatedyn +
                        edu4cat*treatedyn, data = data_clean, na.action = na.omit)
summary(nb_famincyn)

# Creating VIZ
prob_diffs <- data.frame(HS = -.0133, SC = -.0527, C = -.0869, HSTx = -.0139,
                        SCTx = -.0491, CTx = -.034)
prob_diffs


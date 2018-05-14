#Visualizations

#Plotting Number of Drinks vs. Dollars
#Plotting Probability vs. Dollars (Non treated, treated recent, treated distant)

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
# install.packages("gmodels")
library("gmodels")
# install.packages("ggplot2")
library(ggplot2)
# install.packages("scales")
library(scales)
# install.packages("ggthemes")
library(ggthemes)
# install.packages("plyr")
library(plyr)
# install.packages("ggthemes")
library(ggthemes)
# install.packages("gmodels")
library(gmodels)  #modeling crosstables and mosaics

#negative binomial
nb_faminc <- glm.nb(dr5day ~ faminc_mid + treated + alctry + evermj + evercoc +
                      edu4cat + age + sex + marital + dependents + era + faminc_mid*treated +
                      edu4cat*treated, data = data_clean, na.action = na.omit)
summary(nb_faminc)
#creating data fixed for visualizations
data_fixed <- data.frame(faminc_mid = mean(data_clean$faminc_mid), treated = 0:2, alctry = mean(data_clean$alctry),
                         evermj = 2, evercoc = 2, edu4cat = 3, age = 15, sex = 1, marital = 1, 
                         dependents = 2, era = 1)
data_fixed_fac <- data_fixed


#convert all to data_fixed_fac factors matching model
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

data_fixed_fac

#Determine prediction for fixed characteristics and treated (none, distant, recent)
prediction <- predict.glm(nb_faminc, data_fixed_fac)
prediction

#determine probability_dnbinom for each prediction
prob_dnbinom <- 1 - (prediction/ (prediction + 1))
prob_dnbinom

#run dnbinom for each probability[1:3]
none_dnbinom <- dnbinom(0:10, 1, prob_dnbinom[1])
none_dnbinom

distant_dnbinom <- dnbinom(0:10, 1, prob_dnbinom[2])
distant_dnbinom

recent_dnbinom <- dnbinom(0:10, 1, prob_dnbinom[3])
recent_dnbinom

#create dataframe for graph for each none/distant/recent_dnbinom
none_data_dnbinom <- data.frame(dr5day = 0:10, dense = none_dnbinom, treated = 0)
none_data_dnbinom 
distant_data_dnbinom <- data.frame(dr5day = 0:10, dense = distant_dnbinom, treated = 1)
distant_data_dnbinom
recent_data_dnbinom <- data.frame(dr5day = 0:10, dense = recent_dnbinom, treated = 2)
recent_data_dnbinom

#stack dataframes into data_dnbinom
stack5 <- rbind(none_data_dnbinom, distant_data_dnbinom)
data_dnbinom <-rbind(stack5, recent_data_dnbinom)
data_dnbinom

#make treated as factors to match model nb_faminc
data_dnbinom$treated <- factor(data_dnbinom$treated, labels = 
                                 c("None", "Distant", "Recent"))

#plot it
plot_dnbinom <- ggplot(data_dnbinom, 
                       mapping = aes(x = dr5day, y = dense, color = treated))+
  geom_smooth()+
  labs(title = "Predicted Probability of College Graduates to Binge Drink", x = "Days Binge Drinkimg in Last Month",
       y = "Predicted Probability")+
  theme_economist() +
  theme(panel.grid.major.x = element_line())
plot_dnbinom    

#FACETED
#negative binomial model used for this: treatment is yes/no
nb_famincyn <- glm.nb(dr5day ~ faminc_mid + treatedyn + alctry + evermj + evercoc +
                        edu4cat + age + sex + marital + dependents + era + faminc_mid*treatedyn +
                        edu4cat*treatedyn, data = data_clean, na.action = na.omit)
summary(nb_famincyn)

#2.0 Compare lines of college grad and <HS, facet on treatedyn
#creating data fixed for visualizations treatedyn yes and no
data_fixed2.0no <- data.frame(faminc_mid = mean(data_clean$faminc_mid), treatedyn = 0, alctry = mean(data_clean$alctry),
                              evermj = 2, evercoc = 2, edu4cat = 1:2, age = 15, sex = 1, marital = 1, 
                              dependents = 0, era = 1)
data_fixed_fac2.0no <- data_fixed2.0no
data_fixed_fac2.0no


#convert all to data_fixed_fac factors matching model
data_fixed_fac2.0no$treatedyn <- factor(data_fixed_fac2.0no$treatedyn, labels = 
                                          c("No"))

data_fixed_fac2.0no$evermj <- factor(data_fixed_fac2.0no$evermj, labels = 
                                       c("yes"))  

data_fixed_fac2.0no$evercoc <- factor(data_fixed_fac2.0no$evercoc, labels = 
                                        c("yes"))

data_fixed_fac2.0no$edu4cat <- factor(data_fixed_fac2.0no$edu4cat, labels = c("<HS", "C Grad"))

data_fixed_fac2.0no$age <-factor(data_fixed_fac2.0no$age, labels =
                                   c(" 35-49"))

data_fixed_fac2.0no$sex <- factor(data_fixed_fac2.0no$sex, labels = c(" m"))

data_fixed_fac2.0no$marital <-factor(data_fixed_fac2.0no$marital, labels =
                                       c(" married"))

data_fixed_fac2.0no$dependents <-factor(data_fixed_fac2.0no$dependents, labels =
                                          c(" 0"))

data_fixed_fac2.0no

#Determine prediction for fixed characteristics and treated (none, distant, recent)
prediction2.0no <- predict.glm(nb_famincyn, data_fixed_fac2.0no)
prediction2.0no

#determine probability_dnbinom for each prediction
prob_dnbinom2.0no <- 1- (prediction2.0no/ (prediction2.0no + 1))
prob_dnbinom2.0no

#run dnbinom for each probability[1:3]
hsn_dnbinom <- dnbinom(0:10, 1, prob_dnbinom2.0no[1])
hsn_dnbinom

cgradn_dnbinom <- dnbinom(0:10, 1, prob_dnbinom2.0no[2])
cgradn_dnbinom


#create dataframe for graph for each none/distant/recent_dnbinom
hsn_data_dnbinom <- data.frame(dr5day = 0:10, dense = hsn_dnbinom, edu4cat = "<HS", treatedyn = "No Treatment")
hsn_data_dnbinom 
cgradn_data_dnbinom <- data.frame(dr5day = 0:10, dense = cgradn_dnbinom, edu4cat = "C Grad", treatedyn = "No Treatment")
cgradn_data_dnbinom


#stack dataframes into data_dnbinom

datan_dnbinom <-rbind(hsn_data_dnbinom, cgradn_data_dnbinom)
datan_dnbinom


#for those who attended treatment
data_fixed2.0yes <- data.frame(faminc_mid = mean(data_clean$faminc_mid), treatedyn = 1, alctry = mean(data_clean$alctry),
                               evermj = 2, evercoc = 2, edu4cat = 1:2, age = 15, sex = 1, marital = 1,
                               dependents = 0, era = 1)
data_fixed_fac2.0yes <- data_fixed2.0yes
data_fixed_fac2.0yes

#convert all to data_fixed_fac factors matching model
data_fixed_fac2.0yes$treatedyn <- factor(data_fixed_fac2.0yes$treatedyn, labels = 
                                           c("Yes"))

data_fixed_fac2.0yes$evermj <- factor(data_fixed_fac2.0yes$evermj, labels = 
                                        c("yes"))  

data_fixed_fac2.0yes$evercoc <- factor(data_fixed_fac2.0yes$evercoc, labels = 
                                         c("yes"))

data_fixed_fac2.0yes$edu4cat <- factor(data_fixed_fac2.0yes$edu4cat, labels = c("<HS", "C Grad"))

data_fixed_fac2.0yes$age <-factor(data_fixed_fac2.0yes$age, labels =
                                    c(" 35-49"))

data_fixed_fac2.0yes$sex <- factor(data_fixed_fac2.0yes$sex, labels = c(" m"))

data_fixed_fac2.0yes$marital <-factor(data_fixed_fac2.0yes$marital, labels =
                                        c(" married"))

data_fixed_fac2.0yes$dependents <-factor(data_fixed_fac2.0yes$dependents, labels =
                                           c(" 0"))
data_fixed_fac2.0yes

#Determine prediction for fixed characteristics and treated (none, distant, recent)
prediction2.0yes <- predict.glm(nb_famincyn, data_fixed_fac2.0yes)
prediction2.0yes

#determine probability_dnbinom for each prediction
prob_dnbinom2.0yes <- 1- (prediction2.0yes/ (prediction2.0yes + 1))
prob_dnbinom2.0yes

#run dnbinom for each probability[1:3]
hsy_dnbinom <- dnbinom(0:10, 1, prob_dnbinom2.0yes[1])
hsy_dnbinom

cgrady_dnbinom <- dnbinom(0:10, 1, prob_dnbinom2.0yes[2])
cgrady_dnbinom


#create dataframe for graph for each none/distant/recent_dnbinom
hsy_data_dnbinom <- data.frame(dr5day = 0:10, dense = hsy_dnbinom, edu4cat = "<HS", treatedyn = "Treatment")
hsy_data_dnbinom 
cgrady_data_dnbinom <- data.frame(dr5day = 0:10, dense = cgrady_dnbinom, edu4cat = "C Grad", treatedyn = "Treatment")
cgrady_data_dnbinom


#stack dataframes into data_dnbinom

datay_dnbinom <-rbind(hsy_data_dnbinom, cgrady_data_dnbinom)
datay_dnbinom

data2.0_dnbinom <-rbind (datan_dnbinom, datay_dnbinom)
data2.0_dnbinom

#plot it
plot2.0_dnbinom <- ggplot(data2.0_dnbinom, 
                          mapping = aes(x = dr5day, y = dense, color = edu4cat))+
  geom_smooth()+
  labs(title = "Predicted Probability of C Grad's and <HS to Binge Drink w/ and w/out Treatment", x = "Days Binge Drinkimg in Last Month",
       y = "Predicted Probability")+
  facet_grid(.~treatedyn)+
  theme_economist() +
  theme(panel.grid.major.x = element_line())+
  theme(plot.title = element_text(hjust = 0.5))
plot2.0_dnbinom   


#Plotting Means
fixed_era1 <- data.frame(faminc_mid = mean(data_clean$faminc_mid), treated = 0:2, alctry = mean(data_clean$alctry),
                         evermj = 2, evercoc = 2, edu4cat = 3, age = 15, sex = 1, marital = 1, 
                         dependents = 0, era = 1)
fixed_era1_fac <- fixed_era1

fixed_era2 <- data.frame(faminc_mid = mean(data_clean$faminc_mid), treated = 0:2, alctry = mean(data_clean$alctry),
                         evermj = 2, evercoc = 2, edu4cat = 3, age = 15, sex = 1, marital = 1, 
                         dependents = 0, era = 2)

fixed_era2_fac <- fixed_era2


#convert all to data_fixed_fac factors matching model
fixed_era1_fac$treated <- factor(data_fixed$treated, labels = 
                                   c("None", "Distant", "Recent"))

fixed_era1_fac$evermj <- factor(data_fixed$evermj, labels = 
                                  c("yes"))  

fixed_era1_fac$evercoc <- factor(data_fixed$evercoc, labels = 
                                   c("yes"))

fixed_era1_fac$edu4cat <- factor(data_fixed$edu4cat, labels = c("C Grad"))

fixed_era1_fac$age <-factor(data_fixed$age, labels =
                              c(" 35-49"))

fixed_era1_fac$sex <- factor(data_fixed$sex, labels = c(" m"))

fixed_era1_fac$marital <-factor(data_fixed$marital, labels =
                                  c(" married"))

fixed_era1_fac$dependents <-factor(data_fixed$dependents, labels =
                                     c(" 0"))

fixed_era1_fac

#Set factors for era2
#convert all to data_fixed_fac factors matching model
fixed_era2_fac$treated <- factor(data_fixed$treated, labels = 
                                   c("None", "Distant", "Recent"))

fixed_era2_fac$evermj <- factor(data_fixed$evermj, labels = 
                                  c("yes"))  

fixed_era2_fac$evercoc <- factor(data_fixed$evercoc, labels = 
                                   c("yes"))

fixed_era2_fac$edu4cat <- factor(data_fixed$edu4cat, labels = c("C Grad"))

fixed_era2_fac$age <-factor(data_fixed$age, labels =
                              c(" 35-49"))

fixed_era2_fac$sex <- factor(data_fixed$sex, labels = c(" m"))

fixed_era2_fac$marital <-factor(data_fixed$marital, labels =
                                  c(" married"))

fixed_era2_fac$dependents <-factor(data_fixed$dependents, labels =
                                     c(" 0"))

fixed_era2_fac

#Determine prediction for fixed characteristics and treated (none, distant, recent)
#Shows more Binge Drinking in Era 2
prediction_era1 <- predict.glm(nb_faminc, fixed_era1_fac)
prediction_era1

prediction_era2 <- predict.glm(nb_faminc, fixed_era2_fac)
prediction_era2


#Vis of prob diffs
prob_diffs_no <- data.frame(edu = c(1, 2, 3), Treated = "NO", prob = c(-.0133, -.0527, -.0869))
prob_diffs_no

prob_diffs_yes <- data.frame(edu = c(1,2,3), Treated = "YES", prob = c(-.0139, -.0491, -.034))
prob_diffs_yes

# "HS", "SC", "C"
prob_diffs <- rbind(prob_diffs_no, prob_diffs_yes)
prob_diffs

prob_diffs$edu <- factor(prob_diffs$edu, labels = c("High School", "Some College", "College"))
prob_diffs

#plot it
plot_prob_diffs <- ggplot(prob_diffs,
                          mapping = aes(x = edu, y = prob, fill = Treated))+
  geom_bar(stat= "identity", position = "dodge")+
  labs(title = "Difference in Probability to Binge Drink \nRelative to Non-Treated individuals w/out High School Diploma", x = "Level of Education",
       y = "Probability") +
  theme_economist() +
  theme(panel.grid.major.x = element_line())+
  theme(plot.title = element_text(hjust = 0.5))
  # theme(axis.text.x = element_text(angle = 80, hjust = .4, vjust = .45))
  # facet_grid(.~Treated)
plot_prob_diffs



#Vis of prob diffs w/ ERA's
#data: FINAL REGRESSIONs
## # Comparing ERA 5/9/18 ## Probability: Binge [0][1+] FAMILY Treatedyn ERA_1 
###### andERA_2
#ERA_1
prob_diffs_no1 <- data.frame(era = 1, edu = c(1, 2, 3), Treated = "NO", prob = c(-.0150, -.0555, -.1012))
prob_diffs_no1

prob_diffs_yes1 <- data.frame(era = 1, edu = c(1,2,3), Treated = "YES", prob = c(-.0140, -.0526, -.0555))
prob_diffs_yes1

# "HS", "SC", "C"
prob_diffs1 <- rbind(prob_diffs_no1, prob_diffs_yes1)
prob_diffs1

prob_diffs1$edu <- factor(prob_diffs1$edu, labels = c("High School", "Some College", "College"))
prob_diffs1


prob_diffs_no2 <- data.frame(era = 2, edu = c(1, 2, 3), Treated = "NO", prob = c(-.0102, -.0473, -.0711))
prob_diffs_no2

prob_diffs_yes2 <- data.frame(era = 2, edu = c(1,2,3), Treated = "YES", prob = c(-.0203, -.0460, -.0172))
prob_diffs_yes2

# "HS", "SC", "C"
prob_diffs2 <- rbind(prob_diffs_no2, prob_diffs_yes2)
prob_diffs2

prob_diffs2$edu <- factor(prob_diffs2$edu, labels = c("High School", "Some College", "College"))
prob_diffs2

prob_diffs_era <- rbind(prob_diffs1, prob_diffs2)
prob_diffs_era$era <- factor(prob_diffs_era$era, labels = c("99-01, Low Unemployment", "09-11, High Unemployment"))

prob_diffs_era


#plot it
plot_prob_diffs <- ggplot(prob_diffs_era,
                          mapping = aes(x = edu, y = prob, fill = Treated))+
  geom_bar(stat= "identity", position = "dodge")+
  labs(title = "Difference in Probability to Binge Drink \nRelative to Non-Treated individuals w/out High School Diploma", x = "Level of Education",
       y = "Probability") +
  theme_economist() +
  theme(panel.grid.major.x = element_line())+
  theme(plot.title = element_text(hjust = 0.5))+
  facet_grid(.~era)
# theme(axis.text.x = element_text(angle = 80, hjust = .4, vjust = .45))
# facet_grid(.~Treated)
plot_prob_diffs

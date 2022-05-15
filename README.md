# Empirical-Final-Paper

  #title: "Correlation and Logit Models
  #authors: "Cisse, Serigne, Joel Andrade Hertz Casseus
  #output: html_document
  # INTRODUCTIONS:
  #This working data set is called chs2020	and provides sample code to use when analyzing survey data. There are 8,803 observations and 152 variables in the dataset				*
  #The stratification (nesting) variable is strata and this survey data needs to be analyzed using a special procedure in SAS. We have imported the data set as SAS in R Studio. We want to study how individuals in New York City allocate their resources to produce Health, health insurance correlated to the roles of age, housing location and education.
  #We assume that Individuals invest in themselves through education, training and health. The goal is to increase earnings. But we cannot confirm that current conditions such outcomes.
  #Also, we believe that Health is a productive good which produces healthy days. This data mentions mood sentiments, which are not the focus of our research. We do predict a relevance of
  #health status based on age, levels of income, education, gender and neighborhoods.
  # We argue also that Education improves efficiency in production, thus educations matters to choices of Health coverage, Health insurance options, so on.
  # We argue also that there is a relationship between the labor-leisure trade-off with respect to allocation of employment to wage-earning activities.

library(haven)
chs2020_public <- read_sas("chs2020_public.sas7bdat", 
                           NULL)
View(chs2020_public)

library(tibble)
library(data.table)
library(dplyr)
library(standardize)
library(Matrix)
library(MatrixCorrelation)
library(stringi)
library(stringr)
library(ggplot2)
library(ggplot.multistats)
library(ggplotAssist)
library(tidyverse)
library(tidyr)
library(tidyselect)
library(shiny)
library(shinyAce)
library(shinyWidgets)
library(standardize)
library(StandardizeText)
library(randomForest)
library(randomForestExplainer)
library(stargazer)
attach(chs2020_public)
length(chs2020_public)
list(chs2020_public)
summary(chs2020_public$generalhealth)
HealthyGroup <- as.factor(as.numeric(generalhealth == 1 | generalhealth ==2 | generalhealth ==3))
table(HealthyGroup)
#Because we have look Thank you Kevin
str(chs2020_public)
#LINE 42 SERVES TO  DETERMINE THE CATEGORICAL, LOGICAL AND CONTINUOUS VARIABLES. WE HAVE VERY FEW CONTINOUS VARIABLES. 
#SO WE NEED TO BUILD NEW VARIABLES THAT WOULD FACILITATE THE OLS AND LOGIST REGRESSION TO DEMONSTRATE OUR HYPOTHESIS.
ExcellentHealth  <- as.factor(as.numeric(generalhealth ==1))
table(ExcellentHealth)
VeryGoodHealth <- as.factor(as.numeric(generalhealth ==2))
table(VeryGoodHealth)
GoodHealth <- as.factor(as.numeric(generalhealth ==3))
table(GoodHealth)
FairHealth <- as.factor(as.numeric(generalhealth ==4))
table(FairHealth)
PoorHealth <- as.factor(as.numeric(generalhealth ==5))
table(PoorHealth)
Demographic <- as.factor(as.numeric(chs2020_public$age18_64 >0))
summary(Demographic)
table(Demographic)
BlackPeoples <- as.factor(as.numeric(newrace ==2))
table(BlackPeoples)
HispanicPeoples <- as.factor(as.numeric(newrace ==3))
table(HispanicPeoples)
WhitePeoples <- as.factor(as.numeric(newrace ==1))
table(WhitePeoples)
AsianPeoples <- as.factor(as.numeric(newrace ==4))
table(AsianPeoples)
OtherPeoples <-as.factor(as.numeric(newrace ==5))
table(OtherPeoples)
educ_nohs <- as.factor(as.numeric(education==1))
table(educ_nohs)
educ_hs <- as.factor(as.numeric(education==2))
table(educ_hs)
educ_smcoll <- as.factor(as.numeric(education==3))
table(educ_smcoll)
educ_collegegraduate <- as.factor(as.numeric(education==4))
table(educ_collegegraduate)
EmployedforWage <- as.factor(as.numeric(employment20==1))
table(EmployedforWage)
SelfEmployed <- as.factor(as.numeric(employment20==2))
table(SelfEmployed)
UnemployedOver1yr <-as.factor(as.numeric(employment20==2))
table(UnemployedOver1yr)
Unemployedless1yr <-as.factor(as.numeric(employment20==4))
table(Unemployedless1yr)
Homemaker <-as.factor(as.numeric(employment20==5))
table(Homemaker)
Student <-as.factor(as.numeric(employment20==6))
table(Student)
Retired <-as.factor(as.numeric(employment20==7))
table(Retired)
UnableToWOrk <-as.factor(as.numeric(employment20==8))
table(UnableToWOrk)
summary(chs2020_public$nutrition46)
summary(chs2020_public$newrace)
summary(chs2020_public$generalhealth)
summary(chs2020_public$employment20)
summary(chs2020_public$bthcontrollastsex20_q1)
summary(chs2020_public$maritalstatus20)
summary(chs2020_public$weightall)
table(chs2020_public$weightall)
print(chs2020_public)
#Let's create a subset of healthy peoples and reported eXCELLENT ("1), Very Good ("2") and Good ('3)pick_use1 <- ((chs2020_public$generalhealth== 1) & (chs2020_public$generalhealth== 2) & (chs2020_public$generalhealth== 3) & (agegroup > 1))
pick_use1 <- ((chs2020_public$generalhealth== 1) | (chs2020_public$generalhealth== 2) | (chs2020_public$generalhealth== 3)) & (agegroup > 1)
summary(pick_use1)
dat_use1 <- subset(chs2020_public, pick_use1)
summary(pick_use1)
summary(dat_use1)
require(standardize)
# "Borough" demonstrates the five boroughs of new York City
# "dphonew06" explains the District Public Health Offices in New York City
OLS0 <- lm(insure5 ~ weightall + BlackPeoples + WhitePeoples + AsianPeoples +  HispanicPeoples  + OtherPeoples + ExcellentHealth + VeryGoodHealth + FairHealth + PoorHealth + educ_hs + educ_nohs +  educ_smcoll + educ_collegegraduate + EmployedforWage + SelfEmployed + Unemployedless1yr + UnemployedOver1yr + Homemaker + Student + Retired + UnableToWOrk)
# "Borough" demonstrates the five boroughs of new York City
# "dphonew06" explains the District Public Health Offices in New York City
OLS0 <- lm(insure5 ~ weightall + BlackPeoples + WhitePeoples + AsianPeoples +  HispanicPeoples  + OtherPeoples + ExcellentHealth + VeryGoodHealth + FairHealth + PoorHealth + educ_hs + educ_nohs +  educ_smcoll + educ_collegegraduate + EmployedforWage + SelfEmployed + Unemployedless1yr + UnemployedOver1yr + Homemaker + Student + Retired + UnableToWOrk)
summary(OLS0)
stargazer(OLS0, type = "text", title = "Relationshionship between Health Insurance, Nutrition and BMI" )
exp(OLS0$coefficients)
plot(coef(OLS0))
par(mfrow=c(2,2))
plot(OLS0,col="blue",pch=16,cex=1,lwd=1,lty=2)
#Standard Agency area-based poverty measure, based on % of population in respondent's zip code living below 100% FPL per American Community Survey 2014-2018, with imputation of missing cases.
#Neighborhood poverty; percent of zip code population living below 100% FPL per American Community Survey, 2014-2018
summary(chs2020_public$imputed_neighpovgroup4_1519)
table(chs2020_public$imputed_neighpovgroup4_1519)
summary(imputed_neighpovgroup4_1519)


#Now let's look at the impact poverty line based on Zipcodes across the City:
OLS1 <- lm(imputed_neighpovgroup4_1519 ~ weightall +  insure5 +   BlackPeoples + WhitePeoples + AsianPeoples +  HispanicPeoples +OtherPeoples + ExcellentHealth + VeryGoodHealth + FairHealth + PoorHealth + educ_hs + educ_nohs +  educ_smcoll + educ_collegegraduate + EmployedforWage + SelfEmployed + Unemployedless1yr + UnemployedOver1yr + Homemaker + Student + Retired + UnableToWOrk)
summary(OLS1)
stargazer(OLS1, type = "text")
exp(OLS1$coefficients)
plot(coef(OLS1))
par(mfrow=c(2,2))
plot(OLS1,col="red",pch=16,cex=1,lwd=1,lty=2)
OLS2 <- lm(nutrition1 ~weightall + insure5 + BlackPeoples + WhitePeoples + AsianPeoples +  HispanicPeoples +  HispanicPeoples  + OtherPeoples + ExcellentHealth + VeryGoodHealth + FairHealth + PoorHealth + educ_hs + educ_nohs +  educ_smcoll + educ_collegegraduate + EmployedforWage + SelfEmployed + Unemployedless1yr + UnemployedOver1yr + Homemaker + Student + Retired + UnableToWOrk)
summary(OLS2)
stargazer(OLS2, type = "text")
exp(OLS2$coefficients)
plot(coef(OLS2))
par(mfrow=c(2,2))
plot(OLS2,col="brown",pch=16,cex=1,lwd=1,lty=2)
summary(chs2020_public$imputed_povertygroup)
OLS3 <- lm(imputed_povertygroup ~ weightall + insure5 + BlackPeoples + WhitePeoples + AsianPeoples +  HispanicPeoples  + OtherPeoples + ExcellentHealth + VeryGoodHealth + FairHealth + PoorHealth + educ_hs + educ_nohs +  educ_smcoll + educ_collegegraduate + EmployedforWage + SelfEmployed + Unemployedless1yr + UnemployedOver1yr + Homemaker + Student + Retired + UnableToWOrk)
summary(OLS3)
stargazer(OLS3, type = "text")
exp(OLS3$coefficients)
plot(coef(OLS3))
par(mfrow=c(2,2))
plot(OLS3,col="Green",pch=16,cex=1,lwd=1,lty=2)
summary(chs2020_public$imputed_povertygroup)
WealthyGroup <-as.factor(as.numeric(imputed_povertygroup==5))
BlackPeoples <- as.factor(as.numeric(newrace ==2))
LOGIT <- glm(WealthyGroup ~ weightall + insure5 + BlackPeoples + WhitePeoples + HispanicPeoples +
               ExcellentHealth  + VeryGoodHealth + FairHealth + PoorHealth
             +  educ_smcoll + educ_collegegraduate + EmployedforWage + SelfEmployed + Student + Retired, family=binomial)
summary(LOGIT)
LOGIT2 <- glm(HealthyGroup ~ weightall + insure5 + BlackPeoples + WhitePeoples + HispanicPeoples +
                ExcellentHealth  + VeryGoodHealth + FairHealth + PoorHealth
              +  educ_smcoll + educ_collegegraduate + EmployedforWage + SelfEmployed + Student + Retired, family=binomial)
LOGIT2 <- glm(HealthyGroup ~ weightall + insure5 + BlackPeoples + WhitePeoples + HispanicPeoples + PoorHealth
              +  educ_smcoll + educ_collegegraduate + EmployedforWage + SelfEmployed + Student + Retired, family=binomial)
summary(LOGIT2)
stargazer(LOGIT2, type = "text")
#We set Health Insurance coverage as Independent variable. Level of poverty by area is the dependent variable
to_be_predicted<- data.frame(WealthyGroup,    data = pick_use1)
#We set Health Insurance coverage as Independent variable. Level of poverty by area is the dependent variable
to_be_predicted<- data.frame(imputed_povertygroup==5,    data = pick_use1)
#We set Health Insurance coverage as Independent variable. Level of poverty by area is the dependent variable
to_be_predicted<- data.frame(HealthyGroup,    data = pick_use1)
#We set Health Insurance coverage as Independent variable. Level of poverty by area is the dependent variable
to_be_predicted<- data.frame(insure5=="1",    data = pick_use1)
#We set Health Insurance coverage as Independent variable. Level of poverty by area is the dependent variable
to_be_predicted<- data.frame(insure5=="1",    data = pick_use1)
to_be_predicted$yhat<-predict(LOGIT, to_be_predicted, type="response")
summary(to_be_predicted$yhat)
to_be_predicted$yhat<-predict(LOGIT2, to_be_predicted, type="response")
summary(to_be_predicted$yhat)
library(haven)

View(chs2020_public)
str(chs2020_public$insure5)
library(haven)

#As of December 08 2021
OLS0 <- lm(insure5 ~ weightall + BlackPeoples + WhitePeoples + AsianPeoples +  HispanicPeoples  + OtherPeoples + ExcellentHealth + VeryGoodHealth + FairHealth + PoorHealth + educ_hs + educ_nohs +  educ_smcoll + educ_collegegraduate + EmployedforWage + SelfEmployed + Unemployedless1yr + UnemployedOver1yr + Homemaker + Student + Retired + UnableToWOrk)
summary(OLS0)
stargazer(OLS0, type = "text", title = "Relationshionship between Health Insurance, Nutrition and BMI" )
exp(OLS0$coefficients)

HealthyGroup <- as.factor(as.numeric(generalhealth == 1 | generalhealth ==2 | generalhealth ==3))
LOGIT2 <- glm(HealthyGroup ~ weightall + insure5 + BlackPeoples + WhitePeoples + HispanicPeoples + PoorHealth
              +  educ_smcoll + educ_collegegraduate + EmployedforWage + SelfEmployed + Student + Retired, family=binomial)
summary(LOGIT2)
plot(LOGIT2)






summary(chs2020_public$weightall)

str(chs2020_public)
TESTDEC <- ggplot(chs2020_public, aes(x=insure5, y = age18_64))+ geom_bar()
TESTDEC
TESTDEC <- ggplot(chs2020_public, aes(x=BlackPeoples, y = age18_64))+ geom_bar()
plot(TESTDEC)
TESTDEC <- ggplot(chs2020_public, aes(x=BlackPeoples, y = age18_64))+ geom_bar(stat = "identity")
TESTDEC
TESTDEC <- ggplot(chs2020_public, aes(x=weightall, y = insure5))+ geom_bar(stat = "identity")+ coord_flip()
TESTDEC

max(chs2020_public$weightall)
min(chs2020_public$weightall)

TESTDEC <- ggplot(chs2020_public, aes(x=age18_64, y = insure5))+ geom_bar(stat = "identity")+ coord_flip()+xlim(18,64)
TESTDEC
TESTDEC <- ggplot(chs2020_public, aes(x=age18_64, y = insure5))+ geom_bar(stat = "identity")+xlim(18,64)
TESTDEC
TESTDEC <- ggplot(chs2020_public, aes(x=age18_64, y = insure5))+ geom_bar(stat = "identity")+xlim(18,64)
plot(TESTDEC)
TESTDEC <- ggplot(chs2020_public, aes(x=age18_64, y = insure5))+ geom_bar(stat = "identity")+coord_flip()
TESTDEC
TESTDEC1 <- ggplot(chs2020_public, aes(x= insure5))+ geom_bar()
TESTDEC1
TESTDEC1X <- ggplot(chs2020_public, aes(x= generalhealth))+ geom_bar()
plot(TESTDEC1X)
chs2020_public$KEVIN <- factor(chs2020_public$generalhealth, levels = c(1,2,3,4,5),
                               labels =c("ExcellentHealth", "VeryGoodHealth", "GoodHealth", "FairHealth ", "PoorHealth"))
TESTDEC2 <- ggplot(chs2020_public, aes(x= KEVIN))+ geom_bar()+
  plot(TESTDEC2)
TESTDEC2 <- ggplot(chs2020_public, aes(x= KEVIN))+ geom_bar()
plot(TESTDEC2)
chs2020_public$HEALTH <- factor(chs2020_public$generalhealth, levels = c(1,2,3,4,5),
                                labels =c("ExcellentHealth", "VeryGoodHealth", "GoodHealth", "FairHealth ", "PoorHealth"))
TESTDEC2 <- ggplot(chs2020_public, aes(x= HEALTH))+ geom_bar()
T <-TESTDEC+coord_flip()
TESTDEC22 <- ggplot(chs2020_public, aes(x= HEALTH))+ geom_bar()+coord_flip()
plot(TESTDEC22)
TESTDEC3 <- ggplot(chs2020_public, aes(x= HEALTH))+ geom_bar()+coord_flip()
plot(TESTDEC3)
TESTDEC2 <- ggplot(chs2020_public, aes(x= HEALTH))+ geom_bar()
plot(TESTDEC2)
TESTDEC3 <- ggplot(chs2020_public, aes(x= HEALTH))+ geom_bar()+coord_flip()+ scale_fill_hue(c=34)+ ggtitle("HealthCommunity")
plot(TESTDEC3)
plot(TESTDEC4)
TESTDEC4 <- ggplot(chs2020_public, aes(x= HEALTH))+ geom_bar()+ scale_fill_hue(c=34)+ ggtitle("HealthCommunity")
plot(TESTDEC4)
TESTDEC4 <- ggplot(chs2020_public, aes(x= HEALTH))+ geom_bar()+ scale_fill_hue(c=40)+ ggtitle("HealthCommunity")
plot(TESTDEC4)
TESTDEC4 <- ggplot(chs2020_public, aes(x= HEALTH,fill=HEALTH))+ geom_bar()+ scale_fill_hue(c=40)+ ggtitle("HealthCommunity")
plot(TESTDEC4)


chs2020_public$RACE <- factor(chs2020_public$newrace, levels = c(1,2,3,4,5),
                              labels =c("BlackPeoples", "HispanicPeoples", "WhitePeoples", "AsianPeoples ", "OtherPeoples"))


TESTRACE <-ggplot(chs2020_public, aes(x= RACE))+ geom_bar()+coord_flip()
plot(TESTRACE)
TESTRACE1 <- ggplot(chs2020_public, aes(x= RACE,fill=RACE))+ geom_bar()+coord_flip()+ scale_fill_hue(c=40)+ ggtitle("RACIALCommunity")
plot(TESTRACE1)
TESTRACE1A <- ggplot(chs2020_public, aes(x= RACE,fill=RACE))+ geom_bar()+ scale_fill_hue(c=40)+ ggtitle("RACIALCommunity")
plot(TESTRACE1A)



chs2020_public$EDUCATED <- factor(chs2020_public$education, levels = c(1,2,3,4),
                                  labels =c("educ_nohs", "educ_hs", "educ_smcoll", "educ_collegegraduate"))


TESTEDUCATED <-ggplot(chs2020_public, aes(x= EDUCATED))+ geom_bar()+coord_flip()
plot(TESTEDUCATED)
TESTEDUCATED1 <- ggplot(chs2020_public, aes(x= EDUCATED,fill=EDUCATED))+ geom_bar()+coord_flip()+ scale_fill_hue(c=40)+ ggtitle("EDUCATEDCommunity")
plot(TESTEDUCATED1)
TESTEDUCATED1A <- ggplot(chs2020_public, aes(x= EDUCATED,fill=EDUCATED))+ geom_bar()+ scale_fill_hue(c=40)+ ggtitle("EDUCATED Community")
plot(TESTEDUCATED1A)




chs2020_public$EMPLOYMENT <- factor(chs2020_public$employment19, levels = c(1,2,3,4,5,6,7,8),
                                    labels =c("EmployedforWage", "SelfEmployed", "UnemployedOver1yr", "Unemployedless1yr", "Homemaker", "Student", "Retired", "UnableToWOrk"))


TESTEMPLOYMENT <-ggplot(chs2020_public, aes(x= EMPLOYMENT))+ geom_bar()+coord_flip()
plot(TESTEMPLOYMENT)
TESTEMPLOYMENT1 <- ggplot(chs2020_public, aes(x= EMPLOYMENT,fill=EMPLOYMENT))+ geom_bar()+coord_flip()+ scale_fill_hue(c=40)+ ggtitle("EMPLOYMENT IN THE Community")
plot(TESTEMPLOYMENT1)
TESTEMPLOYMENT1A <- ggplot(chs2020_public, aes(x= EMPLOYMENT,fill=EMPLOYMENT))+ geom_bar()+ scale_fill_hue(c=40)+ ggtitle("EMPLOYMENT IN THE Community")
plot(TESTEMPLOYMENT1A)



#LET'S REMEMBER THAT 
chs2020_public$HEALTH <- factor(chs2020_public$generalhealth, levels = c(1,2,3,4,5),
                                labels =c("ExcellentHealth", "VeryGoodHealth", "GoodHealth", "FairHealth ", "PoorHealth"))
#lOOKING AT THE RELATIONSHIP BETWEEN POPULATION IN RELATION WITH BMI AND HEALTH STATUS.
# Despite the presence of 152 variables, we count only 02 continous variable wt20_dual_q1 and wt20_dual, both serving to evaluate to stree the impact / weight of CHS2019 ON THE PREVIOUS STUDIES.

TESTDEC5 <- ggplot(chs2020_public, aes(age18_64, wt20_dual, colour=HEALTH))+ geom_boxplot()
TESTDEC5+ labs(colour = "HEALTH")
TESTDEC5 <- ggplot(chs2020_public, aes(cid, wt20_dual, colour=HEALTH))+ geom_point()
TESTDEC5+ labs(colour = "HEALTH")
TESTDEC5 <- ggplot(chs2020_public, aes(age21up, wt20_dual, colour=HEALTH))+ geom_area()
TESTDEC5 + labs(colour = "HEALTH")


TESTDEC6 <- ggplot(chs2020_public, aes(age18_64, wt20_dual, colour=EMPLOYMENT))+ geom_point()
TESTDEC6+ labs(colour = "EMPLOYMENT")
TESTDEC6 <- ggplot(chs2020_public, aes(cid, wt20_dual, colour=EMPLOYMENT))+ geom_point()
TESTDEC6+ labs(colour = "EMPLOYMENT")
TESTDEC6 <- ggplot(chs2020_public, aes(age21up, wt20_dual, colour=HEALTH))+ geom_area()
TESTDEC6 + labs(colour = "HEALTH")


TESTDEC5 <- ggplot(chs2020_public, aes(age18_64, wt20_dual, colour=RACE))+ geom_boxplot()
TESTDEC5+ labs(colour = "RACE")
TESTDEC5 <- ggplot(chs2020_public, aes(cid, wt20_dual, colour= RACE))+ geom_point()
TESTDEC5+ labs(colour = "RACE")
TESTDEC5 <- ggplot(chs2020_public, aes(age21up, wt20_dual, colour=HEALTH))+ geom_area()
TESTDEC5 + labs(colour = "RACE")




TESTDEC <- ggplot(chs2020_public, aes(x = nutrition46, y = insure5))
TESTDEC <- ggplot(chs2020_public, aes(x= weightall, y = insure5))+ geom_bar()+ coord_flip()
TESTDEC
TESTDEC <- ggplot(chs2020_public, aes(x= weightall, y = newrace))+ geom_bar()
T <-TESTDEC+coord_flip()
T
TESTDEC <- ggplot(chs2020_public, aes(x= BlackPeoples, y = HealthyGroup))+ geom_boxplot()
plot(TESTDEC)
TESTDEC <- ggplot(chs2020_public, aes(x= EDUCATED, y = HealthyGroup))+ geom_area()
plot(TESTDEC)
TESTDEC <- ggplot(chs2020_public, aes(x= mood1, y = WhitePeoples))+ geom_boxplot()
plot(TESTDEC)

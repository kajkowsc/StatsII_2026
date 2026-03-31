rm(list=ls())
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

library(Zelig)
lapply(c("ggplot2", "Zelig", "versions"),  pkgTest)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#------------------------------------------------------------
# DATA MANAGEMENT
#------------------------------------------------------------
our.data <- read.csv("/Users/carolinekajkowski/Desktop/Trinity/StatsII_2026/replication/my_answers/original_files/replication-data.csv")
set.seed(47408) 

# Subset: exclude ambiguous & non-Evangelicals
evan.cath <- subset(our.data, mainline != 1 & ambiguous == 0)
#------------
# Add the interaction terms in the RPS model to the dataset - this helps Zelig out
evan.cath$interaction1 = evan.cath$partyid*evan.cath$secular.knowledge
evan.cath$interaction2 = evan.cath$church*evan.cath$bible.collapsed
evan.cath$interaction3 = evan.cath$church*evan.cath$secular.knowledge
evan.cath$interaction4 = evan.cath$church*evan.cath$rps.ssm

#------------
# Run the RPS model
mylogit = Zelig::zelig(data=evan.cath, ssm ~ age + female + black + education.collapsed 
                + income.collapsed + ideology.collapsed
                + romancatholic + partyid + secular.knowledge
                + interaction1 + church + bible.collapsed + interaction2
                + interaction3 + rps.ssm + interaction4, model = "logit")
summary(mylogit)

x.out = setx(mylogit) 
z.out = sim(mylogit, x = x.out) 

#------------------------------------------------------------
# FIGURE 1
#------------------------------------------------------------
# Designate what the control variables should be set to for the simulations 
# There will be different central tendencies depending on the variable
age.1 = mean(evan.cath$age[evan.cath$tradition=="Evangelical Protestant"],
             na.rm=T)
female.1 = median(evan.cath$female[evan.cath$tradition=="Evangelical Protestant"], 
                  na.rm=T)
black.1 = median(evan.cath$black[evan.cath$tradition=="Evangelical Protestant"], 
                 na.rm=T)
education.1 = mean(evan.cath$education.collapsed[evan.cath$tradition=="Evangelical Protestant"], 
                   na.rm=T)
income.1 = mean(evan.cath$income.collapsed[evan.cath$tradition=="Evangelical Protestant"], 
                na.rm=T)
ideology.1 = mean(evan.cath$ideology.collapsed[evan.cath$tradition=="Evangelical Protestant"], 
                  na.rm=T)
pid.1 = mean(evan.cath$partyid[evan.cath$tradition=="Evangelical Protestant"], 
             na.rm=T)
secular.knowledge.1 = mean(evan.cath$secular.knowledge[evan.cath$tradition=="Evangelical Protestant"], 
                           na.rm=T)
bible.1 = mean(evan.cath$bible.collapsed[evan.cath$tradition=="Evangelical Protestant"], 
               na.rm=T)
#-------------------------------------------------------------------------------
# EFFECT OF RPS FOR PEOPLE THAT 'NEVER' ATTEND CHURCH (CHURCH = 0) 
#------------
# Church = 0; RPS = 0
x.low = setx(z.out, 
             age = age.1,
             female = female.1, 
             black = black.1,
             education.collapsed = education.1,
             income.collapsed = income.1,
             ideology.collapsed = ideology.1,
             romancatholic = 0,
             partyid = pid.1,
             secular.knowledge = secular.knowledge.1,
             interaction1 = pid.1*secular.knowledge.1,
             church = 0, 
             bible.collapsed = bible.1,
             interaction2 = 0*bible.1,
             interaction3 = 0*secular.knowledge.1,
             rps.ssm = 0,
             interaction4 = 0*0)
#------------
# Church = 0, RPS = 1 
x.high = setx(z.out, 
              age = age.1,
              female = female.1, 
              black = black.1,
              education.collapsed = education.1,
              income.collapsed = income.1,
              ideology.collapsed = ideology.1,
              romancatholic = 0,
              partyid = pid.1,
              secular.knowledge = secular.knowledge.1,
              interaction1 = pid.1*secular.knowledge.1,
              church = 0, 
              bible.collapsed = bible.1,
              interaction2 = 0*bible.1,
              interaction3 = 0*secular.knowledge.1,
              rps.ssm = 1,
              interaction4 = 0*1)
#------------
# Simulate first difference
s.out = sim(z.out, x = x.low, x1 = x.high)
summary(s.out)
LB.1 = -0.4501807
UB.1 = 0.1943654
PE.1 = -0.1407096

#-------------------------------------------------------------------------------
# EFFECT OF RPS FOR PEOPLE THAT 'SELDOM' ATTEND CHURCH (CHURCH = 1)
#------------
# Church = 1, RPS = 0
x.low = setx(z.out, 
             age = age.1,
             female = female.1, 
             black = black.1,
             education.collapsed = education.1,
             income.collapsed = income.1,
             ideology.collapsed = ideology.1,
             romancatholic = 0,
             partyid = pid.1,
             secular.knowledge = secular.knowledge.1,
             interaction1 = pid.1*secular.knowledge.1,
             church = 1, 
             bible.collapsed = bible.1,
             interaction2 = 1*bible.1,
             interaction3 = 1*secular.knowledge.1,
             rps.ssm = 0,
             interaction4 = 1*0)
#------------
# Church = 1, RPS = 1
x.high = setx(z.out, 
              age = age.1,
              female = female.1, 
              black = black.1,
              education.collapsed = education.1,
              income.collapsed = income.1,
              ideology.collapsed = ideology.1,
              romancatholic = 0,
              partyid = pid.1,
              secular.knowledge = secular.knowledge.1,
              interaction1 = pid.1*secular.knowledge.1,
              church = 1, 
              bible.collapsed = bible.1,
              interaction2 = 1*bible.1,
              interaction3 = 1*secular.knowledge.1,
              rps.ssm = 1,
              interaction4 = 1*1)
#------------
# Simulate first difference
s.out = sim(z.out, x = x.low, x1 = x.high)
summary(s.out)
LB.2 =  -0.5062194
UB.2 = -0.02585697
PE.2 = -0.2710665
#-------------------------------------------------------------------------------
# EFFECT OF RPS FOR PEOPLE THAT ATTEND CHURCH 'A FEW TIMES A YEAR' (CHURCH = 2)
#------------
# Church = 2, RPS = 0
x.low = setx(z.out, 
             age = age.1,
             female = female.1, 
             black = black.1,
             education.collapsed = education.1,
             income.collapsed = income.1,
             ideology.collapsed = ideology.1,
             romancatholic = 0,
             partyid = pid.1,
             secular.knowledge = secular.knowledge.1,
             interaction1 = pid.1*secular.knowledge.1,
             church = 2, 
             bible.collapsed = bible.1,
             interaction2 = 2*bible.1,
             interaction3 = 2*secular.knowledge.1,
             rps.ssm = 0,
             interaction4 = 2*0)
#------------
# Church = 2, RPS = 1
x.high = setx(z.out, 
              age = age.1,
              female = female.1, 
              black = black.1,
              education.collapsed = education.1,
              income.collapsed = income.1,
              ideology.collapsed = ideology.1,
              romancatholic = 0,
              partyid = pid.1,
              secular.knowledge = secular.knowledge.1,
              interaction1 = pid.1*secular.knowledge.1,
              church = 2, 
              bible.collapsed = bible.1,
              interaction2 = 2*bible.1,
              interaction3 = 2*secular.knowledge.1,
              rps.ssm = 1,
              interaction4 = 2*1)
#------------
# Simulate first difference
s.out = sim(z.out, x = x.low, x1 = x.high)
summary(s.out)
#------------
# Extract the quantities of interest:
LB.3 = -0.5553612
UB.3 = -0.2165906 
PE.3 = -0.390642
#-------------------------------------------------------------------------------
# EFFECT OF RPS FOR PEOPLE THAT ATTEND CHURCH 'ONCE OR TWICE A MONTH' (CHURCH = 3)
#------------
# Church = 3, RPS = 0
x.low = setx(z.out, 
             age = age.1,
             female = female.1, 
             black = black.1,
             education.collapsed = education.1,
             income.collapsed = income.1,
             ideology.collapsed = ideology.1,
             romancatholic = 0,
             partyid = pid.1,
             secular.knowledge = secular.knowledge.1,
             interaction1 = pid.1*secular.knowledge.1,
             church = 3, 
             bible.collapsed = bible.1,
             interaction2 = 3*bible.1,
             interaction3 = 3*secular.knowledge.1,
             rps.ssm = 0,
             interaction4 = 3*0)
#------------
# Church = 3, RPS = 1
x.high = setx(z.out, 
              age = age.1,
              female = female.1, 
              black = black.1,
              education.collapsed = education.1,
              income.collapsed = income.1,
              ideology.collapsed = ideology.1,
              romancatholic = 0,
              partyid = pid.1,
              secular.knowledge = secular.knowledge.1,
              interaction1 = pid.1*secular.knowledge.1,
              church = 3, 
              bible.collapsed = bible.1,
              interaction2 = 3*bible.1,
              interaction3 = 3*secular.knowledge.1,
              rps.ssm = 1,
              interaction4 = 3*1)
#------------
# Simulate first difference
s.out = sim(z.out, x = x.low, x1 = x.high)
summary(s.out)
#------------
# Extract the quantities of interest:
LB.4 = -0.6383059
UB.4 = -0.3230902
PE.4 = -0.487418
#-------------------------------------------------------------------------------
# EFFECT OF RPS FOR PEOPLE THAT ATTEND CHURCH 'WEEKLY' (CHURCH = 4)
#------------
# Church = 4, RPS = 0
x.low = setx(z.out, 
             age = age.1,
             female = female.1, 
             black = black.1,
             education.collapsed = education.1,
             income.collapsed = income.1,
             ideology.collapsed = ideology.1,
             romancatholic = 0,
             partyid = pid.1,
             secular.knowledge = secular.knowledge.1,
             interaction1 = pid.1*secular.knowledge.1,
             church = 4, 
             bible.collapsed = bible.1,
             interaction2 = 4*bible.1,
             interaction3 = 4*secular.knowledge.1,
             rps.ssm = 0,
             interaction4 = 4*0)
#------------
# Church = 4, RPS = 1
x.high = setx(z.out, 
              age = age.1,
              female = female.1, 
              black = black.1,
              education.collapsed = education.1,
              income.collapsed = income.1,
              ideology.collapsed = ideology.1,
              romancatholic = 0,
              partyid = pid.1,
              secular.knowledge = secular.knowledge.1,
              interaction1 = pid.1*secular.knowledge.1,
              church = 4, 
              bible.collapsed = bible.1,
              interaction2 = 4*bible.1,
              interaction3 = 4*secular.knowledge.1,
              rps.ssm = 1,
              interaction4 = 4*1)
#------------
# Simulate first difference
s.out = sim(z.out, x = x.low, x1 = x.high)
summary(s.out)
#------------
# Extract the quantities of interest:
LB.5 = -0.7103571
UB.5 = -0.3404175
PE.5 = -0.5511658
#-------------------------------------------------------------------------------
# EFFECT OF RPS FOR PEOPLE THAT ATTEND CHURCH 'MORE THAN ONCE WEEKLY' (CHURCH = 5)
#------------
# Church = 5, RPS = 0
x.low = setx(z.out, 
             age = age.1,
             female = female.1, 
             black = black.1,
             education.collapsed = education.1,
             income.collapsed = income.1,
             ideology.collapsed = ideology.1,
             romancatholic = 0,
             partyid = pid.1,
             secular.knowledge = secular.knowledge.1,
             interaction1 = pid.1*secular.knowledge.1,
             church = 5, 
             bible.collapsed = bible.1,
             interaction2 = 5*bible.1,
             interaction3 = 5*secular.knowledge.1,
             rps.ssm = 0,
             interaction4 = 5*0)
#------------
# Church = 5, RPS = 1
x.high = setx(z.out, 
              age = age.1,
              female = female.1, 
              black = black.1,
              education.collapsed = education.1,
              income.collapsed = income.1,
              ideology.collapsed = ideology.1,
              romancatholic = 0,
              partyid = pid.1,
              secular.knowledge = secular.knowledge.1,
              interaction1 = pid.1*secular.knowledge.1,
              church = 5, 
              bible.collapsed = bible.1,
              interaction2 = 5*bible.1,
              interaction3 = 5*secular.knowledge.1,
              rps.ssm = 1,
              interaction4 = 5*1)
#------------
# Simulate first difference
s.out = sim(z.out, x = x.low, x1 = x.high)
summary(s.out)
#------------
# Extract the quantities of interest:
LB.6 = -0.7972537
UB.6 = -0.3577968
PE.6 = -0.6075147
#-------------------------------------------------------------------------------
# FIRST DIFFERENCE GRAPH
#------------
# Create vectors with the simulated CI bounds and point estimates
UL = c(UB.1, UB.2, UB.3, UB.4, UB.5, UB.6)
LL = c(LB.1, LB.2, LB.3, LB.4, LB.5, LB.6)
PredictedProb = c(PE.1, PE.2, PE.3, PE.4, PE.5, PE.6)
PredictedProb
# Also create a vector containing the values that the 'church' variable takes on
church = c(0:5)
#------------
# Create data frame with the upper and lower bounds of the confidence intervals,
# the predicted probabilities, and the new 'church' object to correspond with the values
newdata = data.frame(UL, LL, PredictedProb, church)
#------------
figure1 <- ggplot(newdata, aes(church, PredictedProb, ymin = LL, ymax = UL)) +
  geom_pointrange(size=1) +
  geom_hline(yintercept = 0, lty = 2) +
  scale_x_continuous(breaks = 0:5, 
                     labels=c("Never", 
                              "Seldom",
                              "A few times \n a year",
                              "Once or twice \n a month",
                              "Every week",
                              "More than \n once weekly")) +
  labs(
    y = "Change in Probability\n",
    x = "\nFrequency of Church Attendance",
    title =  "Evangelical Protestants: Change in Probability of Supporting \nSame-Sex Marriage when Respondent Knows Church Opposition"
  ) +
  theme_bw() 

figure1

ggsave(plot = figure1, filename = "Fig1.pdf", height = 234, width = 180, units = "mm")



#------------------------------------------------------------
# FIGURE 2 
#------------------------------------------------------------
# Designate what the control variables should be set to for the simulations 
# There will be different central tendencies depending on the variable
age.1 = mean(evan.cath$age[evan.cath$tradition=="Roman Catholic Church"],
             na.rm=T)
female.1 = median(evan.cath$female[evan.cath$tradition=="Roman Catholic Church"], 
                  na.rm=T)
black.1 = median(evan.cath$black[evan.cath$tradition=="Roman Catholic Church"], 
                 na.rm=T)
education.1 = mean(evan.cath$education.collapsed[evan.cath$tradition=="Roman Catholic Church"], 
                   na.rm=T)
income.1 = mean(evan.cath$income.collapsed[evan.cath$tradition=="Roman Catholic Church"], 
                na.rm=T)
ideology.1 = mean(evan.cath$ideology.collapsed[evan.cath$tradition=="Roman Catholic Church"], 
                  na.rm=T)
pid.1 = mean(evan.cath$partyid[evan.cath$tradition=="Roman Catholic Church"], 
             na.rm=T)
secular.knowledge.1 = mean(evan.cath$secular.knowledge[evan.cath$tradition=="Roman Catholic Church"], 
                           na.rm=T)
bible.1 = mean(evan.cath$bible.collapsed[evan.cath$tradition=="Roman Catholic Church"], 
               na.rm=T)
#-------------------------------------------------------------------------------
# EFFECT OF RPS FOR PEOPLE THAT 'NEVER' ATTEND CHURCH (CHURCH = 0) 
#------------
# Church = 0; RPS = 0
x.low = setx(z.out, 
             age = age.1,
             female = female.1, 
             black = black.1,
             education.collapsed = education.1,
             income.collapsed = income.1,
             ideology.collapsed = ideology.1,
             romancatholic = 1,
             partyid = pid.1,
             secular.knowledge = secular.knowledge.1,
             interaction1 = pid.1*secular.knowledge.1,
             church = 0, 
             bible.collapsed = bible.1,
             interaction2 = 0*bible.1,
             interaction3 = 0*secular.knowledge.1,
             rps.ssm = 0,
             interaction4 = 0*0)
#------------
# Church = 0, RPS = 1 
x.high = setx(z.out, 
              age = age.1,
              female = female.1, 
              black = black.1,
              education.collapsed = education.1,
              income.collapsed = income.1,
              ideology.collapsed = ideology.1,
              romancatholic = 1,
              partyid = pid.1,
              secular.knowledge = secular.knowledge.1,
              interaction1 = pid.1*secular.knowledge.1,
              church = 0, 
              bible.collapsed = bible.1,
              interaction2 = 0*bible.1,
              interaction3 = 0*secular.knowledge.1,
              rps.ssm = 1,
              interaction4 = 0*1)
#------------
# Simulate first difference
s.out = sim(z.out, x = x.low, x1 = x.high)
summary(s.out)
#------------
# Extract the quantities of interest:
LB.1 = -0.4200545
UB.1 = 0.1718892
PE.1 = -0.1232143
#-------------------------------------------------------------------------------
# EFFECT OF RPS FOR PEOPLE THAT 'SELDOM' ATTEND CHURCH (CHURCH = 1)
#------------
# Church = 1, RPS = 0
x.low = setx(z.out, 
             age = age.1,
             female = female.1, 
             black = black.1,
             education.collapsed = education.1,
             income.collapsed = income.1,
             ideology.collapsed = ideology.1,
             romancatholic = 1,
             partyid = pid.1,
             secular.knowledge = secular.knowledge.1,
             interaction1 = pid.1*secular.knowledge.1,
             church = 1, 
             bible.collapsed = bible.1,
             interaction2 = 1*bible.1,
             interaction3 = 1*secular.knowledge.1,
             rps.ssm = 0,
             interaction4 = 1*0)
#------------
# Church = 1, RPS = 1
x.high = setx(z.out, 
              age = age.1,
              female = female.1, 
              black = black.1,
              education.collapsed = education.1,
              income.collapsed = income.1,
              ideology.collapsed = ideology.1,
              romancatholic = 1,
              partyid = pid.1,
              secular.knowledge = secular.knowledge.1,
              interaction1 = pid.1*secular.knowledge.1,
              church = 1, 
              bible.collapsed = bible.1,
              interaction2 = 1*bible.1,
              interaction3 = 1*secular.knowledge.1,
              rps.ssm = 1,
              interaction4 = 1*1)
#------------
# Simulate first difference
s.out = sim(z.out, x = x.low, x1 = x.high)
summary(s.out)
#------------
# Extract the quantities of interest:
LB.2 = -0.4735601
UB.2 = -0.03000808
PE.2 = -0.2599081
#-------------------------------------------------------------------------------
# EFFECT OF RPS FOR PEOPLE THAT ATTEND CHURCH 'A FEW TIMES A YEAR' (CHURCH = 2)
#------------
# Church = 2, RPS = 0
x.low = setx(z.out, 
             age = age.1,
             female = female.1, 
             black = black.1,
             education.collapsed = education.1,
             income.collapsed = income.1,
             ideology.collapsed = ideology.1,
             romancatholic = 1,
             partyid = pid.1,
             secular.knowledge = secular.knowledge.1,
             interaction1 = pid.1*secular.knowledge.1,
             church = 2, 
             bible.collapsed = bible.1,
             interaction2 = 2*bible.1,
             interaction3 = 2*secular.knowledge.1,
             rps.ssm = 0,
             interaction4 = 2*0)
#------------
# Church = 2, RPS = 1
x.high = setx(z.out, 
              age = age.1,
              female = female.1, 
              black = black.1,
              education.collapsed = education.1,
              income.collapsed = income.1,
              ideology.collapsed = ideology.1,
              romancatholic = 1,
              partyid = pid.1,
              secular.knowledge = secular.knowledge.1,
              interaction1 = pid.1*secular.knowledge.1,
              church = 2, 
              bible.collapsed = bible.1,
              interaction2 = 2*bible.1,
              interaction3 = 2*secular.knowledge.1,
              rps.ssm = 1,
              interaction4 = 2*1)
#------------
# Simulate first difference
s.out = sim(z.out, x = x.low, x1 = x.high)
summary(s.out)
#------------
# Extract the quantities of interest:
LB.3 = -0.5515479
UB.3 = -0.2146575
PE.3 = -0.3924994
#-------------------------------------------------------------------------------
# EFFECT OF RPS FOR PEOPLE THAT ATTEND CHURCH 'ONCE OR TWICE A MONTH' (CHURCH = 3)
#------------
# Church = 3, RPS = 0
x.low = setx(z.out, 
             age = age.1,
             female = female.1, 
             black = black.1,
             education.collapsed = education.1,
             income.collapsed = income.1,
             ideology.collapsed = ideology.1,
             romancatholic = 1,
             partyid = pid.1,
             secular.knowledge = secular.knowledge.1,
             interaction1 = pid.1*secular.knowledge.1,
             church = 3, 
             bible.collapsed = bible.1,
             interaction2 = 3*bible.1,
             interaction3 = 3*secular.knowledge.1,
             rps.ssm = 0,
             interaction4 = 3*0)
#------------
# Church = 3, RPS = 1
x.high = setx(z.out, 
              age = age.1,
              female = female.1, 
              black = black.1,
              education.collapsed = education.1,
              income.collapsed = income.1,
              ideology.collapsed = ideology.1,
              romancatholic = 1,
              partyid = pid.1,
              secular.knowledge = secular.knowledge.1,
              interaction1 = pid.1*secular.knowledge.1,
              church = 3, 
              bible.collapsed = bible.1,
              interaction2 = 3*bible.1,
              interaction3 = 3*secular.knowledge.1,
              rps.ssm = 1,
              interaction4 = 3*1)
#------------
# Simulate first difference
s.out = sim(z.out, x = x.low, x1 = x.high)
summary(s.out)
#------------
# Extract the quantities of interest:
LB.4 = -0.6626314
UB.4 = -0.3580624
PE.4 = -0.5286927
#-------------------------------------------------------------------------------
# EFFECT OF RPS FOR PEOPLE THAT ATTEND CHURCH 'WEEKLY' (CHURCH = 4)
#------------
# Church = 4, RPS = 0
x.low = setx(z.out, 
             age = age.1,
             female = female.1, 
             black = black.1,
             education.collapsed = education.1,
             income.collapsed = income.1,
             ideology.collapsed = ideology.1,
             romancatholic = 1,
             partyid = pid.1,
             secular.knowledge = secular.knowledge.1,
             interaction1 = pid.1*secular.knowledge.1,
             church = 4, 
             bible.collapsed = bible.1,
             interaction2 = 4*bible.1,
             interaction3 = 4*secular.knowledge.1,
             rps.ssm = 0,
             interaction4 = 4*0)
#------------
# Church = 4, RPS = 1
x.high = setx(z.out, 
              age = age.1,
              female = female.1, 
              black = black.1,
              education.collapsed = education.1,
              income.collapsed = income.1,
              ideology.collapsed = ideology.1,
              romancatholic = 1,
              partyid = pid.1,
              secular.knowledge = secular.knowledge.1,
              interaction1 = pid.1*secular.knowledge.1,
              church = 4, 
              bible.collapsed = bible.1,
              interaction2 = 4*bible.1,
              interaction3 = 4*secular.knowledge.1,
              rps.ssm = 1,
              interaction4 = 4*1)
#------------
# Simulate first difference
s.out = sim(z.out, x = x.low, x1 = x.high)
summary(s.out)
#------------
# Extract the quantities of interest:
LB.5 = -0.7678724
UB.5 = -0.4068371
PE.5 = -0.6148246 
#-------------------------------------------------------------------------------
# EFFECT OF RPS FOR PEOPLE THAT ATTEND CHURCH 'MORE THAN ONCE WEEKLY' (CHURCH = 5)
#------------
# Church = 5, RPS = 0
x.low = setx(z.out, 
             age = age.1,
             female = female.1, 
             black = black.1,
             education.collapsed = education.1,
             income.collapsed = income.1,
             ideology.collapsed = ideology.1,
             romancatholic = 1,
             partyid = pid.1,
             secular.knowledge = secular.knowledge.1,
             interaction1 = pid.1*secular.knowledge.1,
             church = 5, 
             bible.collapsed = bible.1,
             interaction2 = 5*bible.1,
             interaction3 = 5*secular.knowledge.1,
             rps.ssm = 0,
             interaction4 = 5*0)
#------------
# Church = 5, RPS = 1
x.high = setx(z.out, 
              age = age.1,
              female = female.1, 
              black = black.1,
              education.collapsed = education.1,
              income.collapsed = income.1,
              ideology.collapsed = ideology.1,
              romancatholic = 1,
              partyid = pid.1,
              secular.knowledge = secular.knowledge.1,
              interaction1 = pid.1*secular.knowledge.1,
              church = 5, 
              bible.collapsed = bible.1,
              interaction2 = 5*bible.1,
              interaction3 = 5*secular.knowledge.1,
              rps.ssm = 1,
              interaction4 = 5*1)
#------------
# Simulate first difference
s.out = sim(z.out, x = x.low, x1 = x.high)
summary(s.out)
#------------
# Extract the quantities of interest:
LB.6 = -0.8490323
UB.6 = -0.4574311
PE.6 = -0.6900956
#-------------------------------------------------------------------------------
# FIRST DIFFERENCE GRAPH
#------------
# Create vectors with the simulated CI bounds and point estimates
UL = c(UB.1, UB.2, UB.3, UB.4, UB.5, UB.6)
LL = c(LB.1, LB.2, LB.3, LB.4, LB.5, LB.6)
PredictedProb = c(PE.1, PE.2, PE.3, PE.4, PE.5, PE.6)
PredictedProb
# Also create a vector containing the values that the 'church' variable takes on
church = c(0:5)
#------------
# Create data frame with the upper and lower bounds of the confidence intervals,
# the predicted probabilities, and the new 'church' object to correspond with the values
newdata = data.frame(UL, LL, PredictedProb, church)
#------------
figure2 <- ggplot(newdata, aes(church, PredictedProb, ymin = LL, ymax = UL)) +
  geom_pointrange(size=1) +
  geom_hline(yintercept = 0, lty = 2) +
  scale_x_continuous(breaks = 0:5, labels=c("Never", "Seldom",
                                            "A few times \n a year",
                                            "Once or twice \n a month",
                                            "Every week",
                                            "More than \n once weekly")) +
  labs(
    y = "Change in Probability\n",
    x = "\nFrequency of Church Attendance",
    title = "Roman Catholics: Change in Probability of Supporting \nSame-Sex Marriage when Respondent Knows Church Opposition"
  ) +
  theme_bw() 

figure2
ggsave(plot = figure2, filename = "Fig2.pdf", height = 234, width = 180, units = "mm")
#-------------------------------------------------------------------------------
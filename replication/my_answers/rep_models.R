rm(list=ls())
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

lapply(c("stargazer", "foreign", "modelsummary", "dplyr"),  pkgTest)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
#------------------------------------------------------------
#insert data
rep_data <-  read.csv("/Users/carolinekajkowski/Desktop/Trinity/StatsII_2026/replication/my_answers/original_files/replication-data.csv") 

#subsetting the data to exclude ambiguous answers 
mainliners <- subset(rep_data, mainline == 1 & ambiguous == 0) 
evan.cath <- subset(rep_data, mainline != 1 & ambiguous==0)

#------------------------------------------------------------
#TABLE 1: Same-sex marriage (Roman Catholics & Evangelical Protestants)
#------------------------------------------------------------
secular1 <- glm(ssm ~ age + female + black + education.collapsed 
             + income.collapsed + ideology.collapsed + romancatholic 
             + partyid + secular.knowledge + partyid*secular.knowledge, 
             data=evan.cath, family="binomial")
summary(secular1)

naive1 <- glm(ssm ~ age + female + black + education.collapsed 
             + income.collapsed + ideology.collapsed + romancatholic 
             + partyid + secular.knowledge + partyid*secular.knowledge 
             + church + bible.collapsed + church*bible.collapsed, 
             data=evan.cath, family="binomial")
summary(naive1)


cw1 <- glm(ssm ~ age + female + black + education.collapsed 
             + income.collapsed + ideology.collapsed + romancatholic 
             + partyid + secular.knowledge + partyid*secular.knowledge
             + church + bible.collapsed + church*bible.collapsed 
             + church*secular.knowledge, 
             data=evan.cath, family="binomial")
summary(cw1)

rps1 <- glm(ssm ~ age + female + black + education.collapsed 
             + income.collapsed + ideology.collapsed + romancatholic
             + partyid + secular.knowledge + partyid*secular.knowledge
             + church + bible.collapsed + church*bible.collapsed 
             + church*secular.knowledge + rps.ssm + church*rps.ssm, 
             data=evan.cath, family="binomial")
summary(rps1)

stargazer(
  secular1, naive1, cw1, rps1,      
  type = "latex",                       
  title = "Evangelical Protestants and Roman Catholics' support of SSM Replication",       
  column.labels = c("Secular", "Naive", "Culture Wars", "RPS"),
  font.size = "scriptsize" )

#------------------------------------------------------------
#TABLE 2: Abortion (Roman Catholics & Evangelical Protestants)
#------------------------------------------------------------
secular2 <- glm(abortion ~ age + female + black + education.collapsed 
                + income.collapsed + ideology.collapsed + romancatholic 
                + partyid + secular.knowledge + partyid*secular.knowledge, 
                data=evan.cath, family="binomial")
summary(secular2)

naive2 <- glm(abortion ~ age + female + black + education.collapsed 
              + income.collapsed + ideology.collapsed + romancatholic 
              + partyid + secular.knowledge + partyid*secular.knowledge 
              + church + bible.collapsed + church*bible.collapsed, 
              data=evan.cath, family="binomial")
summary(naive2)


cw2 <- glm(abortion ~ age + female + black + education.collapsed 
           + income.collapsed + ideology.collapsed + romancatholic 
           + partyid + secular.knowledge + partyid*secular.knowledge
           + church + bible.collapsed + church*bible.collapsed 
           + church*secular.knowledge, 
           data=evan.cath, family="binomial")
summary(cw2)

rps2 <- glm(abortion ~ age + female + black + education.collapsed 
            + income.collapsed + ideology.collapsed + romancatholic 
            + partyid + secular.knowledge + partyid*secular.knowledge
            + church + bible.collapsed + church*bible.collapsed
            + church*secular.knowledge + rps.abortion + church*rps.abortion, 
            data=evan.cath, family="binomial")
summary(rps2)

stargazer(
  secular2, naive2, cw2, rps2,      
  type = "latex",                       
  title = "Evangelical Protestants and Roman Catholics' support of abortion Replication",       
  column.labels = c("Secular", "Naive", "Culture Wars", "RPS"),
  font.size = "scriptsize" )

#------------------------------------------------------------
#TABLE 3: Linking of views on ssm and abortion (Roman Catholics & Evangelical Protestants)
#------------------------------------------------------------
model1 = glm(ssm ~ age + female + black + education.collapsed 
             + income.collapsed + ideology.collapsed + romancatholic 
             + partyid + secular.knowledge + partyid*secular.knowledge
             + church + bible.collapsed + church*bible.collapsed 
             + church*secular.knowledge + rps.abortion
             + church*rps.abortion, data=evan.cath, family="binomial")
summary(model1)

model2 = glm(abortion ~ age + female + black + education.collapsed 
             + income.collapsed + ideology.collapsed + romancatholic 
             + partyid + secular.knowledge + partyid*secular.knowledge
             + church + bible.collapsed + church*bible.collapsed 
             + church*secular.knowledge + rps.ssm
             + church*rps.ssm, data=evan.cath, family="binomial")
summary(model2)

stargazer(
  model1, model2,       
  type = "latex",                       
  title = "RPS does not promote cross-over issue constraint for Evangelical Protestants and Roman Catholics",       
  column.labels = c("SSM Support", "Abortion Support"),
  font.size = "scriptsize" )

#------------------------------------------------------------
#TABLE 4: Same-sex marriage(Mainline Protestants)
#------------------------------------------------------------
secular4 <- glm(ssm ~ partyid + secular.knowledge + partyid*secular.knowledge, 
             data=mainliners, family="binomial")
summary(secular4)

naive4 <- glm(ssm ~ partyid + secular.knowledge 
             + partyid*secular.knowledge + church + bible.collapsed 
             + church*bible.collapsed, data=mainliners, family="binomial")
summary(naive4)

cw4 <- glm(ssm ~ partyid + secular.knowledge 
             + partyid*secular.knowledge + church + bible.collapsed 
             + church*bible.collapsed + church*secular.knowledge, 
             data=mainliners, family="binomial")
summary(cw4)

rps4 <- glm(ssm ~ partyid + secular.knowledge 
             + partyid*secular.knowledge + church + bible.collapsed 
             + church*bible.collapsed + church*secular.knowledge 
             + rps.ssm + church*rps.ssm, 
             data=mainliners, family="binomial")
summary(rps4)

stargazer(
  secular4, naive4, cw4, rps4,      
  type = "latex",                       
  title = "Mainline Protestants' support of SSM Replication",       
  column.labels = c("Secular", "Naive", "Culture Wars", "RPS"),
  font.size = "scriptsize" )

#------------------------------------------------------------
#TABLE 5: Abortion (Mainline Protestants)
#------------------------------------------------------------
secular5 <- glm(abortion ~ partyid + secular.knowledge + partyid*secular.knowledge, 
                data=mainliners, family="binomial")
summary(secular5)

naive5 <- glm(abortion ~ partyid + secular.knowledge 
              + partyid*secular.knowledge + church + bible.collapsed 
              + church*bible.collapsed, data=mainliners, family="binomial")
summary(naive5)

cw5 <- glm(abortion ~ partyid + secular.knowledge 
           + partyid*secular.knowledge + church + bible.collapsed 
           + church*bible.collapsed + church*secular.knowledge, 
           data=mainliners, family="binomial")
summary(cw5)

rps5 <- glm(abortion ~ partyid + secular.knowledge 
            + partyid*secular.knowledge + church + bible.collapsed 
            + church*bible.collapsed + church*secular.knowledge 
            + rps.abortion + church*rps.abortion, 
            data=mainliners, family="binomial")
summary(rps5)

stargazer(
  secular5, naive5, cw5, rps5,      
  type = "latex",                       
  title = "Mainline Protestants' support of Abortion Replication",       
  column.labels = c("Secular", "Naive", "Culture Wars", "RPS"),
  font.size = "scriptsize" )

#------------------------------------------------------------
#Data manipulation for 6&7
#------------------------------------------------------------
# Reports of Church teaching (ssm)
unique(rep_data$church.ssm)
rep_data$church.ssm <- as.character(rep_data$church.ssm)
str(rep_data$church.ssm)
church.reports.ssm <- recode(rep_data$church.ssm, 
                            "Not Available" = NA_character_)
table(church.reports.ssm)

# Reports of Church Teaching (Abortion)
# Take the 'Not Available' out
unique(rep_data$church.abortion)
table(rep_data$church.abortion)
str(rep_data$church.abortion)
church.reports.abortion <- recode(rep_data$church.abortion, 
                                 "Not Available" = NA_character_)
table(church.reports.abortion)
#------------------------------------------------------------
#TABLE 6: RPS answer (SSM) and support/opposition to SSM
#------------------------------------------------------------
table6 <-  prop.table(table(church.reports.ssm[rep_data$ambiguous==0], rep_data$ssm[rep_data$ambiguous==0]), 2)
round(table6, digits = 2)
table6_counts <- table(church.reports.ssm[rep_data$ambiguous==0], rep_data$ssm[rep_data$ambiguous==0])
sum(table6_counts[, 1]) 
sum(table6_counts[, 2]) 

#------------------------------------------------------------
#TABLE 7: RPS answer (abortion) and support/opposition 
#------------------------------------------------------------
table7 <-  prop.table(table(church.reports.abortion[rep_data$ambiguous==0], rep_data$abortion[rep_data$ambiguous==0]), 2)
round(table7, digits = 2)
table7_counts <- table(church.reports.abortion[rep_data$ambiguous==0], rep_data$abortion[rep_data$ambiguous==0])
sum(table7_counts[, 1]) 
sum(table7_counts[, 2]) 

n_opposes <- sum(table7_counts[, 1])
n_supports <- sum(table7_counts[, 2])
table7_df <- as.data.frame.matrix(table7)
colnames(table7_df) <- c("Opposes", "Supports")

table7_df <- rbind(table7_df, 
                   Total = colSums(table7_df),
                      n  = c(n_opposes, n_supports))

#------------------------------------------------------------
stargazer(table6,
          type = "latex",
          # summary = FALSE,
          rownames = TRUE,
          title = "Beliefs about church teaching on same-sex marriage by personal opinion on same-sex marriage Replication",
          digits = 2)

stargazer(table7_df,
          type = "latex",
          summary = FALSE,
          rownames = TRUE,
          title = "Beliefs about church teaching on abortion by personal opinion on unrestricted abortion rights Replication",
          digits = 2)

#------------------------------------------------------------
#TABLE 8: RPS Models (Incorrect RPS) (Evangelical Protestants and Roman Catholics)
#------------------------------------------------------------
inc.ssm <- glm(ssm ~ age + female + black + education.collapsed 
              + income.collapsed + ideology.collapsed + romancatholic 
              + partyid + secular.knowledge + partyid*secular.knowledge
              + church + bible.collapsed + church*bible.collapsed 
              + church*secular.knowledge + incorrect.rps.ssm 
              + church*incorrect.rps.ssm, data=evan.cath, family="binomial")
summary(inc.ssm)


inc.abortion <- glm(abortion ~ age + female + black + education.collapsed 
                   + income.collapsed + ideology.collapsed + romancatholic 
                   + partyid + secular.knowledge + partyid*secular.knowledge
                   + church + bible.collapsed + church*bible.collapsed 
                   + church*secular.knowledge + incorrect.rps.abortion 
                   + church*incorrect.rps.abortion, 
                   data=evan.cath, family="binomial")
summary(inc.abortion)

stargazer(
  inc.ssm, inc.abortion,      
  type = "latex",                       
  title = "Incorrect RPS reports only influence political attitudes through church attendance: support for same-sex marriage and unrestricted abortion rights, evangelical Protestants and Roman Catholics Replication",       
  column.labels = c("SSM Support", "Abortion Support"),
  font.size = "scriptsize" )

#------------------------------------------------------------
#EXTENSION
#------------------------------------------------------------



#Evangelical and Roman Catholics: SSM
cw1 <- glm(ssm ~ age + female + black + education.collapsed 
           + income.collapsed + ideology.collapsed + romancatholic 
           + partyid + secular.knowledge + partyid*secular.knowledge
           + church + bible.collapsed + church*bible.collapsed 
           + church*secular.knowledge, 
           data=evan.cath, family="binomial")
summary(cw1)

cw1_reduced <- glm(ssm ~ age + female + black + education.collapsed 
           + income.collapsed + ideology.collapsed + romancatholic 
           + partyid + secular.knowledge + partyid*secular.knowledge
           + church + bible.collapsed + church*bible.collapsed, 
           data=evan.cath, family="binomial")
summary(cw1)

evan.cath_ssm <- anova(cw1_reduced, cw1, test = "Chisq")
stargazer(as.data.frame(evan.cath_ssm),
          type = "latex",
          summary = FALSE,
          title = "Partial F-test: Evangelical Protestants and Roman Catholics' support of SSM ")



#Evangelical and Roman Catholics: Abortion
cw2 <- glm(abortion ~ age + female + black + education.collapsed 
           + income.collapsed + ideology.collapsed + romancatholic 
           + partyid + secular.knowledge + partyid*secular.knowledge
           + church + bible.collapsed + church*bible.collapsed 
           + church*secular.knowledge, 
           data=evan.cath, family="binomial")
summary(cw2)

cw2_reduced <- glm(abortion ~ age + female + black + education.collapsed 
           + income.collapsed + ideology.collapsed + romancatholic 
           + partyid + secular.knowledge + partyid*secular.knowledge
           + church + bible.collapsed + church*bible.collapsed,
           data=evan.cath, family="binomial")
summary(cw2)

evan.cath_abort <- anova(cw2_reduced, cw2, test = "Chisq")

stargazer(as.data.frame(evan.cath_abort),
          type = "latex",
          summary = FALSE,
          title = "Partial F-test: Evangelical Protestants and Roman Catholics' support of Abortion ")


#Mainline Protestants: SSM
cw4 <- glm(ssm ~ partyid + secular.knowledge 
           + partyid*secular.knowledge + church + bible.collapsed 
           + church*bible.collapsed + church*secular.knowledge, 
           data=mainliners, family="binomial")
summary(cw4)

cw4_reduced <- glm(ssm ~ partyid + secular.knowledge 
           + partyid*secular.knowledge + church + bible.collapsed 
           + church*bible.collapsed, 
           data=mainliners, family="binomial")
summary(cw4)

main_ssm <- anova(cw4_reduced, cw4, test = "Chisq")

stargazer(as.data.frame(main_ssm),
          type = "latex",
          summary = FALSE,
          title = "Partial F-test: Mainline Protestants' support of Abortion ")
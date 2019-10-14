rm(list = ls(all = TRUE))

# Load packages.
# Data manipulation.
library(data.table)
library(dplyr)
library(stringr)
library(rio)
library(tidyverse)
library(janitor)

# Plots.
library(ggplot2)
library(cowplot)

# Analyses.
library(afex)
library(lme4)
library(lmerTest)

theme_set(theme_bw())

#### Import Data ####

# Session 1
# Set working directory.
setwd("/Volumes/netapp/MyersLab/Dave/SM18-Replication/Stats/session1")

# Read in files.
file_names <- list.files(path = ".", pattern = "*.csv", all.files = FALSE,
                         full.names = FALSE, recursive = FALSE)

# Create data frame.
session1 <- data.frame()

# Loop to create combined dataframe.
for (i in file_names) {
  data <- fread(i, header = TRUE, sep = ",")
  session1 <- rbind(session1, data)
}

# Various housekeeping things.
# make variable names syntactically valid.
session1 <- clean_names(session1)

# Select only columns we want.
session1 <- dplyr::filter(session1,session1$trial_number != "BEGIN TASK")
session1 <- dplyr::filter(session1,session1$trial_number != "END TASK")
session1 <- select(session1,participant_public_id,spreadsheet_name,spreadsheet_row,trial_number,
              zone_type,reaction_time,response,attempt,correct,display,fname,branch_ee59,branch_52fz)

# Remove trials where stimulus failed to load.
session1 <- subset(session1,spreadsheet_name!="")

# Rename some variables (naming in Gorilla is opaque)
names(session1)[12] <- "Headphone.1"
names(session1)[13] <- "Headphone.2"
names(session1)[11] <- "stimulus"
names(session1)[10] <- "block"
names(session1)[1] <- "ID"

# Set vectors.
session1$spreadsheet_row <- as.numeric(session1$spreadsheet_row)
session1$trial_number <- as.numeric(session1$trial_number)
session1$reaction_time <- as.numeric(session1$reaction_time)
session1$correct <- as.numeric(session1$correct)
session1$order <- substr(session1$spreadsheet_name,1,4)
session1$session <- "Session 1"

# Separate data by block.
PC.session1 <- subset(session1,grepl("PC",block))
LD.session1 <- subset(session1,grepl("LD",block))

# Session 2
# Set working directory.
setwd("/Volumes/netapp/MyersLab/Dave/SM18-Replication/Stats/session2")

# Read in files.
file_names <- list.files(path = ".", pattern = "*.csv", all.files = FALSE,
                         full.names = FALSE, recursive = FALSE)

# Create data frame.
session2 <- data.frame()

# Loop to create combined dataframe.
for (i in file_names) {
  data <- fread(i, header = TRUE, sep = ",")
  session2 <- rbind(session2, data)
}

# Various housekeeping things.
# Make variable names syntactically valid.
session2 <- clean_names(session2)

# Select only columns we want.
session2 <- dplyr::filter(session2,session2$trial_number != "BEGIN TASK")
session2 <- dplyr::filter(session2,session2$trial_number != "END TASK")
session2 <- select(session2,participant_public_id,spreadsheet_name,spreadsheet_row,trial_number,
                   zone_type,reaction_time,response,attempt,correct,display,fname,branch_ee59,branch_52fz)

# Remove trials where stimulus failed to load.
session2 <- subset(session2,spreadsheet_name!="")

# Rename some variables (naming in Gorilla is opaque)
names(session2)[12] <- "Headphone.1"
names(session2)[13] <- "Headphone.2"
names(session2)[11] <- "stimulus"
names(session2)[10] <- "block"
names(session2)[1] <- "ID"

# Set vectors.

session2$spreadsheet_row <- as.numeric(session2$spreadsheet_row)
session2$trial_number <- as.numeric(session2$trial_number)
session2$reaction_time <- as.numeric(session2$reaction_time)
session2$correct <- as.numeric(session2$correct)
session2$order <- substr(session2$spreadsheet_name,1,4)
session2$session <- "Session 2"
# Separate data by block.
PC.session2 <- subset(session2,grepl("PC",block))
LD.session2 <- subset(session2,grepl("LD",block))

# Clean up.
rm(data)

 # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Combine data frames.
PC.both.complete <- rbind(PC.session1,PC.session2)

# Only include ID's who completed both sessions.
DNF <- PC.both.complete %>%
  group_by(ID) %>%
  summarise(Count = n_distinct(session))
DNF <- subset(DNF,Count==1)
PC.both.complete <- PC.both.complete[ ! PC.both.complete$ID %in% DNF$ID, ]

# Rename "block" to bias for transparency.
names(PC.both.complete)[10] <- "bias"
PC.both.complete$bias <- ifelse(PC.both.complete$bias=="PC-S","S-Bias",ifelse(PC.both.complete$bias=="PC-SH","SH-Bias",""))

# Check number of IDs by headphone checks; X fail both checks.
Counts <- PC.both.complete %>%
  group_by(spreadsheet_name, Headphone.1, Headphone.2) %>%
  summarise(Count = n_distinct(ID))

# Remove subjects who failed both headphone checks.
PC.both.complete <- filter(PC.both.complete, Headphone.1 == "Pass" | Headphone.2 == "Pass")
PC.both.complete$ID <- factor(PC.both.complete$ID)

# See how many participants remain after removing double headphone check fails.
n_distinct(PC.both.complete$ID) # N=80

# Drop NA trials.
PC.both.complete <- subset(PC.both.complete,zone_type=="response_keyboard_single")

# Change response from string to binary integer.
PC.both.complete$resp1 <- ifelse(PC.both.complete$response=="sign",1,0)

# Add vector for continuum step.
PC.both.complete$step <- ifelse(PC.both.complete$stimulus=="TW001_20SS_80SH-001.mp3",1,
                           ifelse(PC.both.complete$stimulus=="TW002_30SS_70SH-001.mp3",2,
                                  ifelse(PC.both.complete$stimulus=="TW003_40SS_60SH-001.mp3",3,
                                         ifelse(PC.both.complete$stimulus=="TW004_50SS_50SH-001.mp3",4,
                                                ifelse(PC.both.complete$stimulus=="TW005_60SS_40SH-001.mp3",5,
                                                       ifelse(PC.both.complete$stimulus=="TW006_70SS_30SH-001.mp3",6,
                                                              ifelse(PC.both.complete$stimulus=="TW007_80SS_20SH-001.mp3",7,NA)))))))

# Filter out participants who did not meet S&M18 endpoint accuracy criteria (<80% accuracy) on either session.
exclude <- subset(PC.both.complete, step==1 | step==7)
exclude <- exclude %>%
  group_by(ID,step,session) %>% 
  summarize(endpointResp=mean(resp1))
exclude <- subset(exclude,step==1&endpointResp>.2|step==7&endpointResp<.8)

# Count how many participants are going to be excluded for accuracy.
n_distinct(exclude$ID) # N = 18

# Remove those participants.
PC.both.complete <- PC.both.complete[ ! PC.both.complete$ID %in% exclude$ID, ]

# Check how many subjects reman after excluding based on both headphone check fails and acccuracy.
n_distinct(PC.both.complete$ID) # N = 62

#### Session 1 ####

# Break out session 1 from cleaned up combined data.
PC.session1 <- subset(PC.both.complete,session=="Session 1")

# Check counts of stimuli.
Counts <- PC.session1 %>%
  group_by(ID) %>%
  summarise(Count = length(ID))

# Counts by Order; n = X in each order group.
PC.session1 %>%
  group_by(order) %>%
  summarise(Count = n_distinct(ID))

# Plot categorization data.
PC.session1.figure.stats <- Rmisc::summarySE(PC.session1, measurevar="resp1",groupvars = c("step","bias","order"))

# By order.
ggplot(PC.session1.figure.stats, aes(x=as.numeric(step), y=resp1,color=as.factor(bias),linetype=as.factor(order))) +
  geom_point(stat='summary', fun.y='mean', size=3,alpha=0.7) +
  geom_line(stat='summary', fun.y='mean', size=1.25, alpha=0.7) +
  geom_errorbar(aes(ymin=resp1-se,ymax=resp1+se),width=.3) +
  scale_x_continuous('Continuum step', breaks=c(1:7)) +
  scale_y_continuous('Percent "sign" responses', breaks=c(0,0.25,0.5,0.75,1), labels=c(0,25,50,75,100)) +
  scale_color_manual('Biasing Condition', labels=c('S-bias','SH-Bias'),values=c("#B03A2E","#2874A6")) +
  scale_linetype_manual('Order', labels=c('S-SH','SH-S'),values=c("solid","dotted")) +
  coord_cartesian(ylim=c(0,1)) +
  theme(text = element_text(size=20))

# Prep data for models.
PC.session1$ID <- as.factor(PC.session1$ID)
PC.session1$bias <- as.factor(PC.session1$bias)
PC.session1$step <- scale(PC.session1$step)
PC.session1$order <- as.factor(PC.session1$order)
contrasts(PC.session1$bias) = contr.sum(2)
contrasts(PC.session1$order) = contr.sum(2)

# Build models.
model1 <- mixed(resp1 ~ step*bias*order + 
                 (bias*step||ID) + (bias||ID) + (step||ID), family=binomial(link="logit"),data=PC.session1,method="LRT",expand_re = TRUE,
                control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 150000)))

model2 <- mixed(resp1 ~ step*bias*order + 
                  (bias||ID) + (step||ID), family=binomial(link="logit"),data=PC.session1,method="LRT",expand_re = TRUE,
                control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 150000)))

model3 <- mixed(resp1 ~ step*bias*order + 
                  (bias||ID), family=binomial(link="logit"),data=PC.session1,method="LRT",expand_re = TRUE,
                control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 150000)))

model4 <- mixed(resp1 ~ step*bias*order + 
                  (1|ID), family=binomial(link="logit"),data=PC.session1,method="LRT",expand_re = TRUE,
                control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 150000)))

# Compare models.
anova(model1,model2,model3,model4)

# Summarize model2.
model2

# Explore Bias x Order interaction.
BiasOrder <- interaction(PC.session1$bias,PC.session1$order)
m1 <- glmer(resp1 ~ BiasOrder + (1|ID),
            data=PC.session1, family='binomial', control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 200000)))
posthoc <- multcomp::glht(m1, linfct=multcomp::mcp(BiasOrder="Tukey"))
summary(posthoc,test=multcomp::adjusted("bonferroni")) # Interaction occuring as a result of differences in biasing effect on SH-bias block across orders.

#### Session 2 ####

# Break out session 2 PC data from combined data with only participants who completed both days.
PC.session2 <- subset(PC.both.complete,session=="Session 2")

# Counts by Order; n = X in each order group.
PC.session2 %>%
  group_by(order) %>%
  summarise(Count = n_distinct(ID))

# Change response from string to binary integer.
PC.session2$resp1 <- ifelse(PC.session2$response=="sign",1,0)

# Add vector for continuum step.
PC.session2$step <- ifelse(PC.session2$stimulus=="TW001_20SS_80SH-001.mp3",1,
                           ifelse(PC.session2$stimulus=="TW002_30SS_70SH-001.mp3",2,
                                  ifelse(PC.session2$stimulus=="TW003_40SS_60SH-001.mp3",3,
                                         ifelse(PC.session2$stimulus=="TW004_50SS_50SH-001.mp3",4,
                                                ifelse(PC.session2$stimulus=="TW005_60SS_40SH-001.mp3",5,
                                                       ifelse(PC.session2$stimulus=="TW006_70SS_30SH-001.mp3",6,
                                                              ifelse(PC.session2$stimulus=="TW007_80SS_20SH-001.mp3",7,NA)))))))

# Plot categorization data.
PC.session2.figure.stats <- Rmisc::summarySE(PC.session2, measurevar="resp1",groupvars = c("step","bias","order"))

# By order.
ggplot(PC.session2.figure.stats, aes(x=as.numeric(step), y=resp1,color=as.factor(bias),linetype=as.factor(order))) +
  geom_point(stat='summary', fun.y='mean', size=3,alpha=0.7) +
  geom_line(stat='summary', fun.y='mean', size=1.25, alpha=0.7) +
  geom_errorbar(aes(ymin=resp1-se,ymax=resp1+se),width=.3) +
  scale_x_continuous('Continuum step', breaks=c(1:7)) +
  scale_y_continuous('Percent "sign" responses', breaks=c(0,0.25,0.5,0.75,1), labels=c(0,25,50,75,100)) +
  scale_color_manual('Biasing Condition', labels=c('S-bias','SH-Bias'),values=c("#B03A2E","#2874A6")) +
  scale_linetype_manual('Order', labels=c('S-SH','SH-S'),values=c("solid","dotted")) +
  coord_cartesian(ylim=c(0,1)) +
  theme(text = element_text(size=20))

# Prep data for models.
PC.session2$ID <- as.factor(PC.session2$ID)
PC.session2$bias <- as.factor(PC.session2$bias)
PC.session2$step <- scale(PC.session2$step)
PC.session2$order <- as.factor(PC.session2$order)
contrasts(PC.session2$bias) = contr.sum(2)
contrasts(PC.session2$order) = contr.sum(2)

# Build models.
model1 <- mixed(resp1 ~ step*bias*order + 
                  (bias*step||ID) + (bias||ID) + (step||ID), family=binomial(link="logit"),data=PC.session2,method="LRT",expand_re = TRUE,
                control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 150000)))
model2 <- mixed(resp1 ~ step*bias*order + 
                  (bias||ID) + (step||ID), family=binomial(link="logit"),data=PC.session2,method="LRT",expand_re = TRUE,
                control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 150000)))
model3 <- mixed(resp1 ~ step*bias*order + 
                  (bias||ID), family=binomial(link="logit"),data=PC.session2,method="LRT",expand_re = TRUE,
                control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 150000)))
model4 <- mixed(resp1 ~ step*bias*order + 
                  (1|ID), family=binomial(link="logit"),data=PC.session2,method="LRT",expand_re = TRUE,
                control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 150000)))

# Compare models.
anova(model1,model2,model3,model4)

# Summarize model2.
model2
summary(model2)

#### Combined Phonetic Categoization ####

# Combine cleaned up PC data into one dataset.
PC.both.complete <- rbind(PC.session1,PC.session2)

# Continuum step has beeen scaled for analysis, undo scaling for plotting.
PC.both.complete$step <- ifelse(PC.both.complete$stimulus=="TW001_20SS_80SH-001.mp3",1,
                           ifelse(PC.both.complete$stimulus=="TW002_30SS_70SH-001.mp3",2,
                                  ifelse(PC.both.complete$stimulus=="TW003_40SS_60SH-001.mp3",3,
                                         ifelse(PC.both.complete$stimulus=="TW004_50SS_50SH-001.mp3",4,
                                                ifelse(PC.both.complete$stimulus=="TW005_60SS_40SH-001.mp3",5,
                                                       ifelse(PC.both.complete$stimulus=="TW006_70SS_30SH-001.mp3",6,
                                                              ifelse(PC.both.complete$stimulus=="TW007_80SS_20SH-001.mp3",7,NA)))))))

# Calculate stats for plot.
PC.both.complete.stats <- Rmisc::summarySE(data = PC.both.complete, measurevar="resp1",groupvars = c("step","bias","order","session"))

# Plot.
ggplot(PC.both.complete.stats, aes(x=step, y=resp1, colour=factor(bias),linetype=factor(order))) +
  geom_point(stat='summary', fun.y='mean', size=2.5) +
  geom_line(stat='summary', fun.y='mean', size=0.75) +
  geom_errorbar(aes(ymin=resp1-se,ymax=resp1+se),width=.5) +
  facet_wrap(~session) +
  scale_x_continuous('Continuum step', breaks=c(1:7)) +
  scale_y_continuous('Percent "sign" responses', breaks=c(0,0.25,0.5,0.75,1), labels=c(0,25,50,75,100)) +
  scale_color_manual('Biasing Condition', labels=c('S-Bias','SH-Bias'),values=c("#B03A2E","#2874A6")) +
  scale_linetype_manual('Order', labels=c('S-SH','SH-S'),values=c("solid","dotted")) +
  coord_cartesian(ylim=c(0,1)) + 
  theme(text = element_text(size=14))

# Prep models.
PC.both.complete$bias <- as.factor(PC.both.complete$bias)
PC.both.complete$order <- as.factor(PC.both.complete$order)
PC.both.complete$session <- as.factor(PC.both.complete$session)
PC.both.complete$step <- scale(PC.both.complete$step)

# Build models.
model1 <- mixed(resp1 ~ step*bias*order*session + 
                  (bias*step||ID) + (bias||ID) + (step||ID), family=binomial(link="logit"),data=PC.both.complete,method="LRT",expand_re = TRUE,
                control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 1500000)))
model2 <- mixed(resp1 ~ step*bias*order*session + 
                  (bias||ID) + (step||ID), family=binomial(link="logit"),data=PC.both.complete,method="LRT",expand_re = TRUE,
                control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 1500000)))
model3 <- mixed(resp1 ~ step*bias*order*session + 
                  (bias||ID), family=binomial(link="logit"),data=PC.both.complete,method="LRT",expand_re = TRUE,
                control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 1500000)))
model4 <- mixed(resp1 ~ step*bias*order*session + 
                  (1|ID), family=binomial(link="logit"),data=PC.both.complete,method="LRT",expand_re = TRUE,
                control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 1500000)))

# Compare models.
anova(model1,model2,model3,model4)

# Summarize model2.
model1
summary(model1)

# Explore Bias x Order x Session interaction.
BiasOrderSession <- interaction(PC.both.complete$bias,PC.both.complete$order,PC.both.complete$session)
m1 <- glmer(resp1 ~ BiasOrderSession + (1|ID),
            data=PC.both.complete, family='binomial', control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 200000)))
posthoc <- multcomp::glht(m1, linfct=multcomp::mcp(BiasOrderSession="Tukey"))
summary(posthoc,test=multcomp::adjusted("bonferroni")) 

#### SH-S Order only ####

# Subset PC tasks to only look at SH-S order (where learning is present).
SH_S <- subset(PC.both.complete,order=="SH_S")

# Scale continuum step variable.
SH_S$step <- scale(SH_S$step)

# Run model to see how learning changes over sessions.
SH_S.model1 <- model1 <- mixed(resp1 ~ step*bias*session + 
                                 (bias*step||ID) + (bias||ID) + (step||ID), family=binomial(link="logit"),data=SH_S,method="LRT",expand_re = TRUE,
                               control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 1500000)))

SH_S.model2 <- mixed(resp1 ~ step*bias*session + 
                  (bias||ID) + (step||ID), family=binomial(link="logit"),data=SH_S,method="LRT",expand_re = TRUE,
                control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 1500000)))

SH_S.model3 <- mixed(resp1 ~ step*bias*session + 
                  (bias||ID), family=binomial(link="logit"),data=SH_S,method="LRT",expand_re = TRUE,
                control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 1500000)))

SH_S.model4 <- mixed(resp1 ~ step*bias*session + 
                  (1|ID), family=binomial(link="logit"),data=SH_S,method="LRT",expand_re = TRUE,
                control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 1500000)))

# Compare models.
anova(SH_S.model1,SH_S.model2,SH_S.model3,SH_S.model4)
SH_S.model2

# Unscale step for plotting.
SH_S$step <- ifelse(SH_S$stimulus=="TW001_20SS_80SH-001.mp3",1,
                                ifelse(SH_S$stimulus=="TW002_30SS_70SH-001.mp3",2,
                                       ifelse(SH_S$stimulus=="TW003_40SS_60SH-001.mp3",3,
                                              ifelse(SH_S$stimulus=="TW004_50SS_50SH-001.mp3",4,
                                                     ifelse(SH_S$stimulus=="TW005_60SS_40SH-001.mp3",5,
                                                            ifelse(SH_S$stimulus=="TW006_70SS_30SH-001.mp3",6,
                                                                   ifelse(SH_S$stimulus=="TW007_80SS_20SH-001.mp3",7,NA)))))))

# Figure with sessions superimposed on each other.
SH_S.stats <- Rmisc::summarySE(SH_S,measurevar = "resp1",groupvars = c("step","bias","order","session"))

ggplot(SH_S.stats,aes(x=step,y=resp1,color=bias,linetype=session)) +
geom_point(stat='summary', fun.y='mean', size=2.5) +
  geom_point(stat='summary', fun.y='mean', size=3) +
  geom_line(stat='summary', fun.y='mean', size=1.25) +
  geom_errorbar(aes(ymin=resp1-se,ymax=resp1+se),width=.3) +
  scale_x_continuous('Continuum step', breaks=c(1:7)) +
  scale_y_continuous('Percent "sign" responses', breaks=c(0,0.25,0.5,0.75,1), labels=c(0,25,50,75,100)) +
  scale_color_manual('Biasing Condition', labels=c('S-bias','SH-Bias'),values=c("#B03A2E","#2874A6")) +
  scale_linetype_manual('Session', labels=c('Session 1','Session 2'),values=c("solid","dotted")) +
  coord_cartesian(ylim=c(0,1)) +
  theme(text = element_text(size=20))+
  ggtitle("SH-S Order only")
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
library(emmeans)
library(quickpsy)

theme_set(theme_bw())

#### Import Data ####

# Session 1
# Set working directory.
setwd("./session1")

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
              zone_type,reaction_time,response,attempt,correct,display,fname,branch_ee59,branch_52fz,utc_date)

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

# Check stimulus repetitions (rectifying SM18 issue)
PC.session1 %>% group_by(stimulus,ID) %>% summarize(n=length(stimulus))

# Number of participants.
n_distinct(session1$ID) # N=108 (6 subjects data rejected before analysis for non-compliance)

# Session 2
# Set working directory.
setwd("../session2")

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
                   zone_type,reaction_time,response,attempt,correct,display,fname,branch_ee59,branch_52fz,utc_date)

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

# Number of participants.
n_distinct(session2$ID) # N=99

# Clean up.
rm(data)

 # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Combine data frames.
PC.both.complete <- rbind(PC.session1,PC.session2)
PC.both.complete$ID <- as.factor(PC.both.complete$ID)

# Only include ID's who completed both sessions.
DNF <- PC.both.complete %>%
  group_by(ID) %>%
  summarise(Count = n_distinct(session))

DNF <- subset(DNF,Count==1)

PC.both.complete <- PC.both.complete[ ! PC.both.complete$ID %in% DNF$ID, ]

n_distinct(PC.both.complete$ID) # N = 98 who completed both sessions

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
n_distinct(PC.both.complete$ID) # N=83

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
n_distinct(exclude$ID) # N = 20

# Remove those participants.
PC.both.complete <- PC.both.complete[ ! PC.both.complete$ID %in% exclude$ID, ]

# Check how many subjects reman after excluding based on both headphone check fails and acccuracy.
n_distinct(PC.both.complete$ID) # N = 63

# Check distribution of presentation order.
PC.both.complete %>%
  group_by(order) %>%
  summarise(Count = n_distinct(ID)) # S_SH n=32, SH_S n=31

# Calculate statistics about length between session 1 and 2.
library(lubridate)

# Convert to date.
PC.both.complete$utc_date <- as.Date(dmy_hms(PC.both.complete$utc_date))

# Summarise. 
dates <- PC.both.complete %>%
  select(ID,session,utc_date) %>% 
  group_by(ID,session) %>% 
  summarise(date=mean(utc_date)) %>% 
  spread(session,date)

# Compute difference and get descriptives.
dates$diff <- dates$`Session 2`-dates$`Session 1`
Rmisc::summarySE(dates,measurevar = "diff")
range(dates$diff)

#### Session 1 ####
# 
# # Break out session 1 from cleaned up combined data.
# PC.session1 <- subset(PC.both.complete,session=="Session 1")
# 
# # Check counts of stimuli.
# Counts <- PC.session1 %>%
#   group_by(ID) %>%
#   summarise(Count = length(ID))
# 
# # Counts by Order; n = X in each order group.
# PC.session1 %>%
#   group_by(order) %>%
#   summarise(Count = n_distinct(ID))
# 
# # Plot categorization data.
# PC.session1.figure.stats <- Rmisc::summarySE(PC.session1, measurevar="resp1",groupvars = c("step","bias","order"))
# 
# # By order.
# ggplot(PC.session1.figure.stats, aes(x=as.numeric(step), y=resp1,color=as.factor(bias),linetype=as.factor(order))) +
#   geom_point(stat='summary', fun.y='mean', size=3,alpha=0.7) +
#   geom_line(stat='summary', fun.y='mean', size=1.25, alpha=0.7) +
#   geom_errorbar(aes(ymin=resp1-se,ymax=resp1+se),width=.3) +
#   scale_x_continuous('Continuum step', breaks=c(1:7)) +
#   scale_y_continuous('Percent "sign" responses', breaks=c(0,0.25,0.5,0.75,1), labels=c(0,25,50,75,100)) +
#   scale_color_manual('Biasing Condition', labels=c('S-bias','SH-Bias'),values=c("#B03A2E","#2874A6")) +
#   scale_linetype_manual('Order', labels=c('S-SH','SH-S'),values=c("solid","dotted")) +
#   coord_cartesian(ylim=c(0,1)) +
#   theme(text = element_text(size=20))
# 
# # Prep data for models.
# PC.session1$ID <- as.factor(PC.session1$ID)
# PC.session1$bias <- as.factor(PC.session1$bias)
# PC.session1$step <- scale(PC.session1$step)
# PC.session1$order <- as.factor(PC.session1$order)
# contrasts(PC.session1$bias) = contr.sum(2)
# contrasts(PC.session1$order) = contr.sum(2)
# 
# # Build models.
# model1 <- mixed(resp1 ~ step*bias*order + 
#                  (bias*step||ID) + (bias||ID) + (step||ID), family=binomial(link="logit"),data=PC.session1,method="LRT",expand_re = TRUE,
#                 control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 150000)))
# 
# model2 <- mixed(resp1 ~ step*bias*order + 
#                   (bias||ID) + (step||ID), family=binomial(link="logit"),data=PC.session1,method="LRT",expand_re = TRUE,
#                 control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 150000)))
# 
# model3 <- mixed(resp1 ~ step*bias*order + 
#                   (bias||ID), family=binomial(link="logit"),data=PC.session1,method="LRT",expand_re = TRUE,
#                 control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 150000)))
# 
# model4 <- mixed(resp1 ~ step*bias*order + 
#                   (1|ID), family=binomial(link="logit"),data=PC.session1,method="LRT",expand_re = TRUE,
#                 control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 150000)))
# 
# # Compare models.
# anova(model1,model2,model3,model4)
# 
# # Summarize model2.
# model2
# 
# # Explore Bias x Order interaction.
# BiasOrder <- interaction(PC.session1$bias,PC.session1$order)
# m1 <- glmer(resp1 ~ BiasOrder + (1|ID),
#             data=PC.session1, family='binomial', control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 200000)))
# posthoc <- multcomp::glht(m1, linfct=multcomp::mcp(BiasOrder="Tukey"))
# summary(posthoc,test=multcomp::adjusted("bonferroni")) # Interaction occuring as a result of differences in biasing effect on SH-bias block across orders.
# 
#### Session 2 ####

# # Break out session 2 PC data from combined data with only participants who completed both days.
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

# # Prep data for models.
PC.session2$ID <- as.factor(PC.session2$ID)
PC.session2$bias <- as.factor(PC.session2$bias)
PC.session2$step <- scale(PC.session2$step)
PC.session2$order <- as.factor(PC.session2$order)

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
ggsave("replication_PC.png",device="png",dpi="retina",type="cairo")

# Calculate stats for subject plot.
PC.both.complete.stats.subj <- Rmisc::summarySE(data = PC.both.complete, measurevar="resp1",groupvars = c("ID","step","bias","session"))

# Plot.
ggplot(PC.both.complete.stats.subj, aes(x=step, y=resp1, colour=factor(bias),linetype=factor(session))) +
  geom_point(stat='summary', fun.y='mean', size=2.5) +
  geom_line(stat='summary', fun.y='mean', size=0.75) +
  geom_errorbar(aes(ymin=resp1-se,ymax=resp1+se),width=.5) +
  facet_wrap(~ID) +
  scale_x_continuous('Continuum step', breaks=c(1:7)) +
  scale_y_continuous('Percent "sign" responses', breaks=c(0,0.25,0.5,0.75,1), labels=c(0,25,50,75,100)) +
  scale_color_manual('Biasing Condition', labels=c('S-Bias','SH-Bias'),values=c("#B03A2E","#2874A6")) +
  #scale_linetype_manual('Session', labels=c('S-SH','SH-S'),values=c("solid","dotted")) +
  coord_cartesian(ylim=c(0,1)) + 
  theme(text = element_text(size=14))

# Prep models.
PC.both.complete$bias <- as.factor(PC.both.complete$bias)
PC.both.complete$order <- as.factor(PC.both.complete$order)
PC.both.complete$session <- as.factor(PC.both.complete$session)
PC.both.complete$step <- scale(PC.both.complete$step)

# Build models.
model1 <- mixed(resp1 ~ step*bias*order*session + 
                  (bias:step:session||ID) + (bias||ID) + (step||ID) + (session||ID), family=binomial(link="logit"),data=PC.both.complete,method="LRT",expand_re = TRUE,
                control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 1500000)))

model2 <- mixed(resp1 ~ step*bias*order*session + 
                  (bias:step:session||ID), family=binomial(link="logit"),data=PC.both.complete,method="LRT",expand_re = TRUE,
                control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 1500000)))

model3 <- mixed(resp1 ~ step*bias*order*session + 
                  (bias||ID) + (step||ID) + (session||ID), family=binomial(link="logit"),data=PC.both.complete,method="LRT",expand_re = TRUE,
                control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 1500000)))

model4 <- mixed(resp1 ~ step*bias*order*session + 
                  (bias||ID) + (step||ID), family=binomial(link="logit"),data=PC.both.complete,method="LRT",expand_re = TRUE,
                control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 1500000)))

model5 <- mixed(resp1 ~ step*bias*order*session + 
                  (session||ID) + (step||ID), family=binomial(link="logit"),data=PC.both.complete,method="LRT",expand_re = TRUE,
                control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 1500000)))

model6<- mixed(resp1 ~ step*bias*order*session + 
                  (bias||ID) + (session||ID), family=binomial(link="logit"),data=PC.both.complete,method="LRT",expand_re = TRUE,
                control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 1500000)))

model7<- mixed(resp1 ~ step*bias*order*session + 
                 (bias||ID), family=binomial(link="logit"),data=PC.both.complete,method="LRT",expand_re = TRUE,
               control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 1500000)))

model8 <- mixed(resp1 ~ step*bias*order*session + 
                  (1|ID), family=binomial(link="logit"),data=PC.both.complete,method="LRT",expand_re = TRUE,
                control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 1500000)))

# Compare models.
anova(model1,model2,model3,model4,model5,model6,model7,model8)

# Summarize model1.
model1
summary(model1)

# Explore interactions
# First, simple effects to see which order bias effect is significant at.
biasXorder <- emmeans(model1, ~bias*order,interaction="pairwise")
pairs(biasXorder,simple="bias")

# Is there anything to trend that bias by order interaction is weaker in session 2?
biasordersession <- emmeans(model1,~bias|session*order)
con1 <- contrast(biasordersession, interaction = "pairwise")
pairs(con1, by = NULL)

followup <- mixed(resp1 ~ step + (session/bias*order) + 
                    (bias:step:session||ID) + (bias||ID) + (step||ID) + (session||ID), family=binomial(link="logit"),data=PC.both.complete,method="LRT",expand_re = TRUE,
                  control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 1500000)))
summary(followup)

#### Within-subject stability ####
# Fit psychometric functions to the PC data.
session.subject.curves <- quickpsy(PC.both.complete, step, resp1, 
                                   grouping = .(ID,session,bias), 
                                   fun = logistic_fun,
                                   lapses = FALSE,
                                   guess = FALSE,
                                   bootstrap = "nonparametric", 
                                   optimization = "optim",
                                   B = 500)

plotcurves(session.subject.curves,color=bias) + facet_wrap(~ID) + theme(legend.position = "NONE",)

 # Organize and clean up quickpsy output.
boundaries <- as.data.frame(session.subject.curves$par)
boundaries[6:7] <- list(NULL)
boundaries <- boundaries %>% spread(parn, par, drop=TRUE)
colnames(boundaries)[4] <- "Boundary"
boundaries[5] <- NULL

# Add in 'order' and save.
boundaries$order <- with(PC.both.complete,order[match(boundaries$ID,ID)])
save(boundaries,file="boundaries.Rda")
load("boundaries.Rda")

# Plot boundary changes
ggplot(boundaries,aes(x=bias,y=Boundary)) + 
  geom_point(aes(color=bias,group=ID), size=2.5) + 
  geom_line(aes(group=ID,x=bias,linetype=order),size=0.75) +
  facet_wrap(~session)

# Create measure of shift by subtracting SH-bias boundary from S-bias boundary.
boundaries <- boundaries %>% spread(bias, Boundary, drop=TRUE)
boundaries$shift <- boundaries$`S-Bias`-boundaries$`SH-Bias`

# Scatter plot of shift by subject
ggplot(unstack(boundaries,shift~session),aes(x=Session.1,y=Session.2)) + 
  geom_point(size=2) + geom_smooth(method=lm) + stat_cor() +
  xlab("Session 1 shift size") + ylab("Session 2 shift size") + 
  theme(text = element_text(size=14))
ggsave("shift_scatter.png",device="png",dpi="retina",type="cairo")

# Filter only learning order (SH-S).
learn.corr <- filter(boundaries,order=="SH_S")
learn.corr <- unstack(learn.corr,shift~session)
cor.test(learn.corr$Session.1,learn.corr$Session.2)

# Filter only non-learning order (S-SH).
nonlearn.corr <- filter(boundaries,order=="S_SH")
nonlearn.corr <- unstack(nonlearn.corr,shift~session)
cor.test(nonlearn.corr$Session.1,nonlearn.corr$Session.2)

# Correlation across orders.
group.corr <- unstack(boundaries,shift~session)
cor.test(group.corr$Session.1,group.corr$Session.2)

#### Acoustic data ####

# Load data
acoustics <- read.csv("../Sounds/Acoustic Data/Drouin-ET-AL-2016-Acoustic-Measurements.csv",header=TRUE)
acoustics <- subset(acoustics,Block=="Exposure")

# Create dataframe with theoretical boundary values for vertical lines in figures
vlines <- data.frame(bound=c(3750,5100),Bias=c("s-bias","sh-bias"))

localstatistics1 <- ggplot(acoustics,aes(x=Center.Gravity.Hz,fill=Phoneme,color=Phoneme,linetype=Type)) + 
  geom_density(aes(color=Phoneme,fill=Phoneme,linetype=Type),size=1.1,alpha=0.2) + 
  geom_vline(data=vlines,aes(xintercept=bound),linetype="dashed") +
  facet_wrap(~Bias) + 
  scale_color_manual('Phoneme', labels=c('s','sh'),values=c("#B03A2E","#2874A6")) + 
  scale_fill_manual('Phoneme', labels=c('s','sh'),values=c("#B03A2E","#2874A6")) +
  scale_linetype_manual('Type',labels=c("Modified","Natural"),values=c("dashed","solid")) + 
  theme(text = element_text(size=14),axis.text.y = element_blank(),
        axis.title.y = element_blank(),axis.ticks.y = element_blank(),
        legend.position="none",axis.title.x = element_blank(),plot.title = element_text(hjust = 0.5)) +
  ggtitle("Block 1: Recent Statistics")

localstatistics2 <- ggplot(acoustics,aes(x=Center.Gravity.Hz,fill=Phoneme,color=Phoneme,linetype=Type)) + 
  geom_density(aes(color=Phoneme,fill=Phoneme,linetype=Type),size=1.1,alpha=0.2) + 
  geom_vline(data=vlines,aes(xintercept=bound),linetype="dashed") +
  facet_wrap(~factor(Bias,levels=c("sh-bias","s-bias"))) + 
  scale_color_manual('Phoneme', labels=c('s','sh'),values=c("#B03A2E","#2874A6")) + 
  scale_fill_manual('Phoneme', labels=c('s','sh'),values=c("#B03A2E","#2874A6")) +
  scale_linetype_manual('Type',labels=c("Modified","Natural"),values=c("dashed","solid")) + 
  theme(text = element_text(size=14),axis.text.y = element_blank(),
        axis.title.y = element_blank(),axis.ticks.y = element_blank(),
        legend.position = "bottom", legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(10,10,10,10),axis.title.x = element_blank(),plot.title = element_text(hjust = 0.5)) +
  ggtitle("Block 2: Recent Statistics")

# For plotting purposes, duplicate data and create dummy plotting variable to facet global statistics figure
acoustics <- rbind(acoustics,acoustics)
acoustics$plotting[1:80]<-1
acoustics$plotting[81:160]<-2

globalstatistics <- ggplot(acoustics,aes(x=Center.Gravity.Hz,fill=Phoneme,color=Phoneme)) + 
  geom_density(aes(color=Phoneme,fill=Phoneme),size=1.1,alpha=0.2) + 
  geom_vline(aes(xintercept=4425),linetype="dashed") +
  facet_wrap(~plotting) +
  scale_color_manual('Phoneme', labels=c('s','sh'),values=c("#B03A2E","#2874A6")) + 
  scale_fill_manual('Phoneme', labels=c('s','sh'),values=c("#B03A2E","#2874A6")) +
  xlab("Spectral center (Hz)") +
  theme(text = element_text(size=14),axis.text.y = element_blank(),
        axis.title.y = element_blank(),axis.ticks.y = element_blank(),legend.position = "NONE",
        plot.title = element_text(hjust = 0.5),strip.background = element_blank(),
        strip.text.x = element_blank()) +
  ggtitle("Block 2: Global Statistics")

# Visualize 
fig1<-plot_grid(localstatistics1,localstatistics2,globalstatistics,ncol=1,rel_heights = c(1,1.2,1)) +
  draw_text("Order SH-S",hjust = -0.5, vjust = -31.5) + draw_text("Order S-SH",hjust = 3.5, vjust = -31.5)

ggsave("../Figures/fig1.png",device="png",dpi="retina",type="cairo",height = 10,width = 8)

# Fig 4
ggplot(acoustics,aes(x=Center.Gravity.Hz,fill=Phoneme,color=Phoneme,linetype=Type)) + 
  geom_density(aes(color=Phoneme,fill=Phoneme,linetype=Type),size=1.1,alpha=0.2) + 
  scale_color_manual('Phoneme', labels=c('s','sh'),values=c("#B03A2E","#2874A6")) + 
  scale_fill_manual('Phoneme', labels=c('s','sh'),values=c("#B03A2E","#2874A6")) +
  scale_linetype_manual('Type',labels=c("Modified","Natural"),values=c("dashed","solid")) + 
  xlab("Spectral center (Hz)") +
  geom_vline(data=acoustics %>% group_by(Phoneme,Type) %>% summarize(avg=mean(Center.Gravity.Hz)),aes(xintercept=avg,color=Phoneme),linetype="dotted",size=1.25) + 
  theme(text = element_text(size=14),axis.text.y = element_blank(),
        axis.title.y = element_blank(),axis.ticks.y = element_blank())

ggsave("../Figures/fig4.png",device="png",dpi="retina",type="cairo",width=8,height=4,units="in")

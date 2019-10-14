
rm(list = ls(all = TRUE))

library(lme4)
library(ggplot2)
library(plyr)
library(R.utils)
library(nlme)
library(car)
library(phia)
library(multcomp)
library(Rmisc)
library(quickpsy)

theme_set(theme_bw())

##############################################################################
# Load data

data <- as.data.frame(read.delim(file='SM2018_data.txt', sep='\t'))
head(data)

# drop subjects
data <- subset(data,exclude==1)

# drop NA responses
data <- na.omit(data)

# separate data by session
data_session1 <- subset(data,session==1)
data_session2 <- subset(data,session==2)

# endpoint summary
data2 <- subset(data, step==1 | step==7)
endpointSummary <- ddply(data2, .(subject,step), summarize, endpointResp=mean(resp1))
endpointSummary <- ddply(endpointSummary, .(step), summarize, endpointResp=mean(endpointResp))
head(endpointSummary)

# prep data
data_session1$subject <- as.factor(data_session1$subject)
data_session1$bias <- as.factor(data_session1$bias)
data_session1$step <- scale(data_session1$step)
data_session1$order <- as.factor(data_session1$order)
#data_session1$rc <- as.factor(data_session1$rc)
contrasts(data_session1$bias) = contr.sum(2)
#contrasts(data_session1$rc) = contr.sum(2)
contrasts(data_session1$order) = contr.sum(2)
#data_session1$TRIAL_INDEX <- as.factor(data_session1$TRIAL_INDEX)

# session 1 analysis
# figure out random effects structure
model1 <- glmer(resp1 ~ step*bias*order +
      (bias:step||subject) + (bias||subject) + (step||subject),
      data=data_session1, family='binomial', control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 200000)))
model2 <- glmer(resp1 ~ step*bias*order + 
      (bias||subject) + (step||subject),
      data=data_session1, family='binomial', control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 200000)))
model3 <- glmer(resp1 ~ step*bias*order +
      (bias||subject),
      data=data_session1, family='binomial', control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 200000)))
model4 <- glmer(resp1 ~ step*bias*order +
      (1|subject),
      data=data_session1, family='binomial', control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 200000)))   

# compare models
anova(model1,model2,model3,model4)

# summarize model
summary(model2)

# post-hoc for revision
BiasOrder <- interaction(data_session1$bias2,data_session1$order)
m1 <- glmer(resp1 ~ BiasOrder + (1|subject),
      data=data_session1, family='binomial', control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 200000)))
posthoc <- glht(m1, linfct=mcp(BiasOrder="Tukey"))
summary(posthoc,test=adjusted("bonferroni"))

# figures

data <- as.data.frame(read.delim(file='BlockPL2_data.txt', sep='\t'))
data <- subset(data,exclude==1)
data_session1 <- subset(data,session==1)
data <- na.omit(data)
data_session1$sessionLabel <- ifelse(data_session1$session==1,'Session 1',NA)
data_session1$sessionLabel <- ifelse(data_session1$session==2, 'Session 2',data_session1$sessionLabel)
data_session1$biasLabel <- ifelse(data_session1$bias==1,'CAT_S',NA)
data_session1$biasLabel <- ifelse(data_session1$bias==2,'CAT_SH',data_session1$biasLabel)
data_session1$Session <- ifelse(data_session1$biasxsession==11,'S Block Session 1',NA)
data_session1$Session <- ifelse(data_session1$biasxsession==12,'S Block Session 2',data_session1$Session)
data_session1$Session <- ifelse(data_session1$biasxsession==21,'SH Block Session 1',data_session1$Session)
data_session1$Session <- ifelse(data_session1$biasxsession==22,'SH Block Session 2',data_session1$Session)
data_session1$Order <- ifelse(data_session1$oc2==1, 'S Block First',NA)
data_session1$Order <- ifelse(data_session1$oc2==2, 'SH Block Second',data_session1$oc2)
data_session1$Order <- ifelse(data_session1$oc2==3, 'S Block Second',data_session1$oc2)
data_session1$Order <- ifelse(data_session1$oc2==4, 'SH Block First',data_session1$oc2)
data_session1$subject <- as.factor(data_session1$subject)
 
 # bias
ggplot(data_session1, aes(x=step, y=resp1,linetype=factor(order),colour=factor(bias))) + 
  geom_point(stat='summary', fun.y=mean, size=2.5,) +
  geom_line(stat='summary', fun.y=mean, size=0.75) +
  scale_x_continuous('S to SH continuum step', breaks=c(1:7)) +
  scale_y_continuous('Percent /sign/ responses', breaks=c(0,0.25,0.5,0.75,1), labels=c(0,25,50,75,100)) +
  scale_colour_manual('Biasing Condition', values=c("red","blue"), labels=c('S block','SH block')) +
  theme(text = element_text(size=14)) +
  coord_cartesian(ylim=c(0,1))

# session 2 analysis
data <- as.data.frame(read.delim(file='BlockPL2_data.txt', sep='\t'))
data <- subset(data,exclude==1)
data <- na.omit(data)
data_session2 <- subset(data,session==2)

# prep data
data_session2$subject <- as.factor(data_session2$subject)
data_session2$bias <- as.factor(data_session2$bias)
data_session2$step <- scale(data_session2$step)
data_session2$order <- as.factor(data_session2$order)
contrasts(data_session2$bias) = contr.sum(2)
contrasts(data_session2$order) = contr.sum(2)


# build models
model1 <- glmer(resp1 ~ step*bias*order +
                  (bias:step||subject) + (bias||subject) + (step||subject), 
                data=data_session2, family='binomial', control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1500000)))
model2 <- glmer(resp1 ~ step*bias*order + 
                  (bias||subject) + (step||subject),
                data=data_session2, family='binomial', control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1500000)))
model3 <- glmer(resp1 ~ step*bias*order +
                  (bias||subject),
                data=data_session2, family='binomial', control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 200000)))
model4 <- glmer(resp1 ~ step*bias*order +
                  (1|subject),
                data=data_session2, family='binomial', control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 200000)))   


    
# compare different models
anova(model1,model2,model3,model4)

# summarize best model
summary(model2)

# figures
ggplot(data_session2, aes(x=step, y=resp1, colour=factor(order))) +
  geom_point(stat='summary', fun.y='mean', size=2.5) +
  geom_line(stat='summary', fun.y='mean', size=0.75) +
  scale_x_continuous('S to SH continuum step', breaks=c(1:7)) +
  scale_y_continuous('Percent /s/ responses', breaks=c(0,0.25,0.5,0.75,1), labels=c(0,25,50,75,100)) +
  coord_cartesian(ylim=c(0,1))

ggplot(data_session2, aes(x=step, y=resp1, colour=factor(bias))) +
  geom_point(stat='summary', fun.y='mean', size=2.5) +
  geom_line(stat='summary', fun.y='mean', size=0.75) +
  scale_x_continuous('S to SH continuum step', breaks=c(1:7)) +
  scale_y_continuous('Percent /sign/ responses', breaks=c(0,0.25,0.5,0.75,1), labels=c(0,25,50,75,100)) +
  scale_colour_manual('Biasing Condition', values=c("red","blue"), labels=c('S block','SH block')) +
  theme(text = element_text(size=14)) +
  coord_cartesian(ylim=c(0,1))

##########################################################################################################################################

# combined data
data <- as.data.frame(read.delim(file='BlockPL2_data.txt', sep='\t'))
data <- subset(data,exclude==1)
data <- na.omit(data)

data$blocknum <- ifelse(data$order=="S_SH"&data$bias2=="CAT_S",1,ifelse(data$order=="S_SH"&data$bias2=="CAT_SH",2,
               ifelse(data$order=="SH_S"&data$bias2=="CAT_S",2,ifelse(data$order=="SH_S"&data$bias2=="CAT_SH",1,""))))

# prep data
data$subject <- as.factor(data$subject)
data$bias <- as.factor(data$bias)
data$session <- as.factor(data$session)
data$step <- scale(data$step)
data$order <- as.factor(data$order)
contrasts(data$bias) = contr.sum(2)
contrasts(data$order) = contr.sum(2)
contrasts(data$session) = contr.sum(2)

# create models
model1 <- glmer(resp1 ~ step*bias*session*order +
                (bias:step:session||subject) + (bias||subject) + (step||subject) + (session||subject), 
                data=data, family='binomial', control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 4000000)))
model2 <- glmer(resp1 ~ step*bias*session*order +
                  (bias:step:session||subject) + (step||subject) + (bias||subject), 
                data=data, family='binomial', control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1500000)))
model3 <- glmer(resp1 ~ step*bias*session*order +
                  (bias:step:session||subject) + (step||subject), 
                data=data, family='binomial', control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1500000)))
model4 <- glmer(resp1 ~ step*bias*session*order + 
                (bias:step|subject) + (session|subject) + (step|subject),
                data=data, family='binomial', control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1500000)))
model5 <- glmer(resp1 ~ step*bias*session*order +
                (bias|subject) + (session|subject) + (step|subject),
                data=data, family='binomial', control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1500000)))
model6 <- glmer(resp1 ~ step*bias*session*order + 
                (session|subject) + (step|subject),
                data=data, family='binomial', control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1500000)))
model7 <- glmer(resp1 ~ step*bias*session*order +
                (1|subject),
                data=data, family='binomial', control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1000000)))

# compare different models
anova(model1,model2,model3,model4,model5,model6,model7)

# summarize best model
summary(model1)
fit1 <- mixed(resp1 ~ step*bias*session*order + 
                (bias:step:session||subject) + (bias||subject) + (step||subject) + (session||subject),
              family=binomial(link="logit"),data=data,method="LRT",
              expand_re = TRUE, control = lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 4000000)))
# post hoc
data <- as.data.frame(read.delim(file='blockPL2_data.txt', sep='\t'))
data <- subset(data,exclude==1)
data <- na.omit(data)
BiasSession <- interaction(data$bias, data$session, drop = TRUE)
m1 <- glmer(resp1 ~ BiasSession + (bias:step|subject) + (session|subject) + (step|subject),
      data=data, family='binomial', control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 75000))) 
BiasSessionTEST <- glht(m1, linfct = mcp(BiasSession = "Tukey"))
BiasSessionTESTCorrected = summary(BiasSessionTEST, test=adjusted('bonferroni'))

data <- as.data.frame(read.delim(file='blockPL2_data.txt', sep='\t'))
data <- subset(data,exclude==1)
data <- na.omit(data)
StepSession <- interaction(data$step, data$session, drop = TRUE)
m1 <- glmer(resp1 ~ StepSession + (bias:step|subject) + (session|subject) + (step|subject),
            data=data, family='binomial', control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 75000))) 
StepSessionTEST <- glht(m1, linfct = mcp(StepSession = "Tukey"))
StepSessionTESTCorrected = summary(StepSessionTEST, test=adjusted('bonferroni'))

model1 <- glmer(resp1 ~ step*bias*session*order - bias +
                  (bias:step:session|subject) + (bias|subject) + (step|subject) + (session|subject), 
                data=data, family='binomial', control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2000000)))

# figures
data <- as.data.frame(read.delim(file='BlockPL2_data.txt', sep='\t'))
data <- subset(data,exclude==1)
data <- na.omit(data)
data$sessionLabel <- ifelse(data$session==1,'Session 1',NA)
data$sessionLabel <- ifelse(data$session==2, 'Session 2',data$sessionLabel)
data$biasLabel <- ifelse(data$bias==1,'CAT_S',NA)
data$biasLabel <- ifelse(data$bias==2,'CAT_SH',data$biasLabel)
data$subject <- as.factor(data$subject)
head(data)

# create figures
stats <- summarySE(data = data, measurevar="resp1",groupvars = c("step","bias","order","sessionLabel","blocknum","subject2"))

ggplot(stats, aes(x=step, y=resp1, colour=factor(bias),linetype=factor(order))) +
  geom_point(stat='summary', fun.y='mean', size=2.5) +
  geom_line(stat='summary', fun.y='mean', size=0.75) +
  geom_errorbar(aes(ymin=resp1-se,ymax=resp1+se),width=.5) +
  facet_wrap(~sessionLabel) +
  scale_x_continuous('S to SH continuum step', breaks=c(1:7)) +
  scale_y_continuous('Percent "sign" responses', breaks=c(0,0.25,0.5,0.75,1), labels=c(0,25,50,75,100)) +
  scale_color_manual('Biasing Condition', labels=c('"sh"-block','"s"-block'),values=c("#2874A6","#B03A2E")) +
  scale_linetype_manual('Order', labels=c('S-SH','SH-S'),values=c("solid","dashed")) +
  coord_cartesian(ylim=c(0,1)) + 
  theme(text = element_text(size=14))

ggplot(stats, aes(x=step, y=resp1, colour=factor(blocknum),linetype=factor(order))) +
  geom_point(stat='summary', fun.y='mean', size=2.5) +
  geom_line(stat='summary', fun.y='mean', size=0.75) +
  geom_errorbar(aes(ymin=resp1-se,ymax=resp1+se),width=.5) +
  facet_wrap(~blocknum) +
  scale_x_continuous('S to SH continuum step', breaks=c(1:7)) +
  scale_y_continuous('Percent "sign" responses', breaks=c(0,0.25,0.5,0.75,1), labels=c(0,25,50,75,100)) +
 # scale_color_manual('Biasing Condition', labels=c('"sh"-block','"s"-block'),values=c("#2874A6","#B03A2E")) +
  scale_linetype_manual('Order', labels=c('S-SH','SH-S'),values=c("solid","dashed")) +
  coord_cartesian(ylim=c(0,1)) + 
  theme(text = element_text(size=14))

ggplot(data, aes(x=step, y=resp1, colour=factor(bias))) +
  geom_point(stat='summary', fun.y='mean', size=2.5) +
  geom_line(stat='summary', fun.y='mean', size=0.75) +
  facet_wrap(~sessionLabel) +
  scale_x_continuous('S to SH continuum step', breaks=c(1:7)) +
  scale_y_continuous('Percent "sign" responses', breaks=c(0,0.25,0.5,0.75,1), labels=c(0,25,50,75,100)) +
  scale_color_manual('Biasing Condition', labels=c('"sh"-block','"s"-block'),values=c("#2874A6","#B03A2E")) +
  scale_linetype_manual('Order', labels=c('S-SH','SH-S'),values=c("solid","dotted")) +
  coord_cartesian(ylim=c(0,1)) + 
  theme(text = element_text(size=14))

ggplot(data, aes(x=step, y=resp1, colour=factor(oc2))) +
  geom_point(stat='summary', fun.y='mean', size=2.5) +
  geom_line(stat='summary', fun.y='mean', size=0.75) +
  scale_x_continuous('S to SH continuum step', breaks=c(1:7)) +
  scale_y_continuous('Percent /s/ responses', breaks=c(0,0.25,0.5,0.75,1), labels=c(0,25,50,75,100)) +
  coord_cartesian(ylim=c(0,1))

ggplot(data, aes(x=step, y=resp1, colour=factor(biasxsession))) +
  geom_point(stat='summary', fun.y='mean', size=2.5) +
  geom_line(stat='summary', fun.y='mean', size=0.75) +
  facet_wrap(~order2) +
  scale_color_manual('Bias x Session', labels=c('S Session 1','S Session 2','SH Session 1','SH Session 2'),values=c("#B03A2E","#F1948A","#2874A6","#AED6F1")) +
  scale_x_continuous('S to SH continuum step', breaks=c(1:7)) +
  scale_y_continuous('Percent "sign" responses', breaks=c(0,0.25,0.5,0.75,1), labels=c(0,25,50,75,100)) +
  theme(text = element_text(size=14)) +
  coord_cartesian(ylim=c(0,1))
ggsave('day2_session.pdf')

ggplot(data, aes(x=step, y=RT, colour=factor(bias))) +
  geom_point(stat='summary', fun.y='mean', size=2.5) +
  geom_line(stat='summary', fun.y='mean', size=0.75) +
  scale_x_continuous('S to SH continuum step', breaks=c(1:7)) +
  scale_y_continuous('Response time',breaks=c(1175,1200,1225,1250,1275,1300)) +
  scale_colour_discrete('Bias', labels=c('S Bias','SH Bias'))

ggplot(data, aes(x=step, y=resp1, color=factor(bias))) +
  geom_line(aes(linetype=bias,color=factor(subject)), stat='summary', fun.y='mean', size=0.75) +
  scale_x_continuous('S to SH continuum step', breaks=c(1:7)) +
  scale_y_continuous('Percent /s/ responses', breaks=c(0,0.25,0.5,0.75,1), labels=c(0,25,50,75,100)) +
  scale_color_discrete('Subject', guide=FALSE) +
  ggtitle('Day 2')
coord_cartesian(ylim=c(0,1))

# load data 
data <- as.data.frame(read.delim(file='BlockPL2_data.txt', sep='\t'))
head(data)

# drop subjects
data <- subset(data,exclude==1)

# drop NA responses
data <- na.omit(data)

# subset steps 2-6
data3 <- subset(data, step==2|step==3|step==4|step==5|step==6)

# prep data
data3$subject <- as.factor(data3$subject)
data3$bias <- recode(data3$bias,"'CAT_S'=1")
data3$bias <- recode(data3$bias,"'CAT_SH'=2")
data3$bias <- as.factor(data3$bias)
data3$session <- as.factor(data3$session)
data3$step <- scale(data3$step)
data3$order <- recode(data3$order,"'S_SH'=1")
data3$order <- recode(data3$order,"'SH_S'=2")
data3$order <- as.factor(data3$order)

# figure out random effects structure
model1 <- glmer(resp1 ~ step*bias*session +
                  (bias:step:session|subject) + (bias|subject) + (step|subject) + (session|subject), 
                data=data3, family='binomial', control = glmerControl(optCtrl = list(maxfun = 200000)))
model2 <- glmer(resp1 ~ step*bias*session + 
                  (bias:step|subject) + (session|subject) + (step|subject),
                data=data3, family='binomial', control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 200000)))
model3 <- glmer(resp1 ~ step*bias*session +
                  (bias|subject) + (session|subject) + (step|subject),
                data=data3, family='binomial', control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 200000)))
model4 <- glmer(resp1 ~ step*bias*session + 
                  (session|subject) + (step|subject),
                data=data3, family='binomial', control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 200000)))
model5 <- glmer(resp1 ~ step*bias*session*order +
                  (1|subject),
                data=data3, family='binomial', control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 200000)))



ggplot(data,aes(RT,colour=step)) + geom_density(adjust=1/6) + coord_cartesian(xlim=c(250,3000)) + scale_x_continuous(breaks=c(0,250,500,750,1000,1250,1500,1750,2000)) + scale_colour_discrete()
)


library(ggplot2)
library(lme4)
library(effects)
library(bbmle)
library(tidyr)
library(insight)
library(performance)
library(modelr)
library(sjPlot)
library(agricolae)



setwd("~/Desktop")
data <- read.csv("lysozyme activity.csv")

###Descriptives
means <- aggregate(x=data$Activity,
                   by=list(data$Species, data$pH),
                   FUN=mean)
st.err <- function(x) {sd(x)/sqrt(length(x))}
se <- aggregate(x=data$Activity,
                by=list(data$Species, data$pH),
                FUN=st.err)

means <- cbind(means, se$x)
names(means) <- c("Species", "pH", "Activity", "SE")


plot <- ggplot(aes(x=pH, y=Activity, group=Species, color=Species, shape=Species), data=means) + geom_point(size=4) +  geom_line(size=1) + theme_classic() + geom_pointrange(aes(ymin=Activity-SE, ymax=Activity+SE), width=.1, position=position_dodge(0.0005))
plot + scale_color_manual(values=c('#FFB81C','#003594', 'grey')) + theme(legend.position='none')



  ##GLM
  null <- lmer(Activity ~ (1|ID), data=data)
  m1 <- lmer(Activity ~ Species + (1|ID), data=data)
  m2 <- lmer(Activity ~ pH + (1|ID), data=data)
  m3 <- lmer(Activity ~ Species + pH + (1|ID), data=data)
  m4 <- lmer(Activity ~ Species * pH + (1|ID), data=data)
  
  
  AICctab(null,m1,m2,m3, m4, base =T, weights = T)
  
  summary(m4)
  
  tab_model(m4, digits = 2, show.df = TRUE, show.p = TRUE, show.se = TRUE)

  anova <- aov(lmer(Activity ~ Species * pH + (1|ID), data=data))
  
  anova <- aov(Activity ~ Species*pH, data=data)
summary(anova)  

TukeyHSD(anova, which="Species")
TukeyHSD(anova, which = "pH")
TukeyHSD(anova)

LSD.test(Activity, Species, 22, )

# one wave or another in the pandemic

require('lme4')
require('lmerTest')
require('ggpubr')
require('effects')
require('emmeans')
require('ggplot2')
require('plyr')
require('Rmisc')
require('sjstats')
require('effsize') 
require('reshape2')
require('jtools')
require('MuMIn')
require('ggeffects')
require('car')
require('olsrr')

# load datasets
df2 <- read.csv2("Google Drive/1_PhD/1_progetti/23_covid/data_fase2.csv",header=T,sep=";",dec=",",fill = T)
an2 <- read.csv2("Google Drive/1_PhD/1_progetti/23_covid/an_fase2.csv",header=T,sep=";",dec=",",fill = T)
f2 <- read.csv2("Google Drive/1_PhD/1_progetti/23_covid/factors2.csv",header=T,sep=";",dec=",",fill = T)

# merge them
df2 <- merge(df2,an2)
df2 <- merge(df2,f2)

# remove the only emotion not belonging to a factor
df2 <- df2[df2$Type != "Solo",]

# create subset of the four factors
dff1 <- subset(df2, Factor == 'one')
dff2 <- subset(df2, Factor == 'two')
dff3 <- subset(df2, Factor == 'three')
dff4 <- subset(df2, Factor == 'four')

# data analysis

# factor 1
m1 <- lmer(Intensit_Risp ~ Media_Ludiche * Media_utile * Media_vitanor + (1|ID) + (1|Type), data = dff1)

# model selection
options(na.action = "na.fail")
aa <- dredge(m1, rank = AIC)

aa

# best model
m1 <- lmer(Intensit_Risp ~ Media_vitanor + (1|ID) + (1|Type), data = dff1)

# model criticism
fit.mc <- lmer(Intensit_Risp ~ Media_vitanor + (1|ID) + (1|Type),subset=abs(scale(resid(m1)))<2.5, dff1)

summ(fit.mc)

# plot results
plot(predictorEffects(fit.mc, ~ Media_vitanor),
     main = "",
     axes = list(
       grid = T,
       x = list(rotate = 30,rug = F),
       y = list(rotate = 30,rug = F,type = "response", lim = c(0,4))),
     lines=list(multiline=TRUE, col = c('royalblue','red2','green3','orange')),
     confint=list(style="auto"),
     lattice=list(layout=c(1, 1)))


# factor 2
m2 <- lmer(Intensit_Risp ~ Media_Ludiche * Media_utile * Media_vitanor + (1|ID) + (1|Type), data = dff2)

# model selection
options(na.action = "na.fail")
bb <- dredge(m2, rank = AIC)

bb

# best model
m2 <- lmer(Intensit_Risp ~ Media_utile + Media_vitanor + (1|ID) + (1|Type), data = dff2)

# model criticism
fit.mc2 <- lmer(Intensit_Risp ~ Media_utile + Media_vitanor + (1|ID) + (1|Type),subset=abs(scale(resid(m2)))<2.5, dff2)

summ(fit.mc2)

# plot results
plot(predictorEffects(fit.mc2, ~ Media_utile + Media_vitanor),
     main = "",
     axes = list(
       grid = T,
       x = list(rotate = 30,rug = F),
       y = list(rotate = 30,rug = F,type = "response", lim = c(0,4))),
     lines=list(multiline=TRUE, col = c('royalblue','red2','green3','orange')),
     confint=list(style="auto"),
     lattice=list(layout=c(1, 1)))

# factor 3
m3 <- lmer(Intensit_Risp ~ Media_Ludiche * Media_utile * Media_vitanor + (1|ID) + (1|Type), data = dff3)

# model selection
options(na.action = "na.fail")
cc <- dredge(m3, rank = AIC)

cc

# best model
m3 <- lmer(Intensit_Risp ~ Media_vitanor + (1|ID) + (1|Type), data = dff3)

# model criticism
fit.mc3 <- lmer(Intensit_Risp ~ Media_vitanor + (1|ID) + (1|Type),subset=abs(scale(resid(m3)))<2.5, dff3)

summ(fit.mc3)

# plot results
plot(predictorEffects(fit.mc3, ~ Media_vitanor),
     main = "",
     axes = list(
       grid = T,
       x = list(rotate = 30,rug = F),
       y = list(rotate = 30,rug = F,type = "response", lim = c(0,4))),
     lines=list(multiline=TRUE, col = c('royalblue','red2','green3','orange')),
     confint=list(style="auto"),
     lattice=list(layout=c(1, 1)))

# factor 4
m4 <- lmer(Intensit_Risp ~ Media_Ludiche * Media_utile * Media_vitanor + (1|ID) + (1|Type), data = dff4)

# model selection
options(na.action = "na.fail")
dd <- dredge(m4, rank = AIC)

dd

# random effect model is the best one
#### Stats for fly simulation

rm(list=ls()) # Clear memory

#load libraries
library(dplyr)
library(car)
library(foreign)
library(ggplot2)
library(ggpubr)

#gittest
2+2

#read in data & creating RDS
# grid <- read.csv('simulations/grid_l20.csv')
# redgrid <-read.csv('simulations/redgrid_l20.csv')
# 
# dat <- bind_rows(grid, redgrid)
# tail(dat)
# 
# rand1 <- read.csv('simulations/rand1_l20.csv')
# 
# dat <- bind_rows(dat, rand1)
# tail(dat)
# 
# rand2 <- read.csv('simulations/rand2_l20.csv')
# dat <- bind_rows(dat, rand2)
# 
# tail(dat)
# 
# rand3 <- read.csv('simulations/rand3_l20.csv')
# dat <- bind_rows(dat, rand3)
# tail(dat)
# 
# exp1 <- read.csv('simulations/expert1_l20.csv')
# dat <- bind_rows(dat, exp1)
# tail(dat)
# 
# exp2<- read.csv('simulations/expert2_l20.csv')
# dat <- bind_rows(dat, exp2)
# tail(dat)
# 
# eff <- read.csv('simulations/eff_l20.csv')
# dat <- bind_rows(dat, eff)
# tail(dat)
# 
# ineff2 <- read.csv('simulations/ineff2_l20.csv')
# dat <- bind_rows(dat, ineff2)
# tail(dat)
# 
# str(dat)
# dat <- dat %>%
#   dplyr::mutate(type = as.factor(type))%>%
#   dplyr::mutate(lure = as.factor(lure))%>%
#   glimpse()
# 
# saveRDS(dat, 'flies1.RDS')
# CTRL + SHIFT + C to hash a whole section

########################################################################
## difference between 1000 or 5000 simulations?
# dat <- readRDS('flies1.RDS')
# 
# # summary(dat)
# # 
# kilo <- dat %>%
#   dplyr::filter(sim < 1001)%>%
#   glimpse()
# tail(kilo)
# 
# meank <- tapply(kilo$ntrees, kilo$type, mean)
# meank
# sdk <- tapply(kilo$ntrees, kilo$type, sd)
# sdk
# medk <- tapply(kilo$ntrees, kilo$type, median)
# medk
# 
# aggregate(kilo, by=list(kilo$type), mean)
# aggregate(kilo, by=list(kilo$type), sd)

# meandat <- tapply(dat$time, dat$type, mean)
# meandat
# sddat <- tapply(dat$time, dat$type, sd)
# sddat
# meddat <- tapply(dat$time, dat$type, median)
# meddat


##############################################
#K-S Test 

dat <- readRDS('flies1.RDS')%>%
  dplyr::filter(sim < 1001)%>%
  glimpse()

#KS between grid and each other surveillance type

df <- dat %>%
  dplyr::filter(type %in% c("grid", "redgrid"))%>%
  glimpse()


ks.test(df$ntrees~df$type)


####################### Comparing means - ANOVA
qqPlot(flies1$time)
qqPlot(flies1$ntrees)
shapiro.test(flies1$time)

mod <- aov(time~type, data = dat)
#Anova(mod)
resid <- residuals(mod)
plot(resid) 
qplot(resid) 
qqnorm(resid)
qqline(resid)

## not normal for trees or time
kruskal.test(ntrees~type, data = dat)
pairwise.wilcox.test(dat$ntrees, dat$type, p.adjust.method = 'BH')

##################
## 90% worst case scenario

df <- dat %>%
  dplyr::filter(type %in% c("grid"))%>%
  glimpse()
mean(df$time)
summary(df)

summary(dat)

df <- dat %>%
  dplyr::filter(time <= 91) %>%
  glimpse()

9000/100
90*99 #8910
90*90 #8100

#####################################################################
### Lure 50m strength
# 
# grid <- read.csv('simulations/grid_l50.csv')
# 
# redgrid <-read.csv('simulations/redgrid_l50.csv')
# 
# dat <- bind_rows(grid, redgrid)
# tail(dat)
# 
# rand1 <- read.csv('simulations/rand1_l50.csv')
# 
# dat <- bind_rows(dat, rand1)
# tail(dat)
# 
# rand2 <- read.csv('simulations/rand2_l50.csv')
# dat <- bind_rows(dat, rand2)
# 
# tail(dat)
# 
# rand3 <- read.csv('simulations/rand3_l50.csv')
# dat <- bind_rows(dat, rand3)
# tail(dat)
# 
# exp1 <- read.csv('simulations/expert1_l50.csv')
# dat <- bind_rows(dat, exp1)
# tail(dat)
# 
# exp2<- read.csv('simulations/expert2_l50.csv')
# dat <- bind_rows(dat, exp2)
# tail(dat)
# 
# eff <- read.csv('simulations/eff_l50.csv')
# dat <- bind_rows(dat, eff)
# tail(dat)
# 
# ineff <- read.csv('simulations/ineff2_l50.csv')
# dat <- bind_rows(dat, ineff)
# tail(dat)
# str(dat)
# 
# dat <- dat %>%
#   dplyr::mutate(type = as.factor(type))%>%
#   dplyr::mutate(lure = as.factor(lure))%>%
#   glimpse()
# 
# saveRDS(dat, 'flies50.RDS')
##############################################
## Kolmogorov-Smirnov Test

dat <- readRDS('flies50.RDS')
unique(dat$type)

df <- dat %>%
  dplyr::filter(type %in% c("grid", "expert1"))%>%
  glimpse()

unique(df$type)

ks.test(df$ntrees~df$type)

### Normality
mod <- aov(time~type, data = dat)
resid <- residuals(mod)
plot(resid) #normal distribution
qplot(resid) 
qqnorm(resid)
qqline(resid)

## Not normal - Kruskal-Wallis Test
kruskal.test(time~type, data = dat)
pairwise.wilcox.test(dat$time, dat$type, p.adjust.method = 'BH')

## 90% worst case scenario

summary(dat)

df <- dat %>%
  dplyr::filter(time < 78) %>%
  glimpse()

9000/100
90*99 #8910
90*90 #8100

#####################################################################
### Lure 5m strength
# 
# grid <- read.csv('simulations/grid_l5.csv')
# 
# redgrid <-read.csv('simulations/redgrid_l5.csv')
# 
# dat <- bind_rows(grid, redgrid)
# tail(dat)
# 
# rand1 <- read.csv('simulations/rand1_l5.csv')
# 
# dat <- bind_rows(dat, rand1)
# tail(dat)
# 
# rand2 <- read.csv('simulations/rand2_l5.csv')
# dat <- bind_rows(dat, rand2)
# 
# tail(dat)
# 
# rand3 <- read.csv('simulations/rand3_l5.csv')
# dat <- bind_rows(dat, rand3)
# tail(dat)
# 
# exp1 <- read.csv('simulations/expert1_l5.csv')
# dat <- bind_rows(dat, exp1)
# tail(dat)
# 
# exp2<- read.csv('simulations/expert2_l5.csv')
# dat <- bind_rows(dat, exp2)
# tail(dat)
# 
# eff <- read.csv('simulations/eff_l5.csv')
# dat <- bind_rows(dat, eff)
# tail(dat)
# 
# ineff <- read.csv('simulations/ineff2_l5.csv')
# dat <- bind_rows(dat, ineff)
# tail(dat)
# str(dat)
# 
# dat <- dat %>%
#   dplyr::mutate(type = as.factor(type))%>%
#   dplyr::mutate(lure = as.factor(lure))%>%
#   glimpse()
# 
# saveRDS(dat, 'flies5.RDS')
##############################################
## Kolmogorov-Smirnov Test

dat <- readRDS('flies5.RDS')%>%
  glimpse()

unique(dat$type)

df <- dat %>%
  dplyr::filter(type %in% c("grid", "expert2"))%>% #modify for each pair
  glimpse()

unique(df$type)

ks.test(df$ntrees~df$type) #modify for time or ntrees

### ANOVA - test for normality first
mod <- aov(time~type, data = dat)  #modify for ntrees or time
resid <- residuals(mod)
plot(resid) 
qplot(resid) 
qqnorm(resid)
qqline(resid) 
## neither ntrees or time are normal

## Kruskal Wallis - nonparametric data
kruskal.test(ntrees ~ type, data = dat) 
pairwise.wilcox.test(dat$ntrees, dat$type, p.adjust.method = 'BH')


### KW = allflies
kruskal.test(ntrees ~ lure, data = allflies) 
pairwise.wilcox.test(allflies$ntrees, allflies$lure, p.adjust.method = 'BH')

kruskal.test(time ~ lure, data = allflies) 
pairwise.wilcox.test(allflies$time, allflies$lure, p.adjust.method = 'BH')

citation('car')

#### Stats for fly simulation

#load libraries
library(dplyr)
library(ggplot2)
library(cowplot)
library(car)
library(foreign)
library(RColorBrewer)

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
# ineff <- read.csv('simulations/ineff_l20.csv')
# dat <- bind_rows(dat, ineff)
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
dat <- readRDS('flies1.RDS')


# summary(dat)
# 
kilo <- dat %>%
  dplyr::filter(sim < 1001)%>%
  glimpse()
tail(kilo)

meank <- tapply(kilo$ntrees, kilo$type, mean)
meank
sdk <- tapply(kilo$ntrees, kilo$type, sd)
sdk
medk <- tapply(kilo$ntrees, kilo$type, median)
medk

aggregate(kilo, by=list(kilo$type), mean)
aggregate(kilo, by=list(kilo$type), sd)

# meandat <- tapply(dat$time, dat$type, mean)
# meandat
# sddat <- tapply(dat$time, dat$type, sd)
# sddat
# meddat <- tapply(dat$time, dat$type, median)
# meddat


##############################################
#K-S Test 
dat <- dat%>%
  dplyr::filter(sim < 1001)%>%
  glimpse()

ks.test(dat$time~dat$type) #grouping factor == 2 levels

#KS between grid and each other surveillance type?
df <- dat %>%
  dplyr::filter(type %in% c("grid", "ineff"))%>%
  glimpse()

unique(df$type)

ks.test(df$time~df$type)

#plot ecdf of each surveillance type on the y with x = years

cdf.time <- ggplot(
   data=dat, aes(x=time, group = type, col = type)) +
   stat_ecdf()
  
cdf.time

#number of trees infested at first detection

cdf.trees <- ggplot(
  data=dat, aes(x=ntrees, group = type, col = type)) +
  stat_ecdf()

cdf.trees


# y= ecdf, x = males
cdf.males <- ggplot(
  data=dat, aes(x=nmales, group = type, col = type)) +
  stat_ecdf()

cdf.males


####################### Comparing means - ANOVA
mod <- aov(ntrees~type, data = dat)
Anova(mod)
resid <- residuals(mod)
resid
plot(resid) #normal distribution
qplot(resid) 

tuk <- TukeyHSD(mod, conf.level=.95)
plot(tuk)
tuk

##################
## 90% worst case scenario

df <- dat %>%
  dplyr::filter(type %in% c("grid"))%>%
  glimpse()
mean(df$time)
summary(df)

summary(dat)

df <- dat %>%
  dplyr::filter(time < 88) %>%
  glimpse()

9000/100
90*99 #8910
90*90 #8100

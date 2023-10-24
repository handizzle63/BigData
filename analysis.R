#### Stats for fly simulation

#load libraries
library(dplyr)
library(ggplot2)
library(cowplot)

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
# kilo <- dat %>%
#   dplyr::filter(sim < 1001)%>%
#   glimpse()
# tail(kilo)
# 
# meank <- tapply(kilo$time, kilo$type, mean)
# meank
# sdk <- tapply(kilo$time, kilo$type, sd)
# sdk
# medk <- tapply(kilo$time, kilo$type, median)
# medk
# 
# meandat <- tapply(dat$time, dat$type, mean)
# meandat
# sddat <- tapply(dat$time, dat$type, sd)
# sddat
# meddat <- tapply(dat$time, dat$type, median)
# meddat



##############################################
#K-S Test 

ks.test(dat$time~dat$type)

#KS between grid and each other surveillance type?
df <- dat %>%
  dplyr::filter(type %in% c("grid", "redgrid"))


#plot ecdf of each surveillance type on the y with x = years
ecdf(grid$time)

sample.data = read.table ('data.txt', header = TRUE, sep = "\t")
cdf <- ggplot (data=sample.data, aes(x=Delay, group =Type, color = Type)) + stat_ecdf()
cdf

#number of trees infested at first detection



# y= ecdf, x = max distance



# y = ecdf, x = total area infested
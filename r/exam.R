##################
##  EXAM SCRIPT ##
##################

rm(list=ls()) # Clear memory


#libraries
library(dplyr)
library(vegan)
library(ggplot2)

### FREQENCY DATA SET
### dataset like Lab 2
### x = site number
### lat = latitude
### columns 2 - whatever = species names
### frequencies are the number of samples at each site in which the species was found
### Max of 6 samples per site -- check
# IMPORTANT: 
# When you enter answers regarding this data set into the LMS quiz, 
# you must enter the correct Site ID or Species ID where relevant… 
# ie enter “site3” rather than just “3”, 
# or “Fem.mad” rather than “1” or “Species 1”.




## Read in Year 1 & 2

dat1 <- read.csv('allfreq.csv')%>%
  glimpse()


dat2 <- read.csv('allfreq2.csv')%>%
  glimpse()



#checking typos in species
which(!names(dat1)==names(dat2)) #46 & 776
names <- data.frame(cbind(names(dat1),names(dat2))) # comma for list
names$X1[776]
names$X2[776]
names$X1[46]
names$X2[46]

## changing dat2

colnames(dat2)[colnames(dat2) =="Cay.biq"] = "Cay.beq"  #first is what's changing
                                                        #second is what too
colnames(dat2)[colnames(dat2) =="Yez.vel"] = "Yiz.vel"


## checking Sites

which(!(dat1$X)==(dat2$X)) #sites the same - all GOOD

## checking latitudes
which(!(dat1$lat)==(dat2$lat)) #latitudes not the same. Typo below (site562 in dat2)

dat1$lat[562]
dat2$lat[562] 
dat2$lat[562] <- 18.92828 # from 1.892828
dat2$X[562] #site562


#just species dataframes

justsp <- dat1 %>%
  select(-c(X, lat))

justsp2 <- dat2 %>%
  select(-c(X, lat))%>%
  glimpse()



## Checking number of quadrats
q <- (justsp>6)
max(colSums(q))

q <- (justsp2>6)
max(colSums(q))

# #names
# y1 <- data.frame(names(justsp))
# 
# y1 <- y1 %>%
#   rename(species = names.justsp.)%>%
#   glimpse()
# 
# y1 <- y1 %>%
#   arrange(y1$species)%>%  ##likes the arrange in a separate pipe apparently
#   glimpse()
# 
# ### Year 2
# 
# justsp2 <- dat2 %>%
#   select(-c(X, lat))%>%
#   glimpse()
# 
# y2 <- data.frame(names(justsp2))
# 
# y2 <- y2 %>%
#   rename(species = names.justsp2.)%>%
#   glimpse()
# 
# y2 <- y2 %>%
#   arrange(y2$species)%>%  ##likes the arrange in a separate pipe apparently
#   glimpse()
# 
# 
# 
# 
# 
# tail(dat1$X)
# tail(dat2$X)

################# Year 1 
# 
# #site abundance
# abund1 <- apply(justsp, 1, sum)  
# dat1$abund <- abund1
# 
# plot(abund~lat, data = dat1)
# 
# #species abundance
# sptot1 <- apply(justsp, 2, sum) 
# sptot1 <- data.frame(sptot1)
# sptot1$species <-cbind(colnames(justsp))
# 
# barplot(sptot1~species, dat = sptot1)
# 
# #row means
# 
# mean(rowSums(justsp)) #overall mean from all plots
# mean1 <- rowMeans(justsp)/rowMeans(justsp>0) #GOOD (excludes zeros)
# dat1$site.mean <- mean1
# 
# plot(site.mean~lat, data = dat1)


# ### Calculating species richness per plot
# ## 1. Convert to presence/absence
# 
# Y1PA <- (justsp>0)
# Y1PA
# ## 2. Sum p/a to get richness
# sprich1 <- apply(Y1PA, 1, sum)
# sprich1
# 
# ## calculate shannon diversity index - vegan
# 
# shan1 <- diversity(justsp, index = 'shannon')
# shan1
# 
# # Simpson diversity index 
# simp1 <- diversity(justsp, index = "simpson")
# simp1

# 
# ## total number of plots at which each species is found
# plotsum1 <- colSums(Y1PA) #using pres/abs df
# plotsum1
# 
# sptot1$plotsum <- plotsum1
# 
# ## use apply and/or colSums to calculate the frequency for each species 
# ## (ie the proportion of plots at which it is found) 
# length(justsp$Fem.mad)
# freq <- plotsum1/1056
# freq
# sptot1$freq <- freq 
# 
# dat1$sprich <- sprich1
# dat1$shan <- shan1
# dat1$simp <- simp1
# year1 <- rep(seq(1,1), len = 1056)
# dat1$year <- year1
# unique(dat1$year)

#saveRDS(dat1, 'dat1.RDS')

### Year 2 
## Abundace per plot
abund2 <- apply(justsp2, 1, sum)  
dat2$abund <- abund2

plot(abund ~lat, data = dat1)
plot(abund~lat, data = dat2)


#species abundance
sptot2 <- apply(justsp2, 2, sum) 
sptot2 <- data.frame(sptot2)
sptot2$species <-cbind(colnames(justsp2))

barplot(sptot2~species, dat = sptot2)

## Row Means (Mean abundance per plot)
mean(rowSums(justsp2)) #overall mean from all plots
mean2 <- rowMeans(justsp2)/rowMeans(justsp2>0) #GOOD (excludes zeros)
dat2$site.mean <- mean2

plot(site.mean ~ lat,
     ylim = c(2.8, 4.0),
       data = dat1)

plot(site.mean~lat, 
     ylim = c(2.8, 4.0), 
     data = dat2)

### Calculating species richness per plot
## 1. Convert to presence/absence

Y2PA <- (justsp2>0)
Y2PA

## 2. Sum p/a to get richness
sprich2 <- apply(Y2PA, 1, sum)
sprich2

## calculate shannon diversity index - vegan

shan2 <- diversity(justsp2, index = 'shannon')
shan2

# Simpson diversity index 
simp2 <- diversity(justsp2, index = "simpson")
simp2


## total number of plots at which each species is found
plotsum2 <- colSums(Y2PA) #using pres/abs df
plotsum2

sptot2$plotsum <- plotsum2

## use apply and/or colSums to calculate the frequency for each species 
## (ie the proportion of plots at which it is found) 

freq <- plotsum2/1056
freq
sptot2$freq <- freq 

dat2$sprich <- sprich2
dat2$shan <- shan2
dat2$simp <- simp2
year2 <- rep(seq(2,2), len = 1056)
dat2$year <- year2
unique(dat2$year)

#saveRDS(dat2, 'dat2.RDS')

## combining dfs just in case 
alldat <- bind_rows(dat1, dat2)
names(alldat)
1056*2

#saveRDS(alldat, 'alldat.RDS')

################################# looking good so far -- CHECK BEFORE SUBMITTING ABOVE

## Look at some summary bits and pieces

## Q: how many species in total?
# 987 species

## Q: how many sites in total?
#1056 Sites
tail(dat1$X)

dim(justsp)

## Q:  Latitude range
min(dat1$lat) #18.01905
max(dat1$lat) #27.99943

## Q: No. species found > lat 26

sum(dat1$lat>26) #219 sites

## Q: what is the average frequency of each species?
colmeans() #


## Q: which site had the least number of species?
which.min(sprich1)
which.min(dat1$sprich)
dat1$X[159]

## Q: which species was found at the most sites?
a <- colSums(justsp>0)

a[which(a==max(a))] #569 sites Hol.huy in year 1

b <- colSums (justsp2 >0)
b[which(b==max(b))] #Gok.tac 565 year 2 

## Q: which species was found over the greatest geographical range?


latdat <- sapply(justsp, function (x) ifelse (x>0,dat1$lat, x)) #year 1 

df<-data.frame(latdat)

georange <- sapply(df, function(x) max(x) - min(x[x>0]))

print(georange)

georange[which(georange == max(georange))]
## see console


## Q: which species was found over the smallest geographical range? 
georange[which(georange == min(georange))] #year 1
#Tef.qex 

tefgex <- dat1%>%
  dplyr::select(X, lat, Tef.qex)%>%
  dplyr::filter(tefgex$lat >0)%>%
  glimpse()
#~lowest altitude is around 21 
plot(Tef.qex~lat, data = tefgex)


#############################################################
#dat1 <- readRDS('dat1.RDS')
#dat2 <- readRDS('dat2.RDS')
#alldat <- readRDS('alldat.RDS')

### in case of splitting gen & species
allsp <- alldat %>%
  select(-c(X, lat, abund, site.mean, sprich, shan, simp, year))%>%
  glimpse()

names(allsp)[1] #name of first species
strsplit(names(allsp)[1],".",fixed=TRUE) #splitting genus and species at location 1
strsplit(names(allsp),".",fixed=TRUE) #splits at . for entire df
splitlist <- strsplit(names(allsp),".",fixed=TRUE) #creating list with split
splitlist[[1]][1] #first iteration first part of split
splitlist[[1]][2] #first iteration second part of split
unlist(splitlist) #unlist seems to actually pull them aprt
seq(1,length(unlist(splitlist)),by=2) #identifies every second iteration (genus)
unlist(splitlist)[seq(1,length(unlist(splitlist)),by=2)] #just all the genii

gen <- sapply(splitlist, function(x) x[1])
table(gen)

# genus df
gendat <- data.frame(species=names(allsp), genus=gen)
gendat

## Add total number of individuals over both years
gendat$n.ind <- colSums(allsp)
gendat

## No. individuals per genus
tapply(gendat$n.ind, gendat$genus, sum)


### Aggregating data by genus and plot

##1. transpose the data, so species are by rows and plots by columns
(dat1t <- data.frame(t(justsp)))

## to calculate the number of individuals within a genus at each plot
dat1g <- aggregate(dat1t,by=list(gen),sum)
dat1g

## as a challenge you could try to calculate the genus richness of each plot, 
grich <- apply(dat1g[,2:797], 2, function(x) sum(x>0)) #for colums make sure ], 2, ...
grich

## Transposing back the other way

############## need to fiddle around here I think
x <- data.frame(t(dat1g))
#colnames(x) <- c(unique(gen)) 
x
colSums(x)

## Shannon

gshan <- apply(x, 1 ,shannon)
gshan


## and frequency of each genus across the 50 plots
BCIgpa <- (x>0)
BCIgpa

plotsum <- colSums(BCIgpa)
freq <- plotsum/50
freq

#################################

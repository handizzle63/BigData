####################################################################
####################################################################
## community abundance data
rm(list=ls())
## open the "BCI1.csv" file in excel and have a look
## the data is tree counts in 1-hectare plots in the Barro Colorado Island.
## there are 50 plots (rows) of 1 hectare with counts of trees on each plot with total of 225 species (columns). 
## Full Latin names are used for tree species.
## Data give the numbers of trees at least 10 cm in diameter at breast height (1.3 m above the ground) in each one hectare square of forest. 
## Within each one hectare square, all individuals of all species were tallied and are recorded in this table.
## The quadrats are located in a regular grid.

## Data source: http://www.sciencemag.org/cgi/content/full/295/5555/666/DC1 (also available in the 'vegan' R package
## Reference: Condit, R, Pitman, N, Leigh, E.G., Chave, J., Terborgh, J., Foster, R.B., Nu?ez, P., Aguilar, S., Valencia, R., Villa, G., Muller-Landau, H.C., Losos, E. & Hubbell, S.P. (2002). Beta-diversity in tropical forest trees. Science 295, 666?669. 

## first we read in the data set and have a quick look

## columns are species names
## frequencies are tree counts
## each row is a plot, frequencies must be how many of that species in that plot

BCI <- read.csv("data/BCI1.csv")
head(BCI)
names(BCI)

## use 'apply' to calculate the total number of individuals in each plot
#rowSums(BCI) 

apply(BCI, 1, sum) #same as rowSums(BCI)
apply(BCI,2, sum) # colSums(BCI)

colSums(BCI)

## these are the co-ordinates of each plot
UTM.EW <- rep(seq(625754, 626654, by=100), each=5) #lat 
UTM.EW    #makes seq of number from 625754 that go up 100 after every 5 repeats
UTM.NS <- rep(seq(1011569,  1011969, by=100), len=50) #lon
UTM.NS    #makes seq of no. that go up 100 each time for a seq of 50


## VISUALISING spatial trends in total abundance

library(ggplot2)
qplot(UTM.EW, UTM.NS, size = nsp) #deprecated in ggplot 3.4.2 or something
ggplot()+
  geom_point(aes(UTM.EW, UTM.NS, size = nsp))


## Base R Plot
n <- rowSums(BCI) #total abundance = no. of trees
which(n==max(n)) #which site
n[which(n == max(n))] #no. species 

BCI = cbind(plot = plot, BCI) 
library(dplyr)
a <- BCI %>%
  dplyr::filter(plot == 35)

b <- colSums(a)
b
b[which(b == max(b))]

c <- a %>%
  dplyr::select(-c(Gustavia.superba, Alseis.blackiana))
b <- colSums(c)
b[which(b == max(b))]

ptsz <- (n-min(n))/(max(n)-min(n))*5+0.5 #creating a vector within points relative to size of number of species present in each quadrat
plot(UTM.EW,UTM.NS,cex=ptsz) 

## IRL would follow up with statistical testing here

## Calculate mean in each plot
# with apply()
apply(BCI,1,function(x) mean(x[x>0])) #excludes all the zeros and calcs. the mean

## now use 'rowSums' and 'rowMeans' to do the same things
rowMeans(BCI) #considers all the 0s - which we dont want e.g. 27/12 (trees/possible species) is diff to 27/2 (trees/actual species)
rowSums(BCI)/rowSums(BCI>0) #does the same as the above apply() - GOOD!
mean(rowSums(BCI)) #overall mean from all plots
rowMeans(BCI)/rowMeans(BCI>0) #GOOD (excludes zeros)


### Calculating species richness per plot
## 1. Convert to presence/absence

BCIPA <- (BCI>0)
BCIPA
## 2. Sum p/a to get richness
nsp <- apply(BCIPA,1,sum)

## Visualise spatial patterns in species richness
nspz <- (nsp-min(nsp))/(max(nsp)-min(nsp))*5+0.5
plot(UTM.EW,UTM.NS,cex=nspz) 

### Shannon Diversity Index per Site
## now we want to calculate the Shannon's diversity index for each site
## (see http://en.wikipedia.org/wiki/Diversity_index#Shannon_index)
## the function to calculate Shannon's diversity is available in the vegan package
library(vegan)

#long way to calculate it
shannon <- function(x) { #creating a function(x) called shannon
	x <- subset(x,x>0)    
	ps <- x/sum(x)
	-sum(ps*log(ps))
}

## Use the apply() to calculate shannon diversity index
xx=apply(BCI,1,shannon) #Note: applying it to the original dataset
xx

#check to see if same as vegan package - yes it is
shan <- diversity(BCI, index = 'shannon')
shan

## plot to look for spatial trends in shannon diversity
xxz <- (xx^2)/2 #used variety of maths to try to show trend better but none great

plot(UTM.EW, UTM.NS, cex = xx)


## Challenge: calculate Simpson Diversity index
library(vegan)
simp <- diversity(BCI, index = "simpson")
simp
## returns values 0-1. Closer it is to one the lower the diversity

## use apply and/or colSums to calculate the total number of plots at which each species is found
plotsum <- colSums(BCIPA) #using pres/abs df

## use apply and/or colSums to calculate the frequency for each species 
## (ie the proportion of plots at which it is found) 
length(BCI$Abarema.macradenium)
freq <- plotsum/50
freq

## of course from here you would likely go on to produce some ordinations
## or dendograms and do some multi-variate analysis - permanova


### now what if you wanted to analyse by genus instead of species??

names(BCI)[1] #name of first species
strsplit(names(BCI)[1],".",fixed=TRUE) #splitting genus and species at location 1
strsplit(names(BCI),".",fixed=TRUE) #splits at . for entire df
splitlist <- strsplit(names(BCI),".",fixed=TRUE) #creating list with split
splitlist[[1]][1] #first iteration first part of split
splitlist[[1]][2] #first iteration second part of split
unlist(splitlist) #unlist seems to actually pull them aprt
seq(1,length(unlist(splitlist)),by=2) #identifies every second iteration (genus)
unlist(splitlist)[seq(1,length(unlist(splitlist)),by=2)] #just all the genii

## Same as above but with sapply()
sapply(splitlist, function(x) x[1])
gen <- sapply(splitlist, function(x) x[1])

## Number of species per genus
table(gen)

## now we could make a data frame with that information
class.data <- data.frame(species=names(BCI), genus=gen)
class.data

## and if we wanted add the total number of individuals?
class.data$n.ind <- colSums(BCI)
class.data

## and calculate total number if individuals of each genus?
tapply(class.data$n.ind,class.data$genus,sum)

### Aggregating data by genus and plot

##1. transpose the data, so species are by rows and plots by columns
(BCIt <- data.frame(t(BCI)))

## notice how the plots are automatically given a default name
## because a dataframe needs column names

## now use the aggregate function on the transposed data frame
## to calculate the number of individuals within a genus at each plot
BCIg <- aggregate(BCIt,by=list(gen),sum)
BCIg

x <- BCIg %>%
  #select(-c(Group.1))%>%
  filter(Group.1 != "Trichilia")
x <- x %>%
  select(-c(Group.1))

b <- rowSums(x)
b[which(b == max(b))]
which(b==max(b))

print(BCIg$Group.1[139])
print(BCIg$Group.1[52])

## as a challenge you could try to calculate the genus richness of each plot, 
grich <- apply(BCIg[,2:51], 2, function(x) sum(x>0)) #for colums make sure ], 2, ...
grich

#checking correct:
# x <- BCIg %>%
#   select(-c(Group.1))%>%
#   glimpse()
# 
# x <- (x>0)
# colSums(x)


# and the shannon diversity of each plot based on genus instead of species, 
#gshan <- diversity(BCIg[,2:51], index = "shannon") - wrong
#gshan <- diversity(BCIg, index = 'shannon') - nope

## Transposing back the other way

x < - data.frame(t(BCIg))
colnames(x) <- c(unique(gen))
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


###################################################################################
### now what if you wanted to analyse by Family or Order instead??
## open the file 'plant classifications.csv' in excel and have a look at what it contains

class.data2<- read.csv('data/plant classifications.csv')
class.data2
head(class.data2)
table(class.data2$Family)
table(class.data2$Order)

## now we use 'merge' to combine the two dataframes
class.data3 <- merge(class.data,class.data2, by.x='genus',by.y='Genus')
class.data3

unique(class.data3$Family)

## how many species do we have in each family?
table(class.data3$Family)

## how many individuals do we have in each family?
tapply(class.data3$n.ind, class.data3$Family, sum) 

## but actually there is a problem... have you noticed?
length(class.data$species)

## 225 species in the original data set
length(class.data3$species)

## but only 98 in the new one
## can you see why??? because of the join -- fix below

## based on the limited family data available, 
## we can calculate the number of individuals of each family at each site as follows
BCIt$species <- names(BCI)
head(BCIt)
(BCIt.subset <- merge(BCIt,class.data3,by.x='species',by.y='species'))

famlevel = aggregate(BCIt.subset[,2:51],list(BCIt.subset$Family),sum)
famlevel 

## Shannon Diversity by Family Level
fshan <- apply(famlevel[-1],2,shannon)
fshan

## can you calculate the family richness of each site?
frich <- apply(famlevel[-1], 2, function(x) sum(x>0))
frich

## can you calculate the shannon diversity based on order of each site? 
unique(class.data3$Order)
names(BCIt.subset)

#df with Order instead of species
BCIo <- aggregate(BCIt.subset[,2:51], list(BCIt.subset$Order), sum)
BCIo

#per plot with order
oshan <- apply(BCIo[-1],2,shannon)
oshan


### now, here is similar data collected the next year

BCI2 <- read.csv('data/BCI2.csv')%>%
  glimpse()

### we might want to know if there are extra species, 
### or missing species, or do some error checking

names(BCI2)
names(BCI)

### so, what are the differences? 
### there has to be an easier way than comparing these big lists??
### what about?

# elnom <- c(names(BCI2),names(BCI))
# unique(elnom)
# sort(unique(elnom))

### mmm, still not so easy...
### what about?
cbind(names(BCI2),names(BCI))
### can you see the problem? can you fix it so the columns are aligned for comparison?
### give it a try...

cbind(names(BCI2[-1]),names(BCI)) #removing 'plot' from BCI2
## but creates a list

## create a logical list
#names(BCI2)[-1]==names(BCI) #T/F for same name 
names(BCI2)==names(BCI)

#all(names(BCI2)[-1]==names(BCI)) #T/F IF ALL THE SAME - SOME ARE DIFFERENT
all(names(BCI2)==names(BCI))

#cbind(names(BCI),names(BCI2)[-1]==names(BCI),names(BCI2)[-1]) #returns two long lists

which(!names(BCI2)==names(BCI)) #means which are different == 43 & 73 

colnames(BCI2)[colnames(BCI2) == "Chrysochlamis.eclipes"] ="Chrysochlamys.eclipes"
colnames(BCI2)[colnames(BCI2) =="Ficus.costarcana"] = "Ficus.costaricana"
which(!names(BCI2)==names(BCI))

## adding in 'plot' as row 1 for BCI
# names(BCI)
# plot = 1:50
# BCI = cbind(plot = plot, BCI) 
# glimpse(BCI)


####YEAR 3
BCI3 <- read.csv('data/BCI3.csv')%>%
  select(order(colnames(BCI3)))%>% #listing alphabetically
  glimpse()

BCI3 <- BCI3 %>%
  select(-c(X))%>%  #removing X
  glimpse() 
  
# add plot in at first row

plot = 1:50
BCI3 = cbind(plot = plot, BCI3)
which(!names(BCI3)==names(BCI))
cbind(names(BCI3),names(BCI))

colnames(BCI3)[colnames(BCI3) =="Thevetia.aouai"]="Thevetia.ahouai"
colnames(BCI3)[colnames(BCI3) =="Dripetes.standleyi"]="Drypetes.standleyi"

which(!names(BCI3)==names(BCI))


### but now, here is yet another similar dataset collected in the 4th year
BCI4 <- read.csv('data/BCI4.csv')%>%
  glimpse()

### try to check for errors like you did before... can you see what goes wrong??
# missing species - not even going to try the above

### here is a nicer way to check for differences

sapply(names(BCI4), function(spname) spname %in% names(BCI))

res <- sapply(names(BCI4), function(spname) spname %in% names(BCI))
which(!res)
colnames(BCI4)[colnames(BCI4) =="X"]="plot"

res <- sapply(names(BCI4), function(spname) spname %in% names(BCI))
which(!res)

### now check which names from BCI are not in BCI2 ???

sp4 <- colnames(BCI4)

# Get the column names of df2
sp1 <- colnames(BCI)

# Find the column names present in df1 but missing in df2
missing <- setdiff(sp1, sp4)
missing

Alibertia.edulis <- rep(seq(0, 0), len=50)
Colubrina.glandulosa <- rep(seq(0, 0), len=50)
Ficus.colubrinae<- rep(seq(0, 0), len=50)
Trichospermum.galeottii<- rep(seq(0, 0), len=50)

BCI4 <- cbind(Alibertia.edulis = Alibertia.edulis, BCI4)
BCI4 <- cbind(Colubrina.glandulosa = Colubrina.glandulosa, BCI4)
BCI4 <- cbind(Ficus.colubrinae = Ficus.colubrinae, BCI4)
BCI4 <- cbind(Trichospermum.galeottii = Trichospermum.galeottii, BCI4)

which(!names(BCI4)==names(BCI))


BCI4 <- BCI4 %>%
  dplyr::select(-c(plot))%>% 
  glimpse()

BCI4 <- BCI4 %>%
  dplyr::select(order(colnames(BCI4)))%>%
  dplyr::mutate(plot = plot)%>%
  select(plot, everything())%>%
  glimpse()
  
  
which(!names(BCI4)==names(BCI))
cbind(names(BCI4),names(BCI))

## as challenges for later, you could try to
## 1. write a script that automatically checks for and prints differences
## in species between all combinations of the ten available BCI files
## this would be especially useful for error checking with a very large number of files


## 2. write a script that compiles a table of the shannon diversity in each plot
## over the ten years - and then plots it in some useful way

rbind?
  


####################################################################################
### processing non-standard format data
### here we show how the 'plant classifications.csv' data we used earlier was extracted from a non-standard format data file
####################################################################################

## open the file "some plant details.txt" in a text editor like notepad, and have a look
## notice that the data is NOT in any standard structured way that R would expect 
## (it does have some structure though, which we can exploit, especially that the genus is the last in each of the blocks)

## therefore we use the scan function to read it in
therawdata <- scan(file="some plant details.txt",what='character')
therawdata 
## notice that what is read in is just a list of all the 'words' in the file

## define a new object for the processed data, empty to begin with
theprocesseddata <- NULL

## define some variables to act as 'trackers' and set them to nothing to start with
Kingdom<- Order<-Family<-Subfamily<-Genus<-NA
## define some other variables to act as 'trackers' and set them to false to start with
Eudicot<-Angiosperm<-Rosid<-Asterid <- FALSE

## loop through the data word by word, setting trackers as they are encountered


## now in a good standard format, so save it to a csv file for later use
write.csv(theprocesseddata,'plant classifications.csv',row.names=FALSE)


##### so now some challenges for you
## 1. change the script so that you also have a TRUE/FALSE record of whether the genus is in the Magnoliids clade

## 2. change the script so that you also have a record of what tribe the genus is in, if any
##clarify tribe?

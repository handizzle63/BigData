####################################################################
####################################################################
## community abundance data

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
BCI <- read.csv("BCI1.csv")
head(BCI)
names(BCI)

## use 'apply' to calculate the total number of individuals in each plot
n <- rowSums(BCI)
apply(BCI, 1, sum)

## these are the co-ordinates of each plot
UTM.EW <- rep(seq(625754, 626654, by=100), each=5)
UTM.NS <- rep(seq(1011569,  1011969, by=100), len=50)

## we can plot to look for spatial trends in total abundance

## this is definitely easier using qplot from the ggplot library than the base plot function - if you have it installed...

library(ggplot2)
qplot(UTM.EW, UTM.NS, size = nsp) #deprecated in ggplot 3.4.2 or something


## Base R Plot
ptsz <- (n-min(n))/(max(n)-min(n))*5+0.5 #creating a vector within points relative to size of number of species present in each quadrat
plot(UTM.EW,UTM.NS,cex=ptsz) 

## IRL would follow up with statistical testing here

## Calculate mean in each plot
# with apply()
apply(BCI,1,function(x) mean(x[x>0]) )

## now use 'rowSums' and 'rowMeans' to do the same things
rowMeans(BCI)
rowSums(BCI)/rowSums(BCI>0) #does the same as the above apply()
mean(rowSums(BCI)) #overall mean from all plots and species


### Calculating species richness per plot
## 1. Convert to presence/absence

BCIPA <- (BCI>0)

## 2. Sum p/a to get richness
nsp <- apply(BCIPA,1,sum)

## Visualise spatial patterns in species richness
nspz <- (nsp-min(nsp))/(max(nsp)-min(nsp))*5+0.5
plot(UTM.EW,UTM.NS,cex=nspz) 

### Shannon Diversity Index per Site
## now we want to calculate the Shannon's diversity index for each site (see http://en.wikipedia.org/wiki/Diversity_index#Shannon_index)
## the function to calculate Shannon's diversity is available in the vegan package (and others), but we will show how to write it here

shannon <- function(x) { #creating a function(x) called shannon
	x <- subset(x,x>0)    
	ps <- x/sum(x)
	-sum(ps*log(ps))
}

## Use the apply() to calculate shannon diversity index
xx=apply(BCI,1,shannon) #Note: applying it to the original dataset

## plot to look for spatial trends in shannon diversity
xxz <- (xx^2)/2 #used variety of maths to try to show trend better but none great

plot(UTM.EW, UTM.NS, cex = xx)

## Challenge: calculate Simpson Diversity index
library(vegan)
simp <- diversity(BCI, index = "simpson")
## returns values 0-1. Closer it is to one the lower the diversity

## use apply and/or colSums to calculate the total number of plots at which each species is found
plotsum <- colSums(BCIPA) #using pres/abs df

## use apply and/or colSums to calculate the frequency for each species (ie the proportion of plots at which it is found) 
freq <- plotsum/50

## of course from here you would likely go on to produce some ordinations or dendograms and do some multi-variate analysis - permanova


### now what if you wanted to analyse by genus instead of species??
## look carefully at what each of the following lines of code does and see if you can follow - ask if in doubt  
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
## notice how the plots are automatically given a default name because a dataframe needs column names

## now use the aggregate function on the transposed data frame to calculate the number of individuals within a genus at each plot
BCIg <- aggregate(BCIt,by=list(gen),sum)

## as a challenge you could try to calculate the genus richness of each plot, 
grich <- apply(BCIg[, 2:51], 1, function(x) sum(x>0))


## and the shannon diversity of each plot based on genus instead of species, 
gshan <- diversity(BCIg[,2:51], index = "shannon")

## and frequency of each genus across the 50 plots

BCIgpa <- as.matrix(BCIg[,2:51] >0)
gsum <- rowSums(BCIgpa)
gfreq <- gsum/50

###################################################################################
### now what if you wanted to analyse by Family or Order instead??
## open the file 'plant classifications.csv' in excel and have a look at what it contains

class.data2<- read.csv('plant classifications.csv')
class.data2
head(class.data2)
table(class.data2$Family)
table(class.data2$Order)

## now we use 'merge' to combine the two dataframes
class.data3 <- merge(class.data,class.data2, by.x='genus',by.y='Genus')
class.data3

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

## based on the limited family data available, we can calculate the number of individuals of each family at each site as follows
BCIt$species <- names(BCI)
head(BCIt)
(BCIt.subset <- merge(BCIt,class.data3,by.x='species',by.y='species'))

famlevel = aggregate(BCIt.subset[,2:51],list(BCIt.subset$Family),sum)
famlevel 

## Shannon Diversity by Family Level
fshan <- apply(famlevel[-1],2,shannon)

## can you calculate the family richness of each site?
frich <- apply(famlevel[-1], 1, function(x) sum(x>0))

## can you calculate the shannon diversity based on order of each site? 
########################### clarify with MICHAEL - order for each family high to low site?

### now, here is similar data collected the next year
BCI2 <- read.csv('BCI2.csv')

head(BCI2)

### we might want to know if there are extra species, or missing species, or do some error checking

names(BCI2)
names(BCI)

### so, what are the differences? there has to be an easier way than comparing these big lists??
### what about?
namesfromboth <- c(names(BCI2),names(BCI))
unique(namesfromboth)
sort(unique(namesfromboth))

### mmm, still not so easy...
### what about?
cbind(names(BCI2),names(BCI))
### can you see the problem? can you fix it so the columns are aligned for comparison? give it a try...

cbind(names(BCI2[-1]),names(BCI)) #removing 'plot' from BCI2

names(BCI2)[-1]==names(BCI) #BETTER - TRUE/FALSE FOR SAME SAME

all(names(BCI2)[-1]==names(BCI)) #T/F IF ALL THE SAME - SOME ARE DIFFERENT

cbind(names(BCI),names(BCI2)[-1]==names(BCI),names(BCI2)[-1]) #returns two long lists

which(!names(BCI2)[-1]==names(BCI)) #means which are different == 43 & 73 

## must be renaming later

####YEAR 3
BCI3 <- read.csv('BCI3.csv')
head(BCI3)
names(BCI3)
### rename column 1 
names(BCI3)[1] <- 'plot'
BCI
### can you also fix the BCI dataset so it has the same structure as the second two ie first column is for plot number??
names(BCI)
plot = 1:50
BCI = cbind(plot = plot, BCI) #accidentally did it twice
BCI = BCI[-1]
BCI = BCI[-227]
names(BCI)

### now, check for differences between the data sets like we did before...
which(!names(BCI2)==names(BCI)) #2 different
which(!names(BCI3)==names(BCI)) #lots different
cbind(names(BCI3),names(BCI)) #not listed alphabetically

### identify whether there really are any different species or errors, using the 'sort' function 
## HELP
library(dplyr)
colnames(BCI3)
sort(colnames(BCI3[2:226]))

### but now, here is yet another similar dataset collected in the 4th year
BCI4 <- read.csv('BCI4.csv')

### try to check for errors like you did before... can you see what goes wrong??

### here is a nicer way to check for differences

sapply(names(BCI2), function(spname) spname %in% names(BCI))
res <- sapply(names(BCI2), function(spname) spname %in% names(BCI))
which(!res)
### now check which names from BCI are not in BCI2 ???

## as challenges for later, you could try to
## 1. write a script that automatically checks for and prints differences in species between all combinations of the ten available BCI files
## this would be especially useful for error checking with a very large number of files
## 2. write a script that compiles a table of the shannon diversity in each plot over the ten years - and then plots it in some useful way



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
for (i in 1:length(therawdata )){
	print(paste(i,therawdata[i]))
	if (therawdata[i]=="Angiosperms") Angiosperm<-TRUE 
	if (therawdata[i]=="Plantae") Kingdom<-"Plantae"
	if (therawdata[i]=="Eudicots") Eudicot<-TRUE
	if (therawdata[i]=="Asterids") Asterid<-TRUE
	if (therawdata[i]=="Rosids") Rosid<-TRUE
	if (therawdata[i]=="Order:") Order<- therawdata[i+1]
	if (therawdata[i]=="Family:") Family<- therawdata[i+1]
	if (therawdata[i]=="Subfamily:") Subfamily<- therawdata[i+1]
	### when the keyword 'Genus:' is encountered, create a new line to add to the processed data file, with all tracked values
	### and then reset all the trackers
	if (therawdata[i]=="Genus:") {
		Genus<- therawdata[i+1]
		newprocesseddata <- c(Kingdom,Angiosperm,Eudicot,Asterid,Rosid,Order,Family,Subfamily,Genus)
		theprocesseddata <- rbind(theprocesseddata ,newprocesseddata )
		Kingdom<- Order<-Family<-Subfamily<-Genus<-NA
		Eudicot<-Angiosperm<-Rosid<-Asterid <- FALSE
	}
}
## what have we got?
theprocesseddata
## make it into a data frame
theprocesseddata <- data.frame(theprocesseddata)
## add the names
names(theprocesseddata) <- c('Kingdom','Angiosperm','Eudicot','Asterid','Rosid','Order','Family','Subfamily','Genus')
## what have we got?
theprocesseddata
## reverse the column order
theprocesseddata <- theprocesseddata[,9:1]
## what have we got?
theprocesseddata
## now in a good standard format, so save it to a csv file for later use
write.csv(theprocesseddata,'plant classifications.csv',row.names=FALSE)


##### so now some challenges for you
## 1. change the script so that you also have a TRUE/FALSE record of whether the genus is in the Magnoliids clade

## 2. change the script so that you also have a record of what tribe the genus is in, if any
##clarify tribe?

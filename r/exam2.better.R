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
dat2$lat[562] <- 18.9282758249901 # from 1.892828
dat2$X[562] #site562

which(!(dat1$lat)==(dat2$lat)) 

## checking for mismatch between site $ latitude

y <- dat1 %>%
  dplyr::select(X, lat)%>%
  dplyr::mutate(y = paste(X, lat, sep = "."))%>%
  glimpse()

x <- dat2%>%
  dplyr::select(X, lat)%>%
  dplyr::mutate(y = paste(X, lat, sep = "."))%>%
  glimpse()

which(!(x$y)==(y$y)) #all G

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

q <- (justsp>5)
max(colSums(q))

q <- (justsp2>5)
max(colSums(q))
## Q: how many species & sites in total?
dim(justsp)
dim(justsp2)

## Q:  Latitude range
min(dat1$lat) #18.01905
max(dat1$lat) #27.99943

min(dat2$lat)
max(dat2$lat)


## Q: No. Sites found > lat 26
sum(dat1$lat>20) #219 sites
sum(dat2$lat>20)
## Q: Highest Site
which(dat1$lat >= 27.99943)
dat1$X[891]
#print(dat1[891, ])
print(dat1[891, 2])
which(dat2$lat >= 27.99943)

## Q: Lowest Site
which(dat1$lat <= 18.01905)
print(dat1[232, 1:2])
which(dat2$lat <= 18.01905)

## Q: what is the average frequency of each species?
## (hint, use the colMeans function)

#year1
spmean <-  colMeans(justsp)

spmean[which(spmean == max(spmean))] #highest av
spmean[which(spmean == min(spmean))] #lowest average


#year2
spmean2 <-  colMeans(justsp2) 
spmean2[which(spmean2 == max(spmean2))] #highest av
spmean2[which(spmean2 == min(spmean2))] #lowest av


## out of interest for me
spmean2[647] #declined
spmean2[363] #slight increase
spmean[88] #Nuh.yug increased from year 1
spmean[100] #declined - extinction?

## difference in frequency
af <- data.frame(cbind(spmean,spmean2))
d <- af %>%
  dplyr::mutate(result= spmean - spmean2)%>%glimpse()

d$result[which(d$result == max(d$result))] #lowest value
which(d$result==max(d$result)) #site

d$result[which(d$result == min(d$result))] #lowest value
which(d$result==min(d$result)) #site

length(d$result)
colnames(df)[100]
spmean[100]
spmean2[100]
2.268939-2.530303

drich <- data.frame(cbind(rich1,rich2))%>%
  dplyr::mutate(result= rich1 - rich2)

drich$result[which(drich$result == max(drich$result))] #lowest value
which(drich$result==max(drich$result)) #position

rich1[157]
rich2[157]
332-260
dat1$X[157]

drich$result[which(drich$result == min(drich$result))] #lowest value
which(drich$result==min(drich$result)) #position


## Q: how many species were found at each site? 
## (ie what is the species richness of each site?)
## (hint, use indexing and rowSums - lots of other ways too)

rich1 = rowSums(justsp>0) #richness year 1
rich1[which(rich1 == min(rich1))] #207 - lowest species
which(rich1==min(rich1))
rich1[which(rich1 == max(rich1))] #368 - highest species
which(rich1==max(rich1)) #site
rich1[416]

dat1$lat[416]
print(dat1[416, ])

rich2 = rowSums(justsp2>0) #richness year 2
rich2[which.min(rich2)] 
which(rich2==min(rich2))
rich2[which.max(rich2)] #species number
which(rich2==max(rich2)) #site
rich2[765]

####   Year 1
## Q:: which site had the most species found in at least three quadrats at that site?

a <- rowSums(justsp>=3) #equal to or greater than

which(a==max(a)) #which site
a[which(a == max(a))] #no. species 


## Q: which species was found at the most sites?
a <- colSums(justsp>0)

a[which(a==max(a))] #569 site (Hol.huy)

## Q: which species was found over the greatest geographical range?

latdat <- sapply(justsp, function (x) ifelse (x>0,dat1$lat, x)) #year 1 

df<-data.frame(latdat)

georange <- sapply(df, function(x) max(x) - min(x[x>0]))

#print(georange)

b <- georange[which(georange == max(georange))]
b
length(b)

## Q: which species was found over the smallest geographical range?

georange[which(georange == min(georange))]

t <- dat1%>%
  dplyr::select(X, lat, Tef.qex)%>%
  filter(Tef.qex >0)%>%
  glimpse()
t$lat[which(t$lat==max(t$lat))]
t$lat[which(t$lat==min(t$lat))]
################################################################
####   Year 2
## Q:: which site had the most species found in at least three quadrats at that site?

a <- rowSums(justsp2>=3) #equal to or greater than
which(a==max(a)) #should print all the sites max if more than 1 is equal to another
a[which(a == max(a))] 

## Q:: which site had the most species found in at least two quadrats at that site?
a <- rowSums(justsp2>=2)
which(a==max(a)) #site #
a[which(a == max(a))] # No. sp in at least 2 quads


## Q:: which site had the most species found in at least five quadrats at that site?
a <- rowSums(justsp2>=5)
which(a==max(a)) #site #
a[which(a == max(a))] # No. sp in at least 2 quads


## Q: which species was found at the most sites?
a <- colSums(justsp2>0)

a[which(a==max(a))] #164 sites - species 5

## Q: which species was found over the greatest geographical range?

a <- justsp2 %>%
  select(-c(Ved.luv))%>%
  glimpse()

latdat <- sapply(a, function (x) ifelse (x>0,dat2$lat, x)) #year 1 

df<-data.frame(latdat)

georange <- sapply(df, function(x) max(x) - min(x[x>0]))

#print(georange)

b <- georange[which(georange == max(georange))]
b
## Q: which species was found over the smallest geographical range?

georange[which(georange == min(georange))]

georange

g <- dat2%>%
  dplyr::select(X, lat, Gar.kub)%>%
  filter(Gar.kub >0)%>%
  glimpse()
g$lat[which(g$lat==max(g$lat))]
g$lat[which(g$lat==min(g$lat))]

######
# if he asks: range expansions and contractions
# use above to make a data frame and then calculate the difference between
#q6
q <- dat1 %>%
  dplyr::filter(lat <20)%>%
  glimpse()
min(q$lat)
max(q$lat)

qsp <- q %>%
  dplyr::select(-c(X, lat))%>%
  glimpse()
qmean <- colMeans(qsp)

qmean[which(qmean == max(qmean))]

q2 <- dat2 %>%
  dplyr::filter(lat <20)%>%
  glimpse()
min(q2$lat)
max(q2$lat)

qsp2 <- q2 %>%
  dplyr::select(-c(X, lat))%>%
  glimpse()
qmean2 <- colMeans(qsp2)

qmean2[which(qmean2 == max(qmean2))]

qmeana <- data.frame(cbind(qmean,qmean2))%>%
  dplyr::mutate(result= qmean - qmean2)

qmeana$result[which(qmeana$result == max(qmeana$result))] #value
which(qmeana$result==max(qmeana$result)) #position

qmeana$result[which(qmeana$result == min(qmeana$result))] #value
which(qmeana$result==min(qmeana$result)) #position

qmean[653]
qmean2[653]

2.225225-2.995495

plot(dat1$lat~rich1)
plot(rich1~dat1$lat)

#############################
## greatest contraction in range
range1 <- justsp %>%
  dplyr::select(-c(Ved.luv))

latdat <- sapply(range1, function (x) ifelse (x>0,dat1$lat, x)) #year 1 

df<-data.frame(latdat)

georange <- sapply(df, function(x) max(x) - min(x[x>0]))

#print(georange)

#b <- georange[which(georange == max(georange))]

range2 <- justsp2 %>%
  dplyr::select(-c(Ved.luv))%>%
  glimpse()

latdat2 <- sapply(range2, function (x) ifelse (x>0,dat2$lat, x)) #year 1 

df2<-data.frame(latdat2)

georange2 <- sapply(df2, function(x) max(x) - min(x[x>0]))

#print(georange)
range <- data.frame(cbind(georange, georange2))%>%
  dplyr::mutate(result = georange - georange2)

range$result[which(range$result == min(range$result))] #value -3.12038
which(range$result==min(range$result)) #position 129 + Ved.luv = 130
colnames(dat1)[130]
range$result[which(range$result == max(range$result))] #value -3.12038
which(range$result==max(range$result)) #position 100
range[100]
georange[129]
georange2[129]
9.935969-6.815589
